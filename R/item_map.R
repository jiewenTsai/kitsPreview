# 題目表（kits_item_table，和 kits 0.22.0 同名）：一題一列，D、50% 通過的月齡、常模中位數到達的月齡、DIF、題幹；
# 表裡帶著常模，所以 plot() 直接畫成長曲線（type = "growth"）和成長階梯（type = "ladder"）。

#' Item table: each item with its D, ages, DIF and stem, for the report plots
#'
#' One row per item: the D of its milestone step, the age at which half of the children pass it,
#' the age at which the norm median reaches its D, its sex DIF and its stem. The table keeps the
#' norm, so `plot()` draws the report plots from it:
#' ```
#' item_tab <- kits_item_table(thr, norms, age_pass = ap, dif = dif, stems = stems)
#' item_tab |> plot(type = "growth")   # growth curves with the items
#' item_tab |> plot(type = "ladder")   # the developmental ladder
#' ```
#'
#' The columns:
#' * `D`: the threshold of the milestone step (`item_score == 1`) in `thr`, on the scale of the norm
#'   (usually D: convert the thresholds first, `thr$thr <- d_link$intercept + d_link$slope * thr$thr`).
#'   Items of two cohorts: convert the thresholds of each to D first and stack them; an item given
#'   twice keeps its first row, with a message.
#' * `domain`, `column`: the domain of the item (the `domain` column of `thr`, or the domain of the
#'   norm when it has one), and the column of the ladder (`column` of `thr`, by default the domain;
#'   for example motor split into `"gross"` and `"fine"`).
#' * `age50`: the age at which half of the children pass, from `age_pass` (a [kits_age_pass()]
#'   result, its column `p50`).
#' * `DIF`: `"none"`, `"favors girls"`, `"favors boys"` or `"inconsistent"`, from `dif`: a
#'   [kits_dif_combine()] result (its `DIF` column), or one [kits_dif()] (beyond +/- 0.3 logits,
#'   with `ref = "boy"`). Items without a value, or without `dif`, are `"none"`.
#' * `stem`: the item stem (any language), from `stems`; the item id when not given.
#' * `age_reach`, `below_range`, `above_range`, `empirical`, `x`: the place on the growth chart.
#'   Items with an age at 50% passing use it (`empirical = TRUE`); the others the age at which the
#'   norm median first reaches their D (no interpolation). Items easier than the P50 at the youngest
#'   norm age get `below_range = TRUE`, harder than the P50 at the oldest age `above_range = TRUE`;
#'   such an item without an age at 50% passing has no `x` and is not drawn (the panel notes its
#'   id).
#'
#' `plot()`:
#' * `type = "growth"` (the default): one panel per domain. The grey lines are the norm P10, P50 and
#'   P90; the points are the items at (`x`, `D`), filled at an age at 50% passing and open at
#'   `age_reach`. Colour and shape show the DIF. Hovering shows the item and its stem.
#' * `type = "ladder"`: one column per `column`, each item a line at its D with its D and short
#'   stem; the ticks on the right are the ages (`ages`) at which the mean of the norm medians of the
#'   domains reaches that D.
#'
#' kits has the same function with one more argument, `rp` (another response probability).
#'
#' @param thr The thresholds, on the scale of the norm: a table with `item_id`, `item_score`, `thr`
#'   (for example `coef(item)` converted to D), optionally `domain` and `column`; or a list of such
#'   tables (stacked in order).
#' @param norms The norm: a [kits_norms()] result for one domain, or by domain (`by = "domain"`);
#'   P10, P50 and P90 are used.
#' @param age_pass Optional: a [kits_age_pass()] result.
#' @param dif Optional: a [kits_dif_combine()] or a [kits_dif()] result.
#' @param stems Optional: the item stems, a data.frame with `item_id` and `stem`, or a character
#'   vector named by item.
#' @return A `kitsPreview_item_table` (a data.frame, see above); `attr(, "norm")` keeps the norm.
#' @examples
#' set.seed(1)
#' age <- runif(800, 3, 24)
#' D <- 20 + 15 * log(age) + rnorm(800, 0, 3)
#' norm <- kits_norms(D, age, ages = 3:24, degrees = c(3, 2, 1, 1),
#'                    by = data.frame(domain = rep("lan", 800)))
#' thr <- data.frame(item_id = c("a1", "a2", "a3", "a4"), item_score = 1, thr = c(40, 55, 65, 60),
#'                   domain = "lan")
#' ap <- data.frame(item_id = c("a1", "a2", "a3", "a4"), p50 = c(4.5, 11, NA, 15))
#' dif <- data.frame(item_id = c("a2", "a3", "a4"),
#'                   DIF = c("favors girls", "favors boys", "inconsistent"))
#' stems <- c(a1 = "Lifts head", a2 = "Says two words", a3 = "Names colours",
#'            a4 = "Points to pictures")
#' item_tab <- kits_item_table(thr, norm, age_pass = ap, dif = dif, stems = stems)
#' item_tab
#' p <- plot(item_tab)
#' p <- plot(item_tab, type = "ladder", ages = c(6, 12, 18))
#' if (interactive()) p
#' @export
kits_item_table <- function(thr, norms, age_pass = NULL, dif = NULL, stems = NULL) {
  norm <- table_norm(norms)
  items <- item_steps(thr, norm) |>
    dplyr::left_join(table_ages(age_pass), by = "item_id") |>
    dplyr::left_join(table_dif(dif), by = "item_id") |>
    dplyr::left_join(table_stems(stems), by = "item_id") |>
    dplyr::mutate(DIF = dplyr::coalesce(DIF, "none"), stem = dplyr::coalesce(stem, item_id))
  out <- preview_table(item_positions(items, norm), "kitsPreview_item_table")
  attr(out, "norm") <- norm
  out
}

# 常模：一個領域一個（by = "domain"，或一個常模）；有其他的組時停下來
table_norm <- function(norms) {
  if (!is.data.frame(norms)) {
    stop("`norms` must be a kits_norms() result.", call. = FALSE)
  }
  others <- setdiff(attr(norms, "by"), "domain")
  if (length(others) > 0) {
    stop("The item table uses one norm per domain; this norm is also by ", paste(others,
                                                                                 collapse = ", "),
         ". Fit it by domain only, e.g. kits_norms(scores, by = \"domain\").", call. = FALSE)
  }
  norm <- as.data.frame(norms)
  need_cols(norm, c("age_m", "P10", "P50", "P90"), "norms")
  if (!"domain" %in% names(norm)) norm$domain <- NA_character_
  norm
}

# 每題的 milestone step 的門檻（D）、領域、階梯的欄；同一題給了兩次時留第一列
item_steps <- function(thr, norm) {
  one <- is.data.frame(thr) || inherits(thr, "kitsPreview_item")
  parts <- if (one) list(thr) else thr
  steps <- parts |>
    purrr::map(\(x) if (inherits(x, "kitsPreview_item")) stats::coef(x) else x) |>
    purrr::map(as.data.frame) |>
    purrr::list_rbind()
  need_cols(steps, c("item_id", "item_score", "thr"), "thr")
  steps <- dplyr::filter(steps, item_score == 1)
  twice <- unique(steps$item_id[duplicated(steps$item_id)])
  if (length(twice) > 0) {
    message("Item(s) given twice keep their first row: ", paste(utils::head(twice, 10),
                                                                collapse = ", "), ".")
    steps <- dplyr::filter(steps, !duplicated(item_id))
  }
  domains <- unique(norm$domain)
  if (!"domain" %in% names(steps)) {
    if (length(domains) > 1) {
      stop("The norm has several domains (", paste(domains, collapse = ", "),
           "): give `thr` a `domain` column.",
           call. = FALSE)
    }
    steps$domain <- domains
  }
  if (!"column" %in% names(steps)) steps$column <- steps$domain
  dplyr::transmute(steps, item_id = as.character(item_id), domain, column, D = thr)
}

# 50% 通過的月齡：kits_age_pass() 的 p50 欄，改名叫 age50
table_ages <- function(age_pass) {
  if (is.null(age_pass)) {
    return(data.frame(item_id = character(), age50 = numeric()))
  }
  need_cols(age_pass, c("item_id", "p50"), "age_pass")
  data.frame(item_id = as.character(age_pass$item_id), age50 = age_pass$p50)
}

# DIF：kits_dif_combine() 的 DIF 欄，或一個 kits_dif() 照 0.3 logit 判斷方向
table_dif <- function(dif) {
  if (is.null(dif)) {
    return(data.frame(item_id = character(), DIF = character()))
  }
  if ("DIF" %in% names(dif)) {
    return(data.frame(item_id = as.character(dif$item_id), DIF = as.character(dif$DIF)))
  }
  need_cols(dif, c("item_id", "dif"), "dif")
  side <- dplyr::case_when(dif$dif > 0.3 ~ "favors girls", dif$dif < -0.3 ~ "favors boys",
                           .default = "none")
  data.frame(item_id = as.character(dif$item_id), DIF = side)
}

# 題幹：data.frame（item_id、stem）或用題號當名字的文字向量
table_stems <- function(stems) {
  if (is.null(stems)) {
    return(data.frame(item_id = character(), stem = character()))
  }
  if (is.character(stems) && !is.null(names(stems))) {
    return(data.frame(item_id = names(stems), stem = unname(stems)))
  }
  need_cols(stems, c("item_id", "stem"), "stems")
  data.frame(item_id = as.character(stems$item_id), stem = as.character(stems$stem))
}

#' @rdname kits_item_table
#' @param x A `kitsPreview_item_table`.
#' @param type `"growth"` (the default; growth curves with the items) or `"ladder"` (the
#'   developmental ladder).
#' @param items Which items to draw (item ids); by default all of them.
#' @param ages For `type = "ladder"`: the ages of the ticks on the right.
#' @param age_ticks For `type = "growth"`: the age ticks.
#' @param y_range For `type = "growth"`: the range of the D axis, the same in every panel; by
#'   default from the data.
#' @param ... Not used.
#' @export
plot.kitsPreview_item_table <- function(x, type = c("growth", "ladder"), items = NULL,
                                        ages = c(3, 6, 9, 12, 18, 24, 36, 48, 60, 72, 84),
                                        age_ticks = AGE_TICKS, y_range = NULL, ...) {
  type <- match.arg(type)
  norm <- attr(x, "norm")
  if (is.null(norm)) {
    stop("This table keeps no norm; make it with kits_item_table().", call. = FALSE)
  }
  shown <- as.data.frame(x)[x$item_id %in% pick_items(x$item_id, items), , drop = FALSE]
  domains <- unique(shown$domain)
  if (type == "growth") {
    return(growth_plot(norm, shown, domains, age_ticks, y_range))
  }
  marks <- ladder_marks(norm, domains, ages)
  ladder_plot(shown, unique(shown$column), marks)
}

# 要畫的題：給了題號時只畫這些（題號要在表裡）
pick_items <- function(all, items) {
  if (is.null(items)) {
    return(all)
  }
  miss <- setdiff(items, all)
  if (length(miss) > 0) {
    stop("Item(s) not in the table: ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  intersect(all, items)
}


# 常模中位數第一次到達某個 D 的月齡（內部用）。
# 沿常模表的月齡往上找，回傳第一個 P50 >= D 的月齡；不內插。
# 有好幾段（segment 欄）時，從月齡最小的一段開始找。找不到時回傳 NA。
# D 比最小月齡的 P50 還低時，回傳最小月齡（below_range 會標出來）。
reach_age <- function(D, norm) {
  for (seg in norm_segments(norm)) {
    hit <- which(seg$P50 >= D)
    if (length(hit) > 0) {
      return(seg$age_m[hit[1]])
    }
  }
  NA_real_
}

# 題目在成長曲線上的位置（內部用；kits_item_table()）：
# 有 50% 通過的月齡就用它（empirical），沒有時用常模中位數第一次到達這個 D 的月齡（age_reach）
item_positions <- function(items, norm) {
  need_cols(items, c("domain", "D", "age50"), "items")
  need_cols(norm, c("domain", "age_m", "P50"), "norm")
  miss <- setdiff(unique(items$domain), unique(norm$domain))
  if (length(miss) > 0) {
    stop("Score code(s) in `items` with no norm in `norm`: ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  ends <- norm |>
    dplyr::arrange(age_m) |>
    dplyr::summarise(lowest_p50 = P50[1], highest_p50 = max(P50), .by = domain)
  at <- match(items$domain, ends$domain)
  own_norm <- function(dm) norm[norm$domain %in% dm, ]
  items$age_reach <- purrr::map2_dbl(items$D, items$domain, \(D, dm) reach_age(D, own_norm(dm)))
  items$below_range <- items$D < ends$lowest_p50[at]
  items$above_range <- items$D > ends$highest_p50[at]
  items$empirical <- !is.na(items$age50)
  items$x <- ifelse(items$empirical, items$age50, items$age_reach)
  items
}

# 階梯右邊的月齡刻度（內部用）：在給的月齡，幾個領域的常模中位數（P50）的平均；
# 只留每個領域都有常模的月齡。兩段共用的月齡：每個領域先平均
ladder_marks <- function(norm, domains, ages) {
  need_cols(norm, c("domain", "age_m", "P50"), "norm")
  domains <- unique(domains)
  miss <- setdiff(domains, norm$domain)
  if (length(miss) > 0) {
    stop("Score(s) not in `norm`: ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  by_domain <- as.data.frame(norm) |>
    dplyr::filter(domain %in% domains) |>
    dplyr::mutate(age_m = round(age_m, 6)) |>                  # 3 和 3.0000000000000004 當成同一個月齡
    dplyr::filter(age_m %in% round(ages, 6)) |>
    dplyr::summarise(P50 = mean(P50), .by = c(domain, age_m))
  out <- by_domain |>
    dplyr::filter(dplyr::n() == length(domains), .by = age_m) |>
    dplyr::arrange(age_m, domain) |>
    dplyr::summarise(D = mean(P50), .by = age_m)
  if (nrow(out) == 0) {
    stop("None of `ages` has a norm in every score.", call. = FALSE)
  }
  out
}
