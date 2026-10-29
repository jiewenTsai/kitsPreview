#' Person abilities (WLE) that can always be computed
#'
#' The same as `dexter::ability(dataSrc, parms, method = "WLE")`,
#' with kits' column names (see below) and the column `raw_max`.
#' The difference: dexter's WLE sometimes does not converge
#' (very few items, or scores at either end), and then the whole item pattern gets NA.
#' For those persons this function computes the same WLE in another way, so every person gets a value.
#'
#' Why the other way gives the same value:
#' the PCM WLE (Warm, 1989) maximizes "log-likelihood + 1/2 log test information".
#' This is also the posterior mode (MAP) under the Jeffreys prior (proportional to sqrt(test information)).
#' So the Jeffreys MAP is the WLE.
#' Checked on KIT motor (M03, 47,136 person-waves): the two differ by at most 2e-7.
#'
#' How the Jeffreys MAP is computed (only with dexter's public functions):
#' * The log-likelihood uses `dexter::p_score()`: the probability of each raw score.
#'   In the PCM the raw score is a sufficient statistic,
#'   so log(probability of this raw score) is the log-likelihood (up to a constant).
#' * The test information uses `dexter::information()`.
#' * `stats::optimize()` finds the maximum between (the smallest beta of these items - 20) and (the largest + 20).
#' * SE: Warm's formula sqrt(I + (I' / 2I)^2) / I, as in dexter's WLE (I' by numerical differentiation).
#'
#' When this happens, a message says for how many persons.
#'
#' @section Names of the columns:
#' dexter calls the person-wave `person_id`, the item pattern (the combination of items answered)
#' `booklet_id` and the raw score `booklet_score`. kitsPreview gives the data to dexter with
#' dexter's names and returns kits' names: `visit_id` (one child at one visit), `pattern_id` and
#' `raw` (and `subj_id` for the child). Only the original output, `kits_original(s)` (dexter's own
#' table), keeps dexter's names.
#'
#' Until kitsPreview 0.22.0 the columns were `person_id`, `pid`, `booklet_id`, `booklet_score`
#' and `booklet_max`. Data and `persons` need the new names `visit_id` and `subj_id`.
#'
#' @param data Responses: a matrix (one row per person-wave, one column per item,
#'   row names are visit_id, column names are item ids, NA means not administered),
#'   or a long data.frame with `visit_id`, `item_id`, `item_score`.
#'   The same input as `dexter::ability()` (whose argument is `dataSrc`), with `visit_id` for
#'   dexter's `person_id`.
#'   Include only the items of this domain.
#' @param item The item parameters: the result of [kits_item()], of `dexter::fit_enorm()`,
#'   or a data.frame with `item_id`, `item_score`, `beta`.
#' @param persons Optional person information to add to the result (as in kits): a data.frame with
#'   `visit_id` and any of `subj_id` (the child id, the same in every wave), `wave_m` (wave,
#'   months), `age_m` (age, months), `sex`, `cohort`. These columns are joined by `visit_id`;
#'   other columns are not kept.
#'   The norm functions then take the scores directly: `kits_norm_screen(s)`, `kits_norms(s)`,
#'   `s |> kits_norm_z(norm)`; and `s |> kits_subj_table()` gives the table of each wave.
#' @param ... In `kits_subj()`: not used; any argument here is an error. In `print()`: passed to
#'   `print()` for a data.frame.
#' @return A data.frame with one row per person-wave, in the order of `data`:
#'   * `pattern_id`: the item pattern, dexter's `booklet_id`.
#'   * `visit_id`: the person-wave, dexter's `person_id`.
#'   * `raw`: the raw score, dexter's `booklet_score`.
#'   * `raw_max`: the highest possible raw score on the items this person-wave answered.
#'     `raw == raw_max` means a perfect score.
#'   * `theta`, `se`: the WLE and its SE, as in `dexter::ability()`.
#'   * `infit`, `outfit`: person fit, the mean squares of this person's residuals (see [kits_itemfit()]).
#'     About 1 is as the model expects; well above 1 means an unexpected response pattern.
#'     NA for a zero or a perfect score.
#'     With few items the values are much below 1 even when the model holds
#'     (in simulated data, a median infit of 0.45 with 5 items, 0.87 with 10, 0.93 with 20),
#'     so compare persons who answered the same form.
#'   * `flag`: `"zero"` (zero score), `"perfect"` (perfect score), `"misfit"` (infit or outfit above 2,
#'     which Linacre calls "distorts or degrades the measurement"), or `"ok"`.
#'   * With `persons`, then its columns `subj_id`, `wave_m`, `age_m`, `sex`, `cohort` (those present).
#'
#'   The scores keep what [kits_subj_table()] needs (`attr(, "raw_theta")`: the item parameters and
#'   the items of each item pattern), so `s |> kits_subj_table()` gives the raw-score table of each
#'   wave.
#'   To put the scores on another scale, convert `theta` and `se` with the line of a link:
#'   `s$theta <- link$intercept + link$slope * s$theta`, `s$se <- link$slope * s$se`.
#' @section The original code:
#' `dexter::ability()`, plus the Jeffreys MAP where it does not converge (see above):
#' ```
#' parms <- coef(item)[c("item_id", "item_score", "beta")]
#' subj <- dexter::ability(X, parms, method = "WLE")
#' ```
#' The package's own table is kept as `attr(, "original")` (see [kits_original()]), with dexter's
#' names; `summary()` prints both.
#' The result has the class `kitsPreview_subj` (the same columns as `kits::kits_subj()`, with its own class name).
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' item <- kits_item(X)
#' head(kits_subj(X, item))
#'
#' # With the waves and ages of the person-waves
#' persons <- data.frame(visit_id = rownames(X), wave_m = rep(c(12, 24), each = 150),
#'                       age_m = rep(c(12.2, 24.3), each = 150))
#' s <- kits_subj(X, item, persons = persons)
#' head(s)
#'
#' # The same values as dexter (dexter's own names)
#' head(dexter::ability(X, item$fit, method = "WLE"))
#' @export
kits_subj <- function(data, item, persons = NULL, ...) {
  no_dots("kits_subj", ...)
  resp <- resp_long(data, "data")
  parms <- param_frame(item)
  need_items(resp, parms)
  wle <- wle_dexter(resp, parms)
  est <- wle$est
  item_max <- tapply(parms$item_score, parms$item_id, max)
  booklet_max <- tapply(item_max[as.character(resp$item_id)], resp$person_id, sum)
  est$booklet_max <- as.vector(booklet_max[as.character(est$person_id)])

  # 個人適配：0 分和滿分的人沒有意義，給 NA
  r <- pcm_moments(resp, parms, stats::setNames(est$theta, est$person_id))
  ms <- mean_squares(r$item_score, r$E, r$W, r$person_id)
  inner <- est$booklet_score > 0 & est$booklet_score < est$booklet_max
  est$infit <- ifelse(inner, unname(ms$infit[as.character(est$person_id)]), NA_real_)
  est$outfit <- ifelse(inner, unname(ms$outfit[as.character(est$person_id)]), NA_real_)
  est$flag <- ifelse(est$booklet_score == 0, "zero", ifelse(est$booklet_score == est$booklet_max, "perfect",
                     ifelse(pmax(est$infit, est$outfit) > 2, "misfit", "ok")))
  est <- est[c("booklet_id", "person_id", "booklet_score", "booklet_max", "theta", "se", "infit", "outfit", "flag")]
  est <- kits_names(est)   # dexter 的名字 → kits 的名字
  est <- preview_table(join_persons(est, persons), "kitsPreview_subj")
  attr(est, "original") <- wle$original
  attr(est, "raw_theta") <- raw_theta_record(resp, parms, est)
  est
}

#' @rdname kits_subj
#' @param x A `kitsPreview_subj`.
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param n Rows to print: by default all rows of a table of up to 30 rows, else the first 10 (see
#'   [print.kitsPreview_table()]).
#' @export
print.kitsPreview_subj <- function(x, digits = 2, n = NULL, ...) {
  need_digits(digits)
  need_rows(n)
  cat(sprintf("<kits_subj> %d person-waves; WLE (dexter)\n", nrow(x)))
  if (!whole_subj(x)) {   # 使用者選過或排過欄：照使用者的欄和順序印，不加說明的行（和 kits 相同）
    return(print_rows(x, digits, n, ...))
  }
  main <- intersect(c("visit_id", "wave_m", "age_m", "theta", "se", "z", "pct", "flag"), names(x))
  print_rows(as.data.frame(x)[main], digits, n, ...)
  others <- setdiff(names(x), main)
  if (length(others) > 0) cat(sprintf("Other columns: %s\n", paste(others, collapse = ", ")))
  if (!is.null(attr(x, "raw_theta"))) {
    cat("Also kept: the raw score -> theta of each item pattern ",
        "(kits_subj_table(x) gives the table of each wave).\n", sep = "")
  }
  cat("Next: kits_norms(x), kits_norm_z(x, norm), kits_subj_table(x), kits_vertical(x), ",
      "summary(x).\n", sep = "")
  invisible(x)
}

# kits_subj() 的整張表：計分的欄都在，順序也沒變（select() 或 relocate() 之後不是；和 kits 相同）
whole_subj <- function(x) {
  core <- c("pattern_id", "visit_id", "raw", "raw_max", "theta", "se", "infit", "outfit", "flag")
  all(core %in% names(x)) && identical(intersect(names(x), core), core)
}

# dexter 的欄名 → kits 的欄名（給使用者的表）
DEXTER_NAMES <- c(booklet_id = "pattern_id", person_id = "visit_id", booklet_score = "raw",
                  booklet_max = "raw_max")

kits_names <- function(d) {
  hit <- names(d) %in% names(DEXTER_NAMES)
  names(d)[hit] <- DEXTER_NAMES[names(d)[hit]]
  d
}

# 人的資料的欄（和 kits 相同）：照 visit_id 接在 flag 後面
KEEP_PERSON_COLS <- c("subj_id", "wave_m", "age_m", "sex", "cohort")

join_persons <- function(est, persons) {
  if (is.null(persons)) {
    return(est)
  }
  if (!is.data.frame(persons)) {
    stop("`persons` must be a data.frame with visit_id and any of subj_id, wave_m, age_m, sex, ",
         "cohort.",
         call. = FALSE)
  }
  if (!"visit_id" %in% names(persons)) {
    stop("`persons` has no column visit_id (it has: ", paste(names(persons), collapse = ", "),
         "). Name the person-wave column visit_id.", call. = FALSE)
  }
  cols <- intersect(KEEP_PERSON_COLS, names(persons))
  if (length(cols) == 0) {
    has <- paste(names(persons), collapse = ", ")
    stop("`persons` has none of the columns subj_id, wave_m, age_m, sex, cohort (it has: ", has,
         "). Rename them, e.g. dplyr::rename(persons, wave_m = wave).", call. = FALSE)
  }
  unused <- setdiff(names(persons), c("visit_id", cols))
  if (length(unused) > 0) {
    message("Column(s) of `persons` not used: ", paste(unused, collapse = ", "),
            ". kits_subj() adds only subj_id (child id), wave_m (wave, months), age_m (age, months), ",
            "sex and cohort.")
  }
  ids <- as.character(persons$visit_id)
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("`persons$visit_id` has missing or repeated ids; give one row per person-wave.",
         call. = FALSE)
  }
  for (v in intersect(c("wave_m", "age_m"), cols)) {
    if (!is.numeric(persons[[v]])) stop("`persons$", v, "` must be numbers (months).",
                                        call. = FALSE)
  }
  at <- match(as.character(est$visit_id), ids)
  if (anyNA(at)) {
    stop(sum(is.na(at)), " person-wave(s) of the data have no row in `persons`.", call. = FALSE)
  }
  est[cols] <- as.data.frame(persons)[at, cols, drop = FALSE]
  est
}

# 分數表記下原始分換算表要的東西（給 kits_subj_table(scores)）：題目參數、每個作答組合的題目、
# 是哪些人次的分數（疊表或別的分數混進來時才認得出來）。都是摘要，沒有孩子的作答。
# est 已經是 kits 的名字（pattern_id、visit_id）；resp 是內部的長格式（person_id）
raw_theta_record <- function(resp, parms, est) {
  sets <- person_sets(resp)
  patterns <- data.frame(pattern_id = est$pattern_id, set = sets$set[match(est$visit_id,
                                                                           sets$person_id)]) |>
    dplyr::distinct()
  if (anyDuplicated(patterns$pattern_id)) {
    return(NULL)
  }
  list(parms = parms, patterns = patterns, persons = as.character(est$visit_id))
}

# dexter 的 WLE；沒有收斂的人用 Jeffreys MAP 補上。回傳 list(est, original)。
wle_dexter <- function(resp, parms) {
  original <- withCallingHandlers(
    as.data.frame(dexter::ability(resp, parms, method = "WLE")),
    warning = function(w) if (grepl("WLE estimates do not converge", conditionMessage(w))) invokeRestart("muffleWarning"))
  est <- original[order(match(as.character(original$person_id), as.character(unique(resp$person_id)))), ]
  rownames(est) <- NULL

  bad <- which(!is.finite(est$theta) | !is.finite(est$se))
  if (length(bad) > 0) {
    # 只和「題目組合 × 原始分」有關，所以每個組合只算一次
    items <- split(as.character(resp$item_id), as.character(resp$person_id))
    sets <- vapply(items[as.character(est$person_id[bad])], function(x) paste(sort(x), collapse = "\u001f"), character(1))
    key <- paste(sets, est$booklet_score[bad])
    first <- !duplicated(key)
    fill <- vapply(which(first), function(j) jeffreys_map(strsplit(sets[j], "\u001f", fixed = TRUE)[[1]], est$booklet_score[bad[j]], parms),
                   numeric(2))
    hit <- match(key, key[first])
    est$theta[bad] <- fill["theta", hit]
    est$se[bad] <- fill["se", hit]
    message(length(bad), " person(s): dexter's WLE did not converge; computed the same WLE as the Jeffreys MAP (see ?kits_subj).")
  }
  list(est = est, original = original)
}

# 每個作答組合每個原始分的 WLE（0.20.0 的 kits_wle_table() 的計算，數字不變）：
# 和 dexter::ability_tables(parms, design, method = "WLE") 相同；dexter 算不出來的列用 Jeffreys MAP 補上。
# design：pattern_id、item_id。交給 dexter 之前換成 dexter 的名字 booklet_id，結果換回 pattern_id、raw
wle_table <- function(item, design = NULL) {
  parms <- item
  if (inherits(parms, "kitsPreview_fit")) parms <- parms$item
  if (inherits(parms, "kitsPreview_item")) {
    if (is.null(design)) design <- parms$design   # 包括沒放進校準的固定題
    parms <- parms$thresholds
  }
  if (is.null(design)) {
    design <- if (inherits(parms, "enorm")) {
      kits_names(parms$inputs$design[, c("booklet_id", "item_id")])
    } else {
      data.frame(pattern_id = "all", item_id = unique(param_frame(parms)$item_id))
    }
  }
  design <- as.data.frame(design)
  need_cols(design, c("pattern_id", "item_id"), "design")
  parms <- param_frame(parms)
  miss <- setdiff(design$item_id, parms$item_id)
  if (length(miss) > 0) {
    stop("Item(s) in `design` have no parameters in `parms`: ", paste(utils::head(miss, 10), collapse = ", "), ".", call. = FALSE)
  }
  design_dexter <- data.frame(booklet_id = design$pattern_id, item_id = design$item_id)
  tab <- withCallingHandlers(
    as.data.frame(dexter::ability_tables(parms, design = design_dexter, method = "WLE")),
    warning = function(w) if (grepl("WLE estimates do not converge", conditionMessage(w))) invokeRestart("muffleWarning"))
  tab <- preview_table(kits_names(tab[, c("booklet_id", "booklet_score", "theta", "se")]))

  bad <- which(!is.finite(tab$theta) | !is.finite(tab$se))
  if (length(bad) > 0) {
    items <- split(as.character(design$item_id), as.character(design$pattern_id))
    fill <- vapply(bad, function(i) jeffreys_map(items[[as.character(tab$pattern_id[i])]], tab$raw[i], parms),
                   numeric(2))
    tab$theta[bad] <- fill["theta", ]
    tab$se[bad] <- fill["se", ]
  }
  rownames(tab) <- NULL
  tab
}


# kits_item()、kits() 的結果、enorm 的結果或 data.frame → item_id、item_score、beta
param_frame <- function(parms) {
  if (inherits(parms, "kitsPreview_fit")) parms <- parms$item
  if (inherits(parms, "kitsPreview_item")) parms <- parms$thresholds   # 也包括沒放進校準的固定題
  if (inherits(parms, "enorm")) {
    need_cml(parms)
    parms <- stats::coef(parms)
  }
  if (!is.data.frame(parms)) {
    stop("The item parameters must be the result of kitsPreview::kits_item(), of dexter::fit_enorm(), or a data.frame with item_id, item_score, beta. ",
         "For a result of kits::kits_item(), give coef(item).", call. = FALSE)
  }
  need_cols(parms, c("item_id", "item_score", "beta"), "parms")
  parms <- data.frame(item_id = as.character(parms$item_id), item_score = suppressWarnings(as.integer(as.character(parms$item_score))),
                      beta = parms$beta)
  if (!is.numeric(parms$beta) || any(!is.finite(parms$beta))) {
    stop("`parms$beta` must be finite numbers (no NA or Inf).", call. = FALSE)
  }
  if (any(is.na(parms$item_score) | parms$item_score < 1)) {
    stop("`parms$item_score` must be whole numbers 1, 2, ... (one row per step).", call. = FALSE)
  }
  if (anyDuplicated(parms[c("item_id", "item_score")])) {
    stop("`parms` has repeated item steps.", call. = FALSE)
  }
  parms
}

# 只支援 CML 的校準結果
need_cml <- function(fit) {
  if (!identical(fit$inputs$method, "CML")) {
    stop("Only CML calibrations are supported (the default method = \"CML\" of kits_item() and dexter::fit_enorm()).", call. = FALSE)
  }
}

# resp 的題目都要有參數
need_items <- function(resp, parms) {
  miss <- setdiff(unique(as.character(resp$item_id)), as.character(parms$item_id))
  if (length(miss) > 0) {
    stop(length(miss), " item(s) in the responses have no parameters in `parms`: ", paste(utils::head(miss, 10), collapse = ", "),
         if (length(miss) > 10) ", ..." else "", ". Include only the items of this domain.", call. = FALSE)
  }
  item_max <- tapply(parms$item_score, parms$item_id, max)
  high <- resp$item_score > item_max[as.character(resp$item_id)]
  if (any(high)) {
    bad <- unique(paste0(resp$item_id[high], ":", resp$item_score[high]))
    stop("Score(s) above the highest step in `parms`: ", paste(utils::head(bad, 10), collapse = ", "),
         ". Recode the data, or calibrate again.", call. = FALSE)
  }
  steps_of <- split(parms$item_score, parms$item_id)
  gap <- names(steps_of)[vapply(steps_of, function(k) !identical(sort(as.integer(k)), seq_len(max(k))), logical(1))]
  if (length(gap) > 0) {
    stop("Item(s) in `parms` with missing steps (steps must be 1, 2, ...): ", paste(utils::head(gap, 10), collapse = ", "), ".", call. = FALSE)
  }
}

# 一個題目組合、一個原始分的 Jeffreys MAP（＝ WLE）。parms 已經是 param_frame() 的結果。
# 找的範圍：這些題的 β 往外各 20（PCM 的 WLE 一定在這裡面）
jeffreys_map <- function(items, raw, parms) {
  beta <- parms$beta[parms$item_id %in% items]
  range <- c(min(beta) - 20, max(beta) + 20)
  lik <- dexter::p_score(parms, items = items)        # 每個原始分的機率
  inf <- dexter::information(parms, items = items)    # 測驗訊息量
  target <- function(theta) log(lik(theta)[, raw + 1]) + 0.5 * log(inf(theta))
  theta <- stats::optimize(target, range, maximum = TRUE, tol = 1e-10)$maximum
  if (min(abs(theta - range)) < 1e-3) {
    warning("The Jeffreys MAP for raw score ", raw, " on items ", paste(items, collapse = ", "),
            " is at the edge of the search range; it may be wrong.", call. = FALSE)
  }
  I <- inf(theta)
  h <- 1e-4
  dI <- (inf(theta + h) - inf(theta - h)) / (2 * h)
  c(theta = theta, se = sqrt(I + (dI / (2 * I))^2) / I)
}
