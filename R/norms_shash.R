# shash（sinh-arcsinh）用四個數描述一個月齡的分布：mu 位置、sigma 尺度、epsilon 偏態、delta 尾巴（內部用）。
# 公式和 cNORM 的 qshash() 相同。
# 不直接用 cNORM 的原因：在尾端，cNORM 先算機率再轉成 z，會溢位。
# 例如 shash_z(200, 50, 8, 0.3, 1.4) 是 108，但 qnorm(cNORM::pshash(...)) 是 Inf。

# z 分數 → 分數
shash_quantile <- function(z, mu, sigma, epsilon, delta) {
  mu + sigma * sinh((asinh(z) - epsilon) / delta)
}

# 分數 → z 分數（shash_quantile() 的反函數）；分數可以是 0 或負的
shash_z <- function(D, mu, sigma, epsilon, delta) {
  sinh(delta * asinh((D - mu) / sigma) + epsilon)
}


#' Fit a continuous norm (cNORM shash)
#'
#' Uses cNORM's `cnorm.shash()`.
#' Each of the four parameters is a polynomial in age.
#' Age is first transformed by `age_x` (by default the square root).
#' Scores can be 0 or negative; there is no need to add a constant first.
#' Norms are given only within the age range that has data.
#'
#' When the polynomial degrees are too high, the curves can go out of control at both ends of the age range.
#' After fitting, check whether the mean same-age z of each wave is close to 0.
#'
#' `cnorm.shash()` has several local solutions.
#' With the default start value, it sometimes stops at a broken solution.
#' There the tail parameter delta becomes very large, and the whole distribution is squeezed into a narrow band.
#' (KIT's cognition, all children: bad solution delta up to 52, -log-likelihood 184,112;
#' good solution delta 0.73-0.93, -log-likelihood 179,571.)
#' So one fit is run for each start value in `delta_starts`, and the solution with the largest likelihood is kept.
#' Only the cNORM warnings of the kept solution are shown.
#' Warnings from start values that are not kept are not shown.
#' When the kept solution's delta still exceeds `delta_max`, a warning is shown.
#'
#' The age range can be computed with [kits_norm_ages()]; then use `pad = 0`.
#' Screen the waves first with [kits_norm_screen()].
#' With `pad = 0`, data outside the range are not used, so you need not filter them yourself.
#'
#' The first argument chooses the form (as in kits):
#' * scores (numbers): `kits_norms(d, age_m, ages, ...)`;
#' * the scores of [kits_subj()] (with `age_m` from its `persons`): `kits_norms(s, ages, ...)` uses
#'   their `theta` (convert it to D first, `s$theta <- d_link$intercept + d_link$slope * s$theta`) and `age_m`.
#'   Then `ages` can be left out: the rule of [kits_norm_ages()] sets them (from the 1st percentile
#'   of age in the first wave to the 99th in the last), with `pad = 0` unless `pad` is given.
#'
#' Norms by group: `by` fits one norm per group, for example by sex, or by domain and sex when the
#' scores of several domains are stacked with a `domain` column.
#' Each group gets exactly the norm that `kits_norms()` gives on the children of that group alone
#' (the same settings), and the result is one table with the group columns in front.
#' With scores of [kits_subj()], `by` names their columns (`by = "sex"`, `by = c("domain", "sex")`);
#' with scores as numbers, `by` is a data.frame of the group columns, one row per score.
#' [kits_norm_z()] then puts each child on the curves of its group, and `plot()` draws one panel per
#' domain x group. When fitting norms by sex, each group has half the children.
#' With the default degrees, cNORM sometimes warns (Hessian, parameters at the boundary).
#' Then try lower degrees, for example `c(4, 3, 2, 1)`.
#'
#' @param d The score of each person-wave (the D score), or the scores of [kits_subj()].
#' @param age_m Age in months of each person-wave (not with the scores of [kits_subj()]: it is
#'   taken from their column).
#' @param ages The ages at which to output the norms. With the scores of [kits_subj()] it can be
#'   left out (see above).
#' @param degrees The degrees of the four polynomials for mu, sigma, epsilon, delta.
#' @param age_x The transformation function for age.
#' @param pct The percentiles to output.
#' @param pad Use data up to this many months outside the range of `ages`.
#' @param delta_starts Start values for delta (the `delta` of `cnorm.shash()`).
#' @param delta_max When the kept solution's delta exceeds this number, a warning is shown.
#' @param by Optional groups, one norm each: with the scores of [kits_subj()], the names of their
#'   group columns (for example `"sex"`); with scores as numbers, a data.frame of the group columns
#'   (one row per score). No group may be NA.
#' @param ... In `kits_norms()`: not used; any argument here is an error. In `print()`: passed to
#'   `print()` for a data.frame.
#' @return A `kitsPreview_norms` (a data.frame; `plot()` draws the percentile curves) with one row per age:
#'   `age_m`, `mu`, `sigma`, `epsilon`, `delta` (the shash parameters at this age:
#'   location, scale, skewness, tail weight; the same as in cNORM),
#'   `n`, `neg_loglik` (the -log-likelihood of the kept solution),
#'   and the percentiles `P3`, `P5`, ...
#'   Pass it to [kits_norm_z()] for same-age z-scores.
#'   With `by`, the group columns come first, one block of rows per group; `attr(, "by")` names them
#'   and `attr(, "groups")` keeps the norm of each group, a list named by the group (for example
#'   `attr(norm, "groups")$boy`; with several group columns the values are joined by a dot,
#'   `"lan.boy"`).
#'
#'   The norm keeps cNORM's own result, the `cnorm.shash()` fit of the kept start: `kits_original(norm)`
#'   (a named list of them, one per group, for a norm by group). It holds the coefficients and a few
#'   summaries of the data (N, min, max), not the scores of the children.
#' @examples
#' \donttest{
#' set.seed(1)
#' age <- runif(600, 3, 36)
#' d <- 20 + 15 * log(age) + rnorm(600, 0, 3)
#' norm <- kits_norms(d, age, ages = seq(3, 36, by = 3), degrees = c(3, 2, 1, 1))
#' norm[, c("age_m", "P10", "P50", "P90")]
#' p <- plot(norm, scores = data.frame(theta = d, age_m = age))
#' if (interactive()) p
#'
#' # One norm per sex: one table, the group column first
#' sex <- rep(c("boy", "girl"), 300)
#' by_sex <- kits_norms(d, age, ages = seq(3, 36, by = 3), degrees = c(3, 2, 1, 1),
#'                      by = data.frame(sex = sex))
#' by_sex[c(1, 13), c("sex", "age_m", "P50")]
#'
#' # From the scores of kits_subj(): the ages of the norms from the data
#' X <- kits_sim(rnorm(600), list(a1 = c(-1, 0), a2 = 0.5, a3 = -0.5, a4 = 0), seed = 1)
#' persons <- data.frame(visit_id = rownames(X), age_m = age, wave_m = round(age / 6) * 6)
#' s <- kits_subj(X, kits_item(X), persons = persons)
#' s$theta <- d                 # here: the scores converted to D
#' norm_s <- kits_norms(s, degrees = c(3, 2, 1, 1))
#' range(norm_s$age_m)
#' }
#' @export
kits_norms <- function(d, ...) UseMethod("kits_norms")

#' @rdname kits_norms
#' @export
kits_norms.default <- function(d, age_m, ages, degrees = c(5, 4, 3, 2), age_x = sqrt,
                               pct = NORM_PCT,
                               pad = 1, delta_starts = c(0.5, 1, 1.5,
                                                         2), delta_max = 3, by = NULL, ...) {
  no_dots("kits_norms", ...)
  need_d_vector(d, "kits_norms")
  if (missing(ages)) {
    stop("Give the ages of the norms: kits_norms(d, age_m, ages).", call. = FALSE)
  }
  settings <- list(degrees = degrees, age_x = age_x, age_x_text = age_x_code(substitute(age_x),
                                                                             age_x),
                   pct = pct, pad = pad, delta_starts = delta_starts, delta_max = delta_max)
  fit_norms_by(d, age_m, ages, settings, by_frame(by, length(d)))
}

#' @rdname kits_norms
#' @export
kits_norms.kitsPreview_subj <- function(d, ages, degrees = c(5, 4, 3, 2), age_x = sqrt,
                                        pct = NORM_PCT,
                                        pad = 1, delta_starts = c(0.5, 1, 1.5,
                                                                  2), delta_max = 3, by = NULL,
                                        ...) {
  subj_extra_args("kits_norms", "kits_norms(s, ages, ...)", ...)
  set_by <- NULL
  if (missing(ages)) {   # kits_norm_ages() 的規則：第一波的 1% 到最後一波的 99%；範圍外的資料不用（pad = 0）
    ages <- kits_norm_ages(d)
    set_by <- "kits_norm_ages()"
    if (missing(pad)) pad <- 0
  }
  settings <- list(degrees = degrees, age_x = age_x, age_x_text = age_x_code(substitute(age_x),
                                                                             age_x),
                   pct = pct, pad = pad, delta_starts = delta_starts, delta_max = delta_max,
                   set_by = set_by)
  fit_norms_by(d$theta, subj_column(d, "age_m"), ages, settings, by_columns(by, d))
}

# 沒有 by 時一個常模；有 by 時每組一個（和只用這組的孩子的 kits_norms() 相同），疊成一張表，組的欄在前
fit_norms_by <- function(d, age_m, ages, settings, groups) {
  if (is.null(groups)) {
    return(fit_norm(d, age_m, ages, settings))
  }
  if (length(d) != length(age_m)) {
    stop("`d` and `age_m` must have the same length.", call. = FALSE)
  }
  keys <- dplyr::arrange(dplyr::distinct(groups), dplyr::pick(dplyr::everything()))
  id <- match(key_text(groups), key_text(keys))
  norms <- vector("list", nrow(keys))
  names(norms) <- group_names(keys)
  for (i in seq_along(norms)) {   # 不用 map()：錯誤照原樣傳出去，只在前面加上組的名字
    rows <- id == i
    norms[[i]] <- in_group(group_label(keys[i, , drop = FALSE]), fit_norm(d[rows], age_m[rows],
                                                                          ages, settings))
  }
  grouped_norms(norms, keys)
}

# 每組的名字（attr(norm, "groups") 的名字）：組的值，幾欄時用 . 接起來，例如 "boy"、"lan.boy"
group_names <- function(keys) {
  keys |>
    purrr::map(as.character) |>
    purrr::reduce(\(a, b) paste(a, b, sep = "."))
}

# 每一列的組接成一個字串（找每個人次屬於哪一組用）
key_text <- function(groups) {
  groups |>
    purrr::map(as.character) |>
    purrr::reduce(\(a, b) paste(a, b, sep = "\u001f"))
}

# 一組的計算：錯誤和警告前面加上組的名字
in_group <- function(label, expr) {
  tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warning(label, ": ", conditionMessage(w), call. = FALSE)
      invokeRestart("muffleWarning")
    }),
    error = function(e) stop(label, ": ", conditionMessage(e), call. = FALSE))
}

# 組的名字，例如 "domain = lan, sex = boy"
group_label <- function(key) {
  values <- purrr::map_chr(key, \(v) format(v))
  paste(names(key), "=", values, collapse = ", ")
}

# 每組的常模疊成一張表：組的欄在前；設定照第一組（每組相同），加上 by
grouped_norms <- function(norms, keys) {
  rows <- purrr::map2(norms, seq_along(norms), \(norm, i) {   # i：第幾組（norms 有名字）
    key <- keys[rep(i, nrow(norm)), , drop = FALSE]
    rownames(key) <- NULL
    dplyr::bind_cols(key, as.data.frame(norm))
  })
  out <- preview_table(purrr::list_rbind(rows), "kitsPreview_norms")
  attr(out, "kits_args") <- c(attr(norms[[1]], "kits_args"), list(by = names(keys)))
  attr(out, "by") <- names(keys)
  attr(out, "groups") <- norms
  attr(out, "fitted") <- attr(norms[[1]], "fitted")
  out
}

# 數字的分數：by 是一個 data.frame（每個分數一列）；沒給時 NULL
by_frame <- function(by, n) {
  if (is.null(by)) {
    return(NULL)
  }
  if (is.character(by)) {
    stop("With scores as numbers, give `by` as a data.frame of the group columns, ",
         "e.g. by = data.frame(sex = sex); names of columns work with the scores of kits_subj().",
         call. = FALSE)
  }
  if (!is.list(by) || length(by) == 0 || is.null(names(by)) || any(names(by) == "")) {
    stop("`by` must be a data.frame of named group columns, e.g. data.frame(sex = sex).",
         call. = FALSE)
  }
  groups <- as.data.frame(by)
  if (nrow(groups) != n) {
    stop(sprintf("`by` needs one row per score (%d); it has %d.", n, nrow(groups)), call. = FALSE)
  }
  need_groups(groups)
}

# 分數表（kits_subj()）：by 是它的欄名
by_columns <- function(by, s) {
  if (is.null(by)) {
    return(NULL)
  }
  if (!is.character(by) || length(by) == 0 || anyNA(by) || anyDuplicated(by)) {
    stop("`by` must be the names of group columns of the scores, e.g. by = \"sex\".", call. = FALSE)
  }
  miss <- setdiff(by, names(s))
  if (length(miss) > 0) {
    stop("The scores have no column ", paste0("`", miss, "`", collapse = ", "),
         " for `by`; give it in the `persons` of kits_subj(), or add it to the scores.",
         call. = FALSE)
  }
  need_groups(as.data.frame(s)[by])
}

# 組的欄：沒有 NA，文字或數字（因子轉成文字）
need_groups <- function(groups) {
  groups <- groups |>
    purrr::map(\(v) if (is.factor(v)) as.character(v) else v) |>
    as.data.frame()
  has_na <- names(groups)[purrr::map_lgl(groups, anyNA)]
  if (length(has_na) > 0) {
    stop("The group column(s) ", paste(has_na, collapse = ", "), " of `by` have NA; ",
         "give every person-wave a group, or leave those out first.", call. = FALSE)
  }
  groups
}

# 一個常模（0.20.0 的 kits_norms() 的計算，數字不變）
fit_norm <- function(d, age_m, ages, settings) {
  degrees <- settings$degrees
  delta_starts <- settings$delta_starts
  age_x <- settings$age_x
  pad <- settings$pad
  if (length(d) != length(age_m)) {
    stop("`d` and `age_m` must have the same length.", call. = FALSE)
  }
  if (!is.numeric(d) || !is.numeric(age_m)) {
    stop("`d` and `age_m` must be numbers.", call. = FALSE)
  }
  if (length(degrees) != 4 || any(is.na(degrees)) || any(degrees < 1 | degrees != round(degrees))) {
    stop("`degrees` needs 4 whole numbers of at least 1: the degrees for mu, sigma, epsilon, delta.", call. = FALSE)
  }
  if (length(delta_starts) == 0 || any(!is.finite(delta_starts) | delta_starts <= 0)) {
    stop("`delta_starts` needs at least one positive number.", call. = FALSE)
  }
  ages <- sort(unique(ages[is.finite(ages)]))
  ages_in <- range(ages)   # 篩資料用的範圍（kits_code() 需要）
  if (length(ages) < 2) {
    stop("`ages` needs at least 2 ages.", call. = FALSE)
  }
  use <- data.frame(D = d, age_m = age_m)   # 配適用的資料（欄名 D：D 分數）
  use <- use[is.finite(use$D) & is.finite(use$age_m), ]
  use <- use[use$age_m >= min(ages) - pad & use$age_m <= max(ages) + pad, ]
  use <- use[is.finite(age_x(use$age_m)), ]
  if (nrow(use) < 10 * sum(degrees + 1)) {
    stop(nrow(use), " person-waves within the ages; too few for degrees ", paste(degrees, collapse = ", "),
         " (at least ", 10 * sum(degrees + 1), "). Use lower degrees or more data.", call. = FALSE)
  }
  ages <- ages[ages >= floor(min(use$age_m)) & ages <= ceiling(max(use$age_m)) & is.finite(age_x(ages))]
  if (length(ages) < 2 || length(unique(use$age_m)) < 2) {
    stop("The data cover fewer than 2 of the `ages`; a norm over age cannot be fitted.", call. = FALSE)
  }
  model <- best_shash(use, degrees, delta_starts, age_x)
  par <- shash_parameters(model, age_x(ages))
  if (max(par$delta) > settings$delta_max) {
    warning("delta reaches ", round(max(par$delta), 2),
            " even at the best start. Check the percentile curves.",
            call. = FALSE)
  }
  out <- data.frame(age_m = ages, mu = par$mu, sigma = par$sigma, epsilon = par$epsilon,
                    delta = par$delta,
                    n = nrow(use), neg_loglik = model$result$value)
  for (p in settings$pct) {
    out[[paste0("P", p)]] <- shash_quantile(stats::qnorm(p / 100), out$mu, out$sigma, out$epsilon, out$delta)
  }
  attr(out, "kits_args") <- list(ages_in = ages_in, degrees = degrees, pad = pad, delta_starts = delta_starts,
                                 age_x = settings$age_x_text, pct = settings$pct,
                                 delta_max = settings$delta_max)
  attr(out, "fitted") <- list(from = ages_in[1], to = ages_in[2])
  attr(out, "fitted")$set_by <- settings$set_by   # 只有 kits_norm_ages() 定的月齡才有
  attr(out, "original") <- model   # cNORM 的 cnorm.shash() 結果（留下的解；只有係數和摘要，沒有孩子的資料）
  preview_table(out, "kitsPreview_norms")
}

#' @rdname kits_norms
#' @param x A `kitsPreview_norms`.
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param n Rows to print: by default all rows of a table of up to 30 rows, else the first 10 (see
#'   [print.kitsPreview_table()]).
#' @export
print.kitsPreview_norms <- function(x, digits = 2, n = NULL, ...) {
  need_digits(digits)
  need_rows(n)
  cat("<kits_norms> cNORM shash\n")
  fitted <- attr(x, "fitted")
  if (!is.null(fitted)) {
    by <- attr(x, "by")
    cat(sprintf("  norm fitted on %s to %s months%s%s\n", format(fitted$from), format(fitted$to),
                if (is.null(fitted$set_by)) "" else sprintf(" (set by %s)", fitted$set_by),
                if (is.null(by)) "" else sprintf("; one norm per %s", paste(by, collapse = " x "))))
  }
  print_rows(x, digits, n, ...)
  cat("Next: kits_norm_z(scores, x), plot(x), plot(x, scores = scores), kits_original(x).\n")
  invisible(x)
}

# 每個起始值配一次；警告先收起來，只發出留下的那個解的
best_shash <- function(use, degrees, delta_starts, age_x) {
  fits <- lapply(delta_starts, function(d0) {
    warns <- character()
    model <- withCallingHandlers(
      cNORM::cnorm.shash(age = age_x(use$age_m), score = use$D, mu_degree = degrees[1], sigma_degree = degrees[2],
                         epsilon_degree = degrees[3], delta_degree = degrees[4], delta = d0, plot = FALSE),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(model = model, warns = warns)
  })
  best <- fits[[which.min(vapply(fits, function(f) f$model$result$value, numeric(1)))]]
  for (w in unique(best$warns)) warning(w, call. = FALSE)
  best$model
}


#' Plot a continuous norm: percentile curves
#'
#' One norm: its percentile curves.
#' A norm by group ([kits_norms()] with `by`): a row of panels per domain (when `domain` is a group
#' column), a column per other group (for example sex), each group in its own colour.
#'
#' The children are given as their scores, `plot(norm, scores = s)`: a table with `theta` and
#' `age_m` (for example the scores of [kits_subj()]), drawn as grey points behind the curves; with a
#' norm by group, each panel draws the children of its group (the scores need the group columns).
#' Until kitsPreview 0.22.0 the children could also be given as two vectors, `d` and `age_m`;
#' that was removed in 0.23.0 (it stops with an error that names `scores`).
#'
#' kits draws the children binned (no single child in the plot) and has two more arguments,
#' `persons` and `min_cell`; here every child is a point.
#'
#' @param x The result of [kits_norms()] (it can have a `segment` column).
#' @param scores Optional: the scores of the children, a table with `theta` and `age_m`.
#' @param pct Which percentiles to draw; they must be columns of `x`.
#' @param age_ticks Age ticks (for a norm by group).
#' @param y_range The range of the y axis, the same in every panel (for a norm by group); by default
#'   from the data.
#' @param ... Not used; any argument here is an error.
#' @return A plotly plot.
#' @examples
#' norm <- data.frame(age_m = c(6, 12, 18), mu = c(40, 55, 62), sigma = 5, epsilon = 0, delta = 1,
#'                    P3 = c(31, 46, 53), P50 = c(40, 55, 62), P97 = c(49, 64, 71))
#' class(norm) <- c("kitsPreview_norms", "kitsPreview_table", "data.frame")
#' p <- plot(norm, pct = c(3, 50, 97))
#' kids <- data.frame(theta = c(42, 50, 61), age_m = c(7, 11, 17))
#' p <- plot(norm, scores = kids, pct = c(3, 50, 97))
#' if (interactive()) p
#' @export
plot.kitsPreview_norms <- function(x, scores = NULL, pct = c(3, 10, 50, 90, 97),
                                   age_ticks = AGE_TICKS,
                                   y_range = NULL, ...) {
  if ((is.numeric(scores) && !is.data.frame(scores)) || any(c("d", "D", "age_m") %in% ...names())) {
    text <- paste0("plot() of a norm takes the children as `scores`, a table with theta and age_m ",
                   "(for example the scores of kits_subj()); the vectors d and age_m were removed ",
                   "in kitsPreview 0.23.0: plot(norm, scores = data.frame(theta = d, age_m = age_m)).")
    stop_preview(text, "kitsPreview_error_input")
  }
  no_dots("plot", ...)
  if (length(pct) == 0) {
    stop("`pct` needs at least one percentile.", call. = FALSE)
  }
  children <- norm_children(scores)
  if (!is.null(attr(x, "by"))) {
    return(norm_groups_plot(x, pct, age_ticks, y_range, children))
  }
  cols <- paste0("P", pct)
  need_cols(as.data.frame(x), c("age_m", cols), "x")
  p <- plotly::plot_ly()
  if (!is.null(children)) {
    p <- plotly::add_markers(p, x = children$age_m, y = children$theta,
                             marker = list(color = "#BBBBBB", size = 3),
                             name = LAB$persons, hoverinfo = "skip")
  }
  segments <- norm_segments(as.data.frame(x))
  for (s in names(segments)) {
    seg <- segments[[s]]
    for (q in cols) {
      line <- list(color = "#0072B2", width = if (q == "P50") 2.5 else 1,
                   dash = if (q %in% c("P3", "P97")) "dot" else "solid")
      hover <- sprintf("%s, %.1f%s: %.1f", q, seg$age_m, LAB$month, seg[[q]])
      legend <- s == names(segments)[1]
      p <- plotly::add_lines(p, x = seg$age_m, y = seg[[q]], name = q, legendgroup = q,
                             showlegend = legend, line = line, hovertext = hover,
                             hoverinfo = "text")
    }
  }
  plotly::layout(p, title = plot_title(LAB$title_norm_curves), margin = list(t = 70),
                 xaxis = list(title = LAB$age),
                 yaxis = list(title = LAB$D), font = FONT)
}

# 畫在曲線後面的孩子：分數表（theta、age_m，和組的欄）
norm_children <- function(scores) {
  if (is.null(scores)) {
    return(NULL)
  }
  if (!is.data.frame(scores)) {
    stop("`scores` must be the scores of the children: a table with theta and age_m, e.g. from ",
         "kits_subj().",
         call. = FALSE)
  }
  need_cols(scores, c("theta", "age_m"), "scores")
  as.data.frame(scores)
}

# 照組的常模：一列一個領域（domain 是組的欄時），一欄一個其他的組（例如性別）；孩子畫在自己那一組的格子
norm_groups_plot <- function(x, pct, age_ticks, y_range, children) {
  by <- attr(x, "by")
  others <- setdiff(by, "domain")
  panel_keys <- function(t) {
    t$domain <- if ("domain" %in% by) as.character(t$domain) else NA_character_
    t$group <- if (length(others) == 0) NA_character_ else group_text(t[others])
    t
  }
  d <- panel_keys(as.data.frame(x))
  need_cols(d, c("age_m", paste0("P", pct)), "x")
  if (!is.null(children)) {
    need_cols(children, by, "scores")
    children <- panel_keys(children)
  }
  by_text <- if (length(others) > 0) paste0(" by ", paste(others, collapse = " and ")) else ""
  title <- sprintf(LAB$title_norm_groups, by_text, paste0("P", pct, collapse = ", "))
  norm_grid_plot(d, unique(d$domain), unique(d$group), pct, age_ticks, y_range, title, children)
}

# 幾個組的欄接成一個名字（例如 "M03, boy"）；一欄時就是它的值
group_text <- function(cols) {
  cols |>
    purrr::map(as.character) |>
    purrr::reduce(\(a, b) paste(a, b, sep = ", "))
}


# cnorm.shash() 的參數在指定的（已轉換）月齡的值。
# 和 cNORM 內部的 predictCoefficients_shash() 相同；cNORM 沒有匯出它，所以照它的寫法自己算。
shash_parameters <- function(model, x) {
  x_std <- (x - attr(model$result, "age_mean")) / attr(model$result, "age_sd")
  design <- function(degree) cbind(1, stats::poly(x_std, degree = degree, raw = TRUE))
  delta <- if (is.null(model$delta_degree)) rep(model$delta, length(x)) else exp(design(model$delta_degree) %*% model$delta_est)
  data.frame(mu = as.vector(design(model$mu_degree) %*% model$mu_est),
             sigma = as.vector(exp(design(model$sigma_degree) %*% model$sigma_est)),
             epsilon = as.vector(design(model$epsilon_degree) %*% model$epsilon_est),
             delta = as.vector(delta))
}


# age_x 寫成程式：base 的函數用名字（sqrt、log），其他的寫出函數本身，讓產生的程式不依賴呼叫端的物件
age_x_code <- function(expr, fun) {
  if (is.symbol(expr) && exists(as.character(expr), envir = baseenv()) && identical(fun, get(as.character(expr), envir = baseenv()))) {
    return(as.character(expr))
  }
  paste(deparse(fun, control = c("keepInteger", "niceNames")), collapse = "\n")
}
