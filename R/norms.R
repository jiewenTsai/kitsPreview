#' Default percentiles of the norms
#'
#' @format A numeric vector: 3, 5, 10, 15, 25, 50, 75, 85, 90, 95, 97.
#' @examples
#' NORM_PCT
#' @export
NORM_PCT <- c(3, 5, 10, 15, 25, 50, 75, 85, 90, 95, 97)


#' Same-age z-score
#'
#' Look up the shash parameters (`mu`, `sigma`, `epsilon`, `delta`) for this age in the norm table.
#' When the age falls between two rows, interpolate.
#' When the norm has several segments (`segment` column), each segment is looked up on its own.
#' Ages outside the norm range return NA.
#'
#' Children with a perfect score have no upper limit, so their z is not meaningful.
#' Set it to NA yourself (`z[raw == raw_max] <- NA`; see the examples).
#'
#' The first argument chooses the form (as in kits):
#' * scores (numbers): `kits_norm_z(d, age_m, norm)` gives the z-scores;
#' * the scores of [kits_subj()] (with `age_m` from its `persons`): `s |> kits_norm_z(norm)` uses
#'   their `theta` and `age_m` and returns the scores with two more columns, `z` and `pct`
#'   (`100 * pnorm(z)`, the percentile rank).
#'
#' Norms by group ([kits_norms()] with `by`): each child is put on the curves of its group.
#' The scores of [kits_subj()] need the group columns (for example `sex`); with scores as numbers,
#' give the groups in `by` (a data.frame, one row per score). Children of a group without a norm get
#' NA, with a message.
#'
#' @param d Scores (the D score), or the scores of [kits_subj()].
#' @param age_m Age in months (not with the scores of [kits_subj()]).
#' @param norm The result of [kits_norms()].
#'   Several segments can be stacked together, marked by a `segment` column.
#' @param by With scores as numbers and a norm by group: a data.frame of the group columns of the
#'   norm, one row per score.
#' @param ... Not used; any argument here is an error.
#' @return With scores as numbers: the z-scores (the percentile rank is `100 * pnorm(z)`).
#'   With the scores of [kits_subj()]: the scores with the columns `z` and `pct`.
#' @examples
#' norm <- data.frame(age_m = c(6, 12), mu = c(40, 60), sigma = 5, epsilon = 0, delta = 1)
#' kits_norm_z(d = c(40, 50, 66), age_m = c(6, 9, 12), norm = norm)
#'
#' # Ages outside the range get no z
#' kits_norm_z(d = 50, age_m = 20, norm = norm)
#'
#' # Perfect scores: no z
#' raw <- c(3, 5, 8)
#' raw_max <- c(8, 8, 8)
#' z <- kits_norm_z(d = c(40, 50, 66), age_m = c(6, 9, 12), norm = norm)
#' z[raw == raw_max] <- NA
#' z
#'
#' # The scores of kits_subj(): the same scores with z and pct
#' X <- kits_sim(seq(-2, 2, length.out = 100), list(a1 = c(-1, 0), a2 = 0.5, a3 = -0.5), seed = 1)
#' persons <- data.frame(visit_id = rownames(X), age_m = seq(6, 12, length.out = 100))
#' s <- kits_subj(X, kits_item(X), persons = persons)
#' s$theta <- 50 + 5 * s$theta    # here: the scores converted to D (intercept + slope x theta)
#' s |>
#'   kits_norm_z(norm) |>
#'   head()
#' @export
kits_norm_z <- function(d, ...) UseMethod("kits_norm_z")

#' @rdname kits_norm_z
#' @export
kits_norm_z.default <- function(d, age_m, norm, by = NULL, ...) {
  no_dots("kits_norm_z", ...)
  need_d_vector(d, "kits_norm_z")
  if (is.null(attr(norm, "by")) && is.null(by)) {
    return(norm_z(d, age_m, norm))
  }
  if (is.null(by)) {
    stop("The norm is by group (", paste(attr(norm, "by"), collapse = ", "),
         "): give the group of each ",
         "score in `by`, e.g. by = data.frame(sex = sex).", call. = FALSE)
  }
  norm_z_by(d, age_m, norm, by_frame(by, length(d)))
}

#' @rdname kits_norm_z
#' @export
kits_norm_z.kitsPreview_subj <- function(d, norm, ...) {
  subj_extra_args("kits_norm_z", "kits_norm_z(s, norm)", ...)
  if (missing(norm)) {
    stop("Give the norm: kits_norm_z(s, norm).", call. = FALSE)
  }
  by <- attr(norm, "by")
  age_m <- subj_column(d, "age_m")
  z <- if (is.null(by)) norm_z(d$theta, age_m, norm) else norm_z_by(d$theta, age_m, norm,
                                                                    by_columns(by, d))
  d[["z"]] <- z
  d[["pct"]] <- 100 * stats::pnorm(z)
  d
}

# 同齡 z（0.20.0 的 kits_norm_z() 的計算，數字不變）
norm_z <- function(d, age_m, norm) {
  need_cols(norm, c("age_m", "mu", "sigma", "epsilon", "delta"), "norm")
  if (!all(vapply(norm[c("age_m", "mu", "sigma", "epsilon", "delta")], is.numeric, logical(1)))) {
    stop("`norm$age_m`, `mu`, `sigma`, `epsilon`, `delta` must be numbers.", call. = FALSE)
  }
  if (length(d) != length(age_m)) {
    stop("`d` and `age_m` must have the same length.", call. = FALSE)
  }
  if (!is.numeric(d) || !is.numeric(age_m)) {
    stop("`d` and `age_m` must be numbers.", call. = FALSE)
  }
  if ("segment" %in% names(norm) && anyNA(norm$segment)) {
    stop("`norm$segment` has NA.", call. = FALSE)
  }
  segments <- norm_segments(norm)
  for (i in seq_along(segments)) {
    tab <- segments[[i]]
    if (nrow(tab) < 2 || anyDuplicated(tab$age_m)) {
      stop("Each norm segment needs at least 2 rows with different ages (segment ",
           names(segments)[i], ").",
           call. = FALSE)
    }
    if (i > 1 && min(tab$age_m) <= max(segments[[i - 1]]$age_m)) {
      stop("Norm segments overlap in age (", names(segments)[i - 1], " and ", names(segments)[i], ").", call. = FALSE)
    }
  }
  z <- rep(NA_real_, length(d))
  for (tab in segments) {
    hit <- !is.na(age_m) & age_m >= min(tab$age_m) & age_m <= max(tab$age_m)
    if (!any(hit)) next
    at_age <- function(v) stats::approx(tab$age_m, tab[[v]], age_m[hit])$y
    z[hit] <- shash_z(d[hit], at_age("mu"), at_age("sigma"), at_age("epsilon"), at_age("delta"))
  }
  z
}

# 照組的常模：每個孩子用自己那一組的曲線；沒有常模的組是 NA（說一聲）
norm_z_by <- function(d, age_m, norm, groups) {
  by <- attr(norm, "by")
  if (is.null(by)) by <- names(groups)   # 自己疊的常模：by 的欄就是組的欄
  miss <- setdiff(by, names(groups))
  if (length(miss) > 0) {
    stop("The norm is by ", paste(by, collapse = ", "), "; the groups of the scores have no ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  table <- as.data.frame(norm)
  need_cols(table, by, "norm")
  if (length(d) != nrow(groups)) {
    stop("`d` and `by` must have the same length.", call. = FALSE)
  }
  norm_key <- key_text(table[by])
  child_key <- key_text(need_groups(groups[by]))
  no_norm <- setdiff(unique(child_key), norm_key)
  if (length(no_norm) > 0) {
    message("No norm for group(s) ", paste(gsub("\u001f", ", ", no_norm), collapse = "; "),
            ": their z is NA.")
  }
  z <- rep(NA_real_, length(d))
  for (k in unique(norm_key)) {
    rows <- which(child_key == k)
    curve <- table[norm_key == k, setdiff(names(table), by)]
    if (length(rows) > 0) z[rows] <- norm_z(d[rows], age_m[rows], curve)
  }
  z
}

# 分數表的方法多給的參數：age_m 來自分數表（說清楚），其他照 no_dots()
subj_extra_args <- function(fun, form, ...) {
  if ("age_m" %in% ...names()) {
    text <- paste0("%s(): with the scores of kits_subj(), the ages come from their age_m column ",
                   "(the `persons` of kits_subj()): use %s without `age_m`, ",
                   "or give the scores and ages as vectors (s$theta, age_m).")
    stop_preview(sprintf(text, fun, form), "kitsPreview_error_input")
  }
  no_dots(fun, ...)
}

# 分數表的一欄（例如 age_m，來自 persons）；沒有時說清楚
subj_column <- function(s, col) {
  if (!col %in% names(s)) {
    text <- paste0("The scores have no `%s` column: give `persons` with that column to ",
                   "kits_subj(), ",
                   "or give the vectors instead (for example s$theta and age_m).")
    stop_preview(sprintf(text, col), "kitsPreview_error_input")
  }
  s[[col]]
}
