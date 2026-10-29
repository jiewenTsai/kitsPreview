#' Screening waves before fitting norms: waves with scores piled at the floor
#'
#' When many children in a wave get the lowest score of that wave:
#' * You cannot tell who is behind. The most important use of a norm is lost.
#' * The distribution is not continuous, so the fit is unstable.
#'
#' So such waves are left out of the norms.
#' When comparing scores, `d` (the D score) is first rounded to 3 decimals.
#'
#' "The lowest score" is the minimum of `d` in this wave.
#' When a wave has several item patterns (combinations of items answered), the lowest raw score of
#' each item pattern maps to a different D.
#' Then the percentage is underestimated.
#' In that case you can look at each item pattern separately.
#'
#' When both cohorts have the same wave (for example 36 months), give `cohort`, and the two cohorts are computed separately.
#'
#' Waves with scores piled at the highest score (ceiling) still go into the norms.
#' Children with a perfect score just get no percentile rank (see [kits_norm_z()]).
#'
#' The scores of [kits_subj()] can be given instead of `d`: `kits_norm_screen(s)` uses their
#' `theta`, and the `wave_m` and `cohort` columns (from the `persons` given to [kits_subj()]).
#'
#' @param d The score of each person-wave (the D score), or the scores of [kits_subj()].
#' @param wave_m The wave (months) of each person-wave (with the scores of [kits_subj()]: taken from
#'   their column).
#' @param cohort The cohort of each person-wave; optional (with the scores of [kits_subj()]: their
#'   `cohort` column, when they have one).
#' @param cut When the percentage of children at the lowest score reaches this number, the wave is left out of the norms.
#' @param ... Not used; any argument here is an error.
#' @return A `kitsPreview_norm_screen`: a list that prints briefly. Its elements:
#'   * `waves`: one row per (cohort x) wave: `cohort` (when given), `wave_m`, `n`,
#'     `floor_pct` (the percentage at the lowest score of this wave), `normed` (`FALSE` means left out of the norms).
#'   * `normed`: one `TRUE`/`FALSE` per person-wave, in the same order as the input.
#'     Use it directly to select the person-waves for the norms. Person-waves with NA in `d` are `FALSE`.
#' @examples
#' d <- c(10, 10, 10, 10, 12, 15,  20, 22, 25, 30, 31, 33)
#' wave_m <- rep(c(6, 12), each = 6)
#' screen <- kits_norm_screen(d, wave_m)
#' screen
#' screen$normed
#'
#' # The two cohorts computed separately
#' kits_norm_screen(d, wave_m, cohort = rep(c("M03", "M36"), times = 6))
#' @export
kits_norm_screen <- function(d, wave_m, cohort = NULL, cut = 20, ...) {
  no_dots("kits_norm_screen", ...)
  if (inherits(d, "kitsPreview_subj")) {   # 分數表：分數、波次、世代從它的欄拿
    s <- d
    d <- s$theta
    if (missing(wave_m)) wave_m <- subj_column(s, "wave_m")
    if (is.null(cohort) && "cohort" %in% names(s)) cohort <- s$cohort
  }
  need_d_vector(d, "kits_norm_screen")
  if (length(d) != length(wave_m) || (!is.null(cohort) && length(cohort) != length(d))) {
    stop("`d`, `wave_m` (and `cohort`) must have the same length.", call. = FALSE)
  }
  if (!is.numeric(d) && !all(is.na(d))) {
    stop("`d` must be numbers.", call. = FALSE)
  }
  if (!is.numeric(cut) || length(cut) != 1 || is.na(cut)) {
    stop("`cut` must be one number (a percentage).", call. = FALSE)
  }
  d <- round(d, 3)
  if (is.null(cohort)) cohort <- rep(NA, length(d))
  ok <- is.finite(d) & !is.na(wave_m)
  if (!any(ok)) {
    stop("No person-wave has both a finite `d` and a `wave_m`.", call. = FALSE)
  }
  groups <- unique(data.frame(cohort = cohort[ok], wave_m = wave_m[ok]))
  groups <- groups[order(groups$cohort, groups$wave_m), ]
  stats <- t(mapply(function(c0, w0) {
    x <- d[ok & wave_m == w0 & cohort %in% c0]
    c(n = length(x), floor_pct = round(100 * mean(x == min(x)), 1))
  }, groups$cohort, groups$wave_m))
  out <- data.frame(groups, stats, row.names = NULL)
  out$normed <- out$floor_pct < cut

  # 每個人次：所在的（世代 ×）波次有沒有放進常模；d 是 NA 的人次是 FALSE
  key <- paste(cohort, wave_m)
  person <- key %in% paste(out$cohort, out$wave_m)[out$normed] & ok
  if (all(is.na(out$cohort))) out$cohort <- NULL
  structure(list(waves = preview_table(out), normed = person, cut = cut), class = "kitsPreview_norm_screen")
}


#' Print a norm screen
#'
#' @param x The result of [kits_norm_screen()].
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param ... Not used.
#' @return `x` (invisibly).
#' @examples
#' kits_norm_screen(c(10, 10, 10, 10, 12, 15, 20, 22, 25, 30, 31, 33), rep(c(6, 12), each = 6))
#' @export
print.kitsPreview_norm_screen <- function(x, digits = 2, ...) {
  need_digits(digits)
  cat(sprintf("<kits_norm_screen> waves with %g%% or more at the lowest score are left out\n", x$cut))
  print(format_table(x$waves, digits), row.names = FALSE, right = TRUE)
  cat(sprintf("Person-waves in the norms: %d of %d (use $normed)\n", sum(x$normed), length(x$normed)))
  invisible(x)
}


#' Age range of the norms
#'
#' From the 1st percentile of age in the first wave to the 99th percentile of age in the last wave.
#' Both ends are rounded outward to `by` (by default half a month).
#'
#' This way, a few unusually young or old children do not stretch the norms to ages without data.
#' Children outside the range are left out of the norms and get no same-age z.
#'
#' Screen the waves first ([kits_norm_screen()]).
#' Pass only the waves to be normed.
#'
#' The scores of [kits_subj()] can be given instead of `age_m`: `kits_norm_ages(s)` uses their
#' `age_m` and `wave_m` columns (from the `persons` given to [kits_subj()]), for example
#' `kits_norm_ages(s[screen$normed, ])`.
#'
#' @param age_m Age in months of each person-wave, or the scores of [kits_subj()].
#' @param wave_m The wave (months) of each person-wave (with the scores of [kits_subj()]: taken from
#'   their column).
#' @param probs Which percentile to take in the first wave and in the last wave.
#' @param by The age interval.
#' @return A vector of ages, which can be passed directly to `ages` of [kits_norms()].
#' @examples
#' set.seed(1)
#' age <- c(rnorm(200, 12, 0.6), 7, 8, rnorm(200, 24, 0.6))
#' wave_m <- c(rep(12, 202), rep(24, 200))
#' range(kits_norm_ages(age, wave_m))
#' @export
kits_norm_ages <- function(age_m, wave_m, probs = c(0.01, 0.99), by = 0.5) {
  if (inherits(age_m, "kitsPreview_subj")) {   # 分數表：月齡和波次從它的欄拿
    if (!missing(wave_m)) {
      stop("With the scores of kits_subj(), wave_m comes from their column: kits_norm_ages(s), ",
           "other arguments by name.", call. = FALSE)
    }
    s <- age_m
    age_m <- subj_column(s, "age_m")
    wave_m <- subj_column(s, "wave_m")
  }
  if (length(age_m) != length(wave_m)) {
    stop("`age_m` and `wave_m` must have the same length.", call. = FALSE)
  }
  if ((!is.numeric(age_m) && !all(is.na(age_m))) || (!is.numeric(wave_m) && !all(is.na(wave_m)))) {
    stop("`age_m` and `wave_m` must be numbers (months).", call. = FALSE)
  }
  if (!is.numeric(by) || length(by) != 1 || is.na(by) || by <= 0) {
    stop("`by` must be one positive number.", call. = FALSE)
  }
  ok <- is.finite(age_m) & !is.na(wave_m)
  if (!any(ok)) {
    stop("No person-wave has both `age_m` and `wave_m`.", call. = FALSE)
  }
  first <- age_m[ok & wave_m == min(wave_m[ok])]
  last <- age_m[ok & wave_m == max(wave_m[ok])]
  lo <- floor(stats::quantile(first, probs[1], names = FALSE) / by) * by
  hi <- ceiling(stats::quantile(last, probs[2], names = FALSE) / by) * by
  if (hi <= lo) {
    stop("The ages of the last wave are not above those of the first wave (", lo, " to ", hi, "). ",
         "Check `age_m` and `wave_m`.", call. = FALSE)
  }
  round(seq(lo, hi, by = by), 10)
}
