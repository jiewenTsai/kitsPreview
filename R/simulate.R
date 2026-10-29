#' Simulate PCM responses
#'
#' For small examples.
#' Each person has an ability theta.
#' Each item has a set of step parameters beta.
#' Scores start at 0.
#'
#' @param theta Ability of each person.
#' @param betas A list with one element per item.
#'   The names are item ids; each element holds the step parameters of that item.
#'   A dichotomous item has only one beta.
#' @param prefix Start of the person ids.
#' @param seed Random seed. When given, your current random state is left unchanged.
#'   When `NULL` (the default), the current random state is used, so you can call `set.seed()` first.
#' @return A matrix: one row per person and one column per item; row names are the visit ids
#'   (visit_id), column names are item ids.
#'   It can be passed directly to `dexter::fit_enorm()` and to the functions in this package.
#' @examples
#' betas <- list(a1 = 0, a2 = c(-1, 1))
#' kits_sim(theta = c(-2, 0, 2), betas = betas)
#' @export
kits_sim <- function(theta, betas, prefix = "p", seed = NULL) {
  if (is.null(names(betas)) || any(names(betas) == "") || anyDuplicated(names(betas))) {
    stop("`betas` must be a named list: one element per item, named by different item ids.", call. = FALSE)
  }
  if (length(theta) == 0 || !is.numeric(theta) || any(!is.finite(theta))) {
    stop("`theta` must be one or more finite numbers.", call. = FALSE)
  }
  if (any(!vapply(betas, function(b) is.numeric(b) && length(b) > 0 && all(is.finite(b)), logical(1)))) {
    stop("Each element of `betas` must be one or more finite numbers.", call. = FALSE)
  }
  visit_id <- sprintf("%s%04d", prefix, seq_along(theta))

  one_item <- function(item) {
    b <- betas[[item]]
    vapply(theta, function(t) {
      eta <- c(0, cumsum(t - b))
      p <- exp(eta - max(eta))
      sample(seq_along(p) - 1, 1, prob = p)
    }, numeric(1))
  }

  simulate <- function() vapply(names(betas), one_item, numeric(length(theta)))
  out <- if (is.null(seed)) simulate() else with_seed(seed, simulate())
  out <- matrix(as.integer(out), nrow = length(theta), dimnames = list(visit_id, names(betas)))
  out
}
