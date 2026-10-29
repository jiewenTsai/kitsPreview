#' Item drift: do the items behave the same in a new wave of data?
#'
#' Use this before calibrating a new wave with fixed item parameters.
#' `ref` is the [kits_item()] result of the earlier calibration.
#'
#' The steps are:
#' 1. Calibrate the new data on its own, without fixing any item.
#' 2. Check whether each item can be judged (see below).
#' 3. For the items that can be judged, the new threshold of each step is `from`, and the `ref` threshold is `to`.
#' 4. Fit a line with one round of screening ([kits_link_steps]) and drop the points that deviate
#'    too much.
#' 5. Items with a dropped step are flagged as drifted (`drift = TRUE`).
#'
#' Each step is checked separately for enough information: at least `min_n` persons are needed both below and above the step.
#' For example, for an item that nearly every child passes, the lowest step has no information.
#' Only steps with enough information are used to fit the line and to judge drift.
#' An item is judged as long as one of its steps has enough information.
#'
#' These items are not judged (`drift` is NA):
#' * Items with no step that has enough information.
#' * Items with only one score, with no 0 score, or with a score gap (for example 0 and 2 but no 1).
#'   They cannot be calibrated freely.
#' * Items whose top score in the new data is higher than in `ref`.
#'   The score structures of the two sides differ, so they cannot be compared.
#'
#' Items suggested for fixing (`suggest_fix = TRUE`):
#' * Items that were judged and did not drift.
#' * Items that cannot be judged because of too little information.
#'   The thresholds of such items (for example items that nearly everyone passes) are estimated very poorly.
#'   A large residual does not mean drift.
#'   They carry almost no information in this wave anyway.
#'   Fixed or not, their effect on the scores is very small.
#'
#' Items not suggested for fixing:
#' * Items that drifted.
#' * Items whose top score is higher than in `ref`. They must be estimated again.
#'
#' Which items to fix in the end is up to the user.
#' Then calibrate, fixing those items (see the examples): `kits_item(X_new, fixed_params = drift$fix)`.
#' The items not fixed (including items new in this wave) are estimated on the scale of the fixed items.
#' dexter needs every item in the calibration to have a 0 score and at least two scores;
#' it stops and names the items that do not. Leave those columns out first.
#'
#' Limitations:
#' * A drifted item can only be detected if it has enough information itself.
#' * When too few items can be judged (for example only three or four), the drift may be absorbed by the line.
#' * When several steps of a polytomous item drift together, often the slope is pulled away, instead of that item being dropped.
#'
#' The slope of the line is also worth a look.
#' A slope far from 1 means that the unit of the new data differs from `ref`'s.
#'
#' @param data New data: a matrix (one row per person-wave, one column per item, row names are visit_id),
#'   or a long data.frame with `visit_id`, `item_id`, `item_score`.
#' @param ref The [kits_item()] result of the earlier calibration, or its `thresholds` table.
#' @param cut The cutoff for dropping, in residual SDs.
#' @param min_n The minimum number of persons needed both below and above a step.
#' @inheritParams kits_subj
#' @return A list:
#'   * `fix`: the `ref` parameters (`item_id`, `item_score`, `beta`) of the items suggested for fixing
#'     (`suggest_fix` in `items`), ready for `kits_item(X_new, fixed_params = drift$fix)`.
#'     Edit it when you decide otherwise, for example `subset(drift$fix, item_id != "a2")`.
#'     NULL when `ref` has no `beta` column.
#'   * `intercept`, `slope`: the line from the scale of the free calibration of the new data to the
#'     `ref` scale, new value = intercept + slope x old value (using only the items that can be
#'     judged). Until kitsPreview 0.22.0 they were called `a` and `b`.
#'   * `points`: for each step, the thresholds, the numbers of persons below and above, the residual, and whether it was dropped.
#'   * `items`: one row per item: `item_id`, `max_abs_resid`, `drift` (NA when the item is not judged),
#'     `suggest_fix`, and `reason` (why the item is not judged; empty when it is).
#'   * `steps_free`: the step table of the free calibration of the new data.
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5), a6 = 1.5)
#' old <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
#' item_old <- kits_item(old)
#'
#' drifted <- modifyList(betas, list(a3 = 2))   # a3 became harder in the new data
#' new <- kits_sim(seq(-2, 4, length.out = 400), drifted, prefix = "n", seed = 2)
#' drift <- kits_drift(new, item_old)
#' drift$items
#'
#' # Calibrate the new data, fixing the items suggested for fixing
#' drift$fix
#' item_new <- kits_item(new, fixed_params = drift$fix)
#' coef(item_new)
#' @export
kits_drift <- function(data, ref, cut = 2, min_n = 30, ...) {
  no_dots("kits_drift", ...)
  resp <- resp_long(data, "data")
  ref <- as_steps(ref, "ref")
  need_steps(ref, "ref")
  ids <- sort(intersect(unique(resp$item_id), ref$item_id))
  if (length(ids) == 0) {
    stop("No item of the new data is in `ref`.", call. = FALSE)
  }

  # 每題能不能判斷，不能的原因
  max_new <- tapply(resp$item_score, resp$item_id, max)[ids]
  max_ref <- tapply(ref$item_score, ref$item_id, max)[ids]
  can_fit <- tapply(resp$item_score, resp$item_id, function(x) min(x) == 0 && length(unique(x)) > 1)
  no_gap <- tapply(resp$item_score, resp$item_id, function(x) all(seq_len(max(x)) %in% x))
  reason <- ifelse(max_new > max_ref, "score range differs",
                   ifelse(!can_fit[ids], "no 0 score or one score only",
                          ifelse(!no_gap[ids], "score gap", "")))

  # 新資料自由校準
  free <- resp[resp$item_id %in% names(can_fit)[can_fit & no_gap], ]
  if (length(unique(free$item_id)) < 2) {
    stop("Fewer than 2 items of the new data have a 0 score and at least two scores, so the new data cannot be calibrated.", call. = FALSE)
  }
  fit_free <- dexter::fit_enorm(free)
  steps_free <- item_thresholds(fit_free)
  common <- merge(steps_free, ref[ref$item_id %in% ids[reason == ""], ], by = c("item_id", "item_score"), suffixes = c("_new", "_ref"))

  # 每個步驟上下各有幾個人
  scores_of <- split(resp$item_score, resp$item_id)
  common$n_below <- mapply(function(id, k) sum(scores_of[[id]] < k), common$item_id, common$item_score)
  common$n_above <- mapply(function(id, k) sum(scores_of[[id]] >= k), common$item_id, common$item_score)
  common$judged <- pmin(common$n_below, common$n_above) >= min_n   # 資訊夠的步驟才用來判斷
  reason[reason == "" & !ids %in% common$item_id[common$judged]] <- "too little information"

  use <- common[common$judged, ]
  if (nrow(use) < 3) {
    stop("Only ", nrow(use), " item step(s) have at least `min_n` = ", min_n, " persons below and above, ",
         "so drift cannot be judged. At least 3 are needed.", call. = FALSE)
  }
  fit <- mean_sigma_link(use$thr_new, use$thr_ref, id = paste0(use$item_id, ":", use$item_score),
                         cut = cut)

  points <- common[, c("item_id", "item_score", "n_below", "n_above", "judged")]
  points$thr_new <- common$thr_new
  points$thr_ref <- common$thr_ref
  points$resid <- points$thr_ref - fit$intercept - fit$slope * points$thr_new
  points$dropped <- NA
  points$dropped[common$judged] <- fit$points$dropped

  drift <- unname(tapply(points$dropped, points$item_id, function(x) any(x, na.rm = TRUE))[ids])
  max_abs_resid <- unname(tapply(abs(points$resid), points$item_id, max)[ids])
  reason <- unname(reason)
  judged <- reason == ""
  drift[!judged] <- NA
  items <- data.frame(item_id = ids,
                      max_abs_resid = max_abs_resid,
                      drift = drift,
                      suggest_fix = (judged & !drift) | reason %in% c("too little information", "no 0 score or one score only"),
                      reason = reason)
  fix <- if ("beta" %in% names(ref)) ref[ref$item_id %in% items$item_id[items$suggest_fix], c("item_id", "item_score", "beta")] else NULL
  if (!is.null(fix)) {
    rownames(fix) <- NULL
    fix <- preview_table(fix)
  }
  list(fix = fix, items = preview_table(items), intercept = fit$intercept, slope = fit$slope,
       points = preview_table(points), steps_free = steps_free)
}
