#' Age at which a share of children pass each item
#'
#' For each item, fit a logistic regression of pass/fail on age.
#' Find the age at which the probability of passing is `p`: .5 by default (the age at 50% passing),
#' or several values at once, for example `p = c(0.5, 0.75, 0.9)`.
#' The age at 50% passing is the column `p50`.
#'
#' The age range must be wide enough for the pass rate to cross `p`.
#' For items answered in both cohorts, stack the two cohorts' matrices and compute together (see the examples).
#' With only one cohort, many items will be NA.
#'
#' NA is returned for an item (and a `p`) in these cases:
#' * The pass rate is above `p` in every wave, or below `p` in every wave.
#'   That is, within the observed age range, the pass rate does not cross `p`.
#' * The pass rate does not rise with age (regression slope <= 0).
#' * The estimated age is outside the observed ages.
#' * The item has no responses, or is answered at fewer than 2 distinct ages.
#'
#' Perfect separation: for many milestones every younger child fails and every older child passes.
#' Then the logistic regression has no finite slope (`glm()` does not converge, or its fitted
#' probabilities reach 0 or 1), and the age at `p` is only known to lie between the oldest child who
#' fails and the youngest child who passes. The ages are given as before (from the fit where `glm()`
#' stops), and the result flags the item: `separated` is `TRUE`, and `age_last_fail` and
#' `age_first_pass` give the bracket.
#'
#' kits has the same function; it also takes kits' response table (`kits_resp()`).
#' Until kitsPreview 0.22.0 the age at 50% passing also came from `kits_age50()` (removed in
#' 0.23.0); its numbers are the column `p50`.
#'
#' @param pass Pass or fail of each person-wave on each item, or the item scores:
#'   a matrix with one row per person-wave and one column per item (NA means not administered),
#'   or a wide data.frame of the same shape (a `visit_id` column is ignored).
#'   A single vector is also accepted (one item).
#'   Logical values (`TRUE` = pass) are used as they are; numbers are scores, and a score of at
#'   least `step` is a pass (so 0/1 pass codes work with the default `step = 1`).
#' @param age_m Age in months of each person-wave (one per row).
#' @param wave_m Wave of each person-wave (one per row).
#' @param step The step that counts as a pass: a score of at least `step` (default 1, the milestone
#'   step).
#' @param p The share of children who pass: one or more probabilities between 0 and 1 (default 0.5).
#' @return A table (class `kitsPreview_age_pass`, a data.frame) with one row per item:
#'   `item_id` (NA for a single vector), one column per `p` (named `p50`, `p75`, ...), `separated`
#'   (`TRUE` when the regression has perfect separation, `FALSE` otherwise, NA when it was not
#'   fitted), `age_last_fail` (the oldest age of a child who fails) and `age_first_pass` (the
#'   youngest age of a child who passes).
#' @examples
#' age <- rep(c(6, 12, 18), each = 4)
#' X <- cbind(a1 = c(0, 0, 0, 1,  0, 1, 1, 0,  1, 1, 1, 1),
#'            a2 = c(0, 0, 0, 0,  0, 0, 0, 1,  0, 1, 1, 1))
#' kits_age_pass(X, age_m = age, wave_m = age)
#'
#' # The ages at which 25%, 50% and 75% of the children pass
#' kits_age_pass(X, age_m = age, wave_m = age, p = c(0.25, 0.5, 0.75))
#'
#' # Item scores: step 2 counts a score of 2 or more as a pass
#' S <- cbind(a1 = c(0, 1, 1, 1,  1, 1, 2, 2,  1, 2, 2, 2))
#' kits_age_pass(S, age_m = age, wave_m = age, step = 2)
#'
#' # The two cohorts' matrices have different items: give them the same columns
#' # (items not answered are NA), then stack them
#' A <- matrix(c(0, 1, 0, 1), 2, dimnames = list(c("p1", "p2"), c("a1", "a2")))
#' B <- matrix(c(1, 1, 1, 0), 2, dimnames = list(c("p3", "p4"), c("a2", "a3")))
#' items <- union(colnames(A), colnames(B))
#' both <- matrix(NA, 4, length(items), dimnames = list(c(rownames(A), rownames(B)), items))
#' both[rownames(A), colnames(A)] <- A
#' both[rownames(B), colnames(B)] <- B
#' both
#' @export
kits_age_pass <- function(pass, age_m, wave_m, step = 1, p = 0.5) {
  if (missing(age_m) || missing(wave_m) || is.null(age_m) || is.null(wave_m)) {
    stop("Give `age_m` and `wave_m` (one value per person-wave).", call. = FALSE)
  }
  if (!is.numeric(step) || length(step) != 1 || is.na(step) || step < 1 || step != round(step)) {
    stop("`step` must be one whole number (1 = the milestone step).", call. = FALSE)
  }
  if (!is.numeric(p) || length(p) == 0 || any(!is.finite(p)) || any(p <= 0 | p >= 1)) {
    stop("`p` must be one or more probabilities between 0 and 1, for example c(0.5, 0.75, 0.9).",
         call. = FALSE)
  }
  if (is.data.frame(pass)) {
    pass <- pass[setdiff(names(pass), "visit_id")]
    bad <- names(pass)[!vapply(pass, \(v) is.logical(v) || is.numeric(v), logical(1))]
    if (length(bad) > 0) {
      stop("`pass` has column(s) that are not TRUE/FALSE or item scores: ",
           paste(bad, collapse = ", "), ". The only other column can be visit_id.", call. = FALSE)
    }
    pass <- as.matrix(pass)
  }
  if (!is.logical(pass) && !is.numeric(pass)) {
    stop("`pass` must be TRUE/FALSE or item scores.", call. = FALSE)
  }
  given <- if (is.logical(pass)) "TRUE (given as logical)" else sprintf("a score of at least %d",
                                                                        step)
  if (is.numeric(pass)) pass <- pass >= step
  if (is.null(dim(pass))) {
    pass <- matrix(pass, ncol = 1)
  } else if (nrow(pass) != length(age_m) || nrow(pass) != length(wave_m)) {
    stop("`age_m` and `wave_m` need one value per row of `pass` (", nrow(pass), ").", call. = FALSE)
  }
  items <- if (is.null(colnames(pass))) rep(NA_character_, ncol(pass)) else colnames(pass)
  rows <- purrr::map(seq_len(ncol(pass)), \(j) age_pass_one(pass[, j], age_m, wave_m, p))
  out <- age_pass_table(rows, items, p)
  attr(out, "pass") <- given
  out
}

#' @rdname kits_age_pass
#' @param x A `kitsPreview_age_pass`.
#' @param digits Decimals to print.
#' @param n Rows to print: by default all rows of a table of up to 30 rows, else the first 10.
#' @param ... Passed to `print()` for a data.frame.
#' @export
print.kitsPreview_age_pass <- function(x, digits = 2, n = NULL, ...) {
  pass <- attr(x, "pass")
  cat(sprintf("<kits_age_pass> the age at which a share p of the children pass each item%s\n",
              if (is.null(pass)) "" else sprintf("; pass = %s", pass)))
  print_rows(x, digits, n, ...)
}

# 一題一列的表：item_id、每個 p 一欄、separated、age_last_fail、age_first_pass
age_pass_table <- function(rows, items, p) {
  ages <- matrix(unlist(purrr::map(rows, "age")), ncol = length(p), byrow = TRUE,
                 dimnames = list(NULL, sprintf("p%g", 100 * p)))
  out <- data.frame(item_id = items, ages, check.names = FALSE) |>
    dplyr::mutate(separated = purrr::map_lgl(rows, "separated"),
                  age_last_fail = purrr::map_dbl(rows, "last_fail"),
                  age_first_pass = purrr::map_dbl(rows, "first_pass"))
  preview_table(out, "kitsPreview_age_pass")
}

# 一題在 p 通過的月齡（內部用；p 可以是向量，迴歸只配一次）。和 kits 的 age_pass_one() 相同：
# 回傳 age（每個 p 一個）、separated（完全分離；沒配迴歸時 NA）、last_fail、first_pass
age_pass_one <- function(pass, age_m, wave_m, p = 0.5) {
  none <- list(age = rep(NA_real_, length(p)), separated = NA, last_fail = NA_real_,
               first_pass = NA_real_)
  if (length(pass) != length(age_m) || length(pass) != length(wave_m)) {
    stop("`pass`, `age_m` and `wave_m` must have the same length.", call. = FALSE)
  }
  d <- data.frame(pass = as.numeric(pass), age_m = age_m, wave_m = wave_m) |>
    dplyr::filter(!is.na(pass), is.finite(age_m), !is.na(wave_m))
  if (nrow(d) == 0) {
    return(none)
  }
  none$last_fail <- max_or_na(d$age_m[d$pass == 0])
  none$first_pass <- min_or_na(d$age_m[d$pass == 1])
  if (length(unique(d$age_m)) < 2) {
    return(none)
  }
  by_wave <- dplyr::summarise(d, rate = mean(pass), .by = wave_m)$rate   # 每個波次的通過率
  crosses <- max(by_wave) >= p & min(by_wave) <= p
  if (!any(crosses)) {
    return(none)
  }
  fit <- logistic_fit(d$pass, d$age_m)
  none$separated <- fit$separated
  slope <- stats::coef(fit$glm)[2]
  if (!is.finite(slope) || slope <= 0) {
    return(none)
  }
  age <- unname((stats::qlogis(p) - stats::coef(fit$glm)[1]) / slope)
  age[!crosses | age < min(d$age_m) | age > max(d$age_m)] <- NA_real_
  c(list(age = age), none[-1])
}

# 通過對月齡的 logistic 迴歸。glm() 對完全分離的兩個警告（沒有收斂、配適的機率到 0 或 1）不印出來，
# 改記成 separated（其他警告照常發出）。判斷和 glm.fit 相同：機率在 10 x 機器精度以內就算到了 0 或 1
logistic_fit <- function(pass, age_m) {
  separation_warnings <- gettext(c("glm.fit: algorithm did not converge",
                                   "glm.fit: fitted probabilities numerically 0 or 1 occurred"),
                                 domain = "R-stats")
  fit <- withCallingHandlers(stats::glm(pass ~ age_m, family = stats::binomial),
                             warning = function(w) {
    if (conditionMessage(w) %in% separation_warnings) invokeRestart("muffleWarning")
  })
  eps <- 10 * .Machine$double.eps
  at_bound <- any(fit$fitted.values < eps | fit$fitted.values > 1 - eps)
  list(glm = fit, separated = !fit$converged || at_bound)
}

max_or_na <- function(x) if (length(x) == 0) NA_real_ else max(x)
min_or_na <- function(x) if (length(x) == 0) NA_real_ else min(x)


#' Size of sex DIF
#'
#' Uses dexter's `DIF()`.
#' Conditional on the total raw score of this domain, compare the item parameters of boys and girls.
#' Each item gets one number.
#' For polytomous items it is the mean over the score categories.
#'
#' dexter's `DIF()` calibrates boys and girls separately.
#' So in each group, every item needs someone with a score of 0, and at least two different scores.
#' For items where this fails (for example items that nearly everyone passes), the function stops and lists the item ids.
#' Remove those items first.
#'
#' Meaning of the sign (with `ref = "boy"`):
#' * Positive: harder for boys, that is, favors girls.
#' * Negative: favors boys.
#'
#' @param data Responses: a matrix (one row per person-wave, one column per item, row names are visit_id),
#'   or a long data.frame with `visit_id`, `item_id`, `item_score`.
#' @param sex The sex of each person-wave: one value per row of the matrix (in the same order),
#'   or a vector named by visit_id (this also works for a long data.frame).
#'   It must have exactly two values; NA is left out.
#' @param ref The code of the reference group in `sex`.
#' @inheritParams kits_subj
#' @return A data.frame: `item_id`, `dif` (size of the DIF, in logits).
#' @examples
#' \donttest{
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = 0)
#' theta <- seq(-3, 3, length.out = 300)
#' boys <- kits_sim(theta, betas, prefix = "b", seed = 1)
#' girls <- kits_sim(theta, modifyList(betas, list(a5 = 1)), prefix = "g", seed = 2)
#' X <- rbind(boys, girls)
#' # a5 is harder for girls: negative, favors boys
#' kits_dif(X, sex = rep(c("boy", "girl"), each = 300))
#' }
#' @export
kits_dif <- function(data, sex, ref = "boy", ...) {
  no_dots("kits_dif", ...)
  resp <- resp_long(data, "data")
  resp$sex <- as.character(per_person(sex, data, resp, "sex"))
  resp <- resp[!is.na(resp$sex), ]
  if (length(unique(resp$sex)) != 2) {
    stop("`sex` must have exactly two values; it has: ", paste(unique(resp$sex), collapse = ", "), ".", call. = FALSE)
  }
  if (length(ref) != 1) {
    stop("`ref` must be one value of `sex`.", call. = FALSE)
  }
  if (!ref %in% resp$sex) {
    stop("`ref` = \"", ref, "\" is not a value of `sex`.", call. = FALSE)
  }
  bad <- unlist(lapply(split(resp, resp$sex), function(g) {
    ok <- tapply(g$item_score, g$item_id, function(x) any(x == 0) && length(unique(x)) > 1)
    if (any(!ok)) paste0(names(ok)[!ok], " (", g$sex[1], ")")
  }), use.names = FALSE)
  if (length(bad) > 0) {
    stop("dexter::DIF() calibrates each sex separately, so every item needs a 0 score and at least two score values ",
         "in each group. Remove these items first: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  # resp 是內部的長格式，人次的欄已經是 dexter 的名字 person_id
  out <- dexter::DIF(resp[, c("person_id", "item_id", "item_score", "sex")], person_property = "sex")
  sign <- if (out$group_labels[1] == ref) -1 else 1   # dexter 的第一組是 group_labels[1]

  # DIF() 的結果一列是一題的一個分數。先平均每一列，再按題平均。
  by_step <- data.frame(item_id = out$items$item_id, dif = sign * rowMeans(out$Delta_R))
  preview_table(stats::aggregate(dif ~ item_id, data = by_step, FUN = mean))
}


#' Combined DIF decision over two cohorts
#'
#' First, the direction in each cohort is decided by `cut`:
#' larger than `cut` favors girls, smaller than `-cut` favors boys, otherwise none.
#' Then the two are combined by these rules:
#' * Only one cohort has the item: use that cohort's result.
#' * Neither side exceeds `cut`: none.
#' * One side exceeds `cut` and the other has the opposite sign: inconsistent.
#' * Both sides have the same sign and at least one exceeds `cut`: take the direction that exceeds it.
#'
#' The direction names assume that [kits_dif()] used `ref = "boy"`.
#'
#' @param d1,d2 The [kits_dif()] results of the two cohorts.
#' @param cut The cutoff for the decision (logits).
#' @param cohorts The names of the two cohorts, used in the output column names.
#' @return A data.frame: for each item, `dif_` and `dir_` for both sides, and the combined `DIF`.
#' @examples
#' d1 <- data.frame(item_id = c("a1", "a2", "a3", "a4"), dif = c(0.1, 0.5, 0.4, -0.5))
#' d2 <- data.frame(item_id = c("a1", "a2", "a3", "a5"), dif = c(0.2, 0.1, -0.2, 0.6))
#' kits_dif_combine(d1, d2)
#' @export
kits_dif_combine <- function(d1, d2, cut = 0.3, cohorts = c("M03", "M36")) {
  direction <- function(r) {
    ifelse(is.na(r), NA, ifelse(r > cut, "favors girls", ifelse(r < -cut, "favors boys", "none")))
  }
  rule <- function(r1, r2, dir1, dir2) {
    if (is.na(r2)) return(dir1)
    if (is.na(r1)) return(dir2)
    if (dir1 == "none" && dir2 == "none") return("none")
    if (r1 * r2 < 0) return("inconsistent")
    if (dir1 != "none") dir1 else dir2
  }

  need_cols(d1, c("item_id", "dif"), "d1")
  need_cols(d2, c("item_id", "dif"), "d2")
  if (anyDuplicated(d1$item_id) || anyDuplicated(d2$item_id)) {
    stop("`d1` and `d2` must have one row per item.", call. = FALSE)
  }
  if (length(cohorts) != 2 || cohorts[1] == cohorts[2]) {
    stop("`cohorts` needs two different names.", call. = FALSE)
  }
  if (!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut < 0) {
    stop("`cut` must be one number of at least 0 (logits).", call. = FALSE)
  }
  out <- merge(d1, d2, by = "item_id", all = TRUE, suffixes = paste0("_", cohorts))
  r1 <- out[[paste0("dif_", cohorts[1])]]
  r2 <- out[[paste0("dif_", cohorts[2])]]
  out[[paste0("dir_", cohorts[1])]] <- direction(r1)
  out[[paste0("dir_", cohorts[2])]] <- direction(r2)
  out$DIF <- mapply(rule, r1, r2, direction(r1), direction(r2), USE.NAMES = FALSE)
  preview_table(out)
}
