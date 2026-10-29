# 每個作答在 PCM 下的期望值 E 與變異數 W（內部用，給 kits_subj()、kits_itemfit()）。
# resp：person_id、item_id、item_score；theta：每人一個值，名字是 person_id。
pcm_moments <- function(resp, parms, theta) {
  resp$theta <- unname(theta[as.character(resp$person_id)])
  resp$E <- NA_real_
  resp$W <- NA_real_
  for (i in unique(as.character(resp$item_id))) {
    rows <- which(resp$item_id == i)
    p <- dexter::p_score(parms, items = i)(resp$theta[rows])   # 一列一個作答，一欄一個分數（0, 1, ...）
    k <- seq_len(ncol(p)) - 1
    resp$E[rows] <- drop(p %*% k)
    resp$W[rows] <- drop(p %*% k^2) - resp$E[rows]^2
  }
  resp
}

# infit、outfit 均方：infit = Σ(x − E)² / ΣW；outfit = (x − E)² / W 的平均
mean_squares <- function(x, E, W, group) {
  infit <- tapply((x - E)^2, group, sum) / tapply(W, group, sum)
  outfit <- tapply((x - E)^2 / W, group, mean)
  list(infit = infit, outfit = outfit)
}


#' Item fit: infit and outfit mean squares, and the item-rest correlation
#'
#' For each item:
#' * `infit` and `outfit` are mean squares of the residuals (observed score - expected score under the PCM),
#'   with each person at his or her WLE ([kits_subj()]).
#'   The expected value is 1.
#'   Above 1: the responses are more random than the model expects (for example, noise or a different construct).
#'   Below 1: more predictable than expected (for example, a near-copy of another item).
#'   `outfit` is sensitive to unexpected responses far from the item (lucky or careless answers);
#'   `infit` weights responses near the item more.
#'   A common rule of thumb for surveys is 0.6 to 1.4; 0.5 to 1.5 is still "productive for measurement" (Linacre).
#' * `flag`: `"underfit"` when infit or outfit is above 1.4, otherwise `"overfit"` when infit or
#'   outfit is below 0.6, otherwise `"ok"` (exactly 0.6 or 1.4 is ok); NA when both are NA. The
#'   range 0.6 to 1.4 is the one of the printout and of `plot(item, type = "fit")`; the rule is the
#'   same as in kits. A flag is a prompt to look at the item (its content, its scoring, the
#'   children who answered it), not a rule to remove it.
#' * `rir`: the item-rest correlation (the item against the total of the other items of the item
#'   pattern), as `rir` of `dexter::tia_tables()` (dexter's `rit` is the item-total correlation,
#'   which counts the item itself; dexter calls an item pattern a booklet).
#'   With several item patterns it is the mean over item patterns, weighted by the number of persons.
#'   An item pattern where the item or the rest score does not vary (for example one or two persons)
#'   gives no `rir` and is left out.
#'
#' Persons with a zero or a perfect score are left out:
#' their WLE is far out and their residuals carry no information about the fit.
#'
#' With few items per person, the mean squares are below 1 even when the PCM holds,
#' because each WLE is fitted to the same responses.
#' In simulated data that fit the model, the mean item infit was 0.83 with 5 items per person,
#' 0.92 with 10, 0.96 with 20 and 0.98 with 40.
#' So with short forms, compare the items with each other rather than with 1.
#'
#' Works with any number of item patterns.
#' For dexter's interaction-model fit on the standard form of each wave, see [kits_inter()].
#'
#' [kits_item()] already computes this table on its own data and keeps it as `item$itemfit`
#' (`plot(item, type = "fit")` draws it).
#' Use `kits_itemfit()` to check the same items on other data, for example a new cohort scored
#' with items calibrated earlier.
#'
#' @inheritParams kits_subj
#' @return A data.frame with one row per item: `item_id`, `n` (responses used), `pvalue`
#'   (the mean score as a share of the maximum), `rir`, `infit`, `outfit`, `flag`.
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = 0)
#' X <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
#' X[, "a5"] <- sample(0:1, 400, replace = TRUE)   # a5 is pure noise
#' kits_itemfit(X, kits_item(X))
#' @export
kits_itemfit <- function(data, item, ...) {
  no_dots("kits_itemfit", ...)
  itemfit_table(resp_long(data, "data"), param_frame(item))
}

# 題目適配的範圍：問卷常用的 0.6 到 1.4（印出和 plot(item, type = "fit") 都用這個）
ITEMFIT_RANGE <- c(0.6, 1.4)

# 題目適配（內部用，給 kits_itemfit() 和 kits_item()）。resp：長格式；parms：param_frame() 的結果。
# 結果一題一列，只有每題的彙總，沒有每個人次的值。
itemfit_table <- function(resp, parms) {
  need_items(resp, parms)
  if (length(unique(resp$item_id)) < 2) {
    stop("Item fit needs at least 2 items.", call. = FALSE)
  }
  subj <- suppressMessages(kits_subj(resp_mark(resp), parms))
  inner <- subj$visit_id[subj$raw > 0 & subj$raw < subj$raw_max]
  if (length(inner) == 0) {
    stop("Every person has a zero or a perfect score, so the fit cannot be computed.", call. = FALSE)
  }
  r <- pcm_moments(resp[resp$person_id %in% inner, ], parms, stats::setNames(subj$theta, subj$visit_id))
  ms <- mean_squares(r$item_score, r$E, r$W, r$item_id)

  ids <- sort(unique(as.character(resp$item_id)))
  tia <- item_totals(resp)
  w_mean <- function(v) {   # 每個作答組合一個值，照人數加權；沒有值的組合（例如只有一個人）不算
    ok <- is.finite(tia[[v]])
    tapply((tia[[v]] * tia$n_persons)[ok], tia$item_id[ok], sum) / tapply(tia$n_persons[ok], tia$item_id[ok], sum)
  }
  preview_table(data.frame(item_id = ids, n = as.integer(table(factor(r$item_id, levels = ids))),
             pvalue = unname(w_mean("pvalue")[ids]), rir = unname(w_mean("rir")[ids]),
             infit = unname(ms$infit[ids]), outfit = unname(ms$outfit[ids])) |>
    dplyr::mutate(flag = itemfit_flag(infit, outfit)))
}

# 題目適配的旗標（和 kits 相同的規則）：infit 或 outfit 超過上限是 underfit（兩種都有時算 underfit），
# 否則低於下限是 overfit，否則 ok；剛好等於 0.6 或 1.4 算 ok；infit、outfit 都是 NA 時是 NA
itemfit_flag <- function(infit, outfit, range = ITEMFIT_RANGE) {
  above <- function(v) !is.na(v) & v > range[2]
  below <- function(v) !is.na(v) & v < range[1]
  dplyr::case_when(is.na(infit) & is.na(outfit) ~ NA_character_,
                   above(infit) | above(outfit) ~ "underfit",
                   below(infit) | below(outfit) ~ "overfit",
                   .default = "ok")
}

# kits_item() 裡的題目適配：算不出來時是 NULL 加一則說明，絕不讓校準停下來
itemfit_or_note <- function(resp, thresholds) {
  tryCatch(list(itemfit = itemfit_table(resp, param_frame(thresholds)), note = character()),
           error = function(e) list(itemfit = NULL,
                                    note = paste("Item fit could not be computed:", conditionMessage(e))))
}

# 印出 kits_item() 時的一行題目適配摘要
itemfit_line <- function(x) {
  range <- paste(format(ITEMFIT_RANGE, nsmall = 1), collapse = "-")
  if (is.null(x$itemfit)) {
    if (length(x$notes) > 0) cat("Item fit: not computed; see x$notes.\n")
    return(invisible())
  }
  fit <- x$itemfit
  flag <- fit$flag %||% itemfit_flag(fit$infit, fit$outfit)   # 0.23.0 以前的物件沒有 flag 欄
  under <- fit$item_id[flag %in% "underfit"]
  over <- fit$item_id[flag %in% "overfit"]
  bad <- c(under, over)   # underfit 的題在前
  n <- nrow(fit)
  if (length(bad) == 0) {
    cat(sprintf("Item fit: infit and outfit within %s for all %d items.\n", range, n))
    return(invisible())
  }
  ids <- paste(utils::head(bad, 5), collapse = ", ")
  if (length(bad) > 5) ids <- paste0(ids, ", \u2026")
  cat(sprintf(paste0("Item fit: %d of %d items outside %s (%d underfit, %d overfit: %s); ",
                     "see x$itemfit, plot(x, type = \"fit\").\n"),
              length(bad), n, range, length(under), length(over), ids))
}

# 已經轉成長格式的資料，標記起來，下一個函數不用再轉
resp_mark <- function(resp) {
  attr(resp, "kits_long") <- TRUE
  resp
}


#' dexter's interaction-model fit on the standard form of each wave
#'
#' `dexter::fit_inter()` fits the interaction model next to the PCM.
#' Each item gets `sigma`: 0 means the item fits the PCM;
#' `fit_IM` is about sigma / SE, so |fit_IM| > 2 suggests misfit.
#' `plot()` draws dexter's item-total regressions (observed, PCM and interaction model).
#'
#' `fit_inter()` uses one booklet (dexter's name for an item pattern, a complete item set) at a time.
#' So this function runs it on the standard form of each wave:
#' the item set answered by most children in the wave, with only those children.
#' Items with no 0 score or only one score on the standard form of a wave are left out of that wave, with a message.
#' Waves where it still cannot be fitted are left out with a message.
#'
#' @inheritParams kits_subj
#' @param wave_m The wave of each person-wave: one value per row of the matrix (in the same order),
#'   or a vector named by visit_id (this also works for a long data.frame).
#' @return A `kitsPreview_inter`: a list with
#'   * `items`: one row per wave x item: `wave_m`, `n` (children on the standard form), `item_id`,
#'     `sigma`, `SE_sigma`, `fit_IM`.
#'   * `fits`: the `dexter::fit_inter()` result of each wave, named by wave.
#' @examples
#' \donttest{
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
#' inter <- kits_inter(X, wave_m = rep(c(12, 24), each = 200))
#' inter
#' plot(inter, wave = 12, items = "a1")
#' }
#' @export
kits_inter <- function(data, wave_m, ...) {
  no_dots("kits_inter", ...)
  resp <- resp_long(data, "data")
  resp$wave_m <- per_person(wave_m, data, resp, "wave_m")
  if (anyNA(resp$wave_m)) {
    stop("`wave_m` has NA; give every person-wave a wave.", call. = FALSE)
  }
  fits <- list()
  rows <- list()
  for (w in sort(unique(resp$wave_m))) {
    d <- resp[resp$wave_m == w, ]
    form <- modal_form(d)
    sets <- tapply(d$item_id, d$person_id, function(x) setequal(x, form$items))
    d <- d[d$person_id %in% names(sets)[sets], c("person_id", "item_id", "item_score")]
    ok <- tapply(d$item_score, d$item_id, function(x) min(x) == 0 && length(unique(x)) > 1)
    if (any(!ok)) {
      message("Wave ", w, ": item(s) with no 0 score or one score left out: ", paste(names(ok)[!ok], collapse = ", "), ".")
      d <- d[d$item_id %in% names(ok)[ok], ]
    }
    fit <- NULL
    utils::capture.output(fit <- tryCatch(suppressWarnings(suppressMessages(dexter::fit_inter(d))),
                                          error = function(e) "dexter::fit_inter() could not fit this wave"))
    if (is.character(fit)) {
      message("Wave ", w, " left out: ", fit)
      next
    }
    cf <- stats::coef(fit)
    cf <- cf[!duplicated(cf$item_id), ]
    fits[[as.character(w)]] <- fit
    rows[[length(rows) + 1]] <- data.frame(wave_m = w, n = length(unique(d$person_id)), item_id = cf$item_id,
                                           sigma = cf$sigma, SE_sigma = cf$SE_sigma, fit_IM = cf$fit_IM)
  }
  if (length(rows) == 0) {
    stop("dexter::fit_inter() could not be fitted in any wave.", call. = FALSE)
  }
  structure(list(items = preview_table(do.call(rbind, rows)), fits = fits), class = "kitsPreview_inter")
}


#' @rdname kits_inter
#' @param x A `kitsPreview_inter` (the result of `kits_inter()`).
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param ... For `kits_inter()`: not used; any argument here is an error.
#'   For `plot()`: passed to dexter's `plot()` of the `fit_inter()` result, for example `items = "a1"`.
#' @export
print.kitsPreview_inter <- function(x, digits = 2, ...) {
  need_digits(digits)
  it <- x$items
  cat(sprintf("<kits_inter> dexter interaction model on the standard form of %d wave(s)\n", length(x$fits)))
  cat(sprintf("Items with |fit_IM| > 2: %d of %d (wave x item)\n", sum(abs(it$fit_IM) > 2, na.rm = TRUE), nrow(it)))
  print(format_table(utils::head(it[order(-abs(it$fit_IM)), ], 10), digits), row.names = FALSE, right = TRUE)
  invisible(x)
}


#' @rdname kits_inter
#' @param wave For `plot()`: which wave.
#' @export
plot.kitsPreview_inter <- function(x, wave, ...) {
  if (length(wave) != 1) {
    stop("`wave` must be one wave.", call. = FALSE)
  }
  fit <- x$fits[[as.character(wave)]]
  if (is.null(fit)) {
    stop("No fit for wave ", wave, ". Waves: ", paste(names(x$fits), collapse = ", "), ".", call. = FALSE)
  }
  graphics::plot(fit, ...)
}


# 每個題本（作答的題目組合）每一題的 pvalue 和 item-rest 相關（和 dexter 的 tia_tables() 相同的定義，自己算）
item_totals <- function(resp) {
  resp$item_id <- as.character(resp$item_id)
  resp$person_id <- as.character(resp$person_id)
  max_score <- tapply(resp$item_score, resp$item_id, max)
  book <- tapply(resp$item_id, resp$person_id, function(x) paste(sort(x), collapse = "\u001f"))
  total <- tapply(resp$item_score, resp$person_id, sum)
  resp$book <- unname(book[resp$person_id])
  resp$total <- unname(total[resp$person_id])
  rows <- lapply(split(resp, resp$book), function(d) {
    by_item <- split(d, d$item_id)
    data.frame(item_id = names(by_item),
               n_persons = vapply(by_item, nrow, integer(1)),
               pvalue = vapply(by_item, function(x) mean(x$item_score), numeric(1)) / max_score[names(by_item)],
               rir = vapply(by_item, function(x) rest_cor(x$item_score, x$total - x$item_score), numeric(1)))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# 題目和其他題總分的相關。題目沒有變異、只有一個人、或其他題的總分沒有變異時是 NA（0/0），不計入加權平均。
rest_cor <- function(x, rest) {
  if (length(x) < 2 || stats::sd(x) == 0 || stats::sd(rest) == 0) {
    return(NA_real_)
  }
  stats::cor(x, rest)
}
