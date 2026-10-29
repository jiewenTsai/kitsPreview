#' Item calibration: the PCM by dexter or immer, plus the threshold of every step
#'
#' Calibrates the items with the partial credit model (PCM) by conditional maximum likelihood (CML),
#' then computes the Thurstonian threshold of every step.
#' The estimation is done by another package, chosen with `engine`: dexter (the default), immer,
#' or immer's pairwise (composite) CML, the method of the GSED D-score.
#' kitsPreview only converts the result to one table, so that the packages and methods can be compared,
#' and so that you can see how to do this step directly with dexter or immer ([kits_code()]).
#'
#' kits requires the scores of an item to be 0, 1, ..., m (no gaps).
#' Then every engine fits the same model, with one step parameter `beta` per step.
#' Scores with gaps are refused: recode the item first.
#'
#' The threshold `thr` of step k is an ability value:
#' at this ability, the probability of scoring k or more is exactly .5.
#' Unlike the step parameters `beta`, the thresholds of an item are always in increasing order.
#' Step 1 is called the milestone step.
#' The SE of the threshold uses the delta method.
#'
#' The result can be passed directly to [kits_subj()], [kits_linking()], [kits_linking_d()] and [kits_drift()],
#' whatever the engine.
#'
#' Every item in the calibration needs a 0 score and at least two scores.
#' In a later wave, easy items are often passed by everyone.
#' When such an item is fixed (`fixed_params`), it is left out of the calibration and keeps its fixed parameters
#' (in `thresholds`, with SE 0, like the other fixed items); a message names it.
#' When it is not fixed, the function stops: fix it, or leave it out of the data.
#'
#' The result has the class `kitsPreview_item`. The function name, the arguments, the columns and the printout
#' are the same as `kits::kits_item()`, so the two packages can be read side by side;
#' only the class name differs, so loading both packages never changes the methods of kits.
#'
#' @section The origin of the scale:
#' The PCM fixes the unit (the slope is 1) but not the origin: adding a constant to every beta
#' and to every ability gives the same probabilities.
#' Each package picks its own origin:
#' * dexter: the sum of all betas is 0.
#' * immer (`normalization = "sum"`): a sum of its cumulative parameters is 0.
#' * immer's CCML: the origin is left free (the optimizer starts at 0).
#'
#' So that the engines can be compared, immer is put on dexter's origin:
#' the constant `shift` (the mean of immer's betas) is subtracted from every beta,
#' so that the sum of all betas is 0. The thresholds move by the same constant.
#'
#' @section Engine "dexter" (the default):
#' `dexter::fit_enorm()` by CML.
#' dexter fits the extended nominal response model (ENORM): the score of a category is its weight.
#' With scores 0, 1, ..., m it is exactly the PCM. The original code:
#' ```
#' fit <- dexter::fit_enorm(X)
#' cf <- coef(fit)                 # item_id, item_score, beta, SE_beta
#' V <- fit$est$acov.beta          # covariance of the betas
#' ```
#'
#' @section Engine "immer":
#' `immer::immer_cml()` by CML. Its parameters are cumulative:
#' `b_ik` = beta_i1 + ... + beta_ik, so the step parameters are the differences.
#' The original code:
#' ```
#' fit <- immer::immer_cml(dat = as.data.frame(X), irtmodel = "PCM", normalization = "sum")
#' fit$b                           # b_ik, named "item_Cat1", "item_Cat2", ...
#' beta <- b_ik - b_i(k-1)         # within each item; then subtract the mean
#' ```
#' The covariance: `b = W par`, so beta = D W par and V = (D W) vcov (D W)', with D the differences.
#' immer and dexter maximize the same likelihood, but with different optimizers and convergence
#' criteria (immer: `stats::optim()` with BFGS and its default relative tolerance; dexter: its own
#' iterative scaling, then Newton-Raphson), so how far apart they stop depends on the data: about
#' 1e-6 on small simulated data, 9.2e-4 on the KIT language data of M03, and 0.017 (largest beta;
#' 0.009 for the thresholds, about 0.05 SE) on kits' demo data `kits::kitm03` (language).
#' Pass `control = list(reltol = 1e-14, maxit = 5000)` to immer to bring it close to dexter (3e-7
#' on the demo data).
#' immer's faster `use_rcpp = TRUE` is not used: on a simulated KIT-sized design (47,000 person-waves, 24 items)
#' it stopped far from the maximum (1.96 logits from CML) while reporting convergence.
#' immer cannot fix item parameters, so `fixed_params` needs the engine "dexter".
#'
#' @section Why CML needs no population model:
#' CML conditions on each person's raw score within his or her item pattern (the combination of
#' items answered; dexter calls it a booklet). In the PCM the raw score is
#' sufficient for theta, so theta drops out of the conditional likelihood: no population distribution is
#' assumed, and the reason a person got an item pattern does not matter.
#' In KIT the item pattern is chosen by age (the wave), and age strongly predicts theta, so this matters.
#'
#' Marginal maximum likelihood (MML, for example `TAM::tam.mml()`) integrates theta over an assumed
#' population distribution instead. With one normal for everyone, the item pattern depends on theta through a variable
#' the model left out, the missing responses are not ignorable for that model (Mislevy & Wu, 1996),
#' and the item parameters absorb the difference. MML comes close to CML only with one population per wave
#' (`group = wave` in TAM), which puts the variable that drove the design into the model.
#' Formally, CML equals MML with a nonparametric (saturated) score distribution per item pattern
#' (de Leeuw & Verhelst, 1986; Follmann, 1988; Lindsay, Clogg & Grego, 1991; see also Eggen & Verhelst, 2011;
#' Zwitser & Maris, 2015). The comparison with TAM is in a separate tutorial; kitsPreview has no MML engine.
#'
#' In short: MML needs the reason for "who got which item pattern" in the model; CML does not,
#' because it conditions on the raw score within each item pattern. This is why KIT uses CML.
#'
#' @section Engine "ccml" (pairwise, as in the GSED D-score):
#' `immer::immer_ccml()`: composite conditional maximum likelihood (CCML; Zwinderman, 1995).
#' For every pair of items i and j it conditions on the sum of the two scores, so theta drops out,
#' as in CML, and no population model is needed; the pairwise likelihoods are multiplied and maximized.
#' The GSED D-score estimates its item difficulties this way (with `sirt::rasch.pairwise.itemcluster()`,
#' for 0/1 items); `immer_ccml()` extends it to the PCM and gives the same estimates on 0/1 items.
#' immer needs a design matrix `A` (items x categories x parameters): category h of item i is the sum
#' of its first h steps, so `A[i, h, l]` is 1 when parameter l is step h of item i or an earlier step.
#' The original code:
#' ```
#' A <- array(0, dim = c(ncol(X), K, n))     # K = the highest score, n = the number of steps
#' for (l in 1:n) A[item[l], step[l]:K, l] <- 1
#' fit <- immer::immer_ccml(dat = as.data.frame(X), irtmodel = "PCM", A = A,
#'                          control = list(iter.max = 5000, eval.max = 5000))
#' beta <- fit$coef                # the step parameters; vcov(fit) is a sandwich covariance
#' ```
#' `nlminb()`'s default number of iterations is too small for KIT-sized data, so the default here is 5000;
#' a warning says when it did not converge.
#' When the data fit the PCM, CCML and CML estimate the same values. CCML uses only pairs of items;
#' on four KIT calibrations its SEs were 1.2 to 2.6 times those of CML (median over the steps).
#' When the data do not fit the PCM exactly, the two methods find different "closest" PCMs,
#' so on real data they differ, most for some misfitting items.
#'
#' @section What is engine-free:
#' The thresholds (`thr`), the links of [kits_linking()] and [kits_linking_d()], the drift check [kits_drift()],
#' the item and person fit ([kits_itemfit()], [kits_subj()]), the score tables,
#' the norms (cNORM, [kits_norms()]), DIF ([kits_dif()], dexter) and the interaction model
#' ([kits_inter()], dexter) do not depend on the engine: they use the `thresholds` table,
#' or always call the same package.
#'
#' @param data Responses: a matrix (one row per person-wave, one column per item,
#'   row names are visit_id, column names are item ids, NA means not administered),
#'   or a long data.frame with `visit_id`, `item_id`, `item_score`.
#'   Include only the items of one domain.
#'   (dexter calls it `dataSrc`, and the person-wave `person_id`; kitsPreview gives the data to
#'   dexter with dexter's names.)
#' @param ... Passed to the engine's function: `dexter::fit_enorm()`, `immer::immer_cml()` or `immer::immer_ccml()`.
#'   `fixed_params = subset(coef(old), item_id %in% fix)` fixes items at earlier estimates
#'   (engine "dexter" only; `fixed_params` can also be a whole `kits_item()` result).
#'   Predicates are not accepted: select rows and columns first.
#'   For `plot()`: not used.
#' @param engine The package and method that estimate: `"dexter"` (CML, the default), `"immer"` (CML)
#'   or `"ccml"` (immer's pairwise CCML, the method of the GSED D-score).
#'   immer is optional (Suggests); when it is missing, the error (class `kitsPreview_error_missing_package`)
#'   says how to install it.
#' @return A `kitsPreview_item`: a list that prints briefly. Its elements:
#'   * `thresholds`: one row per step: `item_id`, `item_score` (the step), `beta`, `beta_se`, `thr`, `thr_se`.
#'     Also returned by `coef()`.
#'   * `original`: the package's own result (a `fit_enorm`, `immer_cml` or `immer_ccml` object).
#'     Also returned by [kits_original()].
#'   * `fit`: the same object as `original` (its name before version 0.9.0).
#'   * `engine`: the engine used; `method`: `"CML"` or `"CCML"`.
#'   * `shift`: the constant subtracted from the package's betas (see the origin section); 0 for dexter.
#'   * `n_persons`: the number of person-waves in the calibration.
#'   * `left_out`: fixed items left out of the calibration (see below).
#'   * `design`: the item patterns, one per combination of items answered (`pattern_id`,
#'     `item_id`), including left-out items. The item patterns are named `bk01`, `bk02`, ... in the
#'     order of their first visit (visits sorted by `visit_id`), as in kits and dexter.
#'     (The original object keeps dexter's names.)
#'   * `settings`: the arguments passed to the engine (used by [kits_code()]).
#'   * `itemfit`: item fit on the same data, one row per item (`item_id`, `n`, `pvalue`, `rir`,
#'     `infit`, `outfit`, `flag`); the same table as `kits_itemfit(data, item)`, see [kits_itemfit()].
#'     It holds only per-item values. When it cannot be computed (for example with one item), it is
#'     `NULL` and `notes` says why; the calibration itself goes on.
#'     The printout says how many items have an infit or outfit outside 0.6 to 1.4;
#'     `plot(item, type = "fit")` draws them. For other data, use [kits_itemfit()].
#'   * `notes`: notes on the object (empty when there are none).
#' @references
#' de Leeuw, J., & Verhelst, N. (1986). Maximum likelihood estimation in generalized Rasch models.
#' *Journal of Educational Statistics, 11*(3), 183-196. \doi{10.3102/10769986011003183}
#'
#' Eggen, T. J. H. M., & Verhelst, N. D. (2011). Item calibration in incomplete testing designs.
#' *Psicologica, 32*(1), 107-132.
#'
#' Follmann, D. (1988). Consistent estimation in the Rasch model based on nonparametric margins.
#' *Psychometrika, 53*(4), 553-562. \doi{10.1007/BF02294407}
#'
#' Lindsay, B., Clogg, C. C., & Grego, J. (1991). Semiparametric estimation in the Rasch model and related
#' exponential response models, including a simple latent class model for item analysis.
#' *Journal of the American Statistical Association, 86*(413), 96-107. \doi{10.1080/01621459.1991.10475008}
#'
#' Mislevy, R. J., & Wu, P.-K. (1996). *Missing responses and IRT ability estimation: Omits, choice, time limits,
#' and adaptive testing* (ETS Research Report RR-96-30). Educational Testing Service.
#' \doi{10.1002/j.2333-8504.1996.tb01708.x}
#'
#' Zwinderman, A. H. (1995). Pairwise parameter estimation in Rasch models.
#' *Applied Psychological Measurement, 19*(4), 369-375. \doi{10.1177/014662169501900406}
#'
#' Zwitser, R. J., & Maris, G. (2015). Conditional statistical inference with multistage testing designs.
#' *Psychometrika, 80*(1), 65-84. \doi{10.1007/s11336-013-9369-6}
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' item <- kits_item(X)
#' item
#' coef(item)
#'
#' # Item fit on the same data (kits_itemfit() checks the items on other data)
#' item$itemfit
#' p <- plot(item, type = "fit")
#' if (interactive()) p
#'
#' # The same model by another package
#' if (requireNamespace("immer", quietly = TRUE)) {
#'   item_immer <- kits_item(X, engine = "immer")
#'   max(abs(coef(item_immer)$beta - coef(item)$beta))
#'   summary(item_immer)     # the tidy table, then immer's own summary
#'
#'   # Pairwise CCML, as in the GSED D-score
#'   item_ccml <- kits_item(X, engine = "ccml")
#'   max(abs(coef(item_ccml)$beta - coef(item)$beta))
#' }
#' @export
kits_item <- function(data, ..., engine = c("dexter", "immer", "ccml")) {
  no_predicate(match.call(expand.dots = FALSE)$...)
  engine <- match.arg(engine)
  need_package(ENGINE_PACKAGES[[engine]], engine)
  resp <- if (is.data.frame(data) && isTRUE(attr(data, "kits_long"))) data else resp_long(data, "data")
  args <- list(...)
  fixed <- NULL
  if (!is.null(args$fixed_params)) {
    need_fixed_support(engine)
    fixed <- param_frame(args$fixed_params)   # dexter 1.8.1 收 enorm 物件時會出錯，所以一律轉成表
    args$fixed_params <- fixed
  }

  # 每一題都要有 0 分、至少兩種分數。固定的題不符合時，不放進校準，參數照抄固定的值。
  ok <- tapply(resp$item_score, resp$item_id, function(x) min(x) == 0 && length(unique(x)) > 1)
  bad <- names(ok)[!ok]
  not_fixed <- setdiff(bad, fixed$item_id)
  if (length(not_fixed) > 0) {
    stop("Item(s) with no 0 score or only one score: ", paste(not_fixed, collapse = ", "),
         ". They cannot be estimated; leave them out of the data", if (!is.null(fixed)) " or fix them" else "", ".", call. = FALSE)
  }
  if (length(bad) > 0) {
    message("Fixed item(s) with no 0 score or only one score in these data are left out of the calibration ",
            "and keep their fixed parameters: ", paste(bad, collapse = ", "), ".")
  }
  if (all(unique(resp$item_id) %in% bad)) {
    stop("Every item is fixed and has no 0 score or only one score, so there is nothing to calibrate. ",
         "Score with the fixed parameters directly: kits_subj(X, fixed).", call. = FALSE)
  }
  calib <- resp[!resp$item_id %in% bad, ]
  if (engine != "dexter") {
    need_consecutive(calib)   # dexter 自己檢查（在 item_thresholds 裡）
  }
  fitted <- fit_engine(engine, calib, args)
  settings <- list(args = args[setdiff(names(args), "fixed_params")], fixed = fixed)
  thr <- if (engine == "dexter") item_thresholds(fitted$original) else steps_thresholds(fitted$steps, fitted$V)
  if (length(bad) > 0) {
    thr <- rbind(thr, fixed_thresholds(fixed[fixed$item_id %in% bad, ]))
    thr <- thr[order(thr$item_id, thr$item_score), ]
    rownames(thr) <- NULL
  }
  thr <- preview_table(thr)
  item_fit <- itemfit_or_note(resp, thr)   # 和 kits_itemfit(data, item) 相同；算不出來時是 NULL 加說明
  structure(list(thresholds = thr, fit = fitted$original, original = fitted$original,
                 engine = engine, method = ENGINE_METHODS[[engine]], shift = fitted$shift %||% 0,
                 origin = fitted$origin, n_persons = length(unique(calib$person_id)), left_out = bad,
                 design = item_design(resp), settings = settings,
                 itemfit = item_fit$itemfit, notes = item_fit$note),
            class = "kitsPreview_item")
}

# ... 裡不能有沒有名字的引數（dexter 的 predicate）：在求值之前就停下來
no_predicate <- function(dots) {
  if (length(dots) > 0 && (is.null(names(dots)) || any(names(dots) %in% c("", "predicate")))) {
    stop("kits_item() takes no predicate; give other arguments by name (e.g. fixed_params = ...). ",
         "To use part of the data, select rows and columns of the matrix first, e.g. X[keep, items].", call. = FALSE)
  }
}

# 沒放進校準的固定題：照抄固定的 beta，門檻從 beta 算，SE 是 0
fixed_thresholds <- function(copied) {
  one_item <- function(d) {
    d <- d[order(d$item_score), ]
    data.frame(item_id = d$item_id, item_score = d$item_score, beta = d$beta, beta_se = 0,
               thr = vapply(seq_along(d$beta), function(k) step_threshold(d$beta, k)[["thr"]], numeric(1)), thr_se = 0)
  }
  do.call(rbind, lapply(split(copied, copied$item_id), one_item))
}

# 作答組合（item pattern）：每個人答的題目組合（包括沒放進校準的固定題）。
# 編號和 kits、dexter 相同：人次照 visit_id 排（C locale），組合照出現的順序叫 bk01、bk02、...
# （位數是 ceiling(log10(組合數 + 1))）；列名是組合第一個人次的 visit_id 加上 1, 2, ...
item_design <- function(resp) {
  sets <- tapply(as.character(resp$item_id), as.character(resp$person_id),
                 function(x) paste(sort(x), collapse = "\u001f"))
  sets <- sets[sort(names(sets), method = "radix")]
  books <- sets[!duplicated(sets)]
  items <- strsplit(books, "\u001f", fixed = TRUE)
  data.frame(pattern_id = rep(booklet_names(length(books)), lengths(items)),
             item_id = unlist(items, use.names = FALSE))
}

# dexter 給作答組合的名字：bk 加上編號（和 kits 的 booklet_names() 相同）
booklet_names <- function(n) {
  sprintf(paste0("bk%0", ceiling(log10(n + 1)), "i"), seq_len(n))
}

# 只有 dexter 可以固定參數
need_fixed_support <- function(engine) {
  if (engine == "dexter") {
    return(invisible(TRUE))
  }
  stop_preview(sprintf("engine = \"%s\" cannot fix item parameters (`fixed_params`). Use engine = \"dexter\".", engine),
               "kitsPreview_error_engine_unsupported")
}


#' @rdname kits_item
#' @param x,object A `kitsPreview_item` (the result of `kits_item()`).
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param original For `print()`: also print the package's own output (as `summary()` does).
#' @export
print.kitsPreview_item <- function(x, digits = 2, original = FALSE, ...) {
  need_digits(digits)
  thr <- x$thresholds
  if (!is.data.frame(thr) || is.null(x$original %||% x$fit)) {
    cat("<kits_item> (incomplete object)\n")
    return(invisible(x))
  }
  engine <- x$engine %||% "dexter"
  method <- x$method %||% ENGINE_METHODS[[engine]]
  cat(sprintf("<kits_item> PCM (%s, %s): %d person-waves, %d items, %d steps, %d item pattern(s)\n",
              ENGINE_LABELS[[engine]], method, x$n_persons, length(unique(thr$item_id)), nrow(thr),
              length(unique(x$design$pattern_id))))
  print(format_table(utils::head(thr, 6), digits), row.names = FALSE, right = TRUE)
  if (nrow(thr) > 6) cat("... (all steps: coef(x) or x$thresholds)\n")
  if (length(x$left_out) > 0) cat("Fixed, left out of the calibration (no 0 score or one score):", paste(x$left_out, collapse = ", "), "\n")
  itemfit_line(x)
  if (original) show_original(x)
  invisible(x)
}

#' @rdname kits_item
#' @export
summary.kitsPreview_item <- function(object, ...) {
  kits_summary(object)
}

#' @rdname kits_item
#' @export
coef.kitsPreview_item <- function(object, ...) {
  object$thresholds
}

#' @rdname kits_item
#' @param type For `plot()`: what to draw. Only `"fit"`: the infit and outfit of each item
#'   (`x$itemfit`), with 0.6 to 1.4 shaded. There is no default plot; for a Wright map, use
#'   `plot(kits(data))`.
#' @export
plot.kitsPreview_item <- function(x, type, ...) {
  if (missing(type)) {
    stop_preview(paste0("plot() of a kits_item needs `type`: plot(x, type = \"fit\") draws the item fit. ",
                        "For a Wright map, use plot(kits(data))."), "kitsPreview_error_input")
  }
  if (!identical(type, "fit")) {
    stop_preview("`type` must be \"fit\".", "kitsPreview_error_input")
  }
  if (is.null(x$itemfit)) {
    stop_preview(paste0("This kits_item has no item fit", if (length(x$notes) > 0) paste0(" (", x$notes[1], ")"),
                        ". Use kits_itemfit(data, x)."), "kitsPreview_error_input")
  }
  itemfit_plot(x$itemfit)
}

# 題目適配圖（內部用，給 plot(item, type = "fit")）：每題的 infit、outfit，範圍塗灰
itemfit_plot <- function(fit, range = ITEMFIT_RANGE) {
  looks <- list(infit = list(col = COL_TWO[1], symbol = SYM_TWO[1]),
                outfit = list(col = COL_TWO[2], symbol = SYM_TWO[2]))
  p <- plotly::plot_ly()
  for (st in names(looks)) {
    hover <- sprintf("%s: %s %.2f (n = %d)", fit$item_id, st, fit[[st]], as.integer(fit$n))
    marker <- list(color = looks[[st]]$col, symbol = looks[[st]]$symbol, size = 9)
    p <- plotly::add_markers(p, x = fit$item_id, y = fit[[st]], name = st, marker = marker,
                             hovertext = hover, hoverinfo = "text")
  }
  band <- list(type = "rect", x0 = 0, x1 = 1, y0 = range[1], y1 = range[2], xref = "paper", yref = "y",
               fillcolor = "rgba(127,127,127,0.15)", line = list(width = 0), layer = "below")
  x_axis <- list(title = LAB$item, type = "category", categoryorder = "array", categoryarray = fit$item_id)
  plotly::layout(p, title = plot_title(sprintf(LAB$title_itemfit, range[1], range[2])), margin = list(t = 70),
                 xaxis = x_axis, yaxis = list(title = LAB$mean_square), shapes = list(band), font = FONT)
}


#' Calibrate and score in one step
#'
#' [kits_item()] and then [kits_subj()] on the same data.
#' Use it when no link is needed in between (for example a single cohort).
#' With linking, call the two functions separately (see the vignette).
#'
#' `plot()` draws a Wright map: the abilities on the left and the step thresholds on the right,
#' on the same theta axis. The abilities are binned first, so the plot holds only counts per bin.
#'
#' @inheritParams kits_item
#' @param ... Passed to [kits_item()], for example `engine = "immer"`.
#'   The scores always use dexter's WLE ([kits_subj()]), whatever the calibration engine.
#' @return A `kitsPreview_fit`: a list with `item` (the [kits_item()] result) and `subj` (the [kits_subj()] result).
#'   It is the same as the `kits` object of `kits::kits()`, with its own class name.
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' k <- kits(X)
#' k
#' head(k$subj)
#' p <- plot(k)
#' if (interactive()) p
#' @export
kits <- function(data, ...) {
  no_predicate(match.call(expand.dots = FALSE)$...)
  resp <- resp_long(data, "data")
  attr(resp, "kits_long") <- TRUE   # 已經檢查過、轉成長格式
  item <- kits_item(resp, ...)
  structure(list(item = item, subj = kits_subj(resp, item)), class = "kitsPreview_fit")
}


#' @rdname kits
#' @param x A `kitsPreview_fit` (the result of `kits()`).
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @export
print.kitsPreview_fit <- function(x, digits = 2, ...) {
  print(x$item, digits = digits)
  th <- x$subj$theta
  if (length(th) == 0) {
    return(invisible(x))
  }
  cat(sprintf("Abilities (WLE, $subj): %d person-waves, theta mean %s, SD %s, range %s to %s\n",
              length(th), fmt(mean(th), digits), fmt(stats::sd(th), digits), fmt(min(th), digits), fmt(max(th), digits)))
  invisible(x)
}


#' @rdname kits
#' @export
plot.kitsPreview_fit <- function(x, ...) {
  thr <- x$item$thresholds
  first <- thr[thr$item_score == 1, ]
  levels <- first$item_id[order(first$thr)]   # 題目照 milestone step 由易到難排
  thr$x <- match(thr$item_id, levels)
  symbols <- c("circle", "square", "diamond", "triangle-up", "cross", "x")

  # 先分組算次數再畫，圖裡只有每組的人數，沒有個人的值
  bins <- graphics::hist(x$subj$theta, breaks = 30, plot = FALSE)
  persons <- plotly::plot_ly(x = bins$counts, y = bins$mids, type = "bar", orientation = "h", marker = list(color = "#999999"),
                             name = LAB$persons, hovertext = sprintf("%.2f to %.2f: %d", utils::head(bins$breaks, -1),
                                                                         bins$breaks[-1], bins$counts), hoverinfo = "text")
  items <- plotly::plot_ly()
  for (k in sort(unique(thr$item_score))) {
    d <- thr[thr$item_score == k, ]
    items <- plotly::add_markers(items, x = d$x, y = d$thr, name = paste(LAB$step, k),
                                 marker = list(color = "#0072B2", size = 8, symbol = symbols[(k - 1) %% length(symbols) + 1]),
                                 hovertext = sprintf("%s, %s %d: %.2f", d$item_id, LAB$step, k, d$thr), hoverinfo = "text")
  }
  plotly::subplot(persons, items, shareY = TRUE, widths = c(0.25, 0.75), titleX = TRUE, margin = 0.02) |>
    plotly::layout(title = plot_title(LAB$title_wright), margin = list(t = 70),
                   xaxis = list(title = LAB$persons, autorange = "reversed"),
                   xaxis2 = list(title = "", tickvals = seq_along(levels), ticktext = levels, tickangle = -90),
                   yaxis = list(title = LAB$theta),
                   font = FONT)
}
