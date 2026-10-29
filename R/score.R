#' Raw-score to ability table
#'
#' Two raw-score tables, for two uses (as in kits).
#'
#' `scores |> kits_subj_table()`: the table of each wave, from the scores of [kits_subj()] (with
#' `wave_m` from its `persons`; without it, all rows are one group).
#' Each wave uses its standard form: the item pattern (the combination of items answered) of most
#' children in the wave.
#' For each raw score of the standard form, the table lists the WLE
#' and how many children of the wave got this score on the standard form.
#' Children who answered another item pattern are not in the table (their share is
#' `100 - form_pct`); their scores are in the scores themselves.
#' With `forms = "all"`, every item pattern of the wave gets its own table (`form` 1 = the standard
#' form, 2 = the next most common, ...). The item patterns are in `attr(, "forms")`.
#' The scores keep what the table needs (the item parameters and the items of each item pattern),
#' and the counts come from the rows of the scores, so after `dplyr::filter()` they are those of
#' the rows kept.
#'
#' `kits_subj_table(item)`: the WLE of every raw score, from 0 to the maximum, of every item
#' pattern of `item` (a [kits_item()] keeps its item patterns; for a table of item parameters all
#' its items are one item pattern), for looking up scores in the field. It needs no data, and it
#' is the same as `dexter::ability_tables(parms, design, method = "WLE")` (dexter calls an item
#' pattern a booklet), except that the rows where dexter's WLE does not converge are computed as
#' the Jeffreys MAP, which is the same value (see [kits_subj()]).
#'
#' theta is on the scale of `item`.
#' Convert it to the reference cohort and to D the same way as the person scores
#' (`intercept + slope * theta`, `slope * se`).
#'
#' This is the reverse direction of a true-score function such as `TAM::IRT.truescore()`
#' (theta to expected raw score).
#' Solving "expected raw score = raw score" for theta gives the MLE, which is infinite at 0 and at the maximum;
#' this table uses the WLE, which is finite there, as [kits_subj()] does.
#'
#' Until kitsPreview 0.22.0 the responses could be an argument,
#' `kits_subj_table(data, item, wave_m)`, and the table per item pattern was `kits_wle_table(item)`;
#' both were removed in 0.23.0: use `kits_subj(data, item, persons) |> kits_subj_table()` and
#' `kits_subj_table(item)`.
#'
#' @param x The scores of [kits_subj()], or the item parameters: a [kits_item()] or [kits()] result,
#'   a `dexter::fit_enorm()` result, or a data.frame with `item_id`, `item_score`, `beta`.
#' @param forms For the table of each wave: `"standard"` (the standard form of each wave) or `"all"`
#'   (every item pattern of each wave).
#' @param ... Not used; any argument here is an error.
#' @return From scores: a `kitsPreview_subj_table` (a data.frame); one row is one raw score of one
#'   form in one wave: `wave_m`, `form` (1 = the standard form), `n_items`, `raw`, `theta`, `se`,
#'   `n` (children with this raw score on this form), `pct`, `cum_pct`,
#'   `wave_n` (all children in the wave), `form_pct` (percentage of the wave's children on this form).
#'   `attr(, "forms")` lists the item patterns: `wave_m`, `form`, `n`, `items`.
#'
#'   From item parameters: a table with one row per item pattern and per raw score (0 to the
#'   maximum): `pattern_id`, `raw`, `theta`, `se`.
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' item <- kits_item(X)
#'
#' # The standard form of each wave, from the scores
#' persons <- data.frame(visit_id = rownames(X), wave_m = rep(c(12, 24), each = 150))
#' scores <- kits_subj(X, item, persons = persons)
#' scores |>
#'   kits_subj_table()
#'
#' # Every raw score of every item pattern, without data
#' kits_subj_table(item)
#'
#' # A table of item parameters: all its items are one item pattern
#' parms <- data.frame(item_id = c("a1", "a1", "a2"), item_score = c(1, 2, 1), beta = c(-1, 0, 0.5))
#' kits_subj_table(parms)
#' @export
kits_subj_table <- function(x, forms = c("standard", "all"), ...) {
  is_resp <- is.matrix(x) || (is.data.frame(x) && !inherits(x, "kitsPreview_subj") &&
                                 "item_score" %in% names(x) && !"beta" %in% names(x))
  if (...length() > 0 || !is.character(forms) || is_resp) {
    text <- paste0("kits_subj_table() takes the scores of kits_subj() or the item parameters; ",
                   "the responses as an argument, kits_subj_table(data, item, wave_m), were removed ",
                   "in kitsPreview 0.23.0. Score first: kits_subj(data, item, persons) |> ",
                   "kits_subj_table().")
    stop_preview(text, "kitsPreview_error_input")
  }
  forms <- match.arg(forms)
  if (inherits(x, "kitsPreview_subj")) {
    return(subj_table_scores(x, forms))
  }
  wle_table(x)
}

# 分數表（kits_subj() 的結果）的原始分換算表：每個人次的題目組合從分數表的紀錄來，原始分是 raw
subj_table_scores <- function(scores, forms) {
  rec <- attr(scores, "raw_theta")
  if (is.null(rec) || is.null(rec$patterns)) {
    stop("These scores keep no record of their item patterns (made by kitsPreview before 0.23.0, ",
         "or not by ",
         "kits_subj()); score them again with kits_subj().", call. = FALSE)
  }
  need_cols(scores, c("visit_id", "pattern_id", "raw"), "scores")
  if (!all(as.character(scores$visit_id) %in% rec$persons)) {
    stop("Some rows of the scores do not come from this kits_subj() (for example scores stacked ",
         "with rbind()); ",
         "make the table of each calibration before stacking.", call. = FALSE)
  }
  wave <- if ("wave_m" %in% names(scores)) scores$wave_m else rep(NA_real_, nrow(scores))
  if ("wave_m" %in% names(scores) && anyNA(wave)) {
    stop("`wave_m` of the scores has NA; give every person-wave a wave.", call. = FALSE)
  }
  people <- data.frame(person_id = as.character(scores$visit_id),
                       set = rec$patterns$set[match(scores$pattern_id, rec$patterns$pattern_id)],
                       raw = scores$raw, wave_m = wave)
  subj_table_people(people, rec$parms, forms)
}

# 每個波次、每個題本的原始分換算表。people：每個人次一列（person_id、set、raw、wave_m）
subj_table_people <- function(people, parms, forms) {
  one_form <- function(w, f, form) {
    design <- data.frame(pattern_id = "form", item_id = form$items)
    tab <- wle_table(parms[parms$item_id %in% form$items, ], design = design)
    n <- as.integer(table(factor(form$raw, levels = tab$raw)))
    data.frame(wave_m = w, form = f, n_items = length(form$items), raw = tab$raw,
               theta = tab$theta,
               se = tab$se, n = n, pct = 100 * n / sum(n), cum_pct = cumsum(100 * n / sum(n)),
               wave_n = form$n_wave, form_pct = 100 * sum(n) / form$n_wave)
  }
  one_wave <- function(w) {
    in_wave <- dplyr::filter(people, wave_m %in% w)
    wave_forms <- forms_of(stats::setNames(in_wave$set, in_wave$person_id), in_wave$raw)
    use <- if (forms == "standard") 1 else seq_along(wave_forms)
    list(rows = purrr::map(use, \(f) one_form(w, f, wave_forms[[f]])),
         sets = purrr::map(use, \(f) form_row(w, f, wave_forms[[f]])))
  }
  per_wave <- purrr::map(sort(unique(people$wave_m), na.last = TRUE), one_wave)
  out <- per_wave |>
    purrr::map("rows") |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    preview_table("kitsPreview_subj_table")
  attr(out, "forms") <- per_wave |>
    purrr::map("sets") |>
    purrr::list_flatten() |>
    purrr::list_rbind()
  out
}

# attr(, "forms") 的一列：波次、題本、人數、題目
form_row <- function(w, f, form) {
  data.frame(wave_m = w, form = f, n = length(form$raw), items = paste(form$items, collapse = ", "))
}

# 每個人次答的題目組合（題號排序後接起來）和原始分（內部用）
person_sets <- function(resp) {
  sep <- "\u001f"   # 題號裡不會有的分隔字元
  sets <- tapply(resp$item_id, resp$person_id, function(x) paste(sort(x), collapse = sep))
  raw <- tapply(resp$item_score, resp$person_id, sum)
  data.frame(person_id = names(sets), set = unname(sets), raw = unname(raw[names(sets)]))
}

# 一個波次的題本（作答的題目組合），照人數由多到少；第一個是標準題本。
# sets：每個人次的題目組合（名字是 person_id），raw：同樣順序的原始分。
# 每個題本一個 list：items（題號）、raw（答這個題本的人的原始分）、n_wave（這個波次的總人數）。
forms_of <- function(sets, raw) {
  counts <- sort(table(sets), decreasing = TRUE)
  purrr::map(names(counts), \(s) {
    list(items = strsplit(s, "\u001f", fixed = TRUE)[[1]], raw = unname(raw[sets == s]),
         n_wave = length(sets))
  })
}

# 一個波次的題本（其他函數用）
wave_forms <- function(resp) {
  people <- person_sets(resp)
  forms_of(stats::setNames(people$set, people$person_id), people$raw)
}

# 標準題本（其他函數用）
modal_form <- function(resp) wave_forms(resp)[[1]]
