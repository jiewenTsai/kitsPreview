# 套件自己的結果：每個結果都留著它（kits_item() 在 $original；kits_subj() 是表，所以在 attr(, "original")）。
# summary(x) 先印 kitsPreview 的整理，再印套件自己的 summary()（或 print()）。

#' The package's own result behind a kitsPreview result
#'
#' The results of the estimation steps keep the object of the package that computed them,
#' so that you can read it with that package's own tools:
#' * [kits_item()], [kits()]: the `dexter::fit_enorm()`, `immer::immer_cml()` or `immer::immer_ccml()` result.
#' * [kits_subj()]: the `dexter::ability()` table.
#' * [kits_norms()]: the `cNORM::cnorm.shash()` fit of the kept start (a `cnormShash` object); for a
#'   norm by group, a list of them named by the group. It holds the coefficients and a few summaries
#'   of the data, not the scores of the children.
#'
#' The links ([kits_linking()], [kits_linking_d()]) are closed-form mean/sigma lines computed by kitsPreview
#' itself in base R, so they have no original object; `kits_code(link)` writes the whole link.
#' The other results (for example [kits_dif()], [kits_inter()], [kits_itemfit()]) keep no original
#' object either.
#'
#' The original objects keep the package's own names, on purpose: dexter calls the person-wave
#' `person_id` and the item pattern a booklet (`booklet_id`, `booklet_score`), where the tidy
#' results of kitsPreview use kits' names (`visit_id`, `pattern_id`, `raw`).
#'
#' `summary()` of these results prints the tidy summary of kitsPreview first,
#' then the package's own `summary()` (or `print()` when it has none), clearly separated.
#' The package's output is printed as it is, except the line of immer's `summary()` with the name of
#' the computer and the login (`nodename=`, `login=`), which is left out.
#' `print(x, original = TRUE)` does the same for a [kits_item()] result.
#'
#' @param x A result of [kits_item()], [kits()], [kits_subj()] or [kits_norms()].
#' @param object For `summary()`: a [kits_subj()] result (for [kits_item()] see its help page).
#' @param ... Not used.
#' @return The package's own object.
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' item <- kits_item(X)
#' class(kits_original(item))
#' coef(kits_original(item))      # dexter's own table
#' summary(item)
#'
#' \donttest{
#' # A norm keeps cNORM's own fit
#' set.seed(1)
#' age <- runif(600, 3, 36)
#' d <- 20 + 15 * log(age) + rnorm(600, 0, 3)
#' norm <- kits_norms(d, age, ages = seq(3, 36, by = 3), degrees = c(3, 2, 1, 1))
#' class(kits_original(norm))
#' }
#' @export
kits_original <- function(x) {
  if (inherits(x, "kitsPreview_fit")) {
    x <- x$item
  }
  if (inherits(x, "kitsPreview_subj")) {
    return(attr(x, "original"))
  }
  if (inherits(x, "kitsPreview_item")) {
    return(x$original)
  }
  if (inherits(x, "kitsPreview_norms")) {
    return(norm_original(x))
  }
  if (inherits(x, "kitsPreview_link")) {
    stop("A link has no original object: kitsPreview computes it itself (mean/sigma in base R). ",
         "kits_code(link) writes the code.", call. = FALSE)
  }
  stop("`x` must be a result of kits_item(), kits(), kits_subj() or kits_norms(); ",
       "the other results keep no original object.", call. = FALSE)
}

# 常模的 cNORM 物件：一個常模是一個 cnorm.shash() 結果，照組的常模是每組一個（名字是組）
norm_original <- function(x) {
  groups <- attr(x, "groups")
  original <- if (is.null(groups)) list(attr(x, "original")) else lapply(groups, attr, "original")
  if (length(original) == 0 || any(vapply(original, is.null, logical(1)))) {
    stop("This norm table has no original object: the cNORM fit is kept only by the result of ",
         "kits_norms() (a part of it, or a table made by hand, has none). ",
         "kits_code(norm) writes the cnorm.shash() code.", call. = FALSE)
  }
  if (is.null(groups)) original[[1]] else original
}

# 結果用的是哪一個引擎（kits_subj() 一律是 dexter）
engine_of <- function(x) {
  engine <- if (is.data.frame(x)) NULL else x$engine
  if (is.null(engine)) "dexter" else engine
}

# summary() 的結果：印出來時先印整理過的，再印套件自己的。
kits_summary <- function(object) {
  structure(list(object = object), class = "kitsPreview_summary")
}

#' @export
print.kitsPreview_summary <- function(x, ...) {
  object <- x$object
  if (inherits(object, "kitsPreview_subj")) {
    print_subj_summary(object)
  } else {
    print(object)
  }
  show_original(object)
  invisible(x)
}

# kits_subj 的簡短整理：人數、theta 和 se 的分佈、flag
print_subj_summary <- function(subj) {
  cat(sprintf("<kits_subj> WLE (dexter): %d person-waves\n", nrow(subj)))
  print(summary(as.data.frame(subj)[c("theta", "se", "infit", "outfit")]))
  print(table(flag = subj$flag))
}

# 套件自己的輸出，前後用一行標出來
show_original <- function(x) {
  original <- kits_original(x)
  if (is.null(original)) {
    cat("\n(The original output is not kept in this object; for example head() drops it.)\n")
    return(invisible(NULL))
  }
  label <- original_label(x)
  cat(sprintf("\n---- Original output: %s ----\n", label))
  cat(no_machine_lines(utils::capture.output(print_original(original))), sep = "\n")
  cat(sprintf("---- End of the original output (kits_original(x) returns the object) ----\n"))
  invisible(NULL)
}

# 套件自己的 print()（dexter）或 summary()（immer 的 summary() 自己印）
print_original <- function(original) {
  if (inherits(original, c("enorm", "data.frame"))) {
    return(print(original_head(original)))
  }
  shown <- withVisible(summary(original))
  if (shown$visible) print(shown$value)
}

# immer 的 summary() 印出電腦的名字和登入的帳號（"nodename=...", "login=..."）：
# 這幾行拿掉，其他照原樣印
no_machine_lines <- function(lines) {
  lines[!grepl("nodename=|login=", lines)]
}

# 很長的表只印前幾列
original_head <- function(original) {
  if (is.data.frame(original)) utils::head(original) else original
}

# 套件和函數的名字，例如 "immer::immer_cml()"
original_label <- function(x) {
  original <- kits_original(x)
  if (inherits(original, "enorm")) return("dexter::fit_enorm()")
  if (inherits(original, "immer_cml")) return("immer::immer_cml()")
  if (inherits(original, "immer_ccml")) return("immer::immer_ccml()")
  if (is.data.frame(original)) return("dexter::ability() (first rows)")
  class(original)[1]
}

#' @rdname kits_original
#' @export
summary.kitsPreview_subj <- function(object, ...) {
  kits_summary(object)
}
