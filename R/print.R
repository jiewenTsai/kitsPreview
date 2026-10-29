#' Print a kitsPreview table
#'
#' Tables from kitsPreview (for example [kits_subj()], [kits_itemfit()], [kits_subj_table()], `coef()` of [kits_item()])
#' are data.frames with the class `kitsPreview_table`.
#' They print numbers with `digits` decimals (2 by default).
#' Only the printout is rounded: the numbers in the object keep their full precision.
#'
#' A table of up to 30 rows prints in full.
#' A longer table (for example the scores of every child) prints its first 10 rows,
#' then a line with the number of rows left out; `print(x, n = 50)` prints more,
#' and `as.data.frame(x)` gives all rows as a plain data.frame (as in kits).
#'
#' `purrr::list_rbind()` and `dplyr::bind_rows()` of tables of the same kind (for example the
#' scores of two [kits_subj()] calls) keep the class, so the norm functions still take the result;
#' tables of different kinds give a plain data.frame (as in kits).
#'
#' @param x A table from kitsPreview.
#' @param digits Decimals to print.
#' @param n Rows to print. By default all rows of a table of up to 30 rows, and the first 10 rows
#'   of a longer table. `Inf` prints every row.
#' @param ... Passed to `print()` for a data.frame, for example `row.names = FALSE`.
#' @return `x` (invisibly).
#' @examples
#' betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
#' X <- kits_sim(seq(-3, 3, length.out = 300), betas, seed = 1)
#' item <- kits_item(X)
#' coef(item)
#' print(coef(item), digits = 4)
#' coef(item)$thr[1]
#'
#' # A long table prints its first rows
#' s <- kits_subj(X, item)
#' print(s, n = 3)
#' @export
print.kitsPreview_table <- function(x, digits = 2, n = NULL, ...) {
  print_rows(x, digits, n, ...)
}

need_rows <- function(n) {
  if (!is.null(n) && (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0)) {
    stop_preview("`n` must be one number of rows (0 or more; Inf for all rows).",
                 "kitsPreview_error_input")
  }
}

# 印表格的列：30 列以內全印，再長只印前 10 列（或前 n 列），最後一行說還有幾列（和 kits 的 print_rows() 相同）
print_rows <- function(x, digits, n = NULL, ...) {
  need_digits(digits)
  need_rows(n)
  total <- nrow(x)
  if (is.null(n)) n <- if (total <= 30) total else 10
  print(format_table(utils::head(as.data.frame(x), n), digits), right = TRUE, ...)
  if (total > n) {
    cat(sprintf("... %d more rows (more: print(x, n = %d); all rows: as.data.frame(x))\n",
                total - n, min(total, 50)))
  }
  invisible(x)
}
