# 內部用的小工具（不匯出）

# 檢查欄位。缺的時候停下來，說清楚缺哪幾欄。
need_cols <- function(data, cols, arg) {
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data.frame with column(s): %s.", arg, paste(cols, collapse = ", ")), call. = FALSE)
  }
  miss <- setdiff(cols, names(data))
  if (length(miss) > 0) {
    stop(sprintf("`%s` is missing column(s): %s. It needs: %s.",
                 arg, paste(miss, collapse = ", "), paste(cols, collapse = ", ")), call. = FALSE)
  }
}

# 用固定的亂數種子跑。跑完還原使用者原本的亂數狀態。
with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = globalenv())
  old <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", old, envir = globalenv()) else rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  code
}

# 常模表依 segment 分段，依月齡由小到大排。沒有 segment 欄時就是一段。
norm_segments <- function(norm) {
  segments <- if ("segment" %in% names(norm)) split(norm, norm$segment) else list(all = norm)
  segments <- lapply(segments, function(s) s[order(s$age_m), ])
  segments[order(vapply(segments, function(s) min(s$age_m), numeric(1)))]
}

# 作答資料轉成內部的長格式（person_id、item_id、item_score）。
# 內部的長格式就是 dexter 要的格式，所以人次的欄叫 dexter 的名字 person_id；使用者看到的一律是 visit_id
# （輸入的欄和矩陣的列名是 visit_id，結果在交給使用者之前換回 visit_id）。
# 收兩種：
# * 矩陣（主要）：一列一個人次、一欄一題；列名是 visit_id，欄名是題號；NA 是沒施測。和 dexter::fit_enorm() 收的一樣。
# * 長格式 data.frame：visit_id、item_id、item_score（別的欄名，例如 0.22.0 的 person_id，停下來說缺哪一欄）。
resp_long <- function(resp, arg = "resp") {
  if (is.data.frame(resp) && isTRUE(attr(resp, "kits_long"))) {
    return(resp)
  }
  if (is.data.frame(resp) && !all(c("visit_id", "item_id", "item_score") %in% names(resp))) {
    miss <- setdiff(c("visit_id", "item_id", "item_score"), names(resp))
    stop(sprintf("`%s` is a data.frame without the column(s) %s (it has: %s). ", arg,
                 paste(miss, collapse = ", "), paste(names(resp), collapse = ", ")),
         "A long data.frame needs visit_id, item_id, item_score; for a wide table (one column per ",
         "item), use as.matrix() and set the row names to visit_id.", call. = FALSE)
  }
  if (is.matrix(resp)) {
    if (nrow(resp) == 0 || ncol(resp) == 0) {
      stop(sprintf("`%s` has no rows or no columns.", arg), call. = FALSE)
    }
    if (is.null(rownames(resp)) || is.null(colnames(resp))) {
      stop(sprintf("`%s` is a matrix: it needs row names (visit_id) and column names (item_id).", arg), call. = FALSE)
    }
    if (any(is.na(rownames(resp)) | rownames(resp) == "") || any(is.na(colnames(resp)) | colnames(resp) == "")) {
      stop(sprintf("`%s` has empty row or column names.", arg), call. = FALSE)
    }
    if (anyDuplicated(rownames(resp))) {
      stop(sprintf("`%s` has repeated row names (for example %s). ", arg, rownames(resp)[duplicated(rownames(resp))][1]),
           "Each row is one person-wave, so use an id per person-wave (visit_id), e.g. paste(subj_id, wave_m).",
           call. = FALSE)
    }
    if (anyDuplicated(colnames(resp))) {
      stop(sprintf("`%s` has repeated column names (item ids).", arg), call. = FALSE)
    }
    check_scores(resp, arg)
    idx <- which(!is.na(resp), arr.ind = TRUE)
    if (nrow(idx) == 0) {
      stop(sprintf("`%s` has no responses (all NA).", arg), call. = FALSE)
    }
    idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
    empty <- setdiff(rownames(resp), rownames(resp)[idx[, 1]])
    if (length(empty) > 0) {
      message(length(empty), " row(s) of `", arg, "` have no responses and are left out: ", paste(utils::head(empty, 5), collapse = ", "),
              if (length(empty) > 5) ", ..." else "")
    }
    return(data.frame(person_id = rownames(resp)[idx[, 1]], item_id = colnames(resp)[idx[, 2]],
                      item_score = as.integer(resp[idx])))
  }
  if (is.atomic(resp) && is.null(dim(resp))) {
    stop(sprintf("`%s` is a vector, not a matrix. To keep one row as a matrix, use X[1, , drop = FALSE].", arg), call. = FALSE)
  }
  if (!is.data.frame(resp)) {
    stop(sprintf("`%s` must be a matrix (rows = person-waves, columns = items) ", arg),
         "or a long data.frame with visit_id, item_id, item_score.", call. = FALSE)
  }
  resp <- as.data.frame(resp)[!is.na(resp$item_score), c("visit_id", "item_id", "item_score")]
  if (nrow(resp) == 0) {
    stop(sprintf("`%s` has no responses.", arg), call. = FALSE)
  }
  check_scores(resp$item_score, arg)
  resp <- data.frame(person_id = as.character(resp$visit_id), item_id = as.character(resp$item_id),
                     item_score = as.integer(resp$item_score))   # dexter 的名字（內部用）
  if (any(is.na(resp$person_id) | resp$person_id == "") || any(is.na(resp$item_id) | resp$item_id == "")) {
    stop(sprintf("`%s` has empty visit_id or item_id.", arg), call. = FALSE)
  }
  if (anyDuplicated(resp[c("person_id", "item_id")])) {
    stop(sprintf("`%s` has a person-wave with the same item more than once. ", arg),
         "Each person-wave needs its own visit_id, e.g. paste(subj_id, wave_m).", call. = FALSE)
  }
  resp
}

# 作答分數：0, 1, 2, ... 的整數（NA 是沒施測）
check_scores <- function(x, arg) {
  if (!is.numeric(x) && !is.logical(x)) {
    stop(sprintf("`%s` must hold numbers (item scores 0, 1, 2, ...), not %s.", arg, typeof(x)), call. = FALSE)
  }
  x <- x[!is.na(x)]
  if (any(!is.finite(x) | x < 0 | x != round(x))) {
    stop(sprintf("`%s` has scores that are negative, infinite or not whole numbers.", arg), call. = FALSE)
  }
}

# 每人一個值的向量（例如 wave_m、sex），轉成長格式 resp 每一列的值。
# 收兩種：沒有名字時，長度要等於矩陣的列數（同樣的順序）；有名字時，名字是 visit_id。
per_person <- function(x, data, resp, arg) {
  if (!is.null(names(x))) {
    if (anyDuplicated(names(x))) {
      stop(sprintf("`%s` has repeated names (for example %s).", arg, names(x)[duplicated(names(x))][1]), call. = FALSE)
    }
    hit <- match(as.character(resp$person_id), names(x))
    if (anyNA(hit)) {
      stop(sprintf("`%s` has no value for visit_id %s.", arg, resp$person_id[is.na(hit)][1]), call. = FALSE)
    }
    return(unname(x[hit]))
  }
  if (!is.matrix(data)) {
    stop(sprintf("With long data, `%s` must be named by visit_id.", arg), call. = FALSE)
  }
  if (length(x) != nrow(data)) {
    stop(sprintf("`%s` has %d value(s), but the matrix has %d row(s). Give one value per row, or name it by visit_id.",
                 arg, length(x), nrow(data)), call. = FALSE)
  }
  x[match(resp$person_id, rownames(data))]
}


# 常模函數的分數 d：數字向量，或 kitsPreview::kits_subj() 的分數（各自的方法）。
# 其他的表（例如 kits::kits_subj() 的結果）停下來說清楚，不讓後面的引數悄悄錯位。
need_d_vector <- function(d, fun) {
  if (is.list(d)) {
    text <- paste0("%s(): `d` must be a vector of scores, or the scores of ",
                   "kitsPreview::kits_subj() (with age_m from its `persons`). ",
                   "For other tables (for example a result of kits::kits_subj()) ",
                   "give the scores and the ages, e.g. %s(s$theta, s$age_m, ...).")
    stop_preview(sprintf(text, fun, fun), "kitsPreview_error_input")
  }
}


# 報表的小數位數：只在印出來的時候限縮，物件裡的數值不變
need_digits <- function(digits) {
  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0 || digits > 15 || digits != round(digits)) {
    stop("`digits` must be one whole number from 0 to 15.", call. = FALSE)
  }
}

# 一個數字印成小數點下 digits 位
fmt <- function(x, digits) {
  x <- round(x, digits)
  x[!is.na(x) & x == 0] <- 0   # 不印 -0.00
  sprintf("%.*f", as.integer(digits), x)
}

# 表格裡不是整數的數值欄，印成小數點下 digits 位
format_table <- function(d, digits) {
  need_digits(digits)
  d <- as.data.frame(d)
  class(d) <- "data.frame"
  for (j in seq_along(d)) {
    v <- d[[j]]
    if (is.double(v) && any(!is.na(v) & is.finite(v) & v != round(v))) d[[j]] <- fmt(v, digits)
  }
  d
}

# kitsPreview 回傳的表格：還是 data.frame，只是印出來時小數點下 2 位。
# 類別名稱是 kitsPreview_table（不是 kits 的 kits_table），所以兩個套件一起載入時，各自的 print() 不會互相蓋掉。
preview_table <- function(d, cls = NULL) {
  class(d) <- c(cls, "kitsPreview_table", "data.frame")
  d
}

# x 是 NULL 時用 y（R 4.4 以後 base 也有；這裡為了 R 4.1）
`%||%` <- function(x, y) if (is.null(x)) y else x
