# purrr::list_rbind()、dplyr::bind_rows() 疊 kitsPreview 的表（和 kits 的 vctrs.R 相同的作法）。
# 它們用 vctrs 疊表：先用 vec_ptype2() 求共同的型別，再用 vec_cast() 把每張表轉過去。
# vctrs 只看類別的第一個（不看繼承），所以每一對類別都註冊一個方法（.onLoad() 裡註冊）。
# 沒有這些方法時，兩次 kits_subj() 的分數（屬性不同）疊起來變成一般的 data.frame，常模的函數就不收。
# * 同一種 kitsPreview 的表：結果留著類別。
# * kitsPreview 的表和一般的 data.frame（或 tibble）：結果是 kitsPreview 的表
#   （kits 在這裡警告量尺沒有檢查；kitsPreview 不記量尺，所以不警告）。
# * 不同種的 kitsPreview 的表（例如 kits_subj 和門檻表）：結果是一般的 data.frame。
# 屬性：分數的原始分換算紀錄（raw_theta）和 dexter 的表（original），每張表都相同時才留著；
# 不同時，raw_theta 留第一張的（kits_subj_table() 會停下來說清楚），original 拿掉。

# kitsPreview 的表的類別（類別的第一個）
PREVIEW_TABLE_CLASSES <- c("kitsPreview_table", "kitsPreview_subj", "kitsPreview_subj_table",
                           "kitsPreview_norms", "kitsPreview_link_screen", "kitsPreview_age_pass",
                           "kitsPreview_item_table", "kitsPreview_vertical")

# 一般的表
PLAIN_TABLE_CLASSES <- c("data.frame", "tbl_df")

# 疊起來之後還留著的屬性
preview_attrs <- function(out, x, y = NULL) {
  keep_same <- function(a) {
    if (is.null(y) || identical(attr(x, a), attr(y, a))) attr(x, a) else NULL
  }
  attr(out, "raw_theta") <- attr(x, "raw_theta") %||% if (!is.null(y)) attr(y, "raw_theta")
  attr(out, "original") <- keep_same("original")
  out
}

# 共同的型別：同一種 kitsPreview 的表
ptype2_same <- function(x, y, ...) {
  out <- vctrs::df_ptype2(x, y, ...)
  class(out) <- class(x)
  preview_attrs(out, x, y)
}

# 共同的型別：不同種的 kitsPreview 的表，給一般的 data.frame
ptype2_mixed <- function(x, y, ...) {
  vctrs::df_ptype2(x, y, ...)
}

# 共同的型別：kitsPreview 的表和一般的表，給 kitsPreview 的表
ptype2_plain <- function(x, y, ..., preview_first = TRUE) {
  k <- if (preview_first) x else y
  out <- vctrs::df_ptype2(x, y, ...)
  class(out) <- class(k)
  preview_attrs(out, k)
}

# 轉到 kitsPreview 的表：欄照共同的型別，屬性照 to
cast_to_preview <- function(x, to, ...) {
  out <- vctrs::df_cast(x, to, ...)
  class(out) <- class(to)
  preview_attrs(out, to)
}

# 轉到一般的 data.frame（或 tibble）
cast_to_plain <- function(x, to, ...) {
  if (inherits(to, "tbl_df")) vctrs::tib_cast(x, to, ...) else vctrs::df_cast(x, to, ...)
}

# vctrs 的方法（.onLoad() 呼叫）：每一對類別各一個
register_vctrs_methods <- function() {
  ns <- asNamespace("vctrs")
  reg <- function(generic, x_class, y_class, fun) {
    registerS3method(generic, paste(x_class, y_class, sep = "."), fun, envir = ns)
  }
  for (a in PREVIEW_TABLE_CLASSES) {
    for (b in PREVIEW_TABLE_CLASSES) {
      reg("vec_ptype2", a, b, if (a == b) ptype2_same else ptype2_mixed)
      reg("vec_cast", a, b, if (a == b) cast_to_preview else cast_to_plain)
    }
    for (p in PLAIN_TABLE_CLASSES) {
      reg("vec_ptype2", a, p, function(x, y, ...) ptype2_plain(x, y, ..., preview_first = TRUE))
      reg("vec_ptype2", p, a, function(x, y, ...) ptype2_plain(x, y, ..., preview_first = FALSE))
      reg("vec_cast", a, p, cast_to_preview)   # 一般的表轉成 kitsPreview 的表（vec_cast.<to>.<from>）
      reg("vec_cast", p, a, cast_to_plain)
    }
  }
  invisible()
}

.onLoad <- function(libname, pkgname) {
  register_vctrs_methods()
}
