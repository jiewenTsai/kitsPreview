# kits_code() 的 immer 版：用 immer 自己的函數重做 kits_item()。
# 程式很短：幾行 immer 的呼叫，再換成步驟參數、CML 的原點和門檻。
# 在 tests/testthat/test-engines.R 執行過，數字和套件相同。

# 套件的額外引數寫成 R 程式碼：", name = value"（沒有就是空字串）
code_args <- function(args) {
  if (length(args) == 0) {
    return("")
  }
  values <- vapply(args, function(v) paste(deparse(v, width.cutoff = 500L), collapse = " "), character(1))
  paste0(", ", paste(names(args), "=", values, collapse = ", "))
}

# 準備資料：和 kits_item() 一樣拿掉沒有作答的列和題，題目照題號排序；一列一個步驟
code_engine_setup <- r"---(# Persons and items with no responses are left out; the items are sorted (as kits_item() does)
answered <- rowSums(!is.na(X)) > 0
X <- X[answered, colSums(!is.na(X)) > 0, drop = FALSE]
X <- X[, sort(colnames(X)), drop = FALSE]
)---"

code_step_rows <- r"---(# One row per step: item i has steps 1, ..., m_i (the order of the package's parameters)
m <- apply(X, 2, max, na.rm = TRUE)
item_id <- rep(colnames(X), m)
item_score <- sequence(m)
n <- length(item_id)
)---"

code_center <- r"---(# {{n}} The CML origin: subtract the mean, so that the sum of all betas is 0 (C = I - 11'/n)
C <- diag(n) - 1 / n
beta <- drop(C %*% beta)
V <- C %*% V %*% t(C)
cf <- data.frame(item_id = item_id, item_score = item_score, beta = beta, SE_beta = sqrt(diag(V)))
)---"

# ---- 題目 --------------------------------------------------------------------------------------------------

code_item_immer <- function(object) {
  args <- utils::modifyList(list(normalization = "sum"), object$settings$args %||% list())
  c("library(immer)", "", code_engine_setup, code_step_rows, code_pcm,
    "# 1 Calibration: PCM by CML (immer)",
    sprintf("fit <- immer_cml(dat = as.data.frame(X), irtmodel = \"PCM\"%s)", code_args(args)),
    "",
    r"---(# 2 immer's b are cumulative, b_ik = beta_i1 + ... + beta_ik: the steps are the differences, beta = D b
D <- diag(n)
later <- which(item_score > 1)
D[cbind(later, later - 1)] <- -1
beta <- drop(D %*% fit$b)

# b = W par, so the covariance of the steps is (D W) vcov (D W)'
J <- D %*% fit$W
V <- J %*% fit$vcov %*% t(J)
)---", fill(code_center, n = "3"), fill(code_thresholds, n = "4", n2 = "5"))
}

code_item_ccml <- function(object) {
  defaults <- list(control = list(iter.max = 5000, eval.max = 5000))
  args <- utils::modifyList(defaults, object$settings$args %||% list())
  c("library(immer)", "", code_engine_setup, code_step_rows, code_pcm,
    r"---(# 1 Design matrix (items x categories x parameters): category h is the sum of the first h steps,
# so A[i, h, l] = 1 when parameter l is step h of item i or an earlier step of item i
K <- max(m)
item <- match(item_id, colnames(X))
A <- array(0, dim = c(ncol(X), K, n))
for (l in seq_len(n)) {
  A[item[l], item_score[l]:K, l] <- 1
}
)---",
    "# 2 Calibration: PCM by composite (pairwise) CML (immer), the method of the GSED D-score",
    sprintf("fit <- immer_ccml(dat = as.data.frame(X), irtmodel = \"PCM\", A = A%s)", code_args(args)),
    "beta <- unname(fit$coef)",
    "V <- unname(fit$vcov)",
    "", fill(code_center, n = "3"), fill(code_thresholds, n = "4", n2 = "5"))
}

code_item_engine <- function(object) {
  if (engine_of(object) == "immer") {
    return(code_item_immer(object))
  }
  if (engine_of(object) == "ccml") {
    return(code_item_ccml(object))
  }
  code_item_dexter(object)
}
