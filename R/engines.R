# 估計引擎（engine）：每個引擎一個小函數，把套件自己的結果換成同一種步驟參數表。
# dexter、immer 是 CML；ccml 是 immer 的成對條件概似（CCML，GSED 的 D-score 用的方法）。
# TAM 的 MML 和其他方法的比較在另外的教學文件，不在這個套件裡。
# 每個 fit_<engine>() 回傳 list(original, steps, V, origin)：
# * original：套件自己的物件（不動）
# * steps：item_id、item_score、beta、beta_se（一列一題的一個步驟，照 X 的欄、步驟排）
# * V：beta 的共變異矩陣（套件沒有給時是 NULL）
# * origin：原點怎麼定（印出來、寫在說明裡）
# 換到 CML 的原點（所有步驟參數的和是 0）在 center_steps()。

# 每個引擎需要的套件
ENGINE_PACKAGES <- c(dexter = "dexter", immer = "immer", ccml = "immer")

# 每個引擎的估計方法（印出來用）
ENGINE_METHODS <- c(dexter = "CML", immer = "CML", ccml = "CCML")

# 套件的名字（印出來用）
ENGINE_LABELS <- c(dexter = "dexter", immer = "immer", ccml = "immer")


# 有類別的錯誤：使用者可以用 tryCatch(..., kitsPreview_error_missing_package = ...) 接。
# 類別名稱以 kitsPreview_ 開頭，不和 kits 的 kits_error_* 共用。
stop_preview <- function(message, class) {
  cond <- structure(class = c(class, "kitsPreview_error", "error", "condition"),
                    list(message = message, call = NULL))
  stop(cond)
}

# 有類別的警告
warn_preview <- function(message, class) {
  cond <- structure(class = c(class, "kitsPreview_warning", "warning", "condition"),
                    list(message = message, call = NULL))
  warning(cond)
}

# 函數的 ... 不收任何引數：多給的引數停下來，說是哪幾個
no_dots <- function(fun, ...) {
  n <- ...length()
  if (n == 0) {
    return(invisible())
  }
  nm <- ...names()
  if (is.null(nm)) nm <- rep("", n)
  nm[is.na(nm)] <- ""
  what <- c(if (any(nm != "")) paste0("`", nm[nm != ""], "`", collapse = ", "),
            if (any(nm == "")) sprintf("%d unnamed", sum(nm == "")))
  stop_preview(sprintf("%s(): unused argument(s): %s.", fun, paste(what, collapse = ", ")),
               "kitsPreview_error_input")
}

# 選用的套件沒有安裝時停下來，說怎麼裝
need_package <- function(pkg, engine) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }
  stop_preview(sprintf("engine = \"%s\" needs the package '%s'. Install it with install.packages(\"%s\").", engine, pkg, pkg),
               "kitsPreview_error_missing_package")
}

# 呼叫套件的函數。短的引數（一個值）直接寫進呼叫；資料和其他長的引數用名字傳，
# 這樣套件自己記下的呼叫（summary() 的 Call:）讀起來像使用者寫的程式，不會印出整份資料。
call_package <- function(fun, args) {
  short <- vapply(args, function(v) is.atomic(v) && length(v) == 1, logical(1))
  env <- list2env(args[!short], parent = baseenv())
  by_name <- sapply(names(args)[!short], as.name, simplify = FALSE)
  eval(as.call(c(fun, by_name, args[short])), env)
}

# 長格式 → 寬矩陣：一列一個人次（照出現的順序），一欄一題（照題號排序）
resp_wide <- function(resp) {
  persons <- unique(as.character(resp$person_id))
  items <- sort(unique(as.character(resp$item_id)))
  X <- matrix(NA_integer_, nrow = length(persons), ncol = length(items), dimnames = list(persons, items))
  X[cbind(match(resp$person_id, persons), match(resp$item_id, items))] <- as.integer(resp$item_score)
  X
}

# 每題的步驟：item_id 重複 m_i 次，item_score 是 1..m_i（照 X 的欄）。
# immer 的參數是這個順序。
step_index <- function(X) {
  m <- apply(X, 2, max, na.rm = TRUE)
  dplyr::tibble(item_id = rep(colnames(X), m), item_score = sequence(m))
}

# 每題的分數要連續（0, 1, 2, ...）。不連續時停下來，說缺哪幾個分數。
need_consecutive <- function(resp) {
  scores <- split(resp$item_score, resp$item_id)
  gap <- names(scores)[vapply(scores, function(x) !all(seq_len(max(x)) %in% x), logical(1))]
  if (length(gap) == 0) {
    return(invisible(TRUE))
  }
  have <- sort(unique(scores[[gap[1]]]))
  stop("Item ", gap[1], " has scores ", paste(have, collapse = ", "), " (missing ",
       paste(setdiff(seq_len(max(have)), have), collapse = ", "),
       "). Scores must be consecutive (0, 1, 2, ...). Recode the item first.", call. = FALSE)
}

# 換到 CML 的原點：每個 beta 減掉全部 beta 的平均。
# 共變異矩陣跟著換：V_c = C V C'，C = I - 11'/n（只是換一個識別的方式）。
center_steps <- function(fitted) {
  n <- nrow(fitted$steps)
  shift <- mean(fitted$steps$beta)
  fitted$steps$beta <- fitted$steps$beta - shift
  if (!is.null(fitted$V)) {
    C <- diag(n) - 1 / n
    fitted$V <- C %*% fitted$V %*% t(C)
    fitted$steps$beta_se <- sqrt(diag(fitted$V))
  }
  fitted$shift <- shift
  fitted
}


# ---- 每個引擎 ----------------------------------------------------------------------------------------------

# dexter：fit_enorm() 的 CML；步驟參數的和本來就是 0
fit_dexter <- function(resp, args) {
  fit <- do.call(dexter::fit_enorm, c(list(resp), args))
  need_cml(fit)
  cf <- as.data.frame(stats::coef(fit))
  steps <- dplyr::tibble(item_id = cf$item_id, item_score = as.integer(cf$item_score), beta = cf$beta, beta_se = cf$SE_beta)
  list(original = fit, steps = steps, V = fit$est$acov.beta, origin = "sum of all betas = 0 (dexter)")
}

# immer：immer_cml() 的 CML。
# immer 的 b 是累加的：b_ik = beta_i1 + ... + beta_ik，所以步驟參數是相鄰兩個 b 的差。
# b = W par，所以 beta = D W par，共變異 = (D W) vcov (D W)'。
fit_immer <- function(X, args) {
  call_args <- utils::modifyList(list(dat = as.data.frame(X), irtmodel = "PCM", normalization = "sum"), args)
  fit <- call_package(quote(immer::immer_cml), call_args)
  steps <- step_index(X)
  D <- step_differences(steps)
  J <- D %*% fit$W
  V <- J %*% fit$vcov %*% t(J)
  steps$beta <- drop(D %*% fit$b)
  steps$beta_se <- sqrt(diag(V))
  list(original = fit, steps = steps, V = V, origin = "immer normalization = \"sum\", shifted to sum of all betas = 0")
}

# 累加的參數 → 步驟參數：第 k 步 = b_ik - b_i,k-1（第 1 步就是 b_i1）
step_differences <- function(steps) {
  n <- nrow(steps)
  D <- diag(n)
  later <- which(steps$item_score > 1)
  D[cbind(later, later - 1)] <- -1
  D
}

# ccml：immer_ccml() 的成對條件概似（composite conditional ML；Zwinderman, 1995），
# GSED 的 D-score 用的方法（D-score 用 sirt::rasch.pairwise.itemcluster()，只有 0/1 的題）。
# 每次看兩題，以兩題的分數和為條件，theta 消掉。
# immer 的參數就是步驟參數；共變異是 immer 的 sandwich（vcov）。
# nlminb() 預設的迭代次數在 KIT 大小的資料上不夠，所以預設提高到 5000。
fit_ccml <- function(X, args) {
  steps <- step_index(X)
  defaults <- list(dat = as.data.frame(X), irtmodel = "PCM", A = pcm_design(X, steps),
                   control = list(iter.max = 5000, eval.max = 5000))
  fit <- call_package(quote(immer::immer_ccml), utils::modifyList(defaults, args))
  warn_not_converged(fit)
  steps$beta <- unname(fit$coef)
  steps$beta_se <- unname(fit$se)
  list(original = fit, steps = steps, V = unname(fit$vcov),
       origin = "immer_ccml leaves the origin free, shifted to sum of all betas = 0")
}

# immer_ccml() 的設計矩陣（題 × 類別 × 參數）。
# PCM 的第 h 類是前 h 步的和，所以 A[i, h, l] = 1，當參數 l 是第 i 題的第 h 步或更前面。
pcm_design <- function(X, steps) {
  K <- max(steps$item_score)
  item <- match(steps$item_id, colnames(X))
  A <- array(0, dim = c(ncol(X), K, nrow(steps)))
  for (l in seq_len(nrow(steps))) {
    A[item[l], steps$item_score[l]:K, l] <- 1
  }
  A
}

# nlminb() 沒有收斂時警告，說怎麼加大迭代次數
warn_not_converged <- function(fit) {
  if (fit$nlminb_result$convergence == 0) {
    return(invisible(TRUE))
  }
  warning("immer_ccml() did not converge (nlminb: ", fit$nlminb_result$message, "). ",
          "Pass a larger control = list(iter.max = , eval.max = ) to kits_item().", call. = FALSE)
}

# 依引擎估計。固定參數只有 dexter 收（在 args$fixed_params 裡）。
# immer 的兩個引擎換到 CML 的原點（dexter 本來就是）。
fit_engine <- function(engine, resp, args) {
  if (engine == "dexter") {
    return(fit_dexter(resp, args))
  }
  X <- resp_wide(resp)
  fitted <- if (engine == "immer") fit_immer(X, args) else fit_ccml(X, args)
  center_steps(fitted)
}
