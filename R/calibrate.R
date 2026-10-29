# PCM 下，能力 theta 的人在一題得到 k 分以上的機率（內部用，給 step_threshold()）。
# theta 可以是向量。
# 要畫類別機率曲線，用 dexter::p_score(parms, items = "題號")(theta)：每個分數的機率。
p_at_least <- function(theta, betas, k) {
  if (k < 1 || k > length(betas)) {
    stop("`k` must be between 1 and the number of steps (", length(betas), ").", call. = FALSE)
  }
  vapply(theta, function(t) {
    eta <- c(0, cumsum(t - betas))
    p <- exp(eta - max(eta))
    sum(p[(k + 1):length(p)]) / sum(p)
  }, numeric(1))
}


# 一題一個步驟的門檻（Thurstonian threshold；內部用，給 item_thresholds()）。
# 門檻是一個能力值：在這個能力，得到 k 分以上的機率剛好是 .5。
# 給 V（這題步驟參數的共變異矩陣）時，用 delta method 算門檻的 SE。
# 回傳 c(thr, se)；沒給 V 時 se 是 NA。
step_threshold <- function(betas, k = 1, V = NULL, h = 1e-5) {
  thr <- stats::uniroot(function(t) p_at_least(t, betas, k) - 0.5, c(-40, 40), tol = 1e-10)$root
  if (is.null(V)) {
    return(c(thr = thr, se = NA_real_))
  }

  # 隱函數微分：門檻對每個 β 的偏微分
  dp_dtheta <- (p_at_least(thr + h, betas, k) - p_at_least(thr - h, betas, k)) / (2 * h)
  dp_dbeta <- vapply(seq_along(betas), function(j) {
    e <- replace(numeric(length(betas)), j, h)
    (p_at_least(thr, betas + e, k) - p_at_least(thr, betas - e, k)) / (2 * h)
  }, numeric(1))
  J <- -dp_dbeta / dp_dtheta
  c(thr = thr, se = sqrt(drop(t(J) %*% V %*% J)))
}


# 步驟參數加門檻的表（內部用，給 kits_item()、kits_drift()）。一列一題的一個步驟。
# 每題的分數要連續（0, 1, 2, ...），不連續時停下來。
item_thresholds <- function(fit) {
  if (!inherits(fit, "enorm")) {
    stop("`fit` must be the result of dexter::fit_enorm().", call. = FALSE)
  }
  need_cml(fit)
  cf <- as.data.frame(stats::coef(fit))
  steps <- data.frame(item_id = cf$item_id, item_score = cf$item_score, beta = cf$beta, beta_se = cf$SE_beta)
  steps_thresholds(steps, fit$est$acov.beta)
}

# 任何引擎的步驟參數表（item_id、item_score、beta、beta_se）加上門檻。
# V 是 beta 的共變異矩陣（列的順序和 steps 相同）；NULL 時門檻沒有 SE。
steps_thresholds <- function(steps, V = NULL) {
  rows <- split(seq_len(nrow(steps)), steps$item_id)

  one_item <- function(idx) {
    scores <- steps$item_score[idx]
    if (!identical(as.integer(scores), seq_along(scores))) {
      stop("Item ", steps$item_id[idx[1]], " has scores 0, ", paste(scores, collapse = ", "), " (missing ",
           paste(setdiff(seq_len(max(scores)), scores), collapse = ", "),
           "). Scores must be consecutive (0, 1, 2, ...). Recode the item first.", call. = FALSE)
    }
    betas <- steps$beta[idx]
    V_i <- if (is.null(V)) NULL else V[idx, idx, drop = FALSE]
    est <- vapply(seq_along(betas), function(k) step_threshold(betas, k, V_i), numeric(2))
    data.frame(item_id = steps$item_id[idx], item_score = as.integer(scores), beta = betas, beta_se = steps$beta_se[idx],
               thr = est["thr", ], thr_se = est["se", ])
  }

  out <- do.call(rbind, lapply(rows, one_item))
  rownames(out) <- NULL
  out
}
