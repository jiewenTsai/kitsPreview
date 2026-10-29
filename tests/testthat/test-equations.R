# kits_equations() 印出來的每一條式子：照式子直接算，結果要和套件相同

# 式子裡的 PCM：P(X = k | θ) = exp(kθ − Σ_{g≤k} β_g) / Σ_h exp(hθ − Σ_{g≤h} β_g)
pcm <- function(theta, b) {
  eta <- c(0, cumsum(theta - b))
  exp(eta) / sum(exp(eta))
}
moments <- function(theta, b) {
  p <- pcm(theta, b)
  k <- seq_along(p) - 1
  c(E = sum(k * p), W = sum(k^2 * p) - sum(k * p)^2)
}

set.seed(1)
betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-2, -1, 1), a6 = 1.5)
X <- kits_sim(rnorm(500, 0, 1.5), betas, seed = 1)
X[sample(length(X), 400)] <- NA
X <- X[rowSums(!is.na(X)) > 0, ]
item <- kits_item(X)
b_est <- split(coef(item)$beta, coef(item)$item_id)

test_that("item：PCM 的式子等於 dexter 的機率；所有 β 加總是 0", {
  for (i in names(b_est)) {
    p <- dexter::p_score(coef(item)[, c("item_id", "item_score", "beta")], items = i)(c(-1, 0.4, 2))
    expect_equal(unname(p), t(sapply(c(-1, 0.4, 2), pcm, b = b_est[[i]])), tolerance = 1e-10)
  }
  expect_equal(sum(coef(item)$beta), 0, tolerance = 1e-10)
})

test_that("item：門檻是 P(X ≥ k | τ) = .5；SE 是 delta method 的 √(Jᵀ V J)", {
  V <- item$fit$est$acov.beta
  cf <- coef(item)
  for (r in seq_len(nrow(cf))) {
    b <- b_est[[cf$item_id[r]]]
    k <- cf$item_score[r]
    p_ge <- function(theta, bb) sum(pcm(theta, bb)[(k + 1):(length(bb) + 1)])
    expect_equal(p_ge(cf$thr[r], b), 0.5, tolerance = 1e-8)
    h <- 1e-5
    d_theta <- (p_ge(cf$thr[r] + h, b) - p_ge(cf$thr[r] - h, b)) / (2 * h)
    d_beta <- sapply(seq_along(b), function(g) {
      e <- replace(numeric(length(b)), g, h)
      (p_ge(cf$thr[r], b + e) - p_ge(cf$thr[r], b - e)) / (2 * h)
    })
    J <- -d_beta / d_theta
    idx <- which(cf$item_id == cf$item_id[r])
    expect_equal(sqrt(drop(t(J) %*% V[idx, idx, drop = FALSE] %*% J)), cf$thr_se[r], tolerance = 1e-6)
  }
})

test_that("subj：WLE 是 log L + ½ log I 的最大值；SE 是 √(I + (I′/2I)²) / I（也包括 dexter 算不出來時）", {
  info <- function(theta, x, b) sum(sapply(names(x), function(i) moments(theta, b[[i]])[["W"]]))
  target <- function(theta, x, b) sum(sapply(names(x), function(i) log(pcm(theta, b[[i]])[x[[i]] + 1]))) + 0.5 * log(info(theta, x, b))
  check <- function(s, data, b) {
    for (p in unique(s$visit_id)) {
      x <- data[p, !is.na(data[p, ])]
      x <- as.list(x)
      th <- stats::optimize(target, range(unlist(b)) + c(-15, 15), x = x, b = b, maximum = TRUE, tol = 1e-12)$maximum
      expect_equal(s$theta[s$visit_id == p], th, tolerance = 1e-5)
      I <- info(th, x, b)
      dI <- (info(th + 1e-5, x, b) - info(th - 1e-5, x, b)) / 2e-5
      expect_equal(s$se[s$visit_id == p], sqrt(I + (dI / (2 * I))^2) / I, tolerance = 1e-4)
    }
  }
  s <- kits_subj(X, item)
  check(s[unique(c(1:8, utils::head(which(s$flag %in% c("zero", "perfect")), 2))), ], X, b_est)

  # dexter 算不出來的情況（β 全部加 60）：用式子算的值和 kits_subj 的補救算法相同
  shifted <- coef(item)
  shifted$beta <- shifted$beta + 60
  s60 <- suppressMessages(kits_subj(X[1:6, ], shifted))
  check(s60, X[1:6, ], lapply(b_est, `+`, 60))
})

test_that("fit：E、W、infit、outfit 的式子等於 kits_subj 和 kits_itemfit；flag 的規則", {
  s <- kits_subj(X, item)
  inner <- s$visit_id[s$flag %in% c("ok", "misfit")]
  rows <- which(!is.na(X[inner, ]), arr.ind = TRUE)
  d <- data.frame(person = inner[rows[, 1]], item = colnames(X)[rows[, 2]], x = X[inner, ][rows])
  d$theta <- s$theta[match(d$person, s$visit_id)]
  m <- t(mapply(function(th, i) moments(th, b_est[[i]]), d$theta, d$item))
  d$E <- m[, "E"]
  d$W <- m[, "W"]
  infit_p <- tapply((d$x - d$E)^2, d$person, sum) / tapply(d$W, d$person, sum)
  outfit_p <- tapply((d$x - d$E)^2 / d$W, d$person, mean)
  expect_equal(s$infit[match(names(infit_p), s$visit_id)], unname(c(infit_p)), tolerance = 1e-8)
  expect_equal(s$outfit[match(names(outfit_p), s$visit_id)], unname(c(outfit_p)), tolerance = 1e-8)
  expect_equal(s$flag == "misfit", s$flag %in% c("ok", "misfit") & pmax(s$infit, s$outfit) > 2)

  f <- kits_itemfit(X, item)
  infit_i <- tapply((d$x - d$E)^2, d$item, sum) / tapply(d$W, d$item, sum)
  outfit_i <- tapply((d$x - d$E)^2 / d$W, d$item, mean)
  expect_equal(f$infit, unname(c(infit_i[f$item_id])), tolerance = 1e-8)
  expect_equal(f$outfit, unname(c(outfit_i[f$item_id])), tolerance = 1e-8)
})

test_that("link：mean/sigma、一次篩點、共用斜率的式子等於套件", {
  set.seed(4)
  from <- rnorm(14)
  to <- 60 + 7 * from + rnorm(14, 0, 0.4)
  to[3] <- to[3] + 6
  domain <- rep(c("lan", "cog"), each = 7)
  steps <- data.frame(item_id = paste0("a", 1:14), item_score = 1, thr = from, domain = domain)
  link <- kits_linking_d(steps, data.frame(item_id = steps$item_id, tau = to), n_boot = 0)

  # A = 斜率、B = 截距（Kolen & Brennan 的寫法）
  A0 <- sd(to) / sd(from)
  B0 <- mean(to) - A0 * mean(from)
  e <- to - (B0 + A0 * from)
  keep <- !(abs(e) > 2 * sd(e))
  A <- sd(to[keep]) / sd(from[keep])
  B <- mean(to[keep]) - A * mean(from[keep])
  pts <- link$points[match(steps$item_id, link$points$id), ]
  expect_equal(c(link$intercept0, link$slope0), c(B0, A0))
  expect_equal(pts$dropped, !keep)
  expect_equal(coef(link), c(intercept = B, slope = A))

  mt <- ave(to[keep], domain[keep])
  mf <- ave(from[keep], domain[keep])
  A_s <- sqrt(sum((to[keep] - mt)^2) / sum((from[keep] - mf)^2))
  B_d <- tapply(to[keep], domain[keep], mean) - A_s * tapply(from[keep], domain[keep], mean)
  expect_equal(link$domains$slope[1], A_s)
  expect_equal(link$domains$intercept, unname(c(B_d[link$domains$domain])))
})

test_that("norms：篩波次、月齡範圍、多項式、z、百分位的式子等於套件", {
  set.seed(5)
  age <- runif(1200, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(1200, 0, 3)
  wave <- round(age / 6) * 6
  D[wave == 6][1:30] <- min(D[wave == 6])
  screen <- kits_norm_screen(D, wave)
  pct <- tapply(round(D, 3), wave, function(v) 100 * mean(v == min(v)))
  expect_equal(screen$waves$normed, unname(c(round(pct, 1) < 20)))

  ages <- kits_norm_ages(age, wave)
  lo <- floor(quantile(age[wave == min(wave)], 0.01, names = FALSE) / 0.5) * 0.5
  hi <- ceiling(quantile(age[wave == max(wave)], 0.99, names = FALSE) / 0.5) * 0.5
  expect_equal(range(ages), c(lo, hi))

  # 多項式：x = (√age − m)/s；μ = Σ c x^j，log σ = Σ c x^j，ε = Σ c x^j，log δ = Σ c x^j
  deg <- c(3, 2, 1, 1)
  norm <- suppressWarnings(kits_norms(D, age, ages = 6:30, degrees = deg, delta_starts = 1, pad = 0))
  d <- data.frame(D = D, age = age)[age >= 6 & age <= 30, ]
  model <- suppressWarnings(cNORM::cnorm.shash(age = sqrt(d$age), score = d$D, mu_degree = deg[1], sigma_degree = deg[2],
                                               epsilon_degree = deg[3], delta_degree = deg[4], delta = 1, plot = FALSE))
  x <- (sqrt(norm$age_m) - mean(sqrt(d$age))) / sd(sqrt(d$age))
  poly_at <- function(c) drop(outer(x, seq_along(c) - 1, `^`) %*% c)
  expect_equal(norm$mu, poly_at(model$mu_est), tolerance = 1e-8)
  expect_equal(norm$sigma, exp(poly_at(model$sigma_est)), tolerance = 1e-8)
  expect_equal(norm$epsilon, poly_at(model$epsilon_est), tolerance = 1e-8)
  expect_equal(norm$delta, exp(poly_at(model$delta_est)), tolerance = 1e-8)

  # z = sinh(δ asinh((D − μ)/σ) + ε)，參數在兩列之間線性內插；P_q = μ + σ sinh((asinh(Φ⁻¹(q/100)) − ε)/δ)
  a <- c(10.25, 17.5)
  at <- function(v) approx(norm$age_m, norm[[v]], a)$y
  z <- sinh(at("delta") * asinh((c(50, 60) - at("mu")) / at("sigma")) + at("epsilon"))
  expect_equal(kits_norm_z(c(50, 60), a, norm), z)
  q <- norm$mu + norm$sigma * sinh((asinh(qnorm(0.10)) - norm$epsilon) / norm$delta)
  expect_equal(norm$P10, q)
})

test_that("kits_equations：每種物件和主題都印得出來；LaTeX 是 aligned 區塊", {
  expect_output(print(kits_equations()), "Warm")
  expect_output(print(kits_equations(item)), "partial credit")
  expect_output(print(kits_equations(kits_subj(X, item))), "infit")
  expect_output(print(kits_equations(kits(X))), "WLE")
  link <- link_of_points(c(-2, -1, 0, 1, 2), c(-1, 0, 1, 2, 3.2), id = paste0("p", 1:5))
  expect_output(print(kits_equations(link)), "This link")
  # 連結的公式用 intercept、slope（單一字母時用 Kolen & Brennan 的大寫 A、B），沒有小寫的 a、b
  txt <- as.character(kits_equations(link))
  expect_true(any(grepl("new = intercept + slope x old", txt, fixed = TRUE)))
  expect_true(any(grepl("B and A of Kolen & Brennan", txt, fixed = TRUE)))
  formulas <- txt[seq_len(match("References:", txt) - 1)]
  expect_false(any(grepl("\\b[ab]0?\\b *[=+]|linking error a |\\ba0\\b|\\bb0\\b", formulas)))
  tex <- as.character(kits_equations("norms", format = "latex"))
  expect_equal(tex[1], "\\begin{aligned}")
  expect_true("\\end{aligned}" %in% tex)
  expect_true(all(grepl("^%", tex[seq_along(tex) > which(tex == "\\end{aligned}")])))   # 後面只有引用的註解行
  tex_item <- paste(as.character(kits_equations("item", format = "latex")), collapse = "\n")
  expect_true(grepl("$m_{i}$", tex_item, fixed = TRUE))          # 註解裡的符號是行內數學
  expect_true(grepl("$\\beta_{ig}$", tex_item, fixed = TRUE))
  expect_false(grepl("\\_", tex_item, fixed = TRUE))
  expect_error(kits_equations("abc"), "Unknown topic")
  expect_error(kits_equations(1:3), "No formulas")
})

test_that("每一段都有引用；最後列出全文；引用的鍵都存在", {
  txt <- as.character(kits_equations())
  expect_true(any(grepl("^Item calibration.*\\[Masters, 1982; dexter", txt)))
  expect_true(any(txt == "References:"))
  expect_true(any(grepl("^  Warm, T. A. \\(1989\\)", txt)))
  keys <- unique(unlist(lapply(unlist(lapply(eq_topics(), eq_text), recursive = FALSE), function(s) s$ref)))
  expect_true(all(keys %in% names(eq_refs())))
  tex <- as.character(kits_equations("norms", format = "latex"))
  expect_true(any(grepl("^% References:", tex)))
  expect_true(any(grepl("Jones \\\\& Pewsey, 2009", tex)))
})
