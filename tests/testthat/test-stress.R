# 暴力測試（kitscale_20261026/stress）找到的問題：每個修好的問題一個測試
set.seed(1)

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5)
X <- kits_sim(seq(-3, 3, length.out = 300), betas)
item <- kits_item(X)

test_that("B1：參數整體偏移很大時，Jeffreys MAP 仍找到正確的值", {
  p <- coef(item)
  p$beta <- p$beta + 60
  shifted <- suppressMessages(kits_subj(X[1:10, ], p))
  expect_equal(shifted$theta, kits_subj(X[1:10, ], item)$theta + 60, tolerance = 1e-4)
})

test_that("B2、A4：參數有 NA、Inf 或步驟不是整數時停下來", {
  p <- coef(item)
  p$beta[1] <- NA
  expect_error(kits_subj(X, p), "finite")
  p <- coef(item)
  p$item_score <- as.character(p$item_score)
  expect_equal(kits_subj(X, p)$theta, kits_subj(X, item)$theta)
  p$item_score[1] <- "x"
  expect_error(kits_subj(X, p), "whole numbers")
})

test_that("C1：資料的分數比參數的最高步驟高時，說出題號和分數", {
  m <- X[1:6, ]
  m[1, "a3"] <- 2L
  expect_error(kits_subj(m, item), "a3:2")
})

test_that("B3、B4、B5：cut、n_boot 不合理時停下來；只有一題時沒有連結誤差；負相關時警告", {
  f <- c(-2, -1, 0, 1, 2)
  expect_error(link_of_points(f, f + 1, letters[1:5], cut = 0), "positive")
  expect_error(link_of_points(f, f + 1, letters[1:5], n_boot = 1), "at least 2")
  expect_error(link_of_points(f, f + 1, letters[1:5], n_boot = -5), "at least 2")
  expect_warning(link <- link_of_points(f, f + 1 + c(0, .1, 0, .1, 0), letters[1:5],
                                        item = rep("i1", 5), n_boot = 10), "one item")
  expect_true(is.na(link$intercept_error))
  expect_warning(link_of_points(f, -f, letters[1:5]), "negatively")
})

test_that("B6：cohort 有 NA 時，NA 這一組只算 NA 的人", {
  D <- c(1, 2, 3, 4, 5, 6, 7, 8)
  out <- kits_norm_screen(D, rep(6, 8), cohort = c(NA, NA, NA, "M03", "M03", "M03", "M36", "M36"))$waves
  expect_equal(out$n, c(3, 2, 3))
})

test_that("B7、A8：kits_norm_ages 的波次要是數字；by 要是正數；最後一波不比第一波大時說清楚", {
  expect_error(kits_norm_ages(c(5, 6, 11, 12), c("6", "6", "12", "12")), "numbers")
  expect_error(kits_norm_ages(c(5, 6, 11, 12), c(6, 6, 12, 12), by = 0), "positive")
  expect_error(kits_norm_ages(c(30, 31, 5, 6), c(6, 6, 12, 12)), "not above")
})

test_that("B8、A9：階梯的月齡刻度不受浮點誤差影響；找不到時說清楚", {
  norm <- data.frame(domain = "lan", age_m = c(3 + 1e-15, 6, 12), P50 = c(40, 50, 60))
  expect_equal(ladder_marks(norm, "lan", ages = c(3, 6))$age_m, c(3, 6))
  expect_error(ladder_marks(norm, "lan", ages = 99), "None of")
  expect_error(ladder_marks(norm, "cog"), "cog")
})

test_that("B9：kits_dif_combine 的邊界對稱；0 不算相反；重複的題停下來", {
  d1 <- data.frame(item_id = c("a1", "a2", "a3"), dif = c(0.3, -0.3, 0))
  d2 <- data.frame(item_id = c("a1", "a2", "a3"), dif = c(0, 0, 0.5))
  out <- kits_dif_combine(d1, d2)
  expect_equal(out$DIF, c("none", "none", "favors girls"))
  expect_error(kits_dif_combine(rbind(d1, d1), d2), "one row per item")
  expect_error(kits_dif_combine(d1, d2, cohorts = c("X", "X")), "two different")
})

test_that("A1、A2、A3：Bayes 的校準、矩陣裡有 Inf、長格式的分數是 factor，說清楚", {
  fb <- dexter::fit_enorm(X, method = "Bayes", nDraws = 50)
  expect_error(kits_subj(X, fb), "CML")
  m <- X
  m[1, 1] <- Inf
  expect_error(kits_subj(m, item), "infinite")
  long <- data.frame(visit_id = "p1", item_id = "a1", item_score = factor(1))
  expect_error(kits_subj(long, item), "numbers")
})

test_that("A5、A6、A7：常模的次數、資料只有一個月齡、常模表只有一列，說清楚", {
  age <- runif(600, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(600, 0, 3)
  expect_error(kits_norms(D, age, ages = 3:36, degrees = c(1, 1, 0, 0)), "at least 1")
  expect_error(kits_norms(D, rep(12, 600), ages = 3:36, degrees = c(2, 1, 1, 1)), "fewer than 2")
  norm <- data.frame(age_m = 6, mu = 40, sigma = 5, epsilon = 0, delta = 1)
  expect_error(kits_norm_z(40, 6, norm), "at least 2 rows")
})

test_that("A14：題號有逗號時，kits_subj_table 照樣找到標準題本", {
  m <- X
  colnames(m)[1] <- "a,1"
  p <- coef(item)
  p$item_id[p$item_id == "a1"] <- "a,1"
  expect_equal(sum(table_of(m, p, wave_m = rep(12, 300))$n), 300)
})

test_that("A15、B18：kits_linking_d 的 steps 已經有 tau 欄、domain 有 NA", {
  steps <- data.frame(item_id = paste0("a", 1:5), item_score = 1, thr = c(-2, -1, 0, 1, 2), tau = 0)
  anchors <- data.frame(item_id = steps$item_id, tau = 60 + 7 * steps$thr + c(0.1, -0.1, 0, 0.1, -0.1))
  expect_equal(kits_linking_d(steps, anchors, n_boot = 0)$slope, 7, tolerance = 0.05)
  steps$domain <- c("lan", "lan", NA, "cog", "cog")
  expect_error(kits_linking_d(steps, anchors), "domain")
})

test_that("B13、B16、C8：重複的名字、50% 通過月齡在觀察範圍外、模擬的題號重複", {
  persons <- data.frame(visit_id = c(rownames(X), "p0001"), wave_m = 12)
  expect_error(kits_subj(X, item, persons = persons), "repeated ids")
  age <- rep(c(6, 12, 18), each = 20)
  pass <- c(rep(0, 40), rep(c(0, 1), 10))   # 18 個月才到一半：估計值超出時給 NA
  a50 <- kits_age_pass(pass, age, age)$p50
  expect_true(is.na(a50) || a50 <= 18)
  expect_error(kits_sim(1:3, list(a = 0, a = 1)), "different item ids")
})

test_that("第三輪：小題本不讓 rir 變 NA；一題時停下來；fixed 題都沒放進校準時說清楚；flag", {
  m <- X
  m[300, c("a1", "a2")] <- NA   # 一個孩子自己一個題本
  f <- kits_itemfit(m, item)
  expect_true(all(is.finite(f$rir)))
  expect_error(kits_itemfit(X[, "a1", drop = FALSE], item), "at least 2")
  z <- X
  z[] <- 0L
  expect_error(suppressMessages(kits_item(z, fixed_params = item)), "nothing to calibrate")
  s <- kits_subj(X, item)
  expect_equal(s$flag[s$raw == 0][1], "zero")
  expect_true(all(s$flag[s$raw == s$raw_max] == "perfect"))
  expect_true(all(s$flag %in% c("zero", "perfect", "misfit", "ok")))
})

test_that("第三輪：kits_subj_table(item) 用 kits_item 的題本，包括沒放進校準的固定題", {
  Y <- X
  Y[, "a4"] <- 1L
  lo <- suppressMessages(kits_item(Y, fixed_params = item))
  tab <- kits_subj_table(lo)
  s <- kits_subj(Y, lo)
  expect_equal(max(tab$raw), max(s$raw_max))
  hit <- tab$theta[match(s$raw, tab$raw)]
  expect_equal(hit, s$theta, tolerance = 1e-6)
})

test_that("第三輪：Jeffreys MAP 每個題目組合 × 原始分只算一次（很快）", {
  p <- coef(item)
  p$beta <- p$beta + 60
  big <- kits_sim(rnorm(2000), betas, seed = 9)
  expect_lt(system.time(suppressMessages(kits_subj(big, p)))[["elapsed"]], 20)
})
