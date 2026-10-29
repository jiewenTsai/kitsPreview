set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

test_that("kits_norm_z 在常模範圍外回傳 NA；缺欄位時說清楚", {
  norm <- data.frame(age_m = c(6, 12), mu = c(40, 60), sigma = 5, epsilon = 0, delta = 1)
  z <- kits_norm_z(d = c(40, 60, 50), age_m = c(6, 12, 30), norm = norm)
  expect_equal(z[1:2], c(0, 0))
  expect_true(is.na(z[3]))
  expect_error(kits_norm_z(1, 6, norm[, 1:3]), "epsilon")
})

test_that("kits_dif_combine 的規則", {
  d1 <- data.frame(item_id = c("a1", "a2", "a3", "a4"), dif = c(0.1, 0.5, 0.4, -0.5))
  d2 <- data.frame(item_id = c("a1", "a2", "a3", "a5"), dif = c(0.2, 0.1, -0.2, 0.6))
  out <- kits_dif_combine(d1, d2, cohorts = c("A", "B"))
  expect_equal(out$DIF, c("none", "favors girls", "inconsistent", "favors boys", "favors girls"))
  expect_true(all(c("dif_A", "dif_B", "dir_A", "dir_B") %in% names(out)))
})

test_that("kits_age_pass：沒有跨過一半、或斜率不是正的，回傳 NA；矩陣一次算每題", {
  age <- rep(c(6, 12), each = 4)
  P <- cbind(a1 = c(0, 0, 0, 1, 1, 1, 1, 0), a2 = rep(1, 8), a3 = NA)
  out <- kits_age_pass(P, age, age)
  expect_s3_class(out, "kitsPreview_age_pass")
  expect_equal(out$item_id, c("a1", "a2", "a3"))
  expect_equal(is.na(out$p50), c(FALSE, TRUE, TRUE))
  expect_equal(out$age_last_fail, c(12, NA, NA))
  expect_error(kits_age_pass(P, age[-1], age), "one value per row")
  expect_error(kits_age_pass(P, age, age, p = 1), "between 0 and 1")
  expect_error(kits_age_pass(P, age), "wave_m")
  expect_true(is.na(kits_age_pass(rep(1, 8), age, age)$p50))
  expect_true(is.na(kits_age_pass(c(1, 1, 1, 0, 0, 0, 0, 1), age, age)$p50))
  expect_gt(kits_age_pass(c(0, 0, 0, 1, 1, 1, 1, 0), age, age)$p50, 6)
  expect_gt(kits_age_pass(c(0, 0, NA, 1, 1, 1, 1, 0), age, age)$p50, 6)
  expect_output(print(out), "kits_age_pass")
})

test_that("kits_age_pass：分數用 step 判斷通過；幾個 p；完全分離的題標出來", {
  age <- rep(c(6, 12, 18), each = 4)
  S <- cbind(a1 = c(0, 1, 1, 1,  1, 1, 2, 2,  1, 2, 2, 2))
  expect_equal(kits_age_pass(S, age, age, step = 2)$p50, kits_age_pass(S >= 2, age, age)$p50)
  three <- kits_age_pass(S, age, age, step = 2, p = c(0.25, 0.5, 0.75))
  expect_equal(names(three)[2:4], c("p25", "p50", "p75"))
  expect_true(three$p25 < three$p75)
  M <- cbind(a3 = c(0, 0, 0, 0,  1, 1, 1, 1,  1, 1, 1, 1))
  sep <- kits_age_pass(M, age, age)
  expect_true(sep$separated)
  expect_equal(c(sep$age_last_fail, sep$age_first_pass), c(6, 12))
  wide <- data.frame(visit_id = paste0("p", 1:12), a3 = M[, 1])
  expect_equal(kits_age_pass(wide, age, age), sep)
  old <- data.frame(person_id = paste0("p", 1:12), a3 = M[, 1])   # 0.22.0 的欄名：不是題目，停下來
  expect_error(kits_age_pass(old, age, age), "not TRUE/FALSE or item scores: person_id")
})

test_that("kits_dif 每題一列；方向跟著 ref，不跟著字母順序", {
  betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = 0)
  theta <- seq(-3, 3, length.out = 300)
  boys <- kits_sim(theta, betas, prefix = "b", seed = 1)
  girls <- kits_sim(theta, modifyList(betas, list(a5 = 1)), prefix = "g", seed = 2)
  X <- rbind(boys, girls)
  out <- kits_dif(X, sex = rep(c("boy", "girl"), each = 300))
  expect_equal(out$item_id, names(betas))
  expect_lt(out$dif[out$item_id == "a5"], 0)   # 女孩比較難：偏向男孩

  out2 <- kits_dif(X, sex = rep(c("M", "F"), each = 300), ref = "M")
  expect_equal(out2$dif, out$dif)
  expect_error(kits_dif(X, sex = rep("boy", 600)), "exactly two")
  expect_error(kits_dif(X, sex = "boy"), "600 row")
})

test_that("kits_dif：某一組有題沒有 0 分時，說出題號和組別", {
  betas <- list(a1 = c(-1, 0), a2 = 0.5, a3 = -8)
  theta <- seq(-3, 3, length.out = 200)
  boys <- kits_sim(theta, betas, prefix = "b", seed = 1)
  girls <- kits_sim(theta, betas, prefix = "g", seed = 2)
  girls[, "a3"] <- 1L
  X <- rbind(boys, girls)
  expect_error(kits_dif(X, sex = rep(c("boy", "girl"), each = 200)), "a3 \\(girl\\)")
})

test_that("shash_z 和 shash_quantile 互為反函數，也和 cNORM 一致", {
  z <- c(-2, -0.5, 0, 1.5)
  X <- shash_quantile(z, mu = 50, sigma = 8, epsilon = 0.3, delta = 1.4)
  expect_equal(shash_z(X, mu = 50, sigma = 8, epsilon = 0.3, delta = 1.4), z)
  expect_equal(stats::pnorm(z), cNORM::pshash(X, mu = 50, sigma = 8, epsilon = 0.3, delta = 1.4))
})

test_that("kits_norms 找回模擬的中位數；kits_norm_z 認得 shash 常模", {
  set.seed(1)
  age <- runif(2000, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(2000, 0, 3)
  norm <- kits_norms(D, age, ages = 6:24, degrees = c(3, 2, 1, 1))
  expect_equal(norm$P50[norm$age_m %in% c(6, 12, 24)], 20 + 15 * log(c(6, 12, 24)), tolerance = 0.02)
  z <- kits_norm_z(D, age, transform(norm, segment = "all"))
  expect_equal(mean(z[age >= 6 & age <= 24]), 0, tolerance = 0.1)
})

test_that("shash_parameters 和 cNORM 內部的 predictCoefficients_shash 一致", {
  set.seed(2)
  age <- runif(500, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(500, 0, 3)
  m <- suppressWarnings(cNORM::cnorm.shash(age = sqrt(age), score = D, mu_degree = 3, sigma_degree = 2, epsilon_degree = 1, delta_degree = 1, plot = FALSE))
  ours <- shash_parameters(m, sqrt(c(6, 12, 24)))
  theirs <- cNORM:::predictCoefficients_shash(m, sqrt(c(6, 12, 24)))
  expect_equal(ours$mu, theirs$mu)
  expect_equal(ours$delta, theirs$delta)
})

test_that("kits_norm_screen 找出堆在最低分的波次", {
  D <- c(10, 10, 10, 12, 15, 20, 22, 25, 30, 31)
  wave_m <- rep(c(6, 12), each = 5)
  out <- kits_norm_screen(D, wave_m)$waves
  expect_equal(names(out), c("wave_m", "n", "floor_pct", "normed"))
  expect_equal(out$floor_pct, c(60, 20))
  expect_equal(out$normed, c(FALSE, FALSE))
  expect_equal(kits_norm_screen(D, wave_m, cut = 50)$waves$normed, c(FALSE, TRUE))
  by_cohort <- kits_norm_screen(D, wave_m, cohort = rep(c("M03", "M36"), times = 5))$waves
  expect_equal(nrow(by_cohort), 4)
  expect_equal(sum(by_cohort$n), 10)
  expect_output(print(kits_norm_screen(D, wave_m)), "0 of 10")
})

test_that("kits_norm_ages：少數特別小的孩子不會把範圍往前拉", {
  age <- c(seq(11, 13, length.out = 500), 7, 8, seq(23, 25, length.out = 500))
  wave <- c(rep(12, 502), rep(24, 500))
  ages <- kits_norm_ages(age, wave_m = wave)
  expect_equal(range(ages), c(11, 25))
  expect_equal(diff(ages[1:2]), 0.5)
})

test_that("kits_norms：幾個起始值裡留概似最大的解", {
  set.seed(1)
  age <- runif(800, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(800, 0, 3)
  one <- suppressWarnings(kits_norms(D, age, ages = 6:24, degrees = c(3, 2, 1, 2), delta_starts = 1))
  best <- suppressWarnings(kits_norms(D, age, ages = 6:24, degrees = c(3, 2, 1, 2)))
  expect_lte(best$neg_loglik[1], one$neg_loglik[1] + 1e-6)
  expect_warning(kits_norms(D, age, ages = 6:24, degrees = c(3, 2, 1, 2), delta_max = 0), "delta reaches")
})

test_that("kits_norm_screen 的 normed：每個人次一個旗標，順序和輸入相同", {
  D <- c(10, 10, 10, 12, 15, 20, 22, 25, 30, 31, NA)
  wave_m <- c(rep(c(6, 12), each = 5), 12)
  screen <- kits_norm_screen(D, wave_m, cut = 50)
  expect_equal(screen$normed, c(rep(FALSE, 5), rep(TRUE, 5), FALSE))
  by_cohort <- kits_norm_screen(D, wave_m, cohort = rep("M03", 11), cut = 50)
  expect_equal(by_cohort$normed, screen$normed)
  expect_error(kits_norm_screen(D, wave_m[-1]), "same length")
})
