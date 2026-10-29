set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

betas <- list(a1 = -2, a2 = -1, a3 = c(-0.5, 0.5), a4 = 0, a5 = c(0.5, 1.5), a6 = 2, a7 = 3)
X03 <- kits_sim(rnorm(500, 0), betas[1:5], seed = 1)
X36 <- kits_sim(rnorm(500, 2), betas[3:7], prefix = "q", seed = 2)
item03 <- kits_item(X03)
item36 <- kits_item(X36)

test_that("kits_item：門檻表、dexter 的結果、人數；印出來簡短；coef() 是門檻表", {
  expect_s3_class(item03, "kitsPreview_item")
  expect_equal(item03$thresholds, preview_table(item_thresholds(dexter::fit_enorm(X03))))
  expect_equal(coef(item03), item03$thresholds)
  expect_equal(item03$n_persons, 500)
  expect_output(print(item03), "500 person-waves, 5 items, 7 steps")
  expect_error(kits_item(X03, method = "Bayes"), "CML")
})

test_that("kits_item 把 ... 交給 fit_enorm：固定參數", {
  fixed <- subset(coef(item03), item_id != "a3")
  upd <- kits_item(X03, fixed_params = fixed)
  expect_equal(upd$thresholds$beta[upd$thresholds$item_id == "a1"], item03$thresholds$beta[item03$thresholds$item_id == "a1"])
})

test_that("kits_subj 收 kits_item、fit_enorm 的結果、門檻表，結果相同", {
  s <- kits_subj(X03, item03)
  expect_equal(kits_subj(X03, item03$fit)$theta, s$theta)
  expect_equal(kits_subj(X03, coef(item03))$theta, s$theta)
})

test_that("kits_linking、kits_linking_d、kits_drift 直接收 kits_item；kits_linking_d 收有名字的 list", {
  expect_equal(kits_linking(item36, item03, n_boot = 0)$intercept, kits_linking(coef(item36), coef(item03), n_boot = 0)$intercept)
  anchors <- data.frame(item_id = c("a1", "a2", "a3", "a4"), tau = 64 + 7 * coef(item03)$thr[coef(item03)$item_score == 1][1:4])
  expect_equal(kits_linking_d(item03, anchors, n_boot = 0)$slope, 7, tolerance = 1e-8)
  X_cog <- X03
  colnames(X_cog) <- paste0("c", colnames(X_cog))
  item_cog <- kits_item(X_cog)
  anchors2 <- rbind(anchors, data.frame(item_id = c("ca1", "ca2"), tau = 64 + 7 * coef(item_cog)$thr[1:2]))
  two <- kits_linking_d(list(lan = item03, cog = item_cog), anchors2, n_boot = 0)
  expect_equal(two$domains$domain, c("cog", "lan"))
  expect_equal(two$domains$n, c(2, 4))
  expect_error(kits_linking_d(list(lan = item03, cog = item36), anchors), "repeated")   # 兩個世代不是兩個領域
  expect_error(kits_linking_d(list(item03, item36), anchors), "name each element")
  expect_s3_class(kits_drift(X36, item36)$items, "data.frame")
})

test_that("kits：校準加計分；印出來簡短；plot() 是 Wright map", {
  k <- kits(X03)
  expect_s3_class(k, "kitsPreview_fit")
  expect_equal(k$subj, kits_subj(X03, k$item))
  expect_output(print(k), "Abilities")
  p <- plot(k)
  expect_s3_class(p, "plotly")
  expect_silent(plotly::plotly_build(p))
})

test_that("kits_norms 有 class；plot() 畫百分位曲線，可以加上資料點", {
  age <- runif(600, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(600, 0, 3)
  norm <- suppressWarnings(kits_norms(D, age, ages = seq(3, 36, by = 3), degrees = c(3, 2, 1, 1)))
  expect_s3_class(norm, "kitsPreview_norms")
  expect_s3_class(plot(norm, scores = data.frame(theta = D, age_m = age)), "plotly")
  expect_error(plot(norm, pct = 1), "P1")
  expect_error(plot(norm, scores = data.frame(theta = D)), "age_m")
  expect_error(plot(norm, scores = D[1:3]), "table with theta and age_m", class = "kitsPreview_error_input")
  expect_equal(kits_norm_z(D[1:3], age[1:3], norm), kits_norm_z(D[1:3], age[1:3], as.data.frame(norm)))
})

test_that("kits_itemfit：雜訊題的 infit、outfit 比較大，rir 比較小；多題本也可以", {
  set.seed(3)
  b <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = 0)
  Y <- kits_sim(seq(-3, 3, length.out = 400), b, seed = 4)
  Y[, "a5"] <- sample(0:1, 400, replace = TRUE)
  Y[1:100, "a4"] <- NA
  f <- kits_itemfit(Y, kits_item(Y))
  expect_equal(f$item_id, names(b))
  expect_equal(f$item_id[which.max(f$infit)], "a5")
  expect_equal(f$item_id[which.max(f$outfit)], "a5")
  expect_equal(f$item_id[which.min(f$rir)], "a5")
})

test_that("kits_subj 的個人適配：0 分和滿分是 NA；亂答的人 outfit 比較大", {
  b <- setNames(as.list(seq(-2, 2, length.out = 20)), paste0("a", 1:20))
  Y <- kits_sim(rep(0, 300), b, seed = 5)
  Y[1, ] <- rep(c(1L, 0L), 10)   # 簡單的題都錯、難的題都對
  Y[1, ] <- as.integer(seq(-2, 2, length.out = 20) > 0)
  Y[2, ] <- 0L
  s <- kits_subj(Y, kits_item(Y))
  expect_true(is.na(s$infit[2]))
  expect_gt(s$outfit[1], stats::quantile(s$outfit, 0.99, na.rm = TRUE) - 1e-8)
})

test_that("kits_inter：每個波次的標準題本各跑一次 fit_inter；plot 找不到波次時說清楚", {
  inter <- kits_inter(X03, wave_m = rep(c(6, 12), each = 250))
  expect_s3_class(inter, "kitsPreview_inter")
  expect_equal(names(inter$fits), c("6", "12"))
  expect_equal(sort(unique(inter$items$item_id)), colnames(X03))
  expect_output(print(inter), "fit_IM")
  expect_error(plot(inter, wave = 99), "No fit for wave 99")
})

test_that("plot(link) 是 plotly", {
  link <- kits_linking(item36, item03, n_boot = 0)
  expect_s3_class(plot(link), "plotly")
})

test_that("報表印小數點下 2 位，digits 可以改；物件裡的數值不變", {
  thr <- coef(item03)
  expect_s3_class(thr, "kitsPreview_table")
  expect_s3_class(thr, "data.frame")
  printed <- capture.output(print(thr))
  expect_true(any(grepl(sprintf("%.2f", thr$thr[1]), printed, fixed = TRUE)))
  expect_false(any(grepl(sprintf("%.4f", thr$thr[1]), printed, fixed = TRUE)))
  expect_true(any(grepl(sprintf("%.4f", thr$thr[1]), capture.output(print(thr, digits = 4)), fixed = TRUE)))
  expect_false(thr$thr[1] == round(thr$thr[1], 2))
  expect_error(print(thr, digits = -1), "digits")

  link <- kits_linking(item36, item03, n_boot = 0)
  expect_output(print(link), sprintf("intercept = %.2f\n", link$intercept), fixed = TRUE)
  expect_output(print(link, digits = 4), sprintf("slope     = %.4f", link$slope), fixed = TRUE)
  expect_true(any(grepl(sprintf("%.2f + %.2f", link$intercept, link$slope), capture.output(kits_equations(link)), fixed = TRUE)))
  expect_true(any(grepl(sprintf("%.3f", link$slope), capture.output(kits_equations(link, digits = 3)), fixed = TRUE)))

  s <- kits_subj(X03, item03)
  expect_s3_class(s, c("kitsPreview_subj", "kitsPreview_table", "data.frame"), exact = TRUE)
  expect_output(print(utils::head(s, 3)), sprintf("%.2f", s$theta[1]), fixed = TRUE)
  expect_s3_class(kits_itemfit(X03, item03), "kitsPreview_table")
  expect_output(print(item03), sprintf("%.2f", thr$thr[1]), fixed = TRUE)
  expect_output(print(item03, digits = 3), sprintf("%.3f", thr$thr[1]), fixed = TRUE)
})

test_that("kits_item 不收 predicate；fixed_params 可以直接給 kits_item；kits 物件可以當 item", {
  expect_error(kits_item(X03, item_id != "a1"), "predicate")
  upd <- kits_item(X03, fixed_params = item03)
  expect_equal(coef(upd)$beta, coef(item03)$beta)
  k <- kits(X03)
  expect_equal(kits_subj(X03, k)$theta, k$subj$theta)
  expect_equal(kits_linking(item36, k, n_boot = 0)$intercept, kits_linking(item36, item03, n_boot = 0)$intercept)
})

test_that("kits_item：固定的題沒有 0 分時不放進校準、參數照抄；不固定的題沒有 0 分時停下來", {
  Y <- X03
  Y[, "a1"] <- pmax(Y[, "a1"], 1L)
  expect_message(upd <- kits_item(Y, fixed_params = item03), "a1")
  expect_equal(upd$left_out, "a1")
  expect_equal(coef(upd)[coef(upd)$item_id == "a1", "thr"], coef(item03)[coef(item03)$item_id == "a1", "thr"], tolerance = 1e-6)
  expect_true(all(is.finite(kits_subj(Y, upd)$theta)))
  expect_error(kits_item(Y), "no 0 score")
})
