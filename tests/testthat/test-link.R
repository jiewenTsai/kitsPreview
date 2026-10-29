set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

test_that("mean_sigma 的斜率等於 lm 斜率除以 r", {
  from <- c(-1, 0.5, 1, 2, 3)
  to <- c(0, 2, 2.5, 5, 6)
  b_lm <- unname(coef(lm(to ~ from))[2])
  expect_equal(mean_sigma(from, to)$slope, b_lm / cor(from, to))
})

test_that("自己的點的連結（三步）剔除偏移的點，並找回原本的線", {
  from <- seq(-2, 3, length.out = 12)
  to <- 1 + 2 * from
  to[5] <- to[5] + 4
  link <- link_of_points(from, to, id = paste0("p", 1:12))
  expect_s3_class(link, "kitsPreview_link")
  expect_equal(link$dropped, "p5")
  expect_equal(c(link$intercept, link$slope), c(1, 2))
  expect_true(is.na(link$intercept_error))
  expect_output(print(link), "dropped: 1 \\(p5\\)")
})

test_that("自己的點的連結：點剛好在線上時不剔除；輸入有問題時說清楚", {
  from <- c(-1, 0, 1, 2)
  expect_length(link_of_points(from, 1 + from, id = paste0("p", 1:4))$dropped, 0)
  expect_error(link_of_points(c(1, NA, 3), 1:3, letters[1:3]), "NA")
  expect_error(link_of_points(1:2, 1:2, letters[1:2]), "3 points")
  expect_error(link_of_points(rep(1, 4), 1:4, letters[1:4]), "same")
})

test_that("連結誤差：算不出來的重抽另外計數；不改變使用者的亂數狀態", {
  from <- c(-2, -1, 0.5, 2)   # 4 個點：重抽有時 4 次都抽到同一點
  to <- 1 + from + c(0.1, -0.2, 0.1, 0)
  link <- link_of_points(from, to, paste0("p", 1:4), n_boot = 300)
  expect_true(is.finite(link$intercept_error))
  expect_gt(link$n_failed, 0)
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  link_of_points(from, to, paste0("p", 1:4), n_boot = 10)
  expect_equal(runif(1), before)
})

test_that("只有一組時，mean_sigma_shared_slope 等於 mean_sigma", {
  from <- c(-1, 0.5, 1, 2, 3)
  to <- c(0, 2, 2.5, 5, 6)
  g <- mean_sigma_shared_slope(from, to, rep("lan", 5))
  expect_equal(list(intercept = unname(g$intercept), slope = g$slope), mean_sigma(from, to))
})

betas <- list(a1 = -2, a2 = -1, a3 = c(-0.5, 0.5), a4 = 0, a5 = c(0.5, 1.5), a6 = 2, a7 = 3)
young <- item_thresholds(dexter::fit_enorm(kits_sim(rnorm(500, 0), betas[1:5], seed = 1)))
old <- item_thresholds(dexter::fit_enorm(kits_sim(rnorm(500, 2), betas[3:7], prefix = "q", seed = 2)))

test_that("kits_linking 用共同題的所有步驟，整題重抽；和直接用連結的三步相同", {
  link <- kits_linking(old, young, n_boot = 100)
  expect_equal(link$n, 5)
  expect_equal(link$n_items, 3)
  common <- merge(old, young, by = c("item_id", "item_score"))
  direct <- link_of_points(common$thr.x, common$thr.y, paste0(common$item_id, ":",
                                                              common$item_score),
                            item = common$item_id, n_boot = 100)
  expect_equal(link$intercept, direct$intercept)
  expect_equal(link$intercept_error, direct$intercept_error)
})

test_that("kits_linking：共同題太少、表重複時停下來", {
  expect_error(kits_linking(old[old$item_id == "a7", ], young), "common")
  expect_error(kits_linking(rbind(old, old), young), "repeated")
})

test_that("kits_linking_d 用 milestone step；有 domain 欄時給各領域截距", {
  steps <- data.frame(item_id = paste0("a", 1:8), item_score = 1, thr = c(-1, 0, 1, 2, -1.5, 0.5, 1, 2.5),
                      domain = rep(c("lan", "cog"), each = 4))
  steps <- rbind(steps, data.frame(item_id = "a1", item_score = 2, thr = 9, domain = "lan"))   # 第 2 步不用
  anchors <- data.frame(item_id = paste0("a", 1:8), tau = 64 + 7 * steps$thr[1:8] + ifelse(1:8 > 4, 1, 0))
  link <- kits_linking_d(steps, anchors, n_boot = 0)
  expect_equal(link$n_items, 8)
  expect_equal(link$domains$slope, c(7, 7), tolerance = 1e-8)
  expect_equal(link$domains$intercept[link$domains$domain == "cog"] - link$domains$intercept[link$domains$domain == "lan"], 1, tolerance = 1e-8)
  expect_output(print(link), "domains")
  expect_error(kits_linking_d(steps, anchors[1:2, ]), "at least 3")
  expect_error(kits_linking_d(steps, rbind(anchors, anchors[1, ])), "repeated")
})

test_that("kits_linking_d 的通過切點：step 參數和 anchors$step", {
  steps <- data.frame(item_id = rep(paste0("a", 1:6), each = 2), item_score = rep(1:2, 6),
                      thr = as.vector(rbind(c(-1, 0, 1, 2, -1.5, 0.5), c(0, 1, 2, 3, -0.5, 1.5))))
  anchors <- data.frame(item_id = paste0("a", 1:6), tau = 64 + 7 * steps$thr[steps$item_score == 2])
  link2 <- kits_linking_d(steps, anchors, step = 2, n_boot = 0)
  expect_equal(c(link2$intercept, link2$slope), c(64, 7), tolerance = 1e-8)
  link1 <- kits_linking_d(steps, anchors, n_boot = 0)
  expect_false(isTRUE(all.equal(link1$intercept, 64)))
  anchors$step <- c(2, 2, 2, NA, 2, 2)              # NA 用 step 參數
  link_mix <- kits_linking_d(steps, anchors, step = 2, n_boot = 0)
  expect_equal(link_mix$intercept, link2$intercept)
  expect_error(kits_linking_d(steps, anchors, step = 3), "pass step")
  expect_error(kits_linking_d(steps, anchors, step = 0), "step")
})
