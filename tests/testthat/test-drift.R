set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5), a6 = 1.5, a7 = -3.5)
old <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
fit_old <- dexter::fit_enorm(old)
ref <- kits_item(old)$thresholds
new_betas <- c(modifyList(betas, list(a3 = 2)), list(a8 = c(1, 2)))
new <- kits_sim(seq(-2, 4, length.out = 400), new_betas, prefix = "n", seed = 2)

test_that("kits_drift 找到偏移的題，資訊不足的題不判斷", {
  chk <- kits_drift(new, ref)
  expect_true(chk$items$drift[chk$items$item_id == "a3"])
  expect_true(is.na(chk$items$drift[chk$items$item_id == "a7"]))
  expect_equal(chk$items$reason[chk$items$item_id == "a7"], "too little information")
  expect_true(chk$items$suggest_fix[chk$items$item_id == "a7"])
  expect_equal(sum(chk$items$drift, na.rm = TRUE), 1)
  expect_false("a8" %in% chk$items$item_id)   # 新題不判斷
})

test_that("kits_drift：分數範圍比 ref 大的題，不判斷，也不建議固定", {
  wider <- new
  wider[300:400, "a3"] <- 2L
  a3 <- kits_drift(wider, ref)$items
  a3 <- a3[a3$item_id == "a3", ]
  expect_equal(a3$reason, "score range differs")
  expect_false(a3$suggest_fix)
})

test_that("kits_drift：每個步驟分開看資訊；一題只要有一個步驟資訊夠就判斷", {
  # a9 有兩個步驟：第 1 步幾乎人人通過（資訊不夠），第 2 步資訊夠；新的一波整題變容易
  b9 <- c(betas, list(a9 = c(-4, 0.5)))
  ref9 <- item_thresholds(dexter::fit_enorm(kits_sim(seq(-3, 3, length.out = 400), b9, seed = 1)))
  new9 <- kits_sim(seq(-2, 4, length.out = 400), modifyList(b9, list(a9 = c(-5.5, -1))), prefix = "n", seed = 2)
  chk <- kits_drift(new9, ref9)
  pts <- chk$points
  expect_true(all(pts$judged == (pmin(pts$n_below, pts$n_above) >= 30)))
  a9 <- chk$items[chk$items$item_id == "a9", ]
  expect_false(is.na(a9$drift))
  expect_true(a9$drift)
  expect_false(a9$suggest_fix)
  expect_true(all(is.na(chk$items$drift[chk$items$reason != ""])))
  expect_equal(names(chk$items), c("item_id", "max_abs_resid", "drift", "suggest_fix", "reason"))
})

test_that("kits_drift 在有題目沒有 0 分時不會停下來；min_n 太大時說清楚", {
  ceiling <- new
  ceiling[, "a7"] <- 1L
  chk <- kits_drift(ceiling, ref)
  expect_true(is.na(chk$items$drift[chk$items$item_id == "a7"]))
  expect_true(chk$items$suggest_fix[chk$items$item_id == "a7"])
  expect_error(kits_drift(new, ref, min_n = 1000), "min_n")
  expect_error(kits_drift(new, ref[ref$item_id == "zz", ]), "No item")
})

test_that("更新流程：照 kits_drift 的建議，用 dexter 的 fixed_params 固定；固定題的參數不變", {
  chk <- kits_drift(new, ref)
  fix <- unique(chk$fix$item_id)
  expect_equal(fix, chk$items$item_id[chk$items$suggest_fix])
  expect_equal(coef(kits_item(new, fixed_params = chk$fix))$beta[1:2], coef(fit_old)$beta[1:2])
  fit_new <- dexter::fit_enorm(new, fixed_params = subset(coef(fit_old), item_id %in% fix))
  cf <- coef(fit_new)
  fixed <- cf[cf$item_id %in% fix, ]
  old_cf <- coef(fit_old)
  expect_equal(fixed$beta, old_cf$beta[match(paste(fixed$item_id, fixed$item_score), paste(old_cf$item_id, old_cf$item_score))])
  expect_true(all(c("a3", "a8") %in% cf$item_id))
})
