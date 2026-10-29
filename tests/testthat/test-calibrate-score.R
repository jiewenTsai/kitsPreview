set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5))
X <- kits_sim(seq(-3, 3, length.out = 400), betas)
fit <- dexter::fit_enorm(X)
steps <- item_thresholds(fit)

test_that("兩點計分題的門檻等於 beta", {
  expect_equal(step_threshold(0.5, k = 1)[["thr"]], 0.5, tolerance = 1e-8)
})

test_that("在門檻上，達到 k 分以上的機率是 .5；k 超出範圍時停下來", {
  thr <- step_threshold(c(1, -1), k = 2)[["thr"]]
  expect_equal(p_at_least(thr, c(1, -1), k = 2), 0.5, tolerance = 1e-8)
  expect_error(p_at_least(0, c(1, -1), k = 3), "between 1")
})

test_that("item_thresholds 每題每個步驟一列，門檻由小到大", {
  expect_equal(nrow(steps), sum(lengths(betas)))
  expect_equal(names(steps), c("item_id", "item_score", "beta", "beta_se", "thr", "thr_se"))
  expect_true(all(tapply(steps$thr, steps$item_id, function(t) all(diff(t) > 0))))
})

test_that("item_thresholds：分數不連續時、不是 fit_enorm 的結果時停下來", {
  gap <- X
  gap[gap[, "a1"] == 1, "a1"] <- 2L
  expect_error(kits_item(gap)$thresholds, "consecutive")
  expect_error(item_thresholds(steps), "fit_enorm")
})

test_that("p_at_least 對 θ 向量化，和 dexter::p_score 相同", {
  parms <- data.frame(item_id = "a1", item_score = 1:2, beta = c(-1, 1))
  p <- dexter::p_score(parms, items = "a1")(c(-1, 0, 1))
  expect_equal(p_at_least(c(-1, 0, 1), betas = c(-1, 1), k = 1), unname(rowSums(p[, 2:3])))
})

test_that("kits_subj 和 dexter 的 WLE 相同、欄位相同、順序照輸入", {
  wle <- as.data.frame(dexter::ability(X, fit, method = "WLE"))
  s <- kits_subj(X, fit)
  expect_equal(names(s), c("pattern_id", "visit_id", "raw", "raw_max", "theta", "se", "infit", "outfit", "flag"))
  expect_equal(s$visit_id, rownames(X))
  expect_equal(s$theta, wle$theta[match(s$visit_id, wle$person_id)])
  expect_equal(s$raw, wle$booklet_score[match(s$visit_id, wle$person_id)])
  # 原本的輸出（dexter 自己的表）照舊是 dexter 的名字
  expect_true(all(c("booklet_id", "person_id", "booklet_score") %in% names(kits_original(s))))
  expect_equal(kits_subj(X, steps)$theta, s$theta)   # 門檻表也可以當 parms
})

test_that("kits_subj：長格式也收；格式不對時說清楚", {
  long <- data.frame(visit_id = rep(rownames(X), ncol(X)), item_id = rep(colnames(X), each = nrow(X)), item_score = as.vector(X))
  expect_equal(kits_subj(long, fit)$theta, kits_subj(X, fit)$theta)
  expect_error(kits_subj(data.frame(visit_id = "p1", item_id = "a1"), fit), "as.matrix")
  expect_error(kits_subj(unname(X), fit), "row names")
  expect_error(kits_subj(cbind(X, zz = 0L), fit), "zz")
  expect_error(kits_subj(X[1, ], fit), "drop = FALSE")
  expect_equal(nrow(kits_subj(X[1, , drop = FALSE], fit)), 1)
})

test_that("Jeffreys MAP 和 dexter 的 WLE 相同；包括 0 分、滿分、每人答不同的題", {
  set.seed(3)
  betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-2, -1, 1))
  X <- kits_sim(rnorm(200, 0, 2.5), betas)
  X[sample(length(X), 150)] <- NA   # 每人答的題不同
  X <- X[rowSums(!is.na(X)) > 0, ]
  fit <- dexter::fit_enorm(X)
  wle <- as.data.frame(dexter::ability(X, fit, method = "WLE"))
  items <- lapply(wle$person_id, function(p) colnames(X)[!is.na(X[p, ])])
  map <- t(mapply(jeffreys_map, items, wle$booklet_score, MoreArgs = list(parms = param_frame(fit))))
  expect_equal(unname(map[, "theta"]), wle$theta, tolerance = 1e-6)
  expect_equal(unname(map[, "se"]), wle$se, tolerance = 1e-6)
  expect_true(any(wle$booklet_score == 0))
})

test_that("WLE 算不出來時：kits_subj 用 Jeffreys MAP 補上、有訊息；kits_subj_table 的兩種表也不是 NA", {
  parms <- data.frame(item_id = c("i1", "i1", "i1", "i2", "i2", "i2", "i3"), item_score = c(1:3, 1:3, 1),
                      beta = c(-8.6, -8, -3.2, -10.3, -9.6, -8.6, 7.2))
  resp1 <- matrix(c(1, 3, 2, 3, 0, 0), 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  expect_message(s <- kits_subj(resp1, parms), "Jeffreys MAP")
  expect_true(all(is.finite(s$theta)))
  tab <- kits_subj_table(parms)
  expect_true(all(is.finite(tab$theta)))
  tab <- suppressMessages(table_of(resp1, parms, wave_m = c(3, 3)))
  expect_true(all(is.finite(tab$theta)))
})

test_that("kits_subj_table 和 dexter 的 WLE 表相同", {
  parms <- data.frame(item_id = c("a1", "a1", "a2", "a3", "a3"), item_score = c(1, 2, 1, 1, 2), beta = c(-1, 0, 0.5, 0, 1))
  tab <- kits_subj_table(parms)
  wle <- as.data.frame(dexter::ability_tables(parms, method = "WLE"))
  expect_equal(names(tab), c("pattern_id", "raw", "theta", "se"))
  expect_equal(tab$raw, 0:5)
  expect_equal(tab$theta, wle$theta, tolerance = 1e-6)
  expect_error(wle_table(parms, design = data.frame(pattern_id = "b", item_id = "zz")), "zz")
})

test_that("kits_subj_table：人數加總等於答標準題本的人數；人的資料照 visit_id 接上", {
  tab <- table_of(X, fit, wave_m = rep(12, 400))
  expect_equal(sum(tab$n), 400)
  expect_equal(tail(tab$cum_pct, 1), 100)
  persons <- data.frame(visit_id = rev(rownames(X)), wave_m = 12)
  expect_equal(kits_subj_table(kits_subj(X, fit, persons = persons)), tab)
  expect_error(kits_subj(X, fit, persons = persons[-1, ]), "no row in `persons`")
  expect_equal(nrow(kits_subj_table(kits_subj(X, fit))), nrow(tab))   # 沒有 wave_m：所有列一組
  expect_true(is.na(kits_subj_table(kits_subj(X, fit))$wave_m[1]))
  expect_equal(unique(tab$form), 1)
  expect_equal(tab$form_pct[1], 100)
})

test_that("kits_subj_table forms = \"all\"：每個題目組合一張表，人數加起來是整波", {
  Y <- X
  Y[1:100, "a4"] <- NA                                  # 100 人少答一題
  tab <- table_of(Y, fit, wave_m = rep(12, 400))
  expect_equal(sum(tab$n), 300)
  expect_equal(tab$form_pct[1], 75)
  all_forms <- table_of(Y, fit, wave_m = rep(12, 400), forms = "all")
  expect_equal(sort(unique(all_forms$form)), 1:2)
  expect_equal(sum(all_forms$n), 400)
  expect_equal(unique(all_forms$wave_n), 400)
  sets <- attr(all_forms, "forms")
  expect_equal(sets$n, c(300, 100))
  expect_equal(sets$items[2], "a1, a2, a3, a5")
  expect_equal(max(all_forms$raw[all_forms$form == 2]), 2 + 2 + 1 + 2)
})

test_that("raw_max 是作答題目的最高分加總", {
  Y <- X
  Y[1, "a4"] <- NA
  s <- kits_subj(Y, fit)
  expect_equal(s$raw_max[1], 2 + 2 + 1 + 2)
  expect_equal(s$raw_max[2], 8)
  expect_true(all(s$raw <= s$raw_max))
})
