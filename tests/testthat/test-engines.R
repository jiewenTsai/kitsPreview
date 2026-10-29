# 估計引擎：dexter、immer（CML）和 ccml（immer 的成對 CCML，GSED 的方法）
# dexter 和 immer 在最佳化的誤差內相同；每個結果都留著套件自己的物件；kits_code() 重現數字

max_diff <- function(a, b) {
  d <- abs(a - b)
  if (all(is.na(d))) 0 else max(d, na.rm = TRUE)
}

run_code <- function(code, ...) {
  env <- new.env(parent = globalenv())
  for (n in names(list(...))) assign(n, list(...)[[n]], envir = env)
  utils::capture.output(eval(parse(text = code), envir = env))
  env
}

# 不完整設計：三個波次，母體不同（平均 -1.5、0、1.5），每個波次答一段題目
sim_waves <- function(n = 600, seed = 1) {
  betas <- list(a1 = c(-2.5, -1.5), a2 = c(-2, -1), a3 = c(-1.5, -0.5, 0.5), a4 = -0.5, a5 = c(-0.5, 0.5),
                a6 = 0.5, a7 = c(0.5, 1.5), a8 = c(1, 2), a9 = 2)
  wave <- rep(c(6, 12, 24), length.out = n)
  theta <- c(`6` = -1.5, `12` = 0, `24` = 1.5)[as.character(wave)] + stats::rnorm(n)
  X <- kits_sim(theta, betas, seed = seed)
  X[wave == 6, c("a7", "a8", "a9")] <- NA
  X[wave == 12, c("a1", "a9")] <- NA
  X[wave == 24, c("a1", "a2", "a3")] <- NA
  list(X = X, wave = wave)
}

set.seed(11)
d <- sim_waves()
X <- d$X
dex <- kits_item(X)

test_that("immer：同一個 CML，步驟參數、SE、門檻和 dexter 相同（最佳化的誤差內）", {
  skip_if_not_installed("immer")
  im <- kits_item(X, engine = "immer")
  expect_equal(coef(im)$item_id, coef(dex)$item_id)
  expect_equal(coef(im)$item_score, coef(dex)$item_score)
  expect_lt(max_diff(coef(im)$beta, coef(dex)$beta), 1e-4)
  expect_lt(max_diff(coef(im)$beta_se, coef(dex)$beta_se), 1e-4)
  expect_lt(max_diff(coef(im)$thr, coef(dex)$thr), 1e-4)
  expect_lt(max_diff(coef(im)$thr_se, coef(dex)$thr_se), 1e-4)
  expect_equal(sum(coef(im)$beta), 0, tolerance = 1e-10)       # CML 的原點
  expect_equal(im$engine, "immer")
  expect_equal(im$method, "CML")
  expect_s3_class(kits_original(im), "immer_cml")
  # immer 自己的參數換成步驟、減掉平均，就是 beta
  b <- kits_original(im)$b
  steps <- b - ifelse(coef(im)$item_score > 1, c(0, utils::head(b, -1)), 0)
  expect_equal(unname(steps - mean(steps)), coef(im)$beta)
  expect_equal(im$shift, mean(steps))
})

test_that("immer：很多題本、每人答的題不同（隨機缺失）也相同", {
  skip_if_not_installed("immer")
  set.seed(3)
  b <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-2, -1, 1), a6 = 1.5)
  Y <- kits_sim(rnorm(500, 0, 1.5), b, seed = 3)
  Y[sample(length(Y), 900)] <- NA
  Y <- Y[rowSums(!is.na(Y)) > 0, ]
  expect_gt(length(unique(kits_item(Y)$design$pattern_id)), 20)
  expect_lt(max_diff(coef(kits_item(Y, engine = "immer"))$beta, coef(kits_item(Y))$beta), 1e-4)
})

test_that("引擎只有 dexter、immer、ccml：TAM、sirt 已經拿掉", {
  expect_error(kits_item(X, engine = "tam"), "should be one of")
  expect_error(kits_item(X, engine = "sirt"), "should be one of")
  expect_false("engine" %in% names(formals(kits_subj)))
  expect_false(any(c("engine", "method") %in% names(formals(kits_linking))))
})

test_that("固定參數：dexter 可以；immer 停下來（有類別的錯誤）", {
  fixed <- subset(coef(dex), item_id %in% c("a4", "a5", "a6"))
  df <- kits_item(X, fixed_params = fixed)
  hit <- coef(df)$item_id %in% fixed$item_id
  expect_equal(coef(df)$beta[hit], fixed$beta)
  skip_if_not_installed("immer")
  expect_error(kits_item(X, engine = "immer", fixed_params = fixed), class = "kitsPreview_error_engine_unsupported")
  expect_error(kits_item(X, engine = "immer", fixed_params = fixed), class = "kitsPreview_error")
})

test_that("沒有安裝的套件：有類別的錯誤，說怎麼裝", {
  expect_error(need_package("notAPackage123", "x"), class = "kitsPreview_error_missing_package")
  expect_error(need_package("notAPackage123", "x"), "install.packages")
  expect_error(kits_item(X, engine = "psychotools"), "should be one of")
})

test_that("summary() 和 print(original = TRUE)：先印整理過的，再印套件自己的", {
  expect_output(print(summary(dex)), "Original output: dexter::fit_enorm")
  expect_output(print(dex, original = TRUE), "End of the original output")
  s <- kits_subj(X, dex)
  expect_output(print(summary(s)), "dexter::ability")
  expect_s3_class(kits_original(s), "data.frame")
  skip_if_not_installed("immer")
  out <- capture.output(print(summary(kits_item(X, engine = "immer"))))
  expect_lt(grep("<kits_item>", out)[1], grep("Original output: immer::immer_cml", out)[1])
})

test_that("kits_code()：immer 的程式執行後重現門檻；kits(engine = \"immer\") 的計分", {
  same <- function(env, item, tol) {
    got <- env$thresholds
    want <- coef(item)
    expect_equal(got$item_id, want$item_id)
    expect_lt(max_diff(got$beta, want$beta), tol)
    expect_lt(max_diff(got$thr, want$thr), tol)
    expect_lt(max_diff(got$beta_se, want$beta_se), tol)
    expect_lt(max_diff(got$thr_se, want$thr_se), tol)
  }
  skip_if_not_installed("immer")
  im <- kits_item(X, engine = "immer")
  expect_output(print(kits_code(im)), "immer_cml")
  same(run_code(kits_code(im), X = X), im, 1e-10)
  k <- kits(X, engine = "immer")
  expect_equal(k$item$engine, "immer")
  env <- run_code(kits_code(k), X = X)
  expect_lt(max_diff(env$subj$theta, k$subj$theta[match(env$subj$visit_id, k$subj$visit_id)]), 1e-8)
})

test_that("kits_code(kits_subj)：dexter 的程式重現能力值", {
  cf <- coef(dex)
  s <- kits_subj(X, dex)
  env <- run_code(kits_code(s), X = X, cf = cf)
  expect_lt(max_diff(env$subj$theta, s$theta[match(env$subj$visit_id, s$visit_id)]), 1e-10)
  expect_equal(env$subj$flag, s$flag[match(env$subj$visit_id, s$visit_id)])
})

test_that("連結沒有引擎：kitsPreview 自己用 base R 算，沒有原本套件的物件", {
  betas_link <- list(a1 = -2, a2 = -1, a3 = c(-0.5, 0.5), a4 = 0, a5 = c(0.5, 1.5), a6 = 2, a7 = 3)
  young <- kits_sim(stats::rnorm(800, 0), betas_link[1:5], seed = 1)
  old <- kits_sim(stats::rnorm(800, 2), betas_link[3:7], prefix = "q", seed = 2)
  link <- kits_linking(kits_item(old), kits_item(young), n_boot = 0)
  expect_null(link$engine)
  expect_null(link$original)
  expect_error(kits_original(link), "has no original object: kitsPreview computes it itself")
  expect_error(kits_linking(kits_item(old), kits_item(young), engine = "sirt"), "unused argument")
})


test_that("ccml：包裝就是 immer_ccml() 本身（設計矩陣、原點、SE）", {
  skip_if_not_installed("immer")
  cc <- kits_item(X, engine = "ccml")
  expect_s3_class(cc, "kitsPreview_item")
  expect_equal(cc$engine, "ccml")
  expect_equal(cc$method, "CCML")
  expect_s3_class(kits_original(cc), "immer_ccml")
  expect_equal(kits_original(cc)$nlminb_result$convergence, 0L)
  expect_lt(abs(sum(coef(cc)$beta)), 1e-10)
  expect_equal(coef(cc)$item_id, coef(dex)$item_id)
  expect_equal(coef(cc)$item_score, coef(dex)$item_score)
  # 自己建設計矩陣、直接呼叫 immer，減掉平均：和包裝一樣
  m <- apply(X, 2, max, na.rm = TRUE)
  K <- max(m)
  item <- rep(seq_along(m), m)
  step <- sequence(m)
  A <- array(0, dim = c(ncol(X), K, length(item)))
  for (l in seq_along(item)) A[item[l], step[l]:K, l] <- 1
  fit <- immer::immer_ccml(as.data.frame(X), A = A, control = list(iter.max = 5000, eval.max = 5000))
  expect_lt(max_diff(fit$coef - mean(fit$coef), coef(cc)$beta), 1e-10)
  C <- diag(length(item)) - 1 / length(item)
  expect_lt(max_diff(sqrt(diag(C %*% fit$vcov %*% t(C))), coef(cc)$beta_se), 1e-10)
  # 資料符合 PCM，所以和 CML 差不多（抽樣誤差的大小）
  expect_lt(max_diff(coef(cc)$beta, coef(dex)$beta), 0.3)
  expect_true(all(diff(coef(cc)$thr[coef(cc)$item_id == "a3"]) > 0))
})

test_that("ccml：印出、summary、kits_code() 重現數字；不能固定參數", {
  skip_if_not_installed("immer")
  cc <- kits_item(X, engine = "ccml")
  expect_output(print(cc), "immer, CCML")
  out <- capture.output(print(summary(cc)))
  expect_true(any(grepl("Original output: immer::immer_ccml", out)))
  expect_output(print(kits_code(cc)), "immer_ccml")
  env <- run_code(kits_code(cc), X = X)
  expect_lt(max_diff(env$thresholds$beta, coef(cc)$beta), 1e-10)
  expect_lt(max_diff(env$thresholds$thr, coef(cc)$thr), 1e-10)
  expect_lt(max_diff(env$thresholds$beta_se, coef(cc)$beta_se), 1e-10)
  expect_lt(max_diff(env$thresholds$thr_se, coef(cc)$thr_se), 1e-10)
  k <- kits(X, engine = "ccml")
  env <- run_code(kits_code(k), X = X)
  expect_lt(max_diff(env$subj$theta, k$subj$theta[match(env$subj$visit_id, k$subj$visit_id)]), 1e-8)
  fixed <- coef(dex)[coef(dex)$item_id == "a4", ]
  expect_error(kits_item(X, engine = "ccml", fixed_params = fixed), class = "kitsPreview_error_engine_unsupported")
})

test_that("ccml：沒有收斂時警告", {
  skip_if_not_installed("immer")
  expect_warning(kits_item(X, engine = "ccml", control = list(iter.max = 2, eval.max = 3)), "did not converge")
})
