# kits_code() 寫出來的程式：在乾淨的環境裡執行，數字要和套件相同

run_code <- function(code, ...) {
  env <- new.env(parent = globalenv())
  for (n in names(list(...))) assign(n, list(...)[[n]], envir = env)
  utils::capture.output(eval(parse(text = code), envir = env))
  env
}

set.seed(1)
betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-2, -1, 1), a6 = 1.5)
X <- kits_sim(rnorm(400, 0, 1.5), betas, seed = 1)
X[sample(length(X), 300)] <- NA
X <- X[rowSums(!is.na(X)) > 0, ]
k <- kits(X)

same_thresholds <- function(env, item, tol) {
  got <- env$thresholds
  want <- coef(item)
  expect_equal(got$item_id, want$item_id)
  expect_equal(got$beta, want$beta, tolerance = tol)
  expect_equal(got$thr, want$thr, tolerance = tol)
  expect_equal(got$thr_se, want$thr_se, tolerance = tol)
  expect_equal(got$beta_se, want$beta_se, tolerance = tol)
}
same_subj <- function(env, subj, tol) {
  got <- env$subj
  want <- subj[match(got$visit_id, subj$visit_id), ]
  expect_equal(nrow(got), nrow(subj))
  expect_equal(got$raw, want$raw)
  expect_equal(got$theta, want$theta, tolerance = tol)
  expect_equal(got$se, want$se, tolerance = tol)
  expect_equal(unname(got$raw_max), want$raw_max)
  expect_equal(got$infit, want$infit, tolerance = tol)
  expect_equal(got$outfit, want$outfit, tolerance = tol)
  expect_equal(got$flag, want$flag)
}

test_that("kits_code(kits)：dexter 的程式重現門檻和個人分數", {
  env <- run_code(kits_code(k), X = X)
  same_thresholds(env, k$item, 1e-10)
  same_subj(env, k$subj, 1e-10)
})

test_that("固定參數和沒放進校準的題：dexter 的程式重現", {
  Y <- X
  Y[, "a4"] <- pmax(Y[, "a4"], 1L)
  fixed <- subset(coef(k$item), item_id %in% c("a1", "a4"))
  item <- suppressMessages(kits_item(Y, fixed_params = fixed))
  expect_equal(item$left_out, "a4")
  same_thresholds(run_code(kits_code(item), X = Y), item, 1e-10)
})

test_that("連結：線、篩掉的點、連結誤差（同樣的重抽）、共用斜率都相同", {
  set.seed(4)
  from <- rnorm(14)
  to <- 60 + 7 * from + rnorm(14, 0, 0.4)
  to[3] <- to[3] + 6
  steps <- data.frame(item_id = paste0("a", 1:14), item_score = 1, thr = from, domain = rep(c("lan", "cog"), each = 7))
  link <- kits_linking_d(steps, data.frame(item_id = steps$item_id, tau = to), n_boot = 200)
  env <- run_code(kits_code(link))
  expect_equal(env$line, coef(link))
  expect_equal(env$id[env$dropped], link$dropped)
  expect_equal(sd(env$draws[env$ok, "intercept"]), link$intercept_error)
  expect_equal(sd(env$draws[env$ok, "slope"]), link$slope_error)
  expect_equal(env$slope_s, link$domains$slope[1])
  expect_equal(unname(c(env$intercept_d[link$domains$domain])), link$domains$intercept)
  co <- kits_linking(k$item, kits_item(X[, 2:6]), n_boot = 100)
  env <- run_code(kits_code(co))
  expect_equal(env$line, coef(co))
  expect_equal(sum(!env$ok), co$n_failed)
})

test_that("常模：cNORM 的程式完全相同", {
  age <- runif(1500, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(1500, 0, 3)
  norm <- suppressWarnings(kits_norms(D, age, ages = seq(6, 30, by = 0.5), degrees = c(3, 2, 1, 1), pad = 0))
  env <- suppressWarnings(run_code(kits_code(norm), D = D, age_m = age))
  expect_equal(env$norm$P10, norm$P10, tolerance = 1e-10)
  expect_equal(env$norm$mu, norm$mu, tolerance = 1e-10)
  expect_equal(env$z, kits_norm_z(D, age, norm), tolerance = 1e-10)
})

test_that("篩波次和月齡範圍：程式相同", {
  age <- runif(900, 3, 30)
  wave <- round(age / 6) * 6
  D <- 20 + 15 * log(age) + rnorm(900, 0, 3)
  D[wave == 6][1:40] <- min(D[wave == 6])
  screen <- kits_norm_screen(D, wave)
  env <- run_code(kits_code(screen), D = D, wave_m = wave, age_m = age)
  expect_equal(env$normed, screen$normed)
  expect_equal(env$ages, kits_norm_ages(age[screen$normed], wave[screen$normed]))
  cohort <- ifelse(age > 15, "M36", "M03")
  screen2 <- kits_norm_screen(D, wave, cohort = cohort)
  env2 <- run_code(kits_code(screen2), D = D, wave_m = wave, age_m = age, cohort = cohort)
  expect_equal(env2$normed, screen2$normed)
})

test_that("寫到檔案；不支援的物件說清楚", {
  f <- tempfile(fileext = ".R")
  kits_code(k, file = f)
  expect_true(file.exists(f))
  expect_output(print(kits_code(k$item)), "fit_enorm")
  expect_error(kits_code(1:3), "No code")
})
