# 暴力測試第四輪（stress/round4）的回歸測試：產生的程式和公式

run_code <- function(code, ...) {
  env <- new.env(parent = globalenv())
  for (n in names(list(...))) assign(n, list(...)[[n]], envir = env)
  utils::capture.output(suppressMessages(eval(parse(text = code), envir = env)))
  env
}

betas6 <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-2, -1, 1), a6 = 1.5)
X6 <- kits_sim(rnorm(300, 0, 1.5), betas6, seed = 1)
item6 <- kits_item(X6)
Xn <- kits_sim(rnorm(400, 0.5, 1.5), betas6, prefix = "n", seed = 10)

test_that("R4-A1、A2：固定參數（沒有題被留下）和全部固定，程式都能跑", {
  it <- kits_item(Xn, fixed_params = subset(coef(item6), item_id %in% c("a1", "a3")))
  expect_equal(length(it$left_out), 0)
  env <- run_code(kits_code(it), X = Xn)
  expect_equal(env$thresholds$beta, coef(it)$beta, tolerance = 1e-10)
  all_fixed <- suppressMessages(kits_item(Xn, fixed_params = item6))
  env <- run_code(kits_code(all_fixed), X = Xn)
  expect_equal(env$thresholds$beta, coef(all_fixed)$beta)
  expect_equal(env$thresholds$thr, coef(all_fixed)$thr, tolerance = 1e-8)
})

test_that("R4-A3：欄的順序不是排序過的，dexter 版的計分程式也能跑", {
  Y <- X6[, c(6, 1:5)]
  it <- kits_item(Y)
  env <- run_code(kits_code(it, subj = TRUE), X = Y)
  s <- kits_subj(Y, it)
  expect_equal(env$subj$theta[match(s$visit_id, env$subj$visit_id)], s$theta, tolerance = 1e-8)
})

test_that("R4-A4：全部 NA 的欄：程式跳過它", {
  X16 <- cbind(X6, a7 = NA_integer_)
  it <- kits_item(X16)
  env <- run_code(kits_code(it, subj = TRUE), X = X16)
  expect_equal(env$thresholds$beta, coef(it)$beta, tolerance = 1e-10)
})

test_that("R4-A5、C1：seed 要檢查；seed = NULL 的連結程式也能跑", {
  from <- rnorm(14)
  to <- 60 + 7 * from + rnorm(14, 0, 0.4)
  id <- paste0("p", 1:14)
  expect_error(link_of_points(from, to, id, n_boot = 50, seed = NA), "seed")
  expect_error(link_of_points(from, to, id, n_boot = 50, seed = "a"), "seed")
  expect_error(link_of_points(from, to, id, n_boot = 50, seed = 1e10), "seed")
  link <- link_of_points(from, to, id, n_boot = 50, seed = NULL)
  env <- run_code(kits_code(link))
  expect_equal(env$line, coef(link))
  expect_true(grepl("intercept0 <- ", paste(kits_code(link), collapse = "\n")))
})

test_that("R4-A6、A7、A8：age_x 是自己的函數（也可以多行）時，程式不依賴呼叫端；age_x 不是有限值的月齡跳過", {
  set.seed(5)
  age <- runif(1500, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(1500, 0, 3)
  my_age_x <- function(a) sqrt(a)
  nm <- kits_norms(D, age, ages = seq(4, 35, 1), degrees = c(3, 2, 1, 1), age_x = my_age_x)
  code <- kits_code(nm)
  expect_false(any(grepl("my_age_x", code)))
  env <- run_code(code, D = D, age_m = age)
  expect_equal(env$norm$P50, nm$P50, tolerance = 1e-10)
  nm2 <- kits_norms(D, age, ages = seq(4, 35, 1), degrees = c(3, 2, 1, 1), age_x = function(a) {
    y <- sqrt(a)
    y
  })
  expect_silent(parse(text = kits_code(nm2)))
  expect_equal(run_code(kits_code(nm2), D = D, age_m = age)$norm$P50, nm$P50, tolerance = 1e-10)
  age0 <- age
  age0[1:30] <- 0
  nm3 <- suppressWarnings(kits_norms(D, age0, ages = seq(0, 35, 1), degrees = c(3, 2, 1, 1), age_x = log))
  env3 <- suppressWarnings(run_code(kits_code(nm3), D = D, age_m = age0))
  expect_equal(env3$norm$P50, nm3$P50, tolerance = 1e-10)
})

test_that("R4-B1：kits() 的程式重現 CML 和 WLE", {
  k <- kits(X6)
  env <- run_code(kits_code(k), X = X6)
  expect_equal(env$thresholds$beta, coef(k$item)$beta, tolerance = 1e-8)
  expect_equal(env$thresholds$beta_se, coef(k$item)$beta_se, tolerance = 1e-6)
  s <- k$subj
  got <- env$subj[match(s$visit_id, env$subj$visit_id), ]
  expect_equal(got$theta, s$theta, tolerance = 1e-8)
  expect_equal(got$outfit, s$outfit, tolerance = 1e-6)
})

test_that("R4-B2：預設次數的常模（cNORM 的截斷會作用）：程式重現", {
  set.seed(5)
  age <- runif(1500, 3, 36)
  D <- 20 + 15 * log(age) + rnorm(1500, 0, 3)
  nm <- suppressWarnings(kits_norms(D, age, ages = seq(4, 35, by = 1)))   # 預設的次數：cNORM 的截斷會作用
  env <- suppressWarnings(run_code(kits_code(nm), D = D, age_m = age))   # 和套件一樣警告 delta
  expect_equal(env$norm$P50, nm$P50, tolerance = 1e-10)
  expect_equal(env$norm$P97, nm$P97, tolerance = 1e-10)
})

test_that("R4-A9、C4、C5：kits_equations 的輸入檢查", {
  expect_error(kits_equations(structure(list(intercept = 0, slope = 0, n = 3, dropped = character()),
                                        class = "kitsPreview_link")), NA)
  expect_error(kits_equations(structure(list(), class = "kitsPreview_link")), "no `intercept`")
  expect_error(kits_equations(character(0)), "topic")
  expect_error(kits_equations("item", format = c("latex", "unicode")), NA)
  expect_error(kits_equations("item", format = 1), "format")
})

test_that("R4-C2、C3、C9、C10：file 檢查；kits 的 subj；非 ASCII 的 id；digits 先檢查", {
  expect_error(kits_code(item6, file = NA), "file")
  expect_error(kits_code(item6, file = TRUE), "file")
  expect_error(kits_code(item6, file = c("a", "b")), "file")
  expect_error(kits_code(item6, file = "/no/such/folder/x.R"), "folder")
  f <- tempfile(fileext = ".R")
  expect_invisible(kits_code(item6, file = f))
  expect_true(file.exists(f))
  expect_false(any(grepl("ability", kits_code(kits(X6), subj = FALSE))))
  link <- link_of_points(c(-2, -1, 0, 1, 2), c(-1, 0, 1, 2, 3.2), id = c("中", "b", "c", "d", "e"))
  code <- kits_code(link)
  expect_true(all(vapply(code, function(l) all(utf8ToInt(l) < 128), logical(1))))
  expect_equal(run_code(code)$id[1], "中")
  expect_error(capture.output(print(item6, digits = NA)), "digits")
  out <- tryCatch(capture.output(print(item6, digits = NA)), error = function(e) character(0))
  expect_equal(length(out), 0)
})
