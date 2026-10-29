# 0.23.0 的整理：印出來的樣子、作答組合的編號、原本套件的物件、疊表、連結的預設、符號、
# immer 的輸出不印電腦的名字、常模程式的警告、程式裡的長向量

run_code <- function(code, ...) {
  env <- new.env(parent = globalenv())
  for (n in names(list(...))) assign(n, list(...)[[n]], envir = env)
  utils::capture.output(eval(parse(text = code), envir = env))
  env
}

set.seed(11)
betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5), a6 = 1.5)
X <- kits_sim(rnorm(600, 0, 1.5), betas, seed = 2)
X[sample(length(X), round(length(X) * 0.3))] <- NA
X <- X[rowSums(!is.na(X)) > 1, ]
X <- X[sample(nrow(X)), ]
rownames(X)[1:5] <- c("Zed", "abc", "B2", "b1", "_x")   # 大小寫和符號：照 C locale 排
wave <- rep(c(6, 12, 24), length.out = nrow(X))
persons <- data.frame(visit_id = rownames(X), wave_m = wave, age_m = wave + runif(nrow(X), -1, 1),
                      sex = rep(c("boy", "girl"), length.out = nrow(X)))
item <- kits_item(X)
s <- kits_subj(X, item, persons = persons)
s_d <- s
s_d$theta <- 50 + 10 * s_d$theta
norm_sex <- suppressWarnings(kits_norms(s_d, degrees = c(3, 2, 1, 1), by = "sex"))

test_that("分數印出來：標題、主要的欄、前幾列、其他的欄、Next；n =", {
  out <- capture.output(print(s))
  expect_identical(out[1], sprintf("<kits_subj> %d person-waves; WLE (dexter)", nrow(s)))
  expect_match(out[2], "visit_id +wave_m +age_m +theta +se +flag")
  expect_false(any(grepl("infit", out[2])))
  expect_length(grep("^[0-9]+ ", out), 10)
  expect_true(any(grepl("^\\.\\.\\. [0-9]+ more rows \\(more: print\\(x, n = 50\\)", out)))
  expect_true(any(out == "Other columns: pattern_id, raw, raw_max, infit, outfit, sex"))
  expect_true(any(grepl("^Next: kits_norms\\(x\\)", out)))
  out3 <- capture.output(print(s, n = 3))
  expect_length(grep("^[0-9]+ ", out3), 3)
  expect_true(any(grepl(sprintf("^\\.\\.\\. %d more rows", nrow(s) - 3), out3)))
  expect_error(print(s, n = -1), "`n` must be")
  # 選過欄：照使用者的欄印，不加說明的行（和 kits 相同）
  out_sel <- capture.output(print(s[c("visit_id", "theta")], n = 2))
  expect_match(out_sel[2], "visit_id +theta")
  expect_false(any(grepl("^Next", out_sel)))
})

test_that("常模印出來：print(norm, n = 3) 可以用；標題說月齡和組", {
  out <- capture.output(print(norm_sex, n = 3))
  expect_identical(out[1], "<kits_norms> cNORM shash")
  expect_match(out[2], "norm fitted on .* months \\(set by kits_norm_ages\\(\\)\\); one norm per sex")
  expect_true(any(grepl(sprintf("^\\.\\.\\. %d more rows", nrow(norm_sex) - 3), out)))
  expect_true(any(grepl("^Next: kits_norm_z", out)))
  expect_true(any(grepl("^1 +boy", out)))
  # 其他的表也收 n
  expect_output(print(coef(item), n = 2), "more rows")
  expect_output(print(kits_vertical(s_d), n = 1), "more rows")
})

test_that("作答組合的編號：kits_item()$design 和 kits_subj()$pattern_id 用同一套（bk01、bk02、...，和 kits 相同）", {
  bk <- function(n) sprintf(paste0("bk%0", ceiling(log10(n + 1)), "d"), seq_len(n))   # dexter 的名字
  ids <- unique(item$design$pattern_id)
  expect_identical(ids, bk(length(ids)))
  expect_true(all(s$pattern_id %in% ids))
  # 和 kits 相同的規則：人次照 visit_id 排（C locale），組合照出現的順序編號
  sets <- apply(!is.na(X), 1, function(a) paste(sort(colnames(X)[a]), collapse = ","))
  sets <- sets[sort(names(sets), method = "radix")]
  expect_identical(ids, bk(length(unique(sets))))
  design_sets <- c(tapply(item$design$item_id, item$design$pattern_id, function(i) paste(sort(i), collapse = ",")))
  expect_identical(unname(design_sets[ids]), unique(unname(sets)))
  # 每個人次的作答組合，在兩個表裡是同一組題
  expect_identical(unname(design_sets[s$pattern_id]), unname(sets[as.character(s$visit_id)]))
  # 原始分換算表照同樣的編號
  expect_identical(unique(kits_subj_table(item)$pattern_id), ids)
})

test_that("kits_original()：常模給 cNORM 的物件（照組時有名字的 list）；連結和常模的訊息一致", {
  set.seed(1)
  age <- runif(600, 3, 36)
  d <- 20 + 15 * log(age) + rnorm(600, 0, 3)
  norm <- kits_norms(d, age, ages = seq(3, 36, by = 3), degrees = c(3, 2, 1, 1))
  expect_s3_class(kits_original(norm), "cnormShash")
  expect_identical(kits_original(norm)$mu_est, attr(norm, "original")$mu_est)
  orig <- kits_original(norm_sex)
  expect_named(orig, c("boy", "girl"))
  expect_s3_class(orig$girl, "cnormShash")
  expect_null(orig$boy$data)   # 沒有孩子的分數
  hand <- norm
  attr(hand, "original") <- NULL   # 例如自己做的表
  expect_error(kits_original(hand), "has no original object")
  link <- kits_link_line(kits_link_screen(data.frame(id = 1:4, item = 1:4, from = 1:4, to = c(2, 4, 5, 9))))
  expect_error(kits_original(link), "has no original object")
  expect_error(kits_original(coef(item)), "kits_norms\\(\\)")
})

test_that("照組的常模：attr(, \"groups\") 照組命名；kits_code() 的訊息用名字", {
  expect_named(attr(norm_sex, "groups"), c("boy", "girl"))
  expect_identical(attr(norm_sex, "groups")$girl$P50, norm_sex$P50[norm_sex$sex == "girl"])
  expect_error(kits_code(norm_sex), "groups: boy, girl", fixed = TRUE)
  expect_error(kits_code(norm_sex), "kits_code(attr(norm, \"groups\")[[\"boy\"]])", fixed = TRUE)
  expect_s3_class(kits_code(attr(norm_sex, "groups")[["boy"]]), "kitsPreview_code")
  s2 <- s_d
  s2$domain <- "lan"
  two <- suppressWarnings(kits_norms(s2, degrees = c(3, 2, 1, 1), by = c("domain", "sex")))
  expect_named(attr(two, "groups"), c("lan.boy", "lan.girl"))
  expect_identical(two$sex, norm_sex$sex)
})

test_that("list_rbind()、bind_rows() 疊分數：類別留著，常模的函數照收（和 kits 一樣用 vctrs 的方法）", {
  a <- kits_subj(X[1:300, ], item, persons = persons)
  b <- kits_subj(X[301:nrow(X), ], item, persons = persons)
  for (both in list(purrr::list_rbind(list(a, b)), dplyr::bind_rows(a, b))) {
    expect_s3_class(both, "kitsPreview_subj")
    expect_identical(nrow(both), nrow(X))
    expect_identical(both$theta, s$theta)
    both$theta <- 50 + 10 * both$theta
    expect_identical(kits_norm_ages(both), kits_norm_ages(s_d))
    z <- kits_norm_z(both, norm_sex)
    expect_s3_class(z, "kitsPreview_subj")
    expect_identical(z$z, kits_norm_z(s_d, norm_sex)$z)
  }
  # 同一個 kits_subj() 分開再疊回來：紀錄相同，換算表照常
  back <- purrr::list_rbind(list(s[1:100, ], s[101:nrow(s), ]))
  expect_identical(kits_subj_table(back), kits_subj_table(s))
  expect_identical(kits_original(back), kits_original(s))
  # 兩次 kits_subj()：換算表停下來說清楚；dexter 的表不一樣，所以不留
  both <- purrr::list_rbind(list(a, b))
  expect_error(kits_subj_table(both), "make the table of each calibration before stacking")
  expect_null(kits_original(both))
  # 名字的欄（domain）
  stacked <- purrr::list_rbind(list(lan = a, cog = b), names_to = "domain")
  expect_s3_class(stacked, "kitsPreview_subj")
  # 和一般的表：kitsPreview 的表；不同種的表：一般的 data.frame
  expect_s3_class(purrr::list_rbind(list(a, as.data.frame(b))), "kitsPreview_subj")
  expect_s3_class(purrr::list_rbind(list(as.data.frame(a), b)), "kitsPreview_subj")
  expect_s3_class(dplyr::bind_rows(a, as.data.frame(b)), "kitsPreview_subj")
  mixed <- purrr::list_rbind(list(coef(item), a))
  expect_identical(class(mixed), "data.frame")
  v <- kits_vertical(s_d)
  expect_s3_class(purrr::list_rbind(list(lan = v, cog = v), names_to = "domain"), "kitsPreview_vertical")
})

test_that("連結印出來：和 kits 一樣的行；n_boot 預設是 0（和 kits 相同）", {
  expect_identical(formals(kits_linking)$n_boot, 0)
  expect_identical(formals(kits_linking_d)$n_boot, 0)
  young <- kits_sim(rnorm(500, 0), betas[1:5], seed = 6)
  old <- kits_sim(rnorm(500, 2), betas[2:6], seed = 7)
  item_y <- kits_item(young)
  item_o <- kits_item(old)
  link <- kits_linking(item_o, item_y)
  expect_true(is.na(link$intercept_error))
  expect_identical(link$settings$n_boot, 0)
  out <- capture.output(print(link))
  expect_identical(out[1], "<kits_link> new value = intercept + slope x old value")
  expect_match(out[2], "^  intercept = [0-9.-]+$")
  expect_match(out[3], "^  slope     = [0-9.-]+$")
  expect_identical(out[4], "  points from: every common item step")
  expect_match(out[5], "^  points: 6 \\(items: 4\\); dropped: 0$")
  expect_match(out[6], "^  before screening: intercept [0-9.-]+, slope [0-9.-]+; screening: \\|e\\| > 2 SD\\(e\\)$")
  expect_match(out[7], "^Next: ")
  expect_length(out, 7)
  with_error <- kits_linking(item_o, item_y, n_boot = 200)
  expect_identical(unclass(kits_link_error(link, n_boot = 200))[names(with_error)], unclass(with_error))
  out2 <- capture.output(print(with_error))
  expect_match(out2[2], "^  intercept = [0-9.-]+  \\(linking error [0-9.]+\\)$")
  # 接到 D：錨題的及格步驟；幾個領域
  set.seed(1)
  steps <- data.frame(item_id = paste0("a", 1:12), item_score = 1, thr = rnorm(12),
                      domain = rep(c("lan", "cog"), each = 6))
  anchors <- data.frame(item_id = steps$item_id, tau = 64 + 7 * steps$thr + rnorm(12, 0, 0.5))
  out_d <- capture.output(print(kits_linking_d(steps, anchors)))
  expect_true("  points from: the pass step of each anchor item (score >= 1)" %in% out_d)
  expect_true(any(grepl("^  kept points by domain: (lan|cog) [0-9]+ \\([0-9]+%\\)", out_d)))
  expect_true(any(grepl("^  domain check: one slope shared by the domains", out_d)))
  anchors$step <- c(2, rep(NA, 11))
  steps2 <- rbind(steps, data.frame(item_id = "a1", item_score = 2, thr = 1, domain = "cog"))
  out_own <- capture.output(print(kits_linking_d(steps2, anchors)))
  expect_true(any(grepl("some anchors set their own", out_own)))
})

test_that("符號：門檻是 thr（kits 也是），tau 只是 GSED 的難度", {
  eq <- as.character(kits_equations("item"))
  expect_true(any(grepl("P(X_i ≥ k | θ = thr_ik) = 0.5", eq, fixed = TRUE)))
  expect_true(any(grepl("SE(thr_ik)", eq, fixed = TRUE)))
  expect_false(any(grepl("τ", eq)))
  link_eq <- as.character(kits_equations("link"))
  tau_lines <- grep("τ", link_eq, value = TRUE)
  expect_length(tau_lines, 1)
  expect_match(tau_lines, "GSED difficulty")
  expect_false(any(grepl("\\\\tau_\\{i", as.character(kits_equations("item", format = "latex")))))
  code <- as.character(kits_code(item))
  expect_false(any(grepl("\\btau\\b", code[!grepl("^#", code)])))   # 變數是 thr、thr_se
  expect_true(any(grepl("^thr <- numeric", code)))
})

test_that("immer 的原本輸出不印電腦的名字和帳號", {
  skip_if_not_installed("immer")
  Xc <- kits_sim(rnorm(300), betas[1:4], seed = 9)
  for (engine in c("immer", "ccml")) {
    fit <- kits_item(Xc, engine = engine)
    out <- c(capture.output(print(summary(fit))), capture.output(print(fit, original = TRUE)))
    expect_true(any(grepl("Original output: immer", out)))
    expect_false(any(grepl("nodename=|login=", out)))
    for (who in unique(Sys.info()[c("nodename", "login", "user")])) {
      if (is.na(who) || !nzchar(who) || who == "unknown") next
      word <- paste0("(^|[^A-Za-z0-9_])", gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", who), "($|[^A-Za-z0-9_])")
      expect_false(any(grepl(word, out)), label = paste("output contains", who))
    }
    expect_true(any(grepl("^Call:", out)))   # 其他照原樣印
  }
})

test_that("常模的程式：只發出留下的解的警告，和 kits_norms() 相同（數字也相同）", {
  set.seed(2)
  wave_n <- sample(c(3, 6, 9, 12, 18, 24, 30, 36), 800, TRUE)
  age <- wave_n + runif(800, -0.5, 0.5)
  D <- 20 + 15 * log(age) + rnorm(800, 0, 3)
  warnings_of <- function(expr) {
    w <- character()
    value <- withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
    list(value = value, warnings = w)
  }
  fitted <- warnings_of(kits_norms(D, age, ages = seq(3, 36, by = 1)))
  norm <- fitted$value
  ran <- warnings_of(run_code(kits_code(norm), D = D, age_m = age))
  env <- ran$value
  expect_identical(ran$warnings, fitted$warnings)   # 一樣的警告（這裡只有 delta 的）
  expect_false(any(grepl("Hessian", ran$warnings)))
  warned <- vapply(env$fits, function(f) length(f$warns) > 0, logical(1))
  expect_true(any(warned))   # 沒有留下的起始值警告過（例如 Hessian），但不發出
  expect_false(warned[which.min(vapply(env$fits, function(f) f$fit$result$value, numeric(1)))])
  expect_identical(env$norm$mu, norm$mu)
  expect_identical(env$norm$P50, norm$P50)
})

test_that("程式裡的長向量：每行最多 100 個字，數字完全相同", {
  set.seed(3)
  from <- rnorm(60)
  to <- 64 + 7 * from + rnorm(60, 0, 0.3)
  pts <- data.frame(id = paste0("item", 1:60, ":1"), item = paste0("item", 1:60), from = from, to = to)
  link <- pts |>
    kits_link_screen() |>
    kits_link_line() |>
    kits_link_error(n_boot = 50)
  code <- as.character(kits_code(link))
  expect_true(all(nchar(code) <= 100))
  env <- run_code(code)
  expect_identical(env$from, from)
  expect_identical(env$to, to)
  expect_identical(env$id, pts$id)
  expect_identical(env$line, coef(link))
  expect_identical(sd(env$draws[env$ok, "intercept"]), link$intercept_error)
  # 固定參數的表也換行
  fixed <- data.frame(item_id = rep(paste0("a_long_item_name_", 1:9), each = 1), item_score = 1,
                      beta = rnorm(9) / 3)
  Y <- kits_sim(rnorm(400), as.list(stats::setNames(fixed$beta, fixed$item_id)), seed = 4)
  item_f <- kits_item(Y, fixed_params = fixed[1:6, ])
  code_f <- as.character(kits_code(item_f))
  fixed_lines <- code_f[seq(grep("^fixed <- ", code_f), grep("^left_out <- ", code_f) - 1)]
  expect_true(all(nchar(fixed_lines) <= 100))
  env_f <- run_code(code_f, X = Y)
  expect_identical(env_f$fixed$beta, coef(item_f)$beta[coef(item_f)$beta_se == 0])
  expect_equal(env_f$thresholds$thr, coef(item_f)$thr, tolerance = 1e-10)
})
