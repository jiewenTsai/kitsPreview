# 0.23.0 的改名（和 kits 0.23.0 一樣）：
# * 編號欄 person_id → visit_id、pid → subj_id（0.24.1 起舊名字是不認得的欄，停下來說要用 visit_id）
# * 作答組合：booklet_id → pattern_id、booklet_score → raw、booklet_max → raw_max
# * 連結：a、b → intercept、slope（0.24.1 起 link$a 是 NULL，沒有特別的檢查）；coef(link)
# * 拿掉 0.22.0 被取代的函數和寫法，以及 0.11.0 的舊引數名 dataSrc、D
# 數字沒有變：另外和 0.22.0 逐位比過（見 NEWS）。

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5))
X <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
wave <- rep(c(12, 24), each = 200)
sex <- rep(c("boy", "girl"), times = 200)
item <- kits_item(X)
set.seed(2)
age <- runif(600, 3, 36)
score <- 20 + 15 * log(age) + rnorm(600, 0, 3)
norm <- suppressWarnings(kits_norms(score, age, seq(4, 35, by = 1), c(3, 2, 1, 1)))
points <- data.frame(id = paste0("p", 1:8), item = paste0("p", 1:8),
                     from = c(-2, -1, 0, 1, 2, 3, 0.5, 1.5), to = c(-1, 0, 1, 2, 3, 4, 1.5, 5.5))
link <- points |>
  kits_link_screen() |>
  kits_link_line() |>
  kits_link_error(n_boot = 50)

test_that("拿掉 0.22.0 被取代的函數和 0.24.1 的 kits_rtmb_code()：匯出 38 → 28 個", {
  gone <- c("kits_age50", "kits_mean_sigma", "kits_wle_table", "kits_item_positions",
            "kits_ladder_marks", "kits_plot_growth", "kits_plot_ladder", "kits_plot_cohort",
            "kits_plot_norms", "kits_rtmb_code")
  exports <- getNamespaceExports("kitsPreview")
  expect_false(any(gone %in% exports))
  expect_length(exports, 28)
})

test_that("拿掉 0.11.0 的舊引數名 dataSrc、D：沒有這個引數，給了就停下來", {
  data_funs <- c("kits_item", "kits", "kits_subj", "kits_itemfit", "kits_dif", "kits_inter",
                 "kits_drift", "kits_subj_table")
  for (f in data_funs) {
    expect_false("dataSrc" %in% names(formals(getExportedValue("kitsPreview", f))), info = f)
  }
  for (f in c("kits_norms.default", "kits_norm_z.default", "kits_norm_screen",
              "plot.kitsPreview_norms")) {
    args <- names(formals(get(f, envir = asNamespace("kitsPreview"))))
    expect_false(any(c("D", "dataSrc") %in% args), info = f)
  }
  expect_identical(names(formals(kits_subj)), c("data", "item", "persons", "..."))
  expect_identical(names(formals(kits_subj_table)), c("x", "forms", "..."))
  expect_error(kits_subj(dataSrc = X, item = item), "unused argument", class = "kitsPreview_error_input")
  expect_error(kits_norm_z(D = score, age_m = age, norm = norm), "unused argument",
               class = "kitsPreview_error_input")
  expect_error(kits_norm_screen(D = score, wave_m = round(age / 6) * 6), "unused argument",
               class = "kitsPreview_error_input")
})

test_that("拿掉 0.22.0 被取代的寫法：說清楚新的寫法", {
  expect_error(kits_subj_table(X, item, wave), "kits_subj\\(data, item, persons\\)",
               class = "kitsPreview_error_input")
  expect_error(kits_subj_table(X), "removed", class = "kitsPreview_error_input")
  expect_error(plot(norm, d = score, age_m = age), "scores", class = "kitsPreview_error_input")
  expect_error(plot(norm, score, age), "scores", class = "kitsPreview_error_input")
  expect_s3_class(plot(norm, scores = data.frame(theta = score, age_m = age)), "plotly")
})

test_that("編號欄：visit_id（人次）、subj_id（孩子）；作答組合：pattern_id、raw、raw_max", {
  persons <- data.frame(visit_id = rownames(X), subj_id = paste0("c", 1:400), wave_m = wave)
  s <- kits_subj(X, item, persons = persons)
  expect_identical(names(s), c("pattern_id", "visit_id", "raw", "raw_max", "theta", "se", "infit",
                               "outfit", "flag", "subj_id", "wave_m"))
  expect_false(any(c("person_id", "pid", "booklet_id", "booklet_score", "booklet_max") %in% names(s)))
  # dexter 自己的輸出（original）照舊是 dexter 的名字
  expect_true(all(c("booklet_id", "person_id", "booklet_score") %in% names(kits_original(s))))
  expect_identical(s$raw, kits_original(s)$booklet_score[match(s$visit_id,
                                                               kits_original(s)$person_id)])
  expect_identical(names(item$design), c("pattern_id", "item_id"))
  expect_output(print(item), "item pattern\\(s\\)")
  expect_identical(names(kits_subj_table(item)), c("pattern_id", "raw", "theta", "se"))
  tab <- kits_subj_table(s)
  expect_identical(sum(tab$n), 400L)
})

test_that("舊的欄名 person_id、pid：0.24.1 起不認得，停下來說要用 visit_id", {
  long <- data.frame(visit_id = rownames(X)[row(X)], item_id = colnames(X)[col(X)],
                     item_score = c(X))
  long_old <- dplyr::rename(long, person_id = visit_id)
  msg <- "without the column\\(s\\) visit_id \\(it has: person_id, item_id, item_score\\)"
  expect_error(kits_subj(long_old, item), msg)
  expect_error(kits_itemfit(long_old, item), msg)
  old <- data.frame(person_id = rownames(X), pid = paste0("c", 1:400), wave_m = wave)
  expect_error(kits_subj(X, item, persons = old),
               "`persons` has no column visit_id \\(it has: person_id, pid, wave_m\\)")
  pid <- data.frame(visit_id = rownames(X), pid = paste0("c", 1:400), wave_m = wave)
  expect_message(s <- kits_subj(X, item, persons = pid), "not used: pid")
  expect_false("subj_id" %in% names(s))
  expect_false(exists("old_ids", envir = asNamespace("kitsPreview"), inherits = FALSE))
})

test_that("連結：intercept、slope；沒有元素以 a 或 b 開頭；coef() 和 lm() 的順序相同", {
  expect_false(any(grepl("^[ab]", names(link))))
  expect_identical(coef(link), c(intercept = link$intercept, slope = link$slope))
  expect_true(all(c("intercept_error", "slope_error", "intercept0", "slope0") %in% names(link)))
  expect_identical(names(coef(link)), c("intercept", "slope"))
  out <- capture.output(print(link))
  expect_identical(out[1], "<kits_link> new value = intercept + slope x old value")
  expect_match(out[2], "^  intercept = [0-9.]+  \\(linking error [0-9.]+\\)$")
  expect_match(out[3], "^  slope     = [0-9.]+  \\(linking error [0-9.]+\\)$")
  expect_match(out[5], "^  before screening: intercept .*, slope .*; screening: \\|e\\| > 2 SD\\(e\\)$")
})

test_that("舊程式的 link$a、link$b：0.24.1 起沒有特別的檢查，是 NULL（沒有元素以 a、b 開頭）", {
  for (old in c("a", "b", "a_error", "b_error", "a0", "b0")) {
    expect_null(link[[old]], info = old)
  }
  expect_null(link$a)
  expect_null(link$b0)
  expect_false(any(c("$.kitsPreview_link", "[[.kitsPreview_link") %in%
                     ls(asNamespace("kitsPreview"), all.names = TRUE)))
  expect_identical(link$slope, unclass(link)$slope)
  expect_identical(link[["points"]], unclass(link)$points)
})

test_that("其他有 a、b 的結果：kits_drift()、domains 表、kits_link_screen() 的紀錄", {
  drift <- kits_drift(X, item)
  expect_true(all(c("intercept", "slope") %in% names(drift)))
  expect_false(any(grepl("^[ab]", names(drift))))
  set.seed(1)
  steps <- data.frame(item_id = paste0("a", 1:12), item_score = 1, thr = rnorm(12),
                      domain = rep(c("lan", "cog"), each = 6))
  anchors <- data.frame(item_id = steps$item_id, tau = 64 + 7 * steps$thr + rnorm(12, 0, 0.5))
  ld <- kits_linking_d(steps, anchors, n_boot = 0)
  expect_identical(names(ld$domains), c("domain", "n", "intercept", "slope"))
  rec <- attr(kits_link_screen(points), "link")
  expect_identical(names(rec), c("cut", "intercept0", "slope0"))
})

test_that("kits_code(link)、kits_equations(link)：intercept、slope；字母是 Kolen & Brennan 的 A、B", {
  code <- paste(kits_code(link), collapse = "\n")
  expect_match(code, "c(intercept = mean(to) - slope * mean(from), slope = slope)", fixed = TRUE)
  expect_false(grepl("\\b(a|b|a0|b0|a_error|b_error) <-|\"a\"|\"b\"", code))
  eq <- as.character(kits_equations(link))
  expect_true(any(grepl("A = SD(to) / SD(from),  B = mean(to)", eq, fixed = TRUE)))
})

test_that("部分比對：dataSrc 的開頭不是任何引數；data 的開頭只接到 data", {
  expect_error(kits_subj(X, item, dataS = X), "unused argument", class = "kitsPreview_error_input")
  expect_identical(kits_subj(dat = X, item = item), kits_subj(X, item))
})

test_that("多給的引數停下來", {
  expect_error(kits_subj(X, item, NULL, 3), "unused argument\\(s\\): 1 unnamed",
               class = "kitsPreview_error_input")
  expect_error(kits_itemfit(X, item, foo = 1), "kits_itemfit\\(\\): unused argument\\(s\\): `foo`")
  expect_error(kits_norms(score, age, 4:35, c(3, 2, 1, 1), sqrt, NORM_PCT, 1, 1, 3, NULL, 99),
               class = "kitsPreview_error_input")
  expect_error(kits_norm_screen(score, round(age / 6) * 6, NULL, 20, 1), class = "kitsPreview_error_input")
  expect_error(plot(norm, foo = 1), "unused argument", class = "kitsPreview_error_input")
})

test_that("常模函數收 kits_subj() 的分數；其他的表清楚地停下來", {
  s <- kits_subj(X, item)   # 沒有 persons：沒有 age_m、wave_m
  expect_error(kits_norms(s, seq(4, 35, by = 1)), "no `age_m` column",
               class = "kitsPreview_error_input")
  expect_error(kits_norm_z(s, norm), "no `age_m` column", class = "kitsPreview_error_input")
  expect_error(kits_norm_screen(s), "no `wave_m` column", class = "kitsPreview_error_input")
  expect_error(kits_norms(s, age_m = age), "age_m column", class = "kitsPreview_error_input")
  other <- data.frame(theta = score, age_m = age)   # 不是 kits_subj() 的表
  expect_error(kits_norms(other, seq(4, 35, by = 1)), "s\\$theta, s\\$age_m",
               class = "kitsPreview_error_input")
  expect_error(kits_norm_z(other, norm), "vector of scores", class = "kitsPreview_error_input")
  expect_error(kits_norm_screen(other, wave), "vector of scores", class = "kitsPreview_error_input")
})

test_that("錯誤訊息用新的名稱", {
  expect_error(kits_subj(1:3, item), "`data` is a vector")
  expect_error(kits_subj(unname(X), item), "row names \\(visit_id\\)")
  expect_error(kits_subj(data.frame(visit_id = "p1", item_id = "a1"), item), "visit_id, item_id, item_score")
  expect_error(kits_norm_z(score[1:3], age[1:2], norm), "`d` and `age_m` must have the same length")
  expect_error(kits_inter(X, wave[-1]), "`wave_m` has 399 value")
  expect_error(kits_subj(X, item, persons = data.frame(id = rownames(X), wave_m = 1)), "visit_id")
})
