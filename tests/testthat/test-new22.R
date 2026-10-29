# 0.22.0 的新寫法（和 kits 同名、同樣的主要引數；0.23.0 起用 visit_id、intercept、slope 等新名字）：分數表進常模、照組的常模、
# 從分數來的原始分換算表、連結的三步、kits_vertical()、kits_subj(persons = )

set.seed(5)
n <- 1200
age <- runif(n, 3, 36)
sex <- rep(c("boy", "girl"), n / 2)
D <- 20 + 15 * log(age) + ifelse(sex == "girl", 1, 0) + rnorm(n, 0, 3)
ages <- seq(4, 35, by = 1)
deg <- c(3, 2, 1, 1)
fit <- function(...) suppressWarnings(kits_norms(...))

test_that("kits_norms(by = )：每組的常模和只用這組的孩子配的相同（完全相同）；組的欄在前", {
  by_sex <- fit(D, age, ages, deg, by = data.frame(sex = sex))
  expect_s3_class(by_sex, "kitsPreview_norms")
  expect_identical(names(by_sex)[1:2], c("sex", "age_m"))
  expect_identical(attr(by_sex, "by"), "sex")
  for (g in c("boy", "girl")) {
    alone <- fit(D[sex == g], age[sex == g], ages, deg)
    mine <- as.data.frame(by_sex)[by_sex$sex == g, -1]
    expect_identical(lapply(mine, identity), lapply(alone, identity), info = g)
    expect_identical(attr(by_sex, "groups")[[match(g, c("boy", "girl"))]], alone)
  }
  expect_identical(attr(by_sex, "kits_args")$by, "sex")
  expect_error(fit(D, age, ages, deg, by = "sex"), "data.frame")
  expect_error(fit(D, age, ages, deg, by = data.frame(sex = sex[-1])), "one row per score")
  expect_error(fit(D, age, ages, deg, by = data.frame(sex = replace(sex, 1, NA))), "NA")
  expect_error(fit(D[1:200], age[1:200], ages, c(5, 4, 3, 2), by = data.frame(sex = sex[1:200])),
               "sex = boy")
})

test_that("kits_norm_z(by = )：每個孩子用自己那一組的曲線；沒有常模的組是 NA", {
  by_sex <- fit(D, age, ages, deg, by = data.frame(sex = sex))
  z <- kits_norm_z(D, age, by_sex, by = data.frame(sex = sex))
  boy <- attr(by_sex, "groups")[[1]]
  expect_identical(z[sex == "boy"], kits_norm_z(D[sex == "boy"], age[sex == "boy"], boy))
  expect_error(kits_norm_z(D, age, by_sex), "by group")
  expect_message(zz <- kits_norm_z(D[1:2], age[1:2], by_sex, by = data.frame(sex = c("boy", "x"))),
                 "No norm")
  expect_true(is.na(zz[2]))
})

test_that("分數表（kits_subj() 的結果）進常模：和向量的寫法相同", {
  X <- kits_sim(rnorm(n), list(a1 = c(-1, 0), a2 = 0.5, a3 = -0.5, a4 = 0), seed = 1)
  wave <- round(age / 6) * 6
  persons <- data.frame(visit_id = rownames(X), age_m = age, wave_m = wave, sex = sex,
                        cohort = "M03")
  s <- kits_subj(X, kits_item(X), persons = persons)
  s$theta <- D
  expect_s3_class(s, "kitsPreview_subj")
  screen <- kits_norm_screen(s)
  expect_identical(screen, kits_norm_screen(D, wave, cohort = rep("M03", n)))
  expect_identical(kits_norm_ages(s), kits_norm_ages(age, wave))
  norm <- fit(s, ages, deg)
  expect_identical(norm, fit(D, age, ages, deg))
  auto <- fit(s, degrees = deg)   # 沒給 ages：kits_norm_ages() 的規則，pad = 0
  expect_identical(attr(auto, "fitted")$set_by, "kits_norm_ages()")   # 印出來時說月齡是誰定的
  attr(auto, "fitted")$set_by <- NULL
  expect_identical(auto, fit(D, age, kits_norm_ages(age, wave), deg, pad = 0))
  out <- kits_norm_z(s, norm)
  expect_s3_class(out, "kitsPreview_subj")
  expect_identical(out$z, kits_norm_z(D, age, norm))
  expect_identical(out$pct, 100 * pnorm(out$z))
  by_sex <- fit(s, ages, deg, by = "sex")
  expect_identical(by_sex, fit(D, age, ages, deg, by = data.frame(sex = sex)))
  expect_identical(kits_norm_z(s, by_sex)$z, kits_norm_z(D, age, by_sex,
                                                         by = data.frame(sex = sex)))
  expect_error(fit(s, ages, deg, by = "subj_id"), "no column `subj_id`")
  expect_error(kits_norm_z(s), "Give the norm")
  expect_error(kits_norm_ages(s, wave), "from their column")
  expect_s3_class(plot(by_sex, scores = s), "plotly")
  expect_error(kits_code(by_sex), "one norm")
})

test_that("kits_subj(persons = )：只接 subj_id、wave_m、age_m、sex、cohort；照 visit_id 接上", {
  X <- kits_sim(seq(-2, 2, length.out = 50), list(a1 = c(-1, 0), a2 = 0.5, a3 = -0.5), seed = 2)
  item <- kits_item(X)
  persons <- data.frame(visit_id = rev(rownames(X)), wave_m = 50:1, note = "x")
  expect_message(s <- kits_subj(X, item, persons = persons), "not used: note")
  expect_identical(names(s)[10], "wave_m")
  expect_identical(s$wave_m, 1:50)
  plain <- kits_subj(X, item)
  expect_identical(unclass(s)[1:9], unclass(plain)[1:9])
  expect_error(kits_subj(X, item, persons = data.frame(visit_id = rownames(X), wave = 1)),
               "none of the columns")
  expect_error(kits_subj(X, item, persons = data.frame(visit_id = rownames(X), wave_m = "a")),
               "numbers")
  expect_error(kits_subj(X, item, persons = "x"), "data.frame")
})

test_that("kits_subj_table(scores)：filter() 之後的人數；疊起來的分數停下來", {
  X <- kits_sim(seq(-2, 2, length.out = 200), list(a1 = c(-1, 0), a2 = 0.5, a3 = -0.5), seed = 3)
  item <- kits_item(X)
  persons <- data.frame(visit_id = rownames(X), wave_m = rep(c(6, 12), each = 100))
  s <- kits_subj(X, item, persons = persons)
  six <- kits_subj_table(dplyr::filter(s, wave_m == 6))
  expect_equal(unique(six$wave_m), 6)
  expect_equal(sum(six$n), 100)
  full <- kits_subj_table(s)
  expect_identical(lapply(six, identity), lapply(full[full$wave_m == 6, ], identity))
  Y <- X
  rownames(Y) <- paste0("y", seq_len(nrow(Y)))
  other <- kits_subj(Y, item, persons = transform(persons, visit_id = rownames(Y)))
  expect_error(kits_subj_table(rbind(s, other)), "before stacking")
  bare <- s
  attr(bare, "raw_theta") <- NULL
  expect_error(kits_subj_table(bare), "score them again")
  gap <- s
  gap$wave_m[1] <- NA
  expect_error(kits_subj_table(gap), "NA")
})

test_that("連結的三步：每一步的結果；和 kits_linking() 相同的路", {
  points <- data.frame(id = paste0("p", 1:8), item = paste0("p", 1:8),
                       from = c(-2, -1, 0, 1, 2, 3, 0.5, 1.5), to = c(-1, 0, 1, 2, 3, 4, 1.5, 5.5))
  screened <- kits_link_screen(points, from_scale = "x", to_scale = "y")
  expect_s3_class(screened, "kitsPreview_link_screen")
  expect_identical(screened$dropped, c(rep(FALSE, 7), TRUE))
  link <- kits_link_line(screened)
  expect_true(is.na(link$intercept_error))
  expect_identical(c(link$from_scale, link$to_scale), c("x", "y"))
  expect_output(print(link), "scales: x -> y")
  with_error <- kits_link_error(link, n_boot = 50)
  expect_true(is.finite(with_error$intercept_error))
  expect_identical(with_error$settings$n_boot, 50)
  expect_identical(unclass(kits_link_error(with_error, n_boot = 0))[names(link)], unclass(link))
  expect_error(kits_link_line(points), "kits_link_screen")
  expect_error(kits_link_error(points), "a link")
  expect_error(kits_link_screen(points[c("id", "from", "to")]), "item")
  expect_error(kits_link_error(link, n_boot = 1), "at least 2")
  expect_error(kits_link_error(link, seed = "a"), "seed")
  # kits_linking() 也走這三步
  betas <- list(a1 = -2, a2 = -1, a3 = c(-0.5, 0.5), a4 = 0, a5 = c(0.5, 1.5), a6 = 2)
  young <- kits_item(kits_sim(rnorm(400, 0), betas[1:5], seed = 1))
  old <- kits_item(kits_sim(rnorm(400, 1), betas[2:6], prefix = "q", seed = 2))
  lk <- kits_linking(old, young, n_boot = 30)
  pts <- lk$points[c("id", "item", "from", "to")]
  steps <- pts |>
    kits_link_screen() |>
    kits_link_line() |>
    kits_link_error(n_boot = 30)
  keys <- c("intercept", "slope", "intercept_error", "slope_error", "intercept0", "slope0", "dropped")
  expect_identical(unclass(steps)[keys], unclass(lk)[keys])
})

test_that("kits_vertical()：每個世代 x 波次的位置和分散；疊幾個領域", {
  s <- data.frame(cohort = rep(c("M03", "M36"), each = 600), wave_m = rep(c(12, 24, 36), 400),
                  theta = D,
                  age_m = age)
  v <- kits_vertical(s)
  expect_s3_class(v, "kitsPreview_vertical")
  expect_identical(names(v), c("cohort", "wave_m", "n", "age_med", "mean", "sd", "median", "P25",
                               "P75"))
  one <- s$theta[s$cohort == "M03" & s$wave_m == 12]
  expect_identical(v$median[1], median(one))
  expect_identical(v$P75[1], unname(quantile(one, 0.75)))
  expect_identical(names(kits_vertical(s[c("wave_m", "theta")])), c("wave_m", "n", "mean", "sd",
                                                                    "median", "P25", "P75"))
  both <- purrr::list_rbind(list(lan = v, motor = v), names_to = "domain")
  expect_s3_class(both, "kitsPreview_vertical")
  p <- plotly::plotly_build(plot(both))
  expect_gt(length(p$x$data), 2)
  expect_output(print(v), "kits_vertical")
  expect_error(kits_vertical(s[c("theta")]), "wave_m")
})
