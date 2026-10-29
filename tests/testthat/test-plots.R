set.seed(1)   # kits_sim() 不給 seed 時用目前的亂數狀態

test_that("圖的英文字：代碼有名稱；沒有對照的代碼照印", {
  expect_equal(names(LAB$domain), c("lan", "motor", "cog", "social", "emotion"))
  expect_equal(lab_name("domain", c("lan", "reading")), c("Language", "reading"))
})

test_that("short_stem 去掉括號並截短", {
  expect_equal(short_stem("Stacks two blocks (for example, wooden)"), "Stacks two blocks")
  long <- short_stem("Child can say the names of at least three family members")
  expect_lte(nchar(long), 37)
  expect_true(endsWith(long, "…"))
})

# 一個領域的常模（by = "domain"）和題目表
lan_norm <- function(P50 = c(40, 52, 60, 66), age_m = c(6, 12, 18, 24)) {
  norm <- data.frame(domain = "lan", age_m = age_m, P10 = P50 - 5, P50 = P50, P90 = P50 + 5)
  class(norm) <- c("kitsPreview_norms", "kitsPreview_table", "data.frame")
  attr(norm, "by") <- "domain"
  norm
}

test_that("kits_item_table：沒有 50% 通過的月齡時用常模中位數到達的月齡", {
  thr <- data.frame(item_id = c("a1", "a2", "a3"), item_score = 1, thr = c(45, 61, 30))
  ap <- data.frame(item_id = c("a1", "a2", "a3"), p50 = c(8.5, NA, NA))
  out <- kits_item_table(thr, lan_norm(), age_pass = ap)
  expect_s3_class(out, "kitsPreview_item_table")
  expect_equal(out$x, c(8.5, 24, 6))
  expect_equal(out$empirical, c(TRUE, FALSE, FALSE))
  expect_equal(out$below_range, c(FALSE, FALSE, TRUE))
  expect_equal(out$above_range, c(FALSE, FALSE, FALSE))
  expect_equal(out$domain, rep("lan", 3))
  expect_equal(out$DIF, rep("none", 3))
  expect_equal(out$stem, out$item_id)
})

test_that("比常模最大月齡還難、又沒有 50% 通過月齡的題：above_range、x 是 NA，圖會列出題號", {
  thr <- data.frame(item_id = c("a1", "hard"), item_score = 1, thr = c(45, 80))
  ap <- data.frame(item_id = c("a1", "hard"), p50 = c(8.5, NA))
  out <- kits_item_table(thr, lan_norm(), age_pass = ap, stems = c(a1 = "x", hard = "y"))
  expect_equal(out$above_range, c(FALSE, TRUE))
  expect_true(is.na(out$x[2]))
  expect_message(p <- plot(out), "hard")
  notes <- vapply(plotly::plotly_build(p)$x$layout$annotations, function(a) a$text, character(1))
  expect_true(any(grepl("hard", notes)))   # 格子右下角有註記
  expect_error(kits_item_table(transform(thr, domain = "cog"), lan_norm()), "cog")
})

test_that("kits_item_table：階梯的欄、DIF（kits_dif 的數字或 kits_dif_combine 的方向）、題號選圖", {
  thr <- data.frame(item_id = c("a1", "a2"), item_score = 1, thr = c(40, 50), column = "motor")
  dif <- data.frame(item_id = c("a1", "a2"), dif = c(0.5, -0.1))
  tab <- kits_item_table(thr, lan_norm(), dif = dif)
  expect_equal(tab$DIF, c("favors girls", "none"))
  p <- plot(tab, type = "ladder", ages = c(6, 12))
  expect_equal(as.character(plotly::plotly_build(p)$x$layout$xaxis$ticktext),
               "motor")   # 沒有名稱的欄代碼照印
  expect_s3_class(plot(tab, items = "a1"), "plotly")
  expect_error(plot(tab, items = "zz"), "zz")
  twice <- list(thr, transform(thr, thr = thr + 1))
  expect_message(both <- kits_item_table(twice, lan_norm()), "first row")
  expect_equal(both$D, c(40, 50))
})

test_that("kits_item_table：常模有別的組、門檻沒有領域時說清楚", {
  norm <- lan_norm()
  attr(norm, "by") <- c("domain", "sex")
  thr <- data.frame(item_id = "a1", item_score = 1, thr = 40)
  expect_error(kits_item_table(thr, norm), "by domain only")
  two <- rbind(lan_norm(), transform(lan_norm(), domain = "cog"))
  expect_error(kits_item_table(thr, two), "domain")
  expect_error(kits_item_table(thr, "x"), "kits_norms")
})

test_that("reach_age 不受常模列的順序影響", {
  norm <- data.frame(age_m = c(24, 6, 12, 18), P50 = c(66, 40, 52, 60))
  expect_equal(reach_age(45, norm), 12)
})

test_that("階梯的月齡刻度只保留每個領域都有的月齡", {
  norm <- data.frame(domain = c("lan", "lan", "cog"), age_m = c(3, 12, 12), P50 = c(20, 50, 54))
  expect_equal(ladder_marks(norm, c("lan", "cog"), ages = c(3, 12)), data.frame(age_m = 12, D = 52))
})

test_that("圖的輸入有問題時說清楚", {
  norm <- data.frame(domain = "lan", age_m = 3:6, P10 = 1, P50 = 2, P90 = 3)
  items <- data.frame(item_id = "a1", domain = "lan", x = 4, D = 2, empirical = TRUE, DIF = "girls",
                      stem = "x", age50 = 4, age_reach = 4)
  expect_error(growth_plot(norm, items, "lan"), "DIF")
  expect_error(growth_plot(norm, transform(items, DIF = "none"), "language"), "language")
})

test_that("kits_save_plot 的檔名要以 .html 結尾", {
  expect_error(kits_save_plot(plotly::plot_ly(), "fig.htm"), ".html")
})

test_that("新的圖都能建出 plotly 物件：題目表（兩種）、垂直量尺（兩種）、照組的常模", {
  set.seed(3)
  age <- runif(1200, 3, 24)
  sex <- rep(c("boy", "girl"), 600)
  D <- 20 + 15 * log(age) + rnorm(1200, 0, 3)
  norm <- suppressWarnings(kits_norms(D, age, ages = 3:24, degrees = c(3, 2, 1, 1),
                                      by = data.frame(domain = "lan", sex = sex)))
  thr <- data.frame(item_id = c("a1", "a2"), item_score = 1, thr = c(40, 60), domain = "lan")
  tab <- kits_item_table(thr, kits_norms(D, age, ages = 3:24, degrees = c(3, 2, 1, 1),
                                         by = data.frame(domain = rep("lan", 1200))))
  s <- data.frame(cohort = rep(c("M03", "M36"), each = 600), wave_m = rep(c(12, 24), 600),
                  theta = D, age_m = age)
  plots <- list(
    plot(tab),
    plot(tab, type = "ladder", ages = c(6, 12)),
    plot(kits_vertical(s)),
    plot(kits_vertical(s), type = "median", cohorts = "M36"),
    plot(norm),
    plot(norm, scores = data.frame(theta = D, age_m = age, domain = "lan", sex = sex))
  )
  for (p in plots) {
    expect_s3_class(p, "plotly")
    expect_gt(length(plotly::plotly_build(p)$x$data), 0)
  }
  expect_error(plot(norm, scores = data.frame(theta = D, age_m = age)), "sex")
})

test_that("垂直量尺的圖：實際月齡和波次名稱差超過 1 個月時，標題下註明", {
  s <- data.frame(cohort = "M36", wave_m = rep(c(72, 84), each = 3), age_m = rep(c(72.0, 86.5),
                                                                                 each = 3),
                  theta = c(90, 94, 98, 95, 98, 102))
  title <- plotly::plotly_build(plot(kits_vertical(s), type = "median"))$x$layout$title
  if (is.list(title)) title <- title$text
  expect_match(title, "84-month wave 86.5")
  expect_no_match(title, "72-month wave")
  expect_error(plot(kits_vertical(s), cohorts = "M03"), "None of")
})
