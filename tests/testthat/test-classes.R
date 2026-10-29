# 類別名稱：kitsPreview 的每個結果都有自己的類別（kitsPreview_*），不和 kits 共用。
# 所以兩個套件在同一個 R 裡載入時，kits 的方法不會被換掉。

kits_classes <- c("kits", "kits_item", "kits_fit", "kits_subj", "kits_link", "kits_table", "kits_norms",
                  "kits_norm_screen", "kits_subj_table", "kits_inter", "kits_equations", "kits_code",
                  "kits_drift", "kits_dif", "kits_itemfit", "kits_summary")

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = c(-0.5, 0.5))
X <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
wave <- rep(c(12, 24), each = 200)
item <- kits_item(X)
set.seed(2)
age <- runif(600, 3, 36)
D <- 20 + 15 * log(age) + rnorm(600, 0, 3)

results <- list(
  item = item,
  fit = kits(X),
  subj = kits_subj(X, item),
  link = link_of_points(c(-2, -1, 0, 1, 2), c(-1, 0, 1, 2, 3.2), id = paste0("p", 1:5)),
  table = coef(item),
  norms = suppressWarnings(kits_norms(D, age, ages = seq(4, 35, by = 1), degrees = c(3, 2, 1, 1))),
  norm_screen = kits_norm_screen(D, round(age / 6) * 6),
  subj_table = table_of(X, item, wave_m = wave),
  inter = suppressMessages(kits_inter(X, wave_m = wave)),
  equations = kits_equations("item"),
  code = kits_code(item)
)

test_that("每個結果的第一個類別是 kitsPreview_*", {
  want <- c(item = "kitsPreview_item", fit = "kitsPreview_fit", subj = "kitsPreview_subj", link = "kitsPreview_link",
            table = "kitsPreview_table", norms = "kitsPreview_norms", norm_screen = "kitsPreview_norm_screen",
            subj_table = "kitsPreview_subj_table", inter = "kitsPreview_inter", equations = "kitsPreview_equations",
            code = "kitsPreview_code")
  for (n in names(want)) expect_identical(class(results[[n]])[1], want[[n]], info = n)
})

test_that("沒有任何結果繼承 kits 的類別（也不是 kits_ 開頭的類別）", {
  all_classes <- unique(unlist(lapply(results, class)))
  expect_false(any(all_classes %in% kits_classes))
  expect_false(any(startsWith(all_classes, "kits_") | all_classes == "kits"))
  # 表裡的表也一樣
  expect_false(inherits(item$thresholds, kits_classes))
  expect_false(inherits(results$link$points, kits_classes))
  expect_false(inherits(results$norm_screen$waves, kits_classes))
  expect_false(inherits(kits_drift(X, item)$items, kits_classes))
  # 錯誤的類別也不是 kits_error_*
  err <- tryCatch(need_package("notAPackage123", "x"), error = identity)
  expect_false(any(startsWith(class(err), "kits_")))
})

test_that("方法只登記在 kitsPreview 的類別上", {
  registered <- getNamespaceInfo("kitsPreview", "S3methods")
  cls <- setdiff(registered[, 2], c("default", "character"))
  expect_true(all(startsWith(cls, "kitsPreview_")), info = paste(cls, collapse = ", "))
})

test_that("載入 kitsPreview 之後，kits 的方法還是來自 kits", {
  skip_if_not_installed("kits")
  loadNamespace("kits")
  generics <- list(c("coef", "kits_item"), c("print", "kits_item"), c("summary", "kits_item"),
                   c("print", "kits_link"), c("plot", "kits_link"), c("print", "kits"), c("plot", "kits"),
                   c("print", "kits_table"), c("plot", "kits_norms"), c("print", "kits_norm_screen"),
                   c("print", "kits_inter"), c("plot", "kits_inter"), c("print", "kits_equations"),
                   c("as.character", "kits_equations"), c("print", "kits_code"), c("print", "kits_subj"))
  for (g in generics) {
    m <- utils::getS3method(g[1], g[2], optional = TRUE)
    if (is.null(m)) next   # kits 沒有這個方法
    expect_identical(environmentName(environment(m)), "kits", info = paste(g, collapse = "."))
  }
  # 真的叫一次：kits 的 coef(item, rp = 0.8) 不會被 kitsPreview 換掉
  item_kits <- suppressMessages(kits::kits_item(X))
  expect_false(isTRUE(all.equal(coef(item_kits, rp = 0.8)$thr, coef(item_kits)$thr)))
  expect_output(print(item_kits), "kits, CML")
  # 兩邊的數字還是相同
  expect_lt(max(abs(coef(item_kits)$thr - coef(item)$thr)), 1e-6)
})
