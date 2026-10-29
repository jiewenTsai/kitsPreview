# 0.23.0：kits_item() 在同一份資料上算題目適配，放在 item$itemfit（和 kits 0.23.0 一樣）。
# 適配跟著它描述的物件：個人適配在分數表裡，題目適配在題目的物件裡。

betas <- list(a1 = c(-1, 0), a2 = c(0, 1), a3 = 0.5, a4 = -0.5, a5 = 0)
X <- kits_sim(seq(-3, 3, length.out = 400), betas, seed = 1)
item <- kits_item(X)

# 兩題不合：a5 是亂答的，a3 是 a4 的副本
set.seed(3)
Xbad <- X
Xbad[, "a5"] <- sample(0:1, 400, replace = TRUE)
Xbad[, "a3"] <- Xbad[, "a4"]
item_bad <- kits_item(Xbad)

test_that("item$itemfit 和同一份資料的 kits_itemfit() 完全相同（identical）", {
  expect_identical(item$itemfit, kits_itemfit(X, item))
  expect_identical(item_bad$itemfit, kits_itemfit(Xbad, item_bad))
  expect_s3_class(item$itemfit, "kitsPreview_table")
  expect_identical(names(item$itemfit), c("item_id", "n", "pvalue", "rir", "infit", "outfit", "flag"))
})

test_that("flag：underfit（超過 1.4，兩種都有時算 underfit）、overfit（低於 0.6）、ok；剛好 0.6、1.4 是 ok；都 NA 是 NA", {
  flag <- kitsPreview:::itemfit_flag
  infit <- c(1.0, 1.5, 0.5, 1.5, 1.4, 0.6, NA, NA, 1.0, 0.5)
  outfit <- c(1.0, 1.0, 1.0, 0.5, 0.6, 1.4, NA, 1.5, NA, NA)
  want <- c("ok", "underfit", "overfit", "underfit", "ok", "ok", NA, "underfit", "ok", "overfit")
  expect_identical(flag(infit, outfit), want)
  expect_identical(item_bad$itemfit$flag, c("overfit", "ok", "ok", "ok", "underfit"))
  expect_identical(item_bad$itemfit$flag, flag(item_bad$itemfit$infit, item_bad$itemfit$outfit))
  expect_identical(kits_itemfit(Xbad, item_bad)$flag, item_bad$itemfit$flag)
  expect_true(all(item$itemfit$flag == "ok"))
})

test_that("多個作答組合、長格式、固定參數（有沒放進校準的題）也完全相同", {
  set.seed(7)
  Xm <- X
  Xm[sample(length(Xm), 300)] <- NA
  Xm <- Xm[rowSums(!is.na(Xm)) > 1, ]
  im <- kits_item(Xm)
  expect_gt(length(unique(im$design$pattern_id)), 1)
  expect_identical(im$itemfit, kits_itemfit(Xm, im))

  long <- data.frame(visit_id = rownames(Xm)[row(Xm)], item_id = colnames(Xm)[col(Xm)], item_score = c(Xm))
  long <- long[!is.na(long$item_score), ]
  il <- kits_item(long)
  expect_identical(il$itemfit, kits_itemfit(long, il))
  expect_identical(il$itemfit, im$itemfit)

  Xf <- X
  Xf[, "a4"] <- 1   # a4 只有一種分數：固定的題，不放進校準
  fixed <- subset(coef(item), item_id %in% c("a4", "a5"))
  ifx <- suppressMessages(kits_item(Xf, fixed_params = fixed))
  expect_identical(ifx$left_out, "a4")
  expect_identical(ifx$itemfit, kits_itemfit(Xf, ifx))
  expect_identical(ifx$itemfit$item_id, c("a1", "a2", "a3", "a4", "a5"))
})

test_that("其他引擎也存題目適配，和 kits_itemfit() 相同", {
  skip_if_not_installed("immer")
  im <- kits_item(X, engine = "immer")
  expect_identical(im$itemfit, kits_itemfit(X, im))
})

test_that("item$itemfit 只有每題的彙總，沒有任何一個人次一列的東西", {
  fit <- item_bad$itemfit
  n_items <- length(unique(coef(item_bad)$item_id))
  expect_identical(nrow(fit), n_items)
  expect_true(all(lengths(unclass(fit)) == n_items))
  expect_true(all(names(attributes(fit)) %in% c("names", "row.names", "class")))
  expect_false(any(c("visit_id", "subj_id", "theta", "se") %in% names(fit)))
  expect_false(any(rownames(Xbad) %in% unlist(lapply(fit, as.character))))
  expect_false(any(lengths(unclass(fit)) == nrow(Xbad)))
})

test_that("舊的元素都在、名字沒變；新增 itemfit 和 notes", {
  old <- c("thresholds", "fit", "original", "engine", "method", "shift", "origin", "n_persons",
           "left_out", "design", "settings")
  expect_identical(names(item)[seq_along(old)], old)
  expect_identical(setdiff(names(item), old), c("itemfit", "notes"))
  expect_s3_class(item$fit, "enorm")   # kitsPreview 的 item$fit 是 dexter 的物件
  expect_identical(item$notes, character())
})

test_that("算不出題目適配時是 NULL 加一則說明，校準照常完成", {
  local_mocked_bindings(itemfit_table = function(...) stop("boom"), .package = "kitsPreview")
  it <- expect_no_error(kits_item(X))
  expect_true("itemfit" %in% names(it))
  expect_null(it$itemfit)
  expect_match(it$notes, "Item fit could not be computed: boom", fixed = TRUE)
  expect_identical(coef(it), coef(item))
  expect_output(print(it), "Item fit: not computed; see x$notes.", fixed = TRUE)
  expect_error(plot(it, type = "fit"), "no item fit", class = "kitsPreview_error_input")
  k <- expect_no_error(kits(X))
  expect_null(k$item$itemfit)
})

test_that("印出：全在範圍內、有幾題超出（最多列 5 題，再加 …）", {
  expect_output(print(item), "Item fit: infit and outfit within 0.6-1.4 for all 5 items.", fixed = TRUE)
  expect_output(print(item_bad), paste0("Item fit: 2 of 5 items outside 0.6-1.4 (1 underfit, 1 overfit: a5, a1); ",
                                        "see x$itemfit, plot(x, type = \"fit\")."), fixed = TRUE)
  many <- list(itemfit = data.frame(item_id = paste0("i", 1:8), infit = c(rep(1.6, 7), 1),
                                    outfit = c(1, 1, 1, 1, 1, 1, 0.5, 1)))
  line <- capture.output(kitsPreview:::itemfit_line(many))
  expect_identical(line, paste0("Item fit: 7 of 8 items outside 0.6-1.4 (7 underfit, 0 overfit: ",
                                "i1, i2, i3, i4, i5, \u2026); see x$itemfit, plot(x, type = \"fit\")."))
  # underfit 的題在前，最多列 5 題
  mixed <- list(itemfit = data.frame(item_id = paste0("i", 1:8), infit = c(0.5, 0.5, 1.6, 0.5, 1, 0.5, 1.5, 1),
                                     outfit = 1))
  mixed$itemfit$flag <- kitsPreview:::itemfit_flag(mixed$itemfit$infit, mixed$itemfit$outfit)
  line <- capture.output(kitsPreview:::itemfit_line(mixed))
  expect_identical(line, paste0("Item fit: 6 of 8 items outside 0.6-1.4 (2 underfit, 4 overfit: ",
                                "i3, i7, i1, i2, i4, \u2026); see x$itemfit, plot(x, type = \"fit\")."))
  edge <- list(itemfit = data.frame(item_id = c("a", "b"), infit = c(0.6, 1.4), outfit = c(NA, 1.4)))
  expect_output(kitsPreview:::itemfit_line(edge), "within 0.6-1.4 for all 2 items", fixed = TRUE)
  expect_identical(kitsPreview:::ITEMFIT_RANGE, c(0.6, 1.4))
})

test_that("印出：題目適配那一行在表和固定題之後，舊的物件（沒有 itemfit）照舊印", {
  out <- capture.output(print(item))
  expect_match(out[length(out)], "^Item fit: ")
  old <- item
  old$itemfit <- NULL
  old$notes <- NULL
  expect_false(any(grepl("Item fit", capture.output(print(old)))))
  expect_identical(capture.output(print(old)), out[-length(out)])
})

test_that("plot(item, type = \"fit\") 畫題目適配；沒有預設的圖", {
  p <- plot(item_bad, type = "fit")
  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)$x
  band <- built$layout$shapes[[1]]
  expect_identical(c(band$y0, band$y1), kitsPreview:::ITEMFIT_RANGE)
  ys <- lapply(built$data, function(tr) as.numeric(unlist(tr$y)))
  expect_equal(ys[[1]], item_bad$itemfit$infit)
  expect_equal(ys[[2]], item_bad$itemfit$outfit)
  expect_error(plot(item), "type = \"fit\"", class = "kitsPreview_error_input")
  expect_error(plot(item, type = "wright"), "must be \"fit\"", class = "kitsPreview_error_input")
})

test_that("kits() 帶著題目適配：k$item$itemfit 和 kits_itemfit() 相同，印出也有", {
  k <- kits(Xbad)
  expect_identical(k$item$itemfit, kits_itemfit(Xbad, k$item))
  expect_identical(k$item$itemfit, item_bad$itemfit)
  expect_output(print(k), "Item fit: 2 of 5 items outside 0.6-1.4 (1 underfit, 1 overfit: a5, a1)", fixed = TRUE)
})

test_that("kits_itemfit() 照舊：用同樣的題目檢查別的資料", {
  X_new <- kits_sim(rnorm(300, 0.5), betas, seed = 9)
  other <- kits_itemfit(X_new, item)
  expect_identical(nrow(other), 5L)
  expect_false(identical(other, item$itemfit))
  expect_identical(kits_itemfit(X_new, item), kits_itemfit(X_new, coef(item)))
  expect_error(kits_itemfit(X[, "a1", drop = FALSE], item), "at least 2 items")
})
