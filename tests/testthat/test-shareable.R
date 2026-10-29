# 題目參數物件不能帶人次編號（item$design 的列名也不行）

test_that("kits_item() 的結果不帶人次編號", {
  set.seed(7)
  X <- matrix(rbinom(300 * 5, 1, 0.5), 300, 5, dimnames = list(NULL, paste0("i", 1:5)))
  X[1:100, 5] <- NA
  rownames(X) <- sprintf("M03_%07x_M12", sample.int(1e6, 300) + 1e6)
  item <- suppressMessages(kits_item(X))
  s <- rawToChar(serialize(item[setdiff(names(item), "fit")], NULL, ascii = TRUE))
  expect_false(any(vapply(rownames(X), \(i) grepl(i, s, fixed = TRUE), logical(1))))
  expect_identical(rownames(item$design), as.character(seq_len(nrow(item$design))))
})
