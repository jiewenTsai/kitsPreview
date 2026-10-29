# 兩點計分、兩點和多點混合：整條流程各走一次

types <- list(
  dichotomous = stats::setNames(as.list(seq(-2, 2, length.out = 8)), paste0("d", 1:8)),
  mixed = c(stats::setNames(as.list(c(-1.5, -0.5, 0.5, 1.5)), paste0("d", 1:4)),
            list(p1 = c(-1, 0), p2 = c(0, 1), p3 = c(-1, 0, 1), p4 = c(0.5, 1.5)))
)

for (type in names(types)) {
  b <- types[[type]]

  test_that(paste0(type, "：校準、門檻、WLE（和 dexter 相同）、raw_max"), {
    X <- kits_sim(rnorm(800, 0, 1.5), b, seed = 1)
    X[sample(length(X), 1200)] <- NA                # 很多題本
    X <- X[rowSums(!is.na(X)) > 0, ]
    item <- kits_item(X)
    thr <- coef(item)
    two_point <- thr$item_id %in% names(b)[lengths(b) == 1]
    expect_equal(thr$thr[two_point], thr$beta[two_point], tolerance = 1e-6)   # 兩點計分：門檻就是 β
    expect_true(all(tapply(thr$thr, thr$item_id, function(t) all(diff(t) > 0))))

    s <- kits_subj(X, item)
    wle <- as.data.frame(dexter::ability(X, item$fit, method = "WLE"))
    ok <- is.finite(wle$theta)
    expect_equal(s$theta[match(wle$person_id[ok], s$visit_id)], wle$theta[ok], tolerance = 1e-8)
    item_max <- tapply(thr$item_score, thr$item_id, max)
    expect_equal(s$raw_max, unname(apply(!is.na(X[s$visit_id, ]), 1, function(a) sum(item_max[colnames(X)[a]]))))
  })

  test_that(paste0(type, "：換算表、連結、偏移、適配、DIF、50% 通過月齡"), {
    age <- rep(c(6, 12, 24), each = 300)
    theta <- 1.5 * log(age / 12) + rnorm(900, 0, 0.8)
    X <- kits_sim(theta, b, seed = 2)
    item <- kits_item(X)

    tab <- table_of(X, item, wave_m = age)
    expect_equal(sum(tab$n), 900)
    expect_true(all(is.finite(tab$theta)))

    # 兩個世代各答一部分題目；兩點計分的共同題一題一個點
    ids <- names(b)
    first <- ids[1:6]
    second <- ids[3:length(ids)]
    from <- kits_item(kits_sim(rnorm(500, 1), b[second], prefix = "q", seed = 3))
    to <- kits_item(kits_sim(rnorm(500, 0), b[first], seed = 4))
    link <- kits_linking(from, to, n_boot = 50)
    common <- merge(coef(from), coef(to), by = c("item_id", "item_score"))
    expect_equal(link$n, nrow(common))
    expect_true(abs(link$slope - 1) < 0.3)

    drift <- kits_drift(kits_sim(rnorm(500, 0.5), b, prefix = "n", seed = 5), item)
    expect_true(all(c("item_id", "max_abs_resid", "drift", "suggest_fix", "reason") %in% names(drift$items)))
    expect_s3_class(kits_item(X, fixed_params = drift$fix), "kitsPreview_item")

    fit <- kits_itemfit(X, item)
    expect_true(all(is.finite(fit$infit) & is.finite(fit$rir)))
    inter <- suppressMessages(kits_inter(X, wave_m = age))
    expect_s3_class(inter, "kitsPreview_inter")

    dif <- kits_dif(X, sex = rep(c("boy", "girl"), 450))
    expect_equal(sort(dif$item_id), sort(names(b)))
    a50 <- kits_age_pass(X, age_m = age, wave_m = age)
    expect_equal(a50$item_id, names(b))
  })
}
