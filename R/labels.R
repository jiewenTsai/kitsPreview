# 圖上的英文字（標題、軸、圖例）
# 要改標題：plotly::layout(p, title = "...")
LAB <- list(
  domain = c(lan = "Language", motor = "Motor", cog = "Cognition", social = "Social", emotion = "Emotional"),
  sex = c(boy = "Boys", girl = "Girls"),
  dif = c(none = "No DIF", "favors girls" = "Favors girls", "favors boys" = "Favors boys", inconsistent = "Inconsistent"),
  column = c(lan = "Language", gross = "Gross motor", fine = "Fine motor", cog = "Cognition", social = "Social", emotion = "Emotional"),
  age = "Age (months)",
  month = " mo",
  D = "D",
  D_step = "D (milestone step)",
  hover_step = "Milestone step D",
  hover_age50 = "Age at 50% passing",
  hover_reach = "Age at which the median child reaches it",
  hover_dif = "Sex DIF",
  hover_na = "not estimable",
  wave = "-month wave",
  median_age = "actual median age",
  title_growth = "Growth curves with items: grey lines are norm P10, P50, P90<br>Points are items (filled = empirical age at 50% passing; open = age at which the median child reaches the item)",
  title_ladder = "Developmental ladder: one rung per item at its milestone step (D)<br>Right: age at which the median child reaches the D; orange = favors girls, blue = favors boys, black = inconsistent",
  title_cohort = "Cohort linking: wave medians (bars = P25 to P75) of the two cohorts",
  title_norms = "Continuous norms by sex: P3, P10, P50, P90, P97",
  not_drawn = "Not shown (beyond the norm range): ",
  theta = "theta (logit)",
  persons = "Person-waves",
  step = "Step",
  title_wright = "Wright map: abilities (left) and step thresholds (right)",
  title_norm_curves = "Continuous norm: percentile curves",
  kept = "Kept",
  dropped = "Dropped",
  link_from = "From (scale to be converted)",
  link_to = "To (target scale)",
  title_link = "Link: every point and the fitted line",
  first_line = "Before screening (all points): %.2f + %.2f x",
  band = "Screening band: +/- %g x SD(e) around the line before screening; outside = dropped",
  wave_axis = "Wave (months)",
  mean_sd = "Mean +/- SD",
  median_iqr = "Median, P25 to P75",
  title_vertical = "Vertical scale: mean +/- SD of each wave, by cohort",
  title_vertical_median = "Vertical scale: median (bars = P25 to P75) of each wave, by cohort",
  title_norm_groups = "Continuous norms%s: %s",
  item = "Item",
  mean_square = "Mean square",
  title_itemfit = "Item fit: mean squares (shaded: %g to %g)"
)

# 代碼的名稱；沒有對照的代碼直接印代碼
lab_name <- function(group, code) {
  name <- unname(LAB[[group]][code])
  ifelse(is.na(name), code, name)
}
