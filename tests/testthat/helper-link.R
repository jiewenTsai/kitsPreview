# 自己的點的連結（0.22.0 起用連結的三步；kits_mean_sigma() 在 0.23.0 拿掉）
link_of_points <- function(from, to, id, item = id, cut = 2, n_boot = 0, seed = 1) {
  points <- data.frame(id = id, item = item, from = from, to = to)
  link <- points |>
    kits_link_screen(cut = cut) |>
    kits_link_line()
  if (n_boot == 0) {
    return(link)
  }
  kits_link_error(link, n_boot = n_boot, seed = seed)
}

# 每個波次的原始分換算表（0.22.0 起從分數來；以前是 kits_subj_table(data, item, wave_m)）
table_of <- function(data, item, wave_m, forms = "standard") {
  persons <- data.frame(visit_id = rownames(data), wave_m = wave_m)
  kits_subj(data, item, persons = persons) |>
    kits_subj_table(forms = forms)
}
