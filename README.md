# kitsPreview

kitsPreview 是教學用的薄包裝。
它回答一個問題：這一步，直接用 dexter、immer 或 cNORM 要怎麼做。
它不是最佳化的版本。
最佳化的版本是另一個套件 kits（用 RTMB 寫）。

## 定位

- 包裝很薄、很簡單。
- 每個包裝只跑幾行原本套件的程式。
- 結果換成同一種表，原點也對齊，所以可以直接比較。
- 校準、計分和常模的結果原封不動留著套件自己的物件：`kits_original(x)`。
  - `kits_original()` 收 `kits_item()`、`kits()`、`kits_subj()`、`kits_norms()` 的結果。
  - 常模給 cNORM 的 `cnorm.shash()` 結果（留下的那個起始值的解）；照組的常模給每組一個，名字是組（例如 `$boy`）。
    它只有係數和資料的幾個摘要（人數、最小、最大），沒有孩子的分數。
  - `summary(x)` 先印整理過的表，再印套件自己的輸出；只有 `kits_item()`、`kits_subj()` 的結果（`kitsPreview_item`、`kitsPreview_subj`）有這個 `summary()`。
  - 套件自己的輸出照原樣印，只拿掉 immer 的 `summary()` 印的電腦名字和帳號那一行（`nodename=`、`login=`）。
  - DIF 和 `kits_inter()` 的結果不留原本套件的物件。
  - 連結是 kitsPreview 自己用 base R 算的，沒有原本套件的物件；`kits_code(link)` 寫出整個連結。
- `kits_code(x)` 寫出原本套件的那幾行程式，再加上換算。
- 讀完之後，應該知道每一步怎麼直接用原本的套件做。
- kitsPreview 也拿來驗證 kits 的數字（dexter 引擎和 kits 相同）。
- 程式的寫法：處理資料的地方用 tidyverse，一行一個概念，函數都很小。

## 安裝

從 GitHub：

```r
remotes::install_github("jiewenTsai/kitsPreview")
```

或用下載的原始碼套件：

```r
install.packages("kitsPreview_0.25.0.tar.gz", repos = NULL, type = "source")
```

一定要的套件：dexter、cNORM、dplyr、purrr、plotly、htmlwidgets。
選用的套件：immer（`engine = "immer"` 或 `"ccml"` 才需要）。
沒有安裝時，錯誤訊息會說怎麼裝（錯誤的類別是 `kitsPreview_error_missing_package`）。
存 png 另外需要 webshot2。

## 引擎

| 函數 | `engine =` | 套件、方法 |
|---|---|---|
| `kits_item()` | `"dexter"`（預設） | dexter `fit_enorm()`，CML |
| | `"immer"` | immer `immer_cml()`，CML |
| | `"ccml"` | immer `immer_ccml()`，成對的 CCML（GSED 的 D-score 用的方法） |
| `kits_subj()` | （沒有引擎） | dexter `ability()`，WLE |
| `kits_linking()` | （沒有引擎） | mean/sigma 加篩點，base R（連結的三步：`kits_link_screen()`、`kits_link_line()`、`kits_link_error()`） |

```r
item <- kits_item(X)                                   # dexter
item_immer <- kits_item(X, engine = "immer")           # 同一個 CML
summary(item_immer)                                    # 整理過的表，再來是 immer 自己的 summary
kits_original(item_immer)                              # immer 的 immer_cml 物件
kits_code(item_immer)                                  # immer 的那幾行程式，加上換算
item_ccml <- kits_item(X, engine = "ccml")             # 成對的 CCML，GSED 的方法
```

- dexter 和 immer 都是條件最大概似（CML）。
- 這兩個引擎最大化同一個條件概似，但最佳化的方法和收斂的標準不同：
  immer 用 `optim()`（BFGS，預設的相對誤差），dexter 用自己的演算法（iterative scaling，再 Newton-Raphson）。
  所以兩者停下來的地方差多少，要看資料：
  - KIT 的語言 M03（真實資料，比較的教學文件）：β 最大差 9.2e-4（2026-10-27 實測）。
  - kits 的示範資料 `kits::kitm03` 的語言（4,004 個人次、24 題）：β 最大差 0.017，門檻 0.009，約 0.05 個 SE（2026-10-28 實測）。
  - 給 immer `control = list(reltol = 1e-14, maxit = 5000)`，示範資料上降到 3e-7。
- `"ccml"` 每次看兩題，以兩題的分數和為條件（Zwinderman, 1995），一樣不需要假設能力的分佈。
  - GSED 的 D-score 用 sirt 的同一個方法（只有 0/1 的題）；immer 把它推廣到 PCM。
  - 資料符合 PCM 時和 CML 一樣。KIT 的四個校準上，SE 是 CML 的 1.2 到 2.6 倍（各步驟的中位數）（2026-10-27 用 KIT 真實資料實測）。
  - KIT 的真實資料不完全符合 PCM，所以兩者不一樣；比較在另外的教學文件。
- immer 的步驟參數換到 dexter 的原點：所有 β 的和是 0。
- 減掉的常數記在 `item$shift`。
- immer 比較快的 `use_rcpp = TRUE` 不用：在 KIT 大小的設計上，它停在離最大值很遠的地方。
- 固定參數（`fixed_params`）只有 dexter 收。
- CML 在每個作答組合內以原始分為條件，所以不需要假設母體的分佈。
- MML 需要把「誰拿到哪個作答組合」的原因放進模型；CML 不需要，因為它在每個作答組合內以原始分為條件。
- 所以 KIT 用 CML。
- TAM 的 MML 要每個波次一個母體（`group = wave`）才接近 CML。
- 這個比較在另外的教學文件裡；kitsPreview 0.10.0 拿掉了 TAM 和 sirt 引擎。
- 常模（cNORM）、DIF 和交互作用模型（dexter）、接到 D、偏移、適配不分引擎。

三個引擎的比較：`vignette("engines", package = "kitsPreview")`。

## 和 kits 一起用：類別名稱

- 函數名稱、欄位名稱和印出來的樣子都和 kits 相同；主要的引數也和 kits 相同。
- 所以兩個套件可以並排比較。
- 不同的是類別名稱（下表），和幾個引數（見下面的「引數的差別」）。
- kitsPreview 的類別都以 `kitsPreview_` 開頭。

| 函數 | kits 的類別 | kitsPreview 的類別 |
|---|---|---|
| `kits_item()` | `kits_item` | `kitsPreview_item` |
| `kits()` | `kits` | `kitsPreview_fit` |
| `kits_subj()` | `kits_subj` | `kitsPreview_subj` |
| `kits_linking()`、`kits_linking_d()`、`kits_link_line()`、`kits_link_error()` | `kits_link` | `kitsPreview_link` |
| `kits_link_screen()` | `kits_link_screen` | `kitsPreview_link_screen` |
| 表（`coef()`、`kits_itemfit()` 等） | `kits_table` | `kitsPreview_table` |
| `kits_norms()` | `kits_norms` | `kitsPreview_norms` |
| `kits_norm_screen()` | `kits_norm_screen` | `kitsPreview_norm_screen` |
| `kits_subj_table()`（從分數） | `kits_subj_table` | `kitsPreview_subj_table` |
| `kits_age_pass()` | `kits_age_pass` | `kitsPreview_age_pass` |
| `kits_item_table()` | `kits_item_table` | `kitsPreview_item_table` |
| `kits_vertical()` | `kits_vertical` | `kitsPreview_vertical` |
| `kits_inter()` | `kits_inter` | `kitsPreview_inter` |
| `kits_equations()` | `kits_equations` | `kitsPreview_equations` |
| `kits_code()` | `kits_code` | `kitsPreview_code` |

- 沒有任何類別繼承 kits 的類別。
- 方法（`print()`、`coef()`、`plot()`、`summary()` 等）只登記在 kitsPreview 的類別上。
- 所以同一個 R 裡載入兩個套件，kits 的方法不會被換掉。
- 0.9.0 以前兩個套件共用類別名稱，載入 kitsPreview 會蓋掉 kits 的方法；0.10.0 改掉了。
- 比對時，用 `kitsPreview::kits_item()` 和 `kits::kits_item()` 分開叫。
- 引數也和 kits 相同：作答資料是 `data`，D 分數是 `d`（0.11.0 以前叫 `dataSrc`、`D`；0.23.0 拿掉舊名字）。
  呼叫 dexter 自己的函數時，還是用 dexter 的名字：`dexter::fit_enorm(dataSrc = X)`。
- kits 的結果不能直接交給 kitsPreview 的函數。
- 要交 kits 的題目參數，給 `coef(item)`（一張表）。

### 引數的差別

主要的引數相同，但下面這些不同（對照 kits 0.23.0 的程式）。
兩個都有的函數，用同樣的寫法（例如 `kits_norms(s, by = "sex")`、`s |> kits_norm_z(norm)`、`s |> kits_subj_table()`）在兩個套件都能跑。

| 函數 | kitsPreview | kits |
|---|---|---|
| `kits_item()` | `engine =`（只有 kitsPreview 有） | `scale =`（只有 kits 有） |
| `kits()` | — | `scale =`、`persons =`（只有 kits 有） |
| `kits_subj()` | `persons =`（0.22.0 起兩個都有） | 也收 `kits_resp()` 的作答表 |
| `kits_linking()` | `(from, to, cut = 2, n_boot = 0, seed = 1)`：和 kits 一樣預設不算連結誤差（0.22.0 是 `n_boot = 2000`） | 多了 `step`、`to_scale`、`spread`、`unit` |
| `kits_linking_d()` | 接到 D 用這個函數 | 沒有這個函數：錨題也用 `kits_linking()` 接到 D |
| `kits_link_screen()` | `(points, cut = 2, from_scale = NULL, to_scale = NULL)`；點是自己的 data.frame | 多了 `spread`、`unit`；點也可以是 `kits_link_points()` 的結果 |
| `kits_link_line()`、`kits_link_error()` | 相同（`n_boot = 200`、`seed = 1`） | 相同；連結多記 `draws`、`vcov`，`kits_convert()` 用來算 `se_link` |
| `kits_subj_table()` | 多了 `forms =`（`"all"`：每個作答組合一張表）；表一律在校準的量尺上 | 沒有 `forms`；表照分數換過的尺換（`kits_convert()` 的紀錄） |
| `kits_inter()` | `wave_m` 一定要給 | `wave_m = NULL` 可以不給；也收 `kits_resp()` |
| `kits_norms()`、`kits_norm_z()`、`kits_norm_screen()`、`kits_norm_ages()` | 收數字，或 `kitsPreview::kits_subj()` 的分數（0.22.0 起） | 收數字或 `kits_subj()` 的分數；分數和常模記得量尺，不同時停下來 |
| `plot()` 常模 | 孩子一個一個畫成灰點 | 孩子分格畫（不畫單一個孩子）；多了 `persons`、`min_cell` |
| `kits_age_pass()` | 收矩陣、寬的表、向量 | 也收 `kits_resp()` |
| `kits_item_table()` | 沒有 `rp`（RP50） | `rp =` |
| `kits_vertical()` | 每個世代 × 波次的位置和分散：`n`、`age_med`、`mean`、`sd`、`median`、`P25`、`P75` | 多了 `sd_true`、`growth`、`effect_size`、`p_sup`、`se_link`、世代的比較、`extremes =` |
| `kits_dif_combine()` | `cohorts = c("M03", "M36")` | `cohorts = NULL`（從引數名稱或資料取） |
| `kits_dif()` | `(data, sex, ref)` | 也收 `kits_resp()`：`kits_dif(resp, by = "sex")` |
| `kits_code()` | 一個常模；照組的常模停下來，請給一組（`attr(norm, "groups")[["boy"]]`，名字是組） | 照組的常模一組一組寫；多了 `part =`、`full = TRUE` |
| `kits_sim()` | `prefix = "p"` | `prefix = NULL` |

只有 kits 有的（kitsPreview 不做，因為它們是 kits 自己的架構，不是 dexter、immer、cNORM 的一步）：

* 作答表 `kits_resp()`：kitsPreview 的作答和 dexter 一樣是矩陣或長的表，所以「原本套件的那幾行程式」可以照抄。
* 量尺標籤和換算：`kits_convert()`（含 `se_link`）、`kits_check()`、`kits_info()`、`kits_anchors()`。
  kitsPreview 換尺用一條直線自己寫：`s$theta <- link$intercept + link$slope * s$theta`。
* 一步一步的 RTMB 函數：`kits_stats()`、`kits_cml()`、`kits_thresholds()`、`kits_norm_data()`、`kits_norm_fit()`、`kits_norm_table()`、
  `kits_link_points()`。
* 量尺的性質（Kolen & Brennan）：`kits_precision()`、`kits_raw_equate()`、`kits_link_check()`、`kits_information()`、
  `kits_expected_score()`、`summary(link, theta = )`。
* R 內建的方法：`confint()`、`vcov()`、`logLik()`、`nobs()`、`update()`、`simulate()`。
* 0.22.0 的新圖：`plot(item)`（類別機率曲線、Wright 圖）、`plot(dif)`、`plot(itemfit)`、`plot(age_pass)`、`plot(drift)`、
  `plot(screen)`、`plot(resp)`。kitsPreview 有的圖：`plot(kits(X))`（Wright 圖）、`plot(item, type = "fit")`（題目適配）、`plot(link)`、`plot(inter)`、`plot(norm)`、
  `plot(kits_vertical())`、`plot(kits_item_table())`。
* 模擬資料 `kitm03`、`kitm36`。

### 0.23.0 拿掉的函數和寫法（和 kits 0.23.0 相同）

0.22.0 被取代的函數和寫法，在 0.23.0 拿掉了（0.22.0 時用了會警告）。
0.11.0 改名前的引數名 `dataSrc`、`D` 也拿掉了。
匯出的函數從 38 個變成 29 個。

| 舊的寫法 | 新的寫法 |
|---|---|
| `kits_age50(X >= 1, age_m, wave_m)` | `kits_age_pass(X, age_m, wave_m)` 的 `p50` 欄 |
| `kits_mean_sigma(from, to, id, item)` | `points \|> kits_link_screen() \|> kits_link_line() \|> kits_link_error()`，`points = data.frame(id, item, from, to)` |
| `kits_wle_table(item)` | `kits_subj_table(item)` |
| `kits_subj_table(X, item, wave_m)` | `kits_subj(X, item, persons) \|> kits_subj_table()` |
| `kits_plot_cohort(自己做的表, scores)` | `plot(kits_vertical(s), type = "median")` |
| `kits_plot_norms(自己疊的表, scores)` | `kits_norms(s, by = "sex") \|> plot(scores = s)` |
| `kits_item_positions()`、`kits_ladder_marks()` | `kits_item_table(thr, norm, age_pass = , dif = , stems = )` |
| `kits_plot_growth(norm, items)` | `plot(kits_item_table(...))` |
| `kits_plot_ladder(items, marks)` | `plot(kits_item_table(...), type = "ladder", ages = )` |
| `plot(norm, d = s$D, age_m = s$age_m)` | `plot(norm, scores = s)` |
| `kits_item(dataSrc = X)` 等 | `kits_item(data = X)`（或 `kits_item(X)`） |
| `kits_norms(D = d, ...)` 等 | `kits_norms(d = d, ...)` |

## 資料格式

和 dexter 一樣，作答用**矩陣**：

- 一列是一個人次（同一個孩子不同波次要不同列），一欄是一題。
- 列名是 `visit_id`（每個人次一個：一個孩子的一次施測），欄名是題號。
- 分數從 0 開始，要連續（0, 1, 2, …）；NA 是沒施測。
- 寬的資料表用 `as.matrix()` 轉，例如 `X <- as.matrix(wide[, item_cols])`，再 `rownames(X) <- wide$visit_id`。
- 一次只放一個領域的題目。

長格式（`visit_id`、`item_id`、`item_score`）也可以用。

作答資料的引數叫 `data`，例如 `kits_item(data = X)`、`kits_subj(data = X, item)`；通常直接用位置給：`kits_item(X)`。

孩子的資料放在一個表 `persons`（`visit_id`，和 `subj_id`、`wave_m`、`age_m`、`sex`、`cohort` 中的幾欄），
計分時給 `kits_subj(X, item, persons = persons)`，照 visit_id 接上；之後常模的函數直接收這張分數表。
少數函數要每人一個值（例如 `kits_dif()` 的 `sex`、`kits_inter()` 的 `wave_m`），給一個向量：照矩陣的列順序，或用 visit_id 當名字。

### 名稱（0.23.0 起，和 kits 0.23.0 相同）

| 0.22.0 | 0.23.0 | 意思 |
|---|---|---|
| `person_id` | `visit_id` | 人次：一個孩子的一次施測 |
| `pid` | `subj_id` | 孩子（每一波都一樣） |
| `booklet_id` | `pattern_id` | 作答組合（item pattern）：這個人次答了哪幾題；編號 `bk01`、`bk02`、…，`kits_item()$design` 和 `kits_subj()` 相同，也和 kits 相同 |
| `booklet_score` | `raw` | 原始分 |
| `booklet_max` | `raw_max` | 這個作答組合的滿分 |
| 連結的 `a`、`b` | `intercept`、`slope` | 新的值 = intercept + slope × 舊的值（Kolen & Brennan 的 B、A） |
| `a_error`、`b_error`、`a0`、`b0` | `intercept_error`、`slope_error`、`intercept0`、`slope0` | 連結誤差、篩點前的線 |

- 各波次大多數孩子答的那一組仍叫標準題本（standard form）。
- dexter 自己叫 `person_id`、`booklet_id`、`booklet_score`：kitsPreview 交給 dexter 時換成 dexter 的名字，結果再換回上面的名字。
  只有原本套件的輸出（`kits_original()`、`summary()`、`print(x, original = TRUE)`）故意照舊用 dexter 的名字。
- 舊的欄名 `person_id`、`pid` 在 0.24.1 拿掉了：資料要用 `visit_id`，缺欄時訊息會說缺哪一欄。
- 0.22.0 的 `link$a`、`link$b` 在 0.24.1 拿掉了（現在是 `NULL`）：用 `link$intercept`、`link$slope`，
  或 `coef(link)`（給 `c(intercept = , slope = )`）。

## 主要流程（第一梯隊）

| 步驟 | 函數 | 做什麼 |
|---|---|---|
| 1 校準 | `kits_item()` | PCM（預設用 dexter），加上每個步驟的門檻 |
| 2 世代連結 | `kits_linking()` | 把 M36 的量尺接到 M03 |
| 3 接到 D | `kits_linking_d()` | 用錨題把 M03 的量尺接到 GSED 的 D |
| 4 計分 | `kits_subj()`、`kits_subj_table()` | 每個孩子的 WLE（含個人適配）；每個波次的原始分換算表（從分數來）。再用兩條直線換成 D |
| 5 常模 | `kits_norm_screen()`、`kits_norm_ages()`、`kits_norms()`、`kits_norm_z()` | 篩波次、定月齡範圍、配常模（`by =` 一組一個）、算同齡 z |
| 6 檢查 | `kits_itemfit()`、`kits_inter()`、`plot(link)`、`plot(norm)` | 題目適配（infit、outfit、flag、rir；dexter 的交互作用模型）、連結的每個點、常模曲線 |

```r
item03 <- kits_item(X03)
item36 <- kits_item(X36)
co <- kits_linking(item36, item03)
d_link <- kits_linking_d(item03, anchors)   # 幾個領域共用一條線：list(lan = item_lan, cog = item_cog)

s36 <- kits_subj(X36, item36, persons = persons)
s36$theta <- co$intercept + co$slope * s36$theta          # M36 → M03（SE 也乘上斜率）
s36$se <- co$slope * s36$se
s36$theta <- d_link$intercept + d_link$slope * s36$theta  # M03 → D
s36$se <- d_link$slope * s36$se

norm <- kits_norms(s36, by = "sex")            # 月齡範圍用 kits_norm_ages() 的規則
s36 <- s36 |> kits_norm_z(norm)                # 多兩欄：z、pct
```

不需要連結時（只有一個世代），一步做完：

```r
k <- kits(X03)    # k$item、k$subj
plot(k)           # Wright map
```

常模 `norm <- kits_norms(...)` 的百分位曲線：`plot(norm)`；孩子畫在曲線後面：`plot(norm, scores = s)`。
圖都是 plotly（英文），可以用 `kits_save_plot()` 存成 html 和 png。

完整的例子（每一步的結果和怎麼讀）：`vignette("kits-flow", package = "kitsPreview")`。

每條連結都是一條直線：新的值 = intercept + slope × 舊的值；SE 乘上 slope。
連結誤差要給 `n_boot`，例如 `kits_linking(item36, item03, n_boot = 2000)`，或之後 `kits_link_error(co, n_boot = 2000)`。
0.23.0 起 `n_boot` 預設是 0（不算連結誤差），和 kits 相同；0.22.0 預設是 2000。
結果印出來都很簡短，排法和 kits 相同（題目參數、分數、連結、常模、常模篩選）：長的表只印前 10 列，`print(x, n = 50)` 印更多。
它們還是 list 或 data.frame：
`item03$thresholds`、`co$points`、`screen$normed` 都可以直接取。
換過尺的分數（`theta` 已經是 D）和 kits 一樣仍放在 `theta`、`se`，常模的函數用 `theta`。
印出來的數字是小數點下 2 位，用 `digits` 改，例如 `print(coef(item03), digits = 4)`；物件裡的數值沒有捨入。
圖上的字是英文；要改標題用 `plotly::layout(p, title = "...")`。

## 輔助函數（第二梯隊）

用法寫在各函數的說明裡，每個都有小例子。

| 情況 | 函數 |
|---|---|
| 自己準備連結的點；連結一步一步 | `kits_link_screen()` → `kits_link_line()` → `kits_link_error()`（`?kits_link_steps`） |
| 原始分換算表：每個作答組合 | `kits_subj_table(item)` |
| 新的一波資料：題目有沒有偏移 | `kits_drift()` |
| 一定比例（例如 50%）通過的月齡 | `kits_age_pass()` |
| 性別 DIF；兩世代合併判斷 | `kits_dif()`、`kits_dif_combine()` |
| 題目表、成長曲線、成長階梯 | `kits_item_table()`、`plot(tab)`、`plot(tab, type = "ladder")` |
| 兩世代每波的位置和分散 | `kits_vertical()`、`plot(v, type = "median")` |
| 照組的常模（例如男女） | `kits_norms(s, by = "sex")`、`plot(norm, scores = s)` |
| 存檔 | `kits_save_plot()` |
| 模擬作答 | `kits_sim()` |

有幾種圖的 `plot()` 用 `type =` 換（第一個是預設），例如 `plot(v, type = "median")`、`plot(tab, type = "ladder")`；題目表的圖用 `items =` 選題號（和 kits 的規則相同）。

新的一波資料：先 `kits_drift()`，再固定參數校準（`fixed_params` 交給 dexter）：

```r
drift <- kits_drift(X_new, item_old)
item_new <- kits_item(X_new, fixed_params = drift$fix)
```

固定參數要用某個世代自己的量尺（PCM 的斜率固定是 1，轉換過的參數不能拿來固定）。
新資料就在那個世代的量尺上，所以沿用原本的世代連結和 D 連結。
幾波一起進來時疊成一個矩陣；分批進來時，每次都固定在原本的參數，不要固定在上一次更新的結果。
固定的題在新資料裡沒有 0 分時（例如大家都通過），`kits_item()` 不把它放進校準，參數照抄。

題目適配要注意：每個孩子答的題少時，infit、outfit 會低於 1（模擬資料：5 題約 0.83，20 題約 0.96），
所以題數少的作答組合要拿題目互相比。

## 公式與程式

| 函數 | 給什麼 |
|---|---|
| `kits_equations(x)` | 每一步的公式（Unicode；`format = "latex"` 給 LaTeX）；連結會附上這條線的數字 |
| `kits_code(x)` | 用這個結果的引擎（dexter、immer；常模是 cNORM）和 base R 重做的程式（不需要 kitsPreview） |

`x` 可以是 `kits_item()`、`kits()`、`kits_subj()`、連結、`kits_norms()`、`kits_norm_screen()` 的結果。
程式的變數名稱和公式的符號相同。
每一份程式都在測試裡執行過，數字和套件相同。
dexter、immer、cNORM 的程式到 1e-10。
從頭用 RTMB 寫的估計在 kits 的教學腳本裡。

## 引用

`citation("kitsPreview")`。方法的原始文獻在 `?kitsPreview`、`?kits_item` 的 References。

## 小詞彙

| 用語 | 意思 |
|---|---|
| PCM | 部分給分模式。每題有幾個步驟，每個步驟一個參數 β |
| CML | 條件最大概似估計。dexter、immer 用它估題目參數，不需要假設能力的分布 |
| CCML | 成對的條件最大概似。每次看兩題，以兩題的分數和為條件；GSED 的 D-score 用它 |
| MML | 邊際最大概似估計。例如 TAM，要假設能力的分布；kitsPreview 不包它 |
| WLE | 估計個人能力的方法（Warm 加權概似）。PCM 下等於 Jeffreys 先驗的 MAP；dexter 算不出來時，`kits_subj()` 用這個算法 |
| 門檻（Thurstonian threshold） | 得到 k 分以上的機率剛好是 .5 的能力 |
| milestone step | 第 1 個步驟的門檻（勉強做得到） |
| mean/sigma | 讓兩邊的平均和 SD 對齊的直線連結 |
| D、τ | GSED 的發展分數與題目難度，單位相同 |
| shash | sinh-arcsinh 分布：位置 mu、尺度 sigma、偏態 epsilon、尾巴厚度 delta |
| `cut` | 各函數的剔除或判定門檻，單位不同：連結和 `kits_drift()` 是殘差 SD；`kits_dif_combine()` 是 logit；`kits_norm_screen()` 是百分比 |
| DIF | 題目對不同群體（例如男女）難度不同 |

## 套件裡沒有的東西

- KIT 的資料整理（反向題、拆題、題號對照）：每份資料不同，留在各自的分析腳本。
- 資料：套件不含任何個人資料。例子都用模擬資料。
