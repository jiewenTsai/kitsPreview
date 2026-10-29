# kitsPreview 0.25.0

* 版本和 kits 0.25.0 一致；功能沒有改變。

# kitsPreview 0.24.1

* 版本和 kits 0.24.1 一致。
* 拿掉 0.23.0 為了舊程式加的 `link$a`、`link$b`（和 `a_error`、`b_error`、`a0`、`b0`）檢查；現在 `link$a` 是 `NULL`，請用 `link$intercept`、`link$slope` 或 `coef(link)`。錯誤類別 `kitsPreview_error_deprecated` 也拿掉了。
* 拿掉舊欄名 `person_id`、`pid` 的自動改名和 `kitsPreview_message_data` 訊息；資料和 `persons` 要用 `visit_id`、`subj_id`。
* 長格式資料缺欄時，訊息說缺哪一欄、資料有哪些欄（例如 `person_id` 要改成 `visit_id`）。
* `persons` 沒有 `visit_id` 時，訊息列出它有的欄，說人次的欄要叫 `visit_id`；多出的 `pid` 列在「沒有用到的欄」。
* `kits_age_pass()` 寬的表裡有不是分數或 TRUE/FALSE 的欄（例如 `person_id`）時停下來，說是哪幾欄。
* 拿掉 `kits_rtmb_code()`：從頭用 RTMB 寫估計的教學腳本在 kits 裡；kitsPreview 只留 `kits_code()`，寫出直接用原本套件（dexter、immer、cNORM）的程式。匯出 29 → 28 個。

# kitsPreview 0.24.0

* 版本和 kits 0.24.0 一致。
* vignette 改成 Quarto（`.qmd`，`VignetteBuilder: quarto`）。
* 更正 0.23.1 的說明：kits 0.23.1 是拿掉 `kits::kits_inter()`，不是停用；kitsPreview 的 `kits_inter()`（dexter 的 `fit_inter()`）照常可以用。

# kitsPreview 0.23.1

和 kits 0.23.1 同一個版本號；kitsPreview 的計算沒有改。

* kits 0.23.1 起 `kits::kits_inter()` 停用（警告），kitsPreview 的 `kits_inter()`（dexter 的 `fit_inter()`）照常可以用。
* kits 0.23.1 的 WLE 碰到兩個高點時，和 TAM 一樣取牛頓法從起始值走到的那一個（0.23.0 以前取比較高的那個）。
  kitsPreview 的 WLE（dexter 的 `ability()`，dexter 算不出來時用 Jeffreys MAP）沒有改；少數兩個高點的組合，兩個套件可能不同。

# kitsPreview 0.23.0

## 題目參數物件不再帶人次編號

* `item$design` 的列名原本是每個作答組合第一個人次的編號，分享題目參數物件時會帶出這些編號。
  現在列名是 1, 2, …，其他不變；測試檢查物件裡沒有任何人次編號。0.22.0 以前也是這樣。

和 kits 0.23.0 同步：名字改成和 kits 0.23.0 一樣，拿掉 0.22.0 被取代的函數。
所有的數字都沒有變；只有 `kits_linking()`、`kits_linking_d()` 的 `n_boot` 預設改成 0（和 kits 相同），所以沒給 `n_boot` 時不再算連結誤差（見下面）。

## `n_boot` 的預設改成 0（和 kits 相同）

* `kits_linking()`、`kits_linking_d()` 的 `n_boot` 預設從 2000 改成 0：沒給 `n_boot` 時，連結的線照舊，但不算連結誤差
  （`intercept_error`、`slope_error` 是 NA，印出來沒有括號）。
* 要連結誤差，自己給：`kits_linking(from, to, n_boot = 2000)`（200 就看得出大小，報告用 2000），或之後 `kits_link_error(link, n_boot = 2000)`。
  兩種寫法的結果完全相同。
* 舊程式靠預設拿 2000 次重抽的連結誤差的，要加上 `n_boot = 2000`；給了 `n_boot` 的程式，結果和以前完全相同。
* `vignette("kits-flow")` 的世代連結和 D 連結改成明寫 `n_boot = 2000`；說明文件和 README 一併改。

## 印出來的樣子和一致性（和 kits 相同）

* 分數（`kits_subj()`）有自己的 `print()`，和 kits 的排法相同：標題、主要的欄（`visit_id`、`wave_m`、`age_m`、`theta`、`se`、`flag`）的前幾列、
  「Other columns: …」、原始分換算表的紀錄、`Next:`；`print(s, n = 3)` 選列數。以前會印出每一列。
* 表格印出來都收 `n =`（和 kits 相同）：30 列以內全印，再長只印前 10 列，最後一行說還有幾列。
  以前 `print(norm, n = 3)` 會出錯（`invalid 'na.print' specification`）。
* 常模（`kits_norms()`）有自己的 `print()`：標題、配適的月齡（和是不是 `kits_norm_ages()` 定的）、照哪幾欄分組、`Next:`。
* 連結印出來和 kits 一行一行相同：標題、`scales:`、`intercept`、`slope`（和連結誤差）、`points from:`、`points:`、
  `before screening: intercept …, slope …; screening: |e| > 2 SD(e)`、幾個領域時的 `kept points by domain:` 和 domain check 的表、`Next:`。
  kitsPreview 沒有 kits 的檢查表，所以沒有 `Checks:` 那一行。
* 作答組合的編號：`kits_item()$design` 以前是 `set1`、`set2`、…，和 `kits_subj()$pattern_id` 的 `bk01`、`bk02`、… 不同。
  現在兩個都是 `bk01`、`bk02`、…，規則和 kits（和 dexter）相同：人次照 `visit_id` 排（C locale），組合照出現的順序編號。
  模擬資料上，`design` 和 kits 的 `kits_item()$design` 完全相同（`identical()`）。`kits_subj_table(item)` 的 `pattern_id` 跟著改。
* `kits_original(norm)`：常模留著 cNORM 的 `cnorm.shash()` 結果（留下的那個起始值的解）；照組的常模給每組一個，名字是組。
  只有係數和資料的幾個摘要，沒有孩子的分數。以前會說「`x` must be a result of kits_item(), kits() or kits_subj()」。
  連結和其他沒有原本物件的結果，訊息說得一致（「has no original object」，並說 `kits_code()` 寫出程式）。
* 照組的常模：`attr(norm, "groups")` 的元素有名字（例如 `boy`、`girl`；幾欄時是 `lan.boy`）。
  `kits_code()` 遇到照組的常模時，訊息列出組的名字，並說 `kits_code(attr(norm, "groups")[["boy"]])`。
* `purrr::list_rbind()`、`dplyr::bind_rows()` 疊分數時留著類別（和 kits 一樣用 vctrs 的方法），所以疊起來的分數可以直接給常模的函數。
  以前兩次 `kits_subj()` 的分數疊起來變成一般的 data.frame，常模的函數不收。
  同一種表疊起來留著類別；和一般的 data.frame 疊是 kitsPreview 的表；不同種的表是一般的 data.frame。
  vctrs 加進 Imports。
* 符號：門檻（`thr` 欄）在 `kits_equations()` 寫成 thr_ik，`kits_code()` 的變數也叫 `thr`、`thr_se`（以前是 τ、`tau`）；
  τ（`tau`）只是錨題的 GSED 難度（錨題表的 `tau` 欄不變）。和 kits 的 `kits_equations()` 相同。
* immer 的原本輸出（`summary(item)`、`print(item, original = TRUE)`）不再印電腦的名字和登入的帳號：
  immer 的 `summary()` 印的 `nodename=…`、`login=…` 那一行拿掉，其他照原樣印。
* `kits_code(norm)` 寫的 cNORM 程式和 `kits_norms()` 一樣，只發出留下的那個解的警告，和 delta 太大的警告。
  以前程式會發出沒有留下的起始值的警告（例如「Hessian is not positive definite」），套件不會；數字本來就相同。
* `kits_code(link)` 寫的長向量（點的 `from`、`to`、`id`）換行，每行最多 100 個字；數字照舊是 17 位有效數字，程式跑出的數字和套件逐位相同。
  固定參數的表也一樣換行。
* 題目適配的表（`kits_itemfit()` 和 `item$itemfit`）在 `outfit` 後面多一欄 `flag`（和 kits 相同的規則）：infit 或 outfit 超過 1.4 是 `"underfit"`（兩種都有時算 underfit），
  否則低於 0.6 是 `"overfit"`，否則 `"ok"`（剛好 0.6、1.4 算 ok）；兩個都是 NA 時是 NA。旗標是提醒去看這一題，不是刪題的規則。
  `kits_item()` 印出的那一行分開算：`Item fit: 9 of 24 items outside 0.6-1.4 (2 underfit, 7 overfit: …)`，underfit 的題列在前面，最多列 5 題。
* `kits_item()` 的說明和 README：dexter 和 immer 差多少要看資料（最佳化的方法和收斂標準不同）：
  KIT 語言 M03 的真實資料 9.2e-4，kits 的示範資料 `kits::kitm03` 的語言 0.017（約 0.05 個 SE）。
* `vignette("engines")` 第 7 節拿掉「`dataSrc` 這一版還能用」的舊說法（0.23.0 已經拿掉）。

## 連結的 a、b 改叫 `intercept`、`slope`

* 連結是一條直線：新值 = intercept + slope × 舊值。不再用 a、b。
  * 0.22.0 的 `a` 是截距、`b` 是斜率；Kolen & Brennan、TAM、plink 的 A 是斜率、B 是截距，剛好相反，所以改用名字。
* 連結（`kits_linking()`、`kits_linking_d()`、`kits_link_line()`、`kits_link_error()`）的元素：

| 0.22.0 | 0.23.0 |
|---|---|
| `a`、`b` | `intercept`、`slope` |
| `a_error`、`b_error` | `intercept_error`、`slope_error` |
| `a0`、`b0` | `intercept0`、`slope0` |
| `domains` 表的 `a`、`b` 欄 | `intercept`、`slope` 欄 |
| `kits_link_screen()` 的 `attr(, "link")$a0`、`$b0` | `$intercept0`、`$slope0` |
| `kits_drift()` 的 `a`、`b` | `intercept`、`slope` |

* 舊程式寫 `link$a`、`link$b`（或 `a_error`、`b_error`、`a0`、`b0`，用 `$` 或 `[[`）會停下來，不會靜靜地回傳 `NULL`。
  * 錯誤的類別是 `kitsPreview_error_deprecated`（也是 `kitsPreview_error_input`），訊息說新的名字，並說明 0.22.0 的 `a` 是截距、`b` 是斜率。
  * 0.24.0 拿掉這個檢查。
* 連結沒有任何元素以 a 或 b 開頭，所以 `link$a` 不會被 R 的部分比對抓到別的元素。
* 新增 `coef(link)`：`c(intercept = , slope = )`，順序和 `lm()` 一樣。
* 印出來：

```
<kits_link> new value = intercept + slope x old value
  intercept = 64.04  (linking error 0.19)
  slope     = 6.91  (linking error 0.27)
```

* `kits_equations()`：連結的公式寫一次「intercept、slope 就是 Kolen & Brennan 的 B、A」；要單一字母時用大寫 A（斜率）、B（截距），不用小寫的 a、b。
  共用斜率、各領域截距寫成 A_s、B_d。
* `kits_code()`、`kits_rtmb_code()` 寫出的連結程式用同樣的名字：`mean_sigma()` 回傳 `c(intercept = , slope = )`，
  篩點前的線是 `intercept0`、`slope0`，重抽的 `draws` 的欄是 `intercept`、`slope`，連結誤差是 `intercept_error`、`slope_error`，
  共用斜率是 `slope_s`、各領域截距是 `intercept_d`。
* kits 的連結還記 `vcov`（2×2）和重抽的 `draws`；kitsPreview 的連結沒有這兩個元素（和 0.22.0 一樣）。

## 編號欄：`visit_id`、`subj_id`

| 0.22.0 | 0.23.0 | 意思 |
|---|---|---|
| `person_id` | `visit_id` | 人次：一個孩子的一次施測（長格式、寬格式都一樣） |
| `pid` | `subj_id` | 孩子（每一波都一樣） |

* 作答矩陣的列名、長格式的欄、`persons` 的欄、`kits_subj()` 結果的欄、`kits_age_pass()` 寬的表裡不算題目的欄，都用新的名字。
* 舊的欄名 `person_id`、`pid` 這一版照常認得：照常算，結果相同，給一則訊息，類別是 `kitsPreview_message_data`（也是 `kitsPreview_message`）。下一版拿掉。
* dexter 要的是 `person_id`：kitsPreview 交給 dexter 之前換成 dexter 的名字，結果再換回 `visit_id`。使用者看不到 `person_id`。
* `kits_sim()` 的列名照舊（例如 `p0001`），只是說明改叫 visit_id。

## booklet → 作答組合（item pattern）

* 「哪幾題有作答」叫作答組合（item pattern）；「各波大多數孩子答的那一組」仍叫標準題本（standard form）。
* 結果的欄：

| 0.22.0 | 0.23.0 |
|---|---|
| `booklet_id` | `pattern_id` |
| `booklet_score` | `raw` |
| `booklet_max` | `raw_max` |

* 改名的地方：`kits_subj()` 的結果、`kits_subj_table(item)` 的表（`pattern_id`、`raw`、`theta`、`se`）、`kits_item()` 的 `design`（`pattern_id`、`item_id`）。
* `print(kits_item(...))` 印「item pattern(s)」；說明文件、訊息、`kits_equations()` 的 rir 註解都改成 item pattern。
* `kits_code()`、`kits_rtmb_code()` 的計分程式：dexter 的 `ability()` 之後把欄名換成 `pattern_id`、`visit_id`、`raw`；`raw_max` 取代 `booklet_max`。
* 刻意保留 dexter 名字的地方：原本套件的輸出（`kits_original()`、`summary()`、`print(x, original = TRUE)`）是 dexter 自己的表，照舊是 `person_id`、`booklet_id`、`booklet_score`；說明文件寫清楚了。

## 題目適配放進 `kits_item()`（和 kits 0.23.0 一樣）

適配跟著它描述的物件：個人適配本來就在分數表裡（`infit`、`outfit`），題目適配描述題目，所以放進題目的物件。

* `kits_item()` 在同一份資料上算題目適配，存成 `item$itemfit`：一題一列（`item_id`、`n`、`pvalue`、`rir`、`infit`、`outfit`、`flag`），
  和 `kits_itemfit(data, item)` 完全相同（`identical()`；用同一段程式算）。
  * 只存每題的彙總，沒有一個人次一列的東西。
  * 算不出來時（例如只有一題）`item$itemfit` 是 `NULL`，`item$notes` 說明原因；校準照常完成，不會停下來。
  * 舊的元素都沒有改名；`item$fit` 仍是 dexter 的物件。新增的元素是 `itemfit` 和 `notes`。
* 印出 `kits_item()` 的結果時，最後多一行（範圍和適配圖相同，0.6 到 1.4）：

```
Item fit: infit and outfit within 0.6-1.4 for all 5 items.
Item fit: 2 of 5 items outside 0.6-1.4 (1 underfit, 1 overfit: a5, a1); see x$itemfit, plot(x, type = "fit").
```

  超出的題最多列 5 題，後面加「…」。
* 新增 `plot(item, type = "fit")`：每題的 infit、outfit，0.6 到 1.4 塗灰。
  `kits_item()` 的結果原本沒有 `plot()`；沒給 `type` 時停下來，說要用 `type = "fit"`（Wright map 用 `plot(kits(data))`）。
* `kits()` 的 `$item` 也帶著 `itemfit`，印出時也有這一行。
* `kits_itemfit()` 照舊，用來以同樣的題目檢查別的資料（例如新的世代）；說明文件寫明這一點。
* kitsPreview 沒有 kits 的檢查表（diagnostics），所以沒有「item fit」那一列。
* 所有的數字都沒有變（題目參數、分數、`kits_itemfit()` 都逐位相同）；只有 `kits_item()` 印出來多一行。

## 拿掉的函數和寫法

0.22.0 被取代的函數和寫法（用了會警告）在 0.23.0 拿掉；0.11.0 改名前的引數名 `dataSrc`、`D` 也拿掉。
匯出的函數從 38 個變成 29 個。`?kits_deprecated` 也拿掉。

| 拿掉的（0.22.0 以前） | 新的寫法 |
|---|---|
| `kits_age50(X >= 1, age_m, wave_m)` | `kits_age_pass(X, age_m, wave_m)$p50` |
| `kits_mean_sigma(from, to, id, item, cut, n_boot, seed)` | `points \|> kits_link_screen(cut = ) \|> kits_link_line() \|> kits_link_error(n_boot = , seed = )`，`points = data.frame(id, item, from, to)` |
| `kits_wle_table(item)` | `kits_subj_table(item)` |
| `kits_wle_table(item, design)` | `kits_subj_table(dplyr::filter(parms, item_id %in% items))` |
| `kits_subj_table(X, item, wave_m)`（`forms =` 也一樣） | `kits_subj(X, item, persons) \|> kits_subj_table()` |
| `kits_plot_cohort(waves, scores)` | `plot(kits_vertical(s), type = "median")` |
| `kits_plot_norms(norms, scores)` | `kits_norms(s, by = "sex") \|> plot(scores = s)` |
| `kits_item_positions(items, norm)`、`kits_ladder_marks(norm, scores, ages)` | `kits_item_table(thr, norms, age_pass = , dif = , stems = )` |
| `kits_plot_growth(norm, items, scores)` | `plot(kits_item_table(...))` |
| `kits_plot_ladder(items, columns, marks)` | `plot(kits_item_table(...), type = "ladder", ages = )` |
| `plot(norm, d = D, age_m = age)`、`plot(norm, D, age)` | `plot(norm, scores = s)`，`s` 是有 `theta`、`age_m` 的表 |
| `kits_item(dataSrc = X)`（`kits()`、`kits_subj()`、`kits_itemfit()`、`kits_inter()`、`kits_dif()`、`kits_drift()` 也一樣） | `kits_item(data = X)` 或 `kits_item(X)` |
| `kits_norms(D = d, ...)`（`kits_norm_z()`、`kits_norm_screen()`、`plot()` 常模也一樣） | `kits_norms(d = d, ...)` 或用位置給 |

* 舊的寫法現在停下來，說新的寫法（類別 `kitsPreview_error_input`）：
  * `kits_subj_table(X, item, wave_m)` 說先 `kits_subj(data, item, persons) |> kits_subj_table()`。
  * `plot(norm, d = , age_m = )`、`plot(norm, d, age)` 說用 `scores =`。
  * `dataSrc =`、`D =` 是多給的引數（`unused argument`）。
* `kits_subj_table()` 的引數變成 `(x, forms = c("standard", "all"), ...)`，和 kits 的 `kits_subj_table(x, ...)` 一樣以分數或題目參數開頭。
* 函數仍保留 `...`（和 kits 一樣）；多給的引數停下來，說是哪幾個。

## 數字沒有變

* 所有的數字和 0.22.0 完全相同（容許誤差 0；新的欄名換回舊的名字之後逐位比較）。
* 比過的結果：三個引擎的校準、計分（矩陣、長格式、有 `persons`）、作答組合（`design`）、原始分換算表（從分數、從題目參數、`forms = "all"`）、
  題目適配、交互作用模型、DIF、偏移、世代連結和 D 連結（含連結誤差、`domains`）、連結的三步、篩波次、月齡範圍、常模（向量、分數表、照組）、
  同齡 z、垂直量尺、50% 通過的月齡、題目表。
* 新的 `intercept`、`slope`（和誤差、篩點前的線）和 0.22.0 的 `a`、`b` 逐位相同。
* 比較用模擬資料，兩個版本各跑一次（dexter 用一個執行緒：`OMP_THREAD_LIMIT=1`）。
* `kits_code()`、`kits_rtmb_code()` 寫出的程式改了名字（上面的改名）、第一行的版本號、門檻的變數名（`thr`）、長向量的換行、
  常模的警告；測試確認程式跑出的數字和套件相同。
* 作答組合的編號（`bk01`、…）只是名字：照題目組合對回去，每個組合的題和每個原始分的 θ 都和以前相同。

## 其他

* README 新增「名稱」一節，「被取代的函數」改成「0.23.0 拿掉的函數和寫法」，「引數的差別」對照 kits 0.23.0。
* `vignette("kits-flow")`、`vignette("engines")` 改用新的名字；中文的「題本」只留給標準題本，其他改成作答組合。
* 測試：新增 `test-print23.R`（分數和常模的 `print()`、`n =`、作答組合的編號、`kits_original(norm)`、組的名字、疊表、連結的排法和 `n_boot` 的預設、
  thr 和 τ、immer 的輸出沒有電腦的名字、常模程式的警告、程式的換行）；新增 `test-itemfit-item.R`（題目適配放進 `kits_item()`）；`test-deprecated22.R` 拿掉；`test-rename.R` 改寫成 0.23.0 的改名（拿掉的函數、舊引數名、編號欄和舊欄名的訊息、作答組合、
  `intercept`、`slope`、`link$a` 的錯誤、`coef(link)`、程式和公式的名字）；其他的測試改用新的名字。

# kitsPreview 0.22.0

和 kits 0.22.0 同步：同一件事只留一個入口，兩個套件都有的函數用同樣的寫法。
沒有 kitsPreview 0.21.0；版本號跟著 kits。

## 數字沒有變

* 所有的數字和 0.20.0 完全相同（容許誤差 0）。
* 比過的結果：三個引擎的校準、計分、原始分換算表、題目適配、交互作用模型、DIF、偏移、世代連結和 D 連結（含連結誤差）、篩波次、月齡範圍、常模、同齡 z、50% 通過的月齡、題目的位置、階梯的刻度。
* 比較用模擬資料，兩個版本各跑一次（dexter 用 `OMP_NUM_THREADS=1`；dexter 多執行緒時，同一個版本跑兩次也會差 1e-16）。
* `kits_code()`、`kits_rtmb_code()` 寫出的程式，除了第一行的版本號，一個字都沒有變。

## 舊的寫法 → 新的寫法

舊的寫法這一版還能用，結果和 0.20.0 相同。
用了會警告，類別是 `kitsPreview_warning_deprecated`（也是 `kitsPreview_warning`），訊息說出新的寫法。
0.23.0 拿掉。
完整的說明在 `?kits_deprecated`。

| 舊的寫法（0.20.0） | 新的寫法（0.22.0） | 狀態 |
|---|---|---|
| `kits_age50(X >= 1, age_m, wave_m)` | `kits_age_pass(X, age_m, wave_m)$p50` | 被取代，0.23.0 拿掉 |
| `kits_mean_sigma(from, to, id, item, cut, n_boot, seed)` | `points \|> kits_link_screen(cut = ) \|> kits_link_line() \|> kits_link_error(n_boot = , seed = )`，`points = data.frame(id, item, from, to)` | 被取代，0.23.0 拿掉 |
| `kits_wle_table(item)` | `kits_subj_table(item)` | 被取代，0.23.0 拿掉 |
| `kits_wle_table(item, design)` | `kits_subj_table(dplyr::filter(parms, item_id %in% items))` | 被取代，0.23.0 拿掉 |
| `kits_subj_table(X, item, wave_m)`（`forms =` 也一樣） | `kits_subj(X, item, persons) \|> kits_subj_table()` | 寫法被取代，0.23.0 拿掉 |
| `kits_plot_cohort(waves, scores)` | `plot(kits_vertical(s), type = "median")` | 被取代，0.23.0 拿掉 |
| `kits_plot_norms(norms, scores)` | `kits_norms(s, by = "sex") \|> plot(scores = s)`（幾個領域：`by = c("domain", "sex")`） | 被取代，0.23.0 拿掉 |
| `kits_item_positions(items, norm)`、`kits_ladder_marks(norm, scores, ages)` | `kits_item_table(thr, norms, age_pass = , dif = , stems = )` | 被取代，0.23.0 拿掉 |
| `kits_plot_growth(norm, items, scores)` | `plot(kits_item_table(...))`（`type = "growth"`） | 被取代，0.23.0 拿掉 |
| `kits_plot_ladder(items, columns, marks)` | `plot(kits_item_table(...), type = "ladder", ages = )` | 被取代，0.23.0 拿掉 |
| `plot(norm, d = D, age_m = age)`、`plot(norm, D, age)` | `plot(norm, scores = s)`，`s` 是有 `theta`、`age_m` 的表 | 寫法被取代，0.23.0 拿掉 |
| `kits_subj(X, item)` 之後自己 `merge()` 孩子的資料 | `kits_subj(X, item, persons = persons)` | 新的引數 |
| `kits_norms(s$D, s$age_m, ages, pad = 0)` | 也可以 `kits_norms(s)`：不給 `ages` 時用 `kits_norm_ages()` 的規則（和 `pad = 0`） | 新的寫法 |
| `kits_norm_z(s$D, s$age_m, norm)` | 也可以 `s \|> kits_norm_z(norm)`：分數表多兩欄 `z`、`pct` | 新的寫法 |
| `kits_norm_screen(s$D, s$wave_m, cohort = s$cohort)`、`kits_norm_ages(s$age_m, s$wave_m)` | 也可以 `kits_norm_screen(s)`、`kits_norm_ages(s)` | 新的寫法 |
| 男女各配一次常模，自己疊 | `kits_norms(s, by = "sex")`；數字的分數：`by = data.frame(sex = sex)` | 新的引數 |
| `kits_norm_z()` 用一個常模 | 照組的常模：每個孩子用自己那一組的曲線（分數表要有組的欄；數字給 `by =`） | 新的引數 |
| 表的 `score` 欄（領域） | `domain` 欄；被取代的函數照舊收、給 `score` | 改名 |

## 新的函數（和 kits 0.22.0 同名）

* `kits_age_pass(pass, age_m, wave_m, step = 1, p = 0.5)`：一題一列，`p50`（50% 通過的月齡）、`separated`（完全分離）、`age_last_fail`、`age_first_pass`。
  * `p50` 和 0.20.0 的 `kits_age50()` 完全相同。
  * 收分數（`step` 分以上算通過）或 `TRUE`／`FALSE`；也收寬的表。
  * `p = c(0.5, 0.75)` 一次給幾個比例。
* `kits_link_screen()`、`kits_link_line()`、`kits_link_error()`：連結的三步（`?kits_link_steps`）。
  * 三步的結果和 0.20.0 的 `kits_mean_sigma()` 完全相同。
  * `kits_linking()`、`kits_linking_d()`、`kits_drift()` 也走這三步。
  * `kits_link_screen(from_scale = , to_scale = )` 記下兩端的名字，連結印出來多一行 `scales: x -> y`。
  * kits 多了 `kits_link_points()` 和其他篩點規則（`spread`、`unit`）；kitsPreview 只有 kits 的預設規則。
* `kits_item_table(thr, norms, age_pass, dif, stems)`：一題一列的題目表，帶著常模。
  * `plot()` 畫成長曲線（`type = "growth"`）和成長階梯（`type = "ladder"`、`ages =`）；`items =` 選題號。
  * 兩個世代的門檻可以給一個 list；同一題給兩次時留第一列，並說一聲。
  * 題目的位置和 0.20.0 的 `kits_item_positions()` 完全相同。
* `kits_vertical(s)`：每個世代 × 波次的 `n`、`age_med`、`mean`、`sd`、`median`、`P25`、`P75`。
  * `plot(v, type = "median")` 是以前的 `kits_plot_cohort()`：點在實際月齡的中位數，月齡和波次差超過 1 個月時標題下註明。
  * 幾個領域：`list(lan = v1, motor = v2) |> purrr::list_rbind(names_to = "domain")`，一個領域一格。
  * kits 的 `kits_vertical()` 還有成長、效果量、扣掉測量誤差的 SD 和世代的比較；kitsPreview 只有畫圖要的位置和分散。

## 新的寫法

* `kits_subj(data, item, persons = )`：接上 `pid`、`wave_m`、`age_m`、`sex`、`cohort`（有的幾欄），照 person_id。
  * 沒有 `persons` 時，結果和 0.20.0 完全相同。
  * 分數記得原始分換算表要的東西（題目參數、每個作答組合的題目），所以 `s |> kits_subj_table()` 給每一波的換算表。
  * `filter()` 之後，換算表的人數照留下的列算。
  * 兩個校準的分數疊起來以後，換算表會停下來說清楚；先各自做表，再疊。
* `kits_subj_table(x)`：給分數，是每一波標準題本的表；給題目參數，是每個題本每個原始分的表（以前的 `kits_wle_table()`）。
  * kitsPreview 保留 `forms = "all"`（每個題目組合一張表）；kits 沒有。
  * kitsPreview 不記分數換過的尺，所以表一律在校準的量尺上；kits 的表會照分數換過的尺換。
* 常模的函數收 `kitsPreview::kits_subj()` 的分數（0.11.0 起停下來的寫法，現在和 kits 一樣可以用）。
  * `kits_norms()`、`kits_norm_z()` 改成 S3 generic：數字用 `default` 方法，分數表用 `kitsPreview_subj` 方法。
  * 分數是 `theta` 欄；先換到 D：`s$theta <- d_link$a + d_link$b * s$theta`（和 kits 一樣，換尺後仍叫 `theta`）。
  * 分數表沒有 `age_m`、`wave_m` 欄時，錯誤訊息說要在 `kits_subj()` 給 `persons`。
  * 其他的表（例如 `kits::kits_subj()` 的結果）照舊停下來，叫你給向量。
* `kits_norms(by = )`：每組的常模和只用這組的孩子配的完全相同。
  * 結果是一張表，組的欄在前；`attr(, "by")` 是組的欄名，`attr(, "groups")` 是每組的常模。
  * 一組出錯或警告時，訊息前面有組的名字（例如 `sex = boy:`）。
  * `plot()`：一列一個領域（`domain` 是組的欄時），一欄一個其他的組；`scores =` 的孩子畫在自己那一組。
  * `kits_code()`、`kits_rtmb_code()` 只寫一個常模的程式；照組的常模停下來，叫你給一組（`attr(norm, "groups")[[1]]`）。
* `plot(norm, scores = s)`：孩子畫成灰點（kits 分格畫，不畫單一個孩子）。

## 其他

* `kits_save_plot()` 的例子改用 `plot(kits_vertical(s))`。
* 套件裡面不叫被取代的函數，所以平常用不會有被取代的警告（測試檢查過）。
* `purrr` 從 Suggests 移到 Imports（新的程式用 `purrr::map()` 和 `list_rbind()`）；dplyr 要 1.1.0 以上。
* README 的「引數的差別」對照 kits 0.22.0 重寫，新增「只有 kits 有的」和「被取代的函數」兩節。
* `vignette("kits-flow")` 改用新的寫法：`kits_subj(persons = )`、分數表進常模、`kits_age_pass()`、`kits_item_table()`、`kits_vertical()`、`kits_norms(by = )`、連結的三步。
* 測試：`test-deprecated22.R`（每個被取代的函數和寫法：有警告、結果和新的寫法相同、舊的檢查照舊；平常用沒有警告）、`test-new22.R`（照組的常模、分數表進常模、`persons`、從分數來的換算表、連結的三步、`kits_vertical()`）。
  其他的測試改用新的寫法。

# kitsPreview 0.20.0

kits 和 kitsPreview 從 0.20.0 起用同一個版本號（kitsPreview 從 0.11 直接跳到 0.20）。

## 文件

* 只改文件，程式和數字都沒有變。
* README 的安裝指令改成目前的版本（原本還寫 0.10.0）。
* 「引數都和 kits 相同」改成「主要的引數相同」，README 新增一節「引數的差別」，對照兩個套件的程式列出不同的地方
  （例如 `kits_linking()` 的 `n_boot` 預設值、只有 kits 有的 `scale =`、`persons =`、只有 kitsPreview 有的 `kits_linking_d()`）。
* 「每個估計結果都留著原本套件的物件」改成實際的範圍：`kits_original()` 只收 `kits_item()`、`kits()`、`kits_subj()` 的結果，
  `summary()` 印原本套件的輸出只有 `kitsPreview_item`、`kitsPreview_subj`；常模（cNORM）、DIF、`kits_inter()` 不留原本的物件。
* `?kitsPreview` 補上作答資料也可以是長格式（`person_id`、`item_id`、`item_score`）。
* `vignette("kits-flow")`：套件名稱改成 kitsPreview；`kits_code()` 用的原本套件補上 immer；引用文獻改指 `?kitsPreview` 和 `?kits_item`。
* README 的「SE 1.2–2.6 倍」「約 1e-5 到 1e-3」數字不變，註明是 2026-10-27 用 KIT 真實資料實測。

# kitsPreview 0.11.0

## 引數改名：`dataSrc` → `data`、`D` → `d`（和 kits 0.18.0 一樣）

* 匯出的函數裡，作答資料的引數從 `dataSrc` 改叫 `data`，D 分數的引數從 `D` 改叫 `d`。
* kits 0.18.0 做同樣的改名，所以兩個套件的引數還是一樣，可以並排比較。
* 改名的引數：

| 函數 | 舊 | 新 |
|---|---|---|
| `kits_item()`、`kits()` | `dataSrc` | `data` |
| `kits_subj()`、`kits_subj_table()` | `dataSrc` | `data` |
| `kits_itemfit()`、`kits_inter()` | `dataSrc` | `data` |
| `kits_dif()`、`kits_drift()` | `dataSrc` | `data` |
| `kits_norms()`、`kits_norm_z()`、`kits_norm_screen()` | `D` | `d` |
| `plot()`（`kits_norms()` 的結果） | `D` | `d` |

* 舊名字這一版還能用，結果完全相同，但會發出警告。
  * 警告的類別是 `kitsPreview_warning_deprecated`（也是 `kitsPreview_warning`），訊息說新的名字。
  * 可以用 `withCallingHandlers(..., kitsPreview_warning_deprecated = ...)` 接。
  * 下一版會拿掉舊名字。
* 舊名字放在 `...` 後面，所以只能用完整的名字給（`D = x`、`dataSrc = X`）。
  * 位置和部分比對都接不到舊名字；`D` 和 `d`、`dataSrc` 和 `data` 不會互相搶。
  * 新舊名字都給（例如 `kits_subj(dataSrc = X, item)`：`item` 用位置給，先接到了 `data`），
    會停下來說清楚（類別 `kitsPreview_error_input`），不猜引數的位置。用舊名字時，其他引數也用名字給。
* 原本沒有 `...` 的函數（`kits_subj()`、`kits_subj_table()`、`kits_itemfit()`、`kits_inter()`、`kits_dif()`、
  `kits_drift()`、`kits_norms()`、`kits_norm_z()`、`kits_norm_screen()`）多了 `...`，只是為了放舊名字。
  多給的其他引數和以前一樣會停下來（類別 `kitsPreview_error_input`）。
* 位置引數沒有變：用位置給的程式（例如 `kits_subj(X, item)`、`kits_norms(D, age, ages)`）不用改。
* 呼叫 dexter 自己的函數時，還是用 dexter 的名字（`dexter::fit_enorm(dataSrc = X)`）；只有 kitsPreview 自己的函數改名。

## 常模函數不收表

* kits 的 `kits_norms(s, ages)`、`kits_norm_z(s, norm)` 收 `kits_subj()` 的結果，第二個引數的意思跟著改變。
  kitsPreview 沒有這個寫法，引數的位置一律不變。
* 把表（例如 `kits_subj()` 的結果）當成 `d` 給 `kits_norms()`、`kits_norm_z()`、`kits_norm_screen()` 時，
  現在會停下來說清楚（類別 `kitsPreview_error_input`），叫你給分數和月齡兩個向量；以前的錯誤訊息只說長度不同。

## 其他

* 錯誤訊息裡的引數名稱跟著改（`data`、`d`）。
* `kits_code()`、`kits_rtmb_code()` 寫的常模程式，開頭說明改成「`D`（給 `kits_norms()` 的 `d`）」；
  程式裡的分數仍叫 `D`，和 `kits_equations()` 的符號相同。程式本身和數字都沒有變。
* README、`vignette("engines")` 說明新的引數名稱；`vignette("kits-flow")` 改用 `plot(norm, d = ...)`。
* 數字沒有任何改變：新測試確認新舊名字的結果完全相同，也和 0.10.0 相同。

# kitsPreview 0.10.0

## 類別名稱和 kits 完全分開

* kitsPreview 的每個結果都有自己的類別名稱，以 `kitsPreview_` 開頭。
* 改名的類別：
  * `kits_item()`：`kits_item` → `kitsPreview_item`。
  * `kits()`：`kits` → `kitsPreview_fit`。
  * `kits_subj()`：`kits_subj` → `kitsPreview_subj`。
  * `kits_linking()`、`kits_linking_d()`、`kits_mean_sigma()`：`kits_link` → `kitsPreview_link`。
  * 表（`coef()`、`kits_itemfit()`、`kits_wle_table()` 等）：`kits_table` → `kitsPreview_table`。
  * `kits_norms()`：`kits_norms` → `kitsPreview_norms`。
  * `kits_norm_screen()`：`kits_norm_screen` → `kitsPreview_norm_screen`。
  * `kits_subj_table()`：`kitsPreview_subj_table`（以前只是 `kits_table`）。
  * `kits_inter()`：`kits_inter` → `kitsPreview_inter`。
  * `kits_equations()`：`kits_equations` → `kitsPreview_equations`。
  * `kits_code()`、`kits_rtmb_code()`：`kits_code` → `kitsPreview_code`。
* 錯誤的類別也改名：`kitsPreview_error_missing_package`、`kitsPreview_error_engine_unsupported`（都也是 `kitsPreview_error`）。
* 沒有任何類別繼承 kits 的類別，方法也只登記在 kitsPreview 的類別上。
* 所以同一個 R 裡載入兩個套件，kits 的方法不會被換掉（以前 `coef(item, rp = 0.8)` 會被 kitsPreview 的 `coef()` 蓋掉；見專家審查 A1）。
* 新的測試確認：載入 kitsPreview 之後，`getS3method("coef", "kits_item")` 等方法還是來自 kits。
* 函數名稱、引數、欄位名稱和印出來的樣子都和 kits 相同，所以可以並排比較；只有類別名稱不同。
* kits 的結果不再直接收：要把 kits 的題目參數交給 kitsPreview，給 `coef(item)`。

## 引擎：dexter、immer，和 GSED 的成對估法

* `kits_item()` 的引擎是 `engine = c("dexter", "immer", "ccml")`。
  * `"dexter"`、`"immer"`：CML。
  * `"ccml"`（新）：`immer::immer_ccml()` 的成對條件概似（composite CML；Zwinderman, 1995），GSED 的 D-score 用的方法。
    kitsPreview 自己建 immer 要的設計矩陣 `A`；`nlminb()` 的迭代次數預設提高到 5000，沒收斂時警告。
    `kits_code()` 寫出直接用 immer 的程式；`kits_rtmb_code()` 只有 CML，所以這個引擎會停下來。
    資料符合 PCM 時和 CML 一樣（SE 比較大）；KIT 的真實資料上兩者不一樣，比較在另外的教學文件。
* 拿掉 TAM（`"tam"`，MML、JML）和 sirt（`"sirt"`，EVM）；`wave_m`、`tam_group`、`tam_method` 引數也拿掉。
* `kits_subj()` 一律用 dexter 的 WLE，拿掉 `engine` 引數。
* `kits_linking()` 只留 mean/sigma 加篩點，拿掉 `engine`、`method`、`...` 引數。
* 連結的結果不再有 `engine`、`method`、`original`；連結沒有原本套件的物件，`kits_original(link)` 會說明。
* `kits_code()` 只寫 dexter、immer（和 cNORM）的程式；TAM、sirt 的程式產生器拿掉。
* `kits_rtmb_code()` 給 CML 的引擎（dexter、immer）。
* TAM、sirt 從 Suggests 拿掉。
* TAM 的 MML 和 MML（`group = wave`）的比較，搬到另外的教學文件。

## 文件

* `vignette("engines")` 改成三個引擎的比較：immer 的 `use_rcpp`、為什麼 CML 不需要母體的模型、一段說明 TAM 的 MML 要每個波次一個母體才接近 CML、成對的 CCML（GSED 的方法）怎麼直接用 immer 做。
* `?kits_item` 的說明同樣改寫；README 說明類別名稱、引擎和定位。
* 定位：kitsPreview 是薄、簡單、教學用的包裝，回答「這一步直接用 dexter、immer、cNORM 要怎麼做」。

# kitsPreview 0.9.0

## 定位

* kitsPreview 改成教學用的薄包裝：收集其他套件的估計方法，放在一起比較。
* 每個包裝只跑幾行原本套件的程式，再換成同一種表。
* 讀者應該學到每一步怎麼直接用原本的套件做。
* 最佳化的版本是 kits（RTMB），這次沒有改動。

## 估計引擎（`engine =`）

* `kits_item()` 新增 `engine = c("dexter", "immer", "tam", "sirt")`。
* 預設的 `"dexter"` 和 0.8.0 相同，數字沒有變。
* `"immer"`：`immer::immer_cml()`，CML；和 dexter 相同到最佳化的誤差（約 1e-5）。
* `"tam"`：`TAM::tam.mml()`，MML；積分點預設是 −10 到 10 的 41 個點（TAM 預設是 −6 到 6 的 21 個點）。
* `"tam"` 的能力分佈預設每個波次一個常態（`wave_m`）；沒有 `wave_m` 時每個題本一個常態；`tam_group = "none"` 是全部一個常態。
* `tam_method = "jml"` 改用 `TAM::tam.jml()`，當作教學的對照。
* `"sirt"`：`sirt::rasch.evm.pcm()`，eigenvector method。
* 每個引擎的步驟參數都換到 CML 的原點（所有 β 的和是 0）；減掉的常數記在 `shift`。
* 套件沒有給的 SE 是 NA（TAM 沒有共變異矩陣，所以門檻沒有 SE）。
* `fixed_params` 只有 dexter 和 TAM 收；其他引擎給了會停下來（`kits_error_engine_unsupported`）。
* `kits_subj()` 新增 `engine = c("dexter", "tam")`；TAM 用 `tam.wle()`，θ 相同，SE 沒有 Warm 的校正。
* `kits_linking()` 新增 `engine = c("kits", "tam", "sirt")` 和 `method =`。
* `"tam"` 用 `TAM::tam.linking()`（Haebara、Stocking–Lord）；兩個世代都要用 `engine = "tam"` 校準。
* `"sirt"` 用 `sirt::equating.rasch()`（Haebara、Stocking–Lord、mean/mean），斜率固定是 1。
* 連結的結果一律是 kits 的寫法：新的值 = a + b × 舊的值。
* `kits()` 把 `engine`、`wave_m` 交給 `kits_item()`；計分一律用 dexter 的 WLE。

## 原本套件的結果

* 每個結果都留著套件自己的物件：`kits_item()`、`kits_linking()` 在 `$original`，`kits_subj()` 在 `attr(, "original")`。
* 新函數 `kits_original(x)` 拿出這個物件。
* `summary(x)` 先印整理過的表，再印套件自己的 `summary()`，中間用一行標出來。
* `print(x, original = TRUE)` 一樣。
* `summary()` 只登記在 kitsPreview 自己的類別（`kitsPreview_item`、`kitsPreview_subj`、`kitsPreview_link`），不會蓋掉 kits 的 `summary()`。
* `kits_item()$fit` 還在，是 `$original` 的舊名字。

## 教學

* `kits_code(x)` 依照結果的引擎，寫出原本套件的程式和換算。
* `kits_code()` 也收 `kits_subj()` 的結果和 TAM、sirt 的連結。
* 每一份程式都在測試裡執行過，數字和套件相同。
* `kits_rtmb_code()` 只給 CML 的引擎（dexter、immer）。
* `?kits_item` 的每個引擎段落有原本套件的幾行程式，以及每個套件的原點怎麼換。
* 新的 vignette `engines`：同一份不完整設計的模擬資料，每個引擎跑一次。
* vignette 說明為什麼分組讓 MML 接近 CML，為什麼 KIT 用 CML。

## 相依套件

* immer、TAM、sirt 放在 Suggests；沒有安裝時，錯誤（`kits_error_missing_package`）會說怎麼裝。
* dplyr 加進 Imports；purrr、tidyr 放在 Suggests（vignette 用）。
