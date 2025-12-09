clear all
set more off
set linesize 200

local DATA "C:\Users\Administrator\Desktop\29.dta"
local desktop_path "C:\Users\Administrator\Desktop"
local log_file "`desktop_path'\a1.txt"

capture log close _all
log using "`log_file'", text replace



/********************************************************************
  变量中英文映射（数据实际变量名 → 论文中的记号）


【基础识别变量】
  城市                     → city          （城市名称，用于合并/描述）
  代码                     → city_id       （城市代码）
  经度                     → lon           （经度，用于空间权重构造）
  纬度                     → lat           （纬度）
  年份                     → year          （年份 t）

【核心被解释变量】
  ln能耗强度               → CI            （Carbon Intensity，城级 CO2/GDP 的对数）

【数字金融主变量及子指标】
  数字金融                 → DF_raw        （原始数字金融指数，北大 DF）
  数字广度                 → Breadth_raw   （覆盖广度子指标）
  数字深度                 → Depth_raw     （使用深度子指标）
  电子化水平               → Digit_raw     （数字化/电子化水平子指标）

  ln数字金融               → lnDF_raw      （数字金融指数对数，若有使用）
  ln数字广度               → lnBreadth_raw
  ln数字深度               → lnDepth_raw
  ln电子化水平             → lnDigit_raw

  —— 论文中：
     DF           → 数字金融（经平滑、去极值、within-city 中心化后的版本）
     Breadth      → 数字广度的处理中版本
     Depth        → 数字深度的处理中版本
     Digitization → 电子化水平/数字化水平的处理中版本

【经济规模与物流】
  地区生产总值             → GDP_raw       （名义或不变价 GDP）
  ln地区生产总值           → lnGDP        （论文 Table 1: lnGDP，全球均值中心化后）
  公路运量                 → Freight_raw   （货运量原始指标）
  ln公路运量               → Freight       （论文 Table 1: Freight = log freight volume）
  ln公路运量源数据         → lnFreight0    （如有原始对数版本）

  —— 论文中：
     Freight      → ln 公路运量（全球中心化）  
     Effres       → Effres（物流效率残差，由 lnFreight 回归 lnGDP 后的 FE 残差）

【产业结构与控制变量】
  产业结构高级化           → IndUp         （IndUp：第三产业/第二产业比，产业升级）
  第二产业占比             → SecShare      （SecShare：第二产业占 GDP 比重）
  对外开放                 → Openness      （开放度控制变量，如进出口/GDP 等）
  财政环境支出             → EnvFiscal     （环境相关财政支出）
  人口                     → Pop_raw       （常住人口）
  人口密度                 → PopDens_raw   （人口密度）
  ln人口密度               → l_PopDens     （人口密度对数）
  金融发展程度             → FinDev        （金融发展指标）
  人力资本水平             → HumanCap      （人力资本）
  环境规则强度             → EnvReg        （环境规制强度）

【VC / GF 及绿色创新机制】
  风险投资额               → VC_amount_raw （风险投资金额原始值）
  ln风险投资额             → VC            （论文 Table 1: VC = log venture capital）
  风险投资数               → VC_count      （项目数量类指标，用于稳健性/机制）

  绿色金融                 → GF            （Green Finance Index，论文 Table 1: GF）
  绿色专利                 → GreenPat_raw  （绿色专利数量）
  ln绿色专利               → GreenPat      （对数绿色专利，用作拓展机制变量）

  战略性新兴产业专利       → StratPat_raw  （战略性新兴产业专利）
  ln战略性新兴产业专利     → StratPat      （对数版本）
  绿色全要素BOM            → GTFP_BOM      （绿色全要素生产率，BOM 方法，稳健性用）

【人口与人才流动】
  高等教育在校人数         → HigherEdu     （高校在校人数，人才存量 proxy）
  人才引进强度             → TalentInflow  （人才引进强度）
  资本要素流动             → CapitalFlow   （资本要素流动）
  人才要素流动             → TalentFlow    （人才要素流动）

【空间权重与双核结构相关变量】
  广州地理距离权重         → W_gz_geo      （广州核心的地理距离权重 wij^Geo, GZ）
  广州经济距离权重         → W_gz_econ     （广州核心的经济距离权重 wij^Eco, GZ）
  广州嵌套权重             → W_gz_nested   （广州嵌套权重：Geo×Eco，Appendix A4）

  深圳地理距离权重         → W_sz_geo      （深圳核心地理距离权重）
  深圳经济距离权重         → W_sz_econ     （深圳核心经济距离权重）
  深圳嵌套权重             → W_sz_nested   （深圳嵌套权重：Geo×Eco）

  汕头嵌套权重             → W_st_nested   （汕头 pseudo-core / 网络节点权重）
  珠海嵌套权重             → W_zh_nested
  韶关嵌套权重             → W_sg_nested
  湛江嵌套权重             → W_zj_nested
  佛山嵌套权重             → W_fs_nested

  —— 论文中：
     Nested (Benchmark)  → wij = wij^Geo × wij^Eco
     Geography / Economic → Appendix A4 的地理/经济矩阵

【IV 与 Bartik 构造相关变量】
  固定电话数               → FixedTel      （某年固定电话总量）
  f                        → TeleIndex     （电话相关指数/中间变量）

  2000 年固定电话总量（户）→ FixedTel2000 （城市层面 2000 年固定电话数）
  2000 年常住人口（人）    → Pop2000       （2000 年常住人口）
  2000 年人均固定电话数    → TelPc2000     （FixedTel2000 / Pop2000）
  每百人固定电话数量       → TelPer100     （每百人固定电话数）
  每百万人邮局数量         → PostPerM      （每百万人邮局数）

  光缆距离                 → FiberDist     （光缆距离/通信基础设施，若用于拓展 IV）
  邮电                     → PostTelecom   （邮电业务总量）

  互联网03年普及率         → Internet03    （2003 年互联网普及率）
  互联网04年普及率         → Internet04
  互联网05年普及率         → Internet05
  互联网06年普及率         → Internet06

  互联网用户数交互电话     → IntUsers_x_Tel  （互联网用户数 × 电话渗透，用于 IV）
  互联网用户数交互邮局     → IntUsers_x_Post （互联网用户数 × 邮局密度，用于 IV）


  人均GDP                 → PGDP_raw     （人均 GDP 原始值)
  ln人均GDP               → lnPGDP       （人均 GDP 对数)

********************************************************************/



use "`DATA'", clear
xtset 代码 年份

local basevars ln能耗强度 绿色全要素BOM ln数字金融 绿色金融 ln地区生产总值 ln公路运量源数据 ln风险投资额 ln绿色专利
foreach v of local basevars {
    capture confirm variable `v'
    if _rc {
        continue
    }
    capture confirm variable `v'_c
    if _rc {
        quietly su `v'
        gen `v'_c = `v' - r(mean)
    }
}

capture drop lnDF_citymean lnDF_wc
bysort 代码: egen lnDF_citymean = mean(ln数字金融)
gen lnDF_wc = ln数字金融 - lnDF_citymean

quietly xtreg ln公路运量源数据 ln地区生产总值_c i.年份, fe
predict ln物流效率_c, residual
quietly su ln物流效率_c
replace ln物流效率_c = ln物流效率_c - r(mean)

local mech_vars_centered 产业结构高级化 第二产业占比
foreach v of local mech_vars_centered {
    capture confirm variable `v'
    if _rc {
        continue
    }
    capture confirm variable `v'_c
    if _rc {
        quietly su `v'
        gen `v'_c = `v' - r(mean)
    }
}

local mech_vars_raw 人才要素流动 资本要素流动
foreach v of local mech_vars_raw {
    capture confirm variable `v'
}

capture program drop mk_basis_z
program define mk_basis_z
    syntax, k(real) [weight_type(string)]
    if "`weight_type'" == "" local weight_type "nested"
    if `k' <= 0 | `k' > 10 {
        error 198
    }
    if !inlist("`weight_type'", "nested", "geo", "econ") {
        error 198
    }
    if "`weight_type'" == "nested" {
        local wgz_var "广州嵌套权重"
        local wsz_var "深圳嵌套权重"
    }
    else if "`weight_type'" == "geo" {
        local wgz_var "广州地理距离权重"
        local wsz_var "深圳地理距离权重"
    }
    else if "`weight_type'" == "econ" {
        local wgz_var "广州经济距离权重"
        local wsz_var "深圳经济距离权重"
    }
    foreach w in `wgz_var' `wsz_var' {
        capture confirm variable `w'
        if _rc {
            error 111
        }
    }
    capture drop DFz_Level DFz_Contrast L1_DFz_Level L1_DFz_Contrast
    capture drop DFz_GZ DFz_SZ L1_DFz_GZ L1_DFz_SZ
    tempvar Wgz_pow min_gz max_gz range_gz
    quietly {
        bysort 年份: egen `min_gz' = min(`wgz_var')
        bysort 年份: egen `max_gz' = max(`wgz_var')
        gen double `range_gz' = `max_gz' - `min_gz'
        replace `range_gz' = 1 if `range_gz' < 1e-9
        gen double `Wgz_pow' = ((`wgz_var' - `min_gz') / `range_gz')^`k'
    }
    tempvar Wsz_pow min_sz max_sz range_sz
    quietly {
        bysort 年份: egen `min_sz' = min(`wsz_var')
        bysort 年份: egen `max_sz' = max(`wsz_var')
        gen double `range_sz' = `max_sz' - `min_sz'
        replace `range_sz' = 1 if `range_sz' < 1e-9
        gen double `Wsz_pow' = ((`wsz_var' - `min_sz') / `range_sz')^`k'
    }
    tempvar Level_Var Contrast_Var
    gen double `Level_Var' = `Wgz_pow' + `Wsz_pow'
    gen double `Contrast_Var' = (`Wsz_pow' - `Wgz_pow') / (`Level_Var' + 1e-6)
    count if missing(lnDF_wc)
    if r(N) > 0 {
        gen DFz_Level = 0
        gen DFz_Contrast = 0
        gen DFz_GZ = 0
        gen DFz_SZ = 0
    }
    else {
        gen double DFz_Level = lnDF_wc * `Level_Var'
        gen double DFz_Contrast = lnDF_wc * `Contrast_Var'
        gen double DFz_GZ = lnDF_wc * `Wgz_pow'
        gen double DFz_SZ = lnDF_wc * `Wsz_pow'
    }
    sort 代码 年份
    by 代码: gen double L1_DFz_Level = DFz_Level[_n-1]
    by 代码: gen double L1_DFz_Contrast = DFz_Contrast[_n-1]
    by 代码: gen double L1_DFz_GZ = DFz_GZ[_n-1]
    by 代码: gen double L1_DFz_SZ = DFz_SZ[_n-1]
end

capture program drop mk_mech_z
program define mk_mech_z
    capture drop Level_mech_* Contrast_mech_* L1_Level_mech_* L1_Contrast_mech_*
    local mech_vars ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    local count_success 0
    foreach mech in `mech_vars' {
        capture confirm variable `mech'
        if _rc {
            continue
        }
        local mech_clean = subinstr("`mech'", " ", "_", .)
        local mech_clean = subinstr("`mech_clean'", "-", "_", .)
        quietly {
            gen double Level_mech_`mech_clean' = DFz_Level * `mech'
            gen double Contrast_mech_`mech_clean' = DFz_Contrast * `mech'
            sort 代码 年份
            by 代码: gen double L1_Level_mech_`mech_clean' = Level_mech_`mech_clean'[_n-1]
            by 代码: gen double L1_Contrast_mech_`mech_clean' = Contrast_mech_`mech_clean'[_n-1]
        }
        local count_success = `count_success' + 1
    }
end

capture program drop get_mech_varlist
program define get_mech_varlist, rclass
    local mech_base ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    local energy_list ""
    local gtfp_list ""
    foreach v of local mech_base {
        local v_clean = subinstr("`v'", " ", "_", .)
        local v_clean = subinstr("`v_clean'", "-", "_", .)
        capture confirm variable Level_mech_`v_clean'
        if !_rc {
            local energy_list `energy_list' Level_mech_`v_clean' Contrast_mech_`v_clean'
            local gtfp_list `gtfp_list' L1_Level_mech_`v_clean' L1_Contrast_mech_`v_clean'
        }
    }
    return local energy "`energy_list'"
    return local gtfp "`gtfp_list'"
end

capture program drop mk_basis_z
program define mk_basis_z
    syntax, k(real) [weight_type(string)]
    if "`weight_type'" == "" local weight_type "nested"
    if `k' <= 0 | `k' > 10 {
        error 198
    }
    if !inlist("`weight_type'", "nested", "geo", "econ") {
        error 198
    }
    if "`weight_type'" == "nested" {
        local wgz_var "广州嵌套权重"
        local wsz_var "深圳嵌套权重"
    }
    else if "`weight_type'" == "geo" {
        local wgz_var "广州地理距离权重"
        local wsz_var "深圳地理距离权重"
    }
    else if "`weight_type'" == "econ" {
        local wgz_var "广州经济距离权重"
        local wsz_var "深圳经济距离权重"
    }
    foreach w in `wgz_var' `wsz_var' {
        capture confirm variable `w'
        if _rc {
            error 111
        }
    }
    capture drop DFz_Level DFz_Contrast L1_DFz_Level L1_DFz_Contrast
    capture drop DFz_GZ DFz_SZ L1_DFz_GZ L1_DFz_SZ
    tempvar Wgz_pow min_gz max_gz range_gz
    quietly {
        bysort 年份: egen `min_gz' = min(`wgz_var')
        bysort 年份: egen `max_gz' = max(`wgz_var')
        gen double `range_gz' = `max_gz' - `min_gz'
        replace `range_gz' = 1 if `range_gz' < 1e-9
        gen double `Wgz_pow' = ((`wgz_var' - `min_gz') / `range_gz')^`k'
    }
    tempvar Wsz_pow min_sz max_sz range_sz
    quietly {
        bysort 年份: egen `min_sz' = min(`wsz_var')
        bysort 年份: egen `max_sz' = max(`wsz_var')
        gen double `range_sz' = `max_sz' - `min_sz'
        replace `range_sz' = 1 if `range_sz' < 1e-9
        gen double `Wsz_pow' = ((`wsz_var' - `min_sz') / `range_sz')^`k'
    }
    tempvar Level_Var Contrast_Var
    gen double `Level_Var' = `Wgz_pow' + `Wsz_pow'
    gen double `Contrast_Var' = (`Wsz_pow' - `Wgz_pow') / (`Level_Var' + 1e-6)
    count if missing(lnDF_wc)
    if r(N) > 0 {
        gen DFz_Level = 0
        gen DFz_Contrast = 0
        gen DFz_GZ = 0
        gen DFz_SZ = 0
    }
    else {
        gen double DFz_Level = lnDF_wc * `Level_Var'
        gen double DFz_Contrast = lnDF_wc * `Contrast_Var'
        gen double DFz_GZ = lnDF_wc * `Wgz_pow'
        gen double DFz_SZ = lnDF_wc * `Wsz_pow'
    }
    sort 代码 年份
    by 代码: gen double L1_DFz_Level = DFz_Level[_n-1]
    by 代码: gen double L1_DFz_Contrast = DFz_Contrast[_n-1]
    by 代码: gen double L1_DFz_GZ = DFz_GZ[_n-1]
    by 代码: gen double L1_DFz_SZ = DFz_SZ[_n-1]
end

capture program drop mk_mech_z
program define mk_mech_z
    capture drop Level_mech_* Contrast_mech_* L1_Level_mech_* L1_Contrast_mech_*
    local mech_vars ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    local count_success 0
    foreach mech in `mech_vars' {
        capture confirm variable `mech'
        if _rc {
            continue
        }
        local mech_clean = subinstr("`mech'", " ", "_", .)
        local mech_clean = subinstr("`mech_clean'", "-", "_", .)
        quietly {
            gen double Level_mech_`mech_clean' = DFz_Level * `mech'
            gen double Contrast_mech_`mech_clean' = DFz_Contrast * `mech'
            sort 代码 年份
            by 代码: gen double L1_Level_mech_`mech_clean' = Level_mech_`mech_clean'[_n-1]
            by 代码: gen double L1_Contrast_mech_`mech_clean' = Contrast_mech_`mech_clean'[_n-1]
        }
        local count_success = `count_success' + 1
    }
end

capture program drop get_mech_varlist
program define get_mech_varlist, rclass
    local mech_base ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    local energy_list ""
    local gtfp_list ""
    foreach v of local mech_base {
        local v_clean = subinstr("`v'", " ", "_", .)
        local v_clean = subinstr("`v_clean'", "-", "_", .)
        capture confirm variable Level_mech_`v_clean'
        if !_rc {
            local energy_list `energy_list' Level_mech_`v_clean' Contrast_mech_`v_clean'
            local gtfp_list `gtfp_list' L1_Level_mech_`v_clean' L1_Contrast_mech_`v_clean'
        }
    }
    return local energy "`energy_list'"
    return local gtfp "`gtfp_list'"
end

capture program drop run_xtreg
program define run_xtreg
    syntax, depvar(string) indepvars(string) [model_name(string) lag(string) sample(string)]
    local new_indepvars `indepvars'
    if "`lag'" == "L1" {
        local new_indepvars = subinstr("`new_indepvars'", "DFz_Level", "L1_DFz_Level", .)
        local new_indepvars = subinstr("`new_indepvars'", "DFz_Contrast", "L1_DFz_Contrast", .)
        local new_indepvars : list uniq new_indepvars
    }
    if "`sample'" != "" {
        xtreg `depvar' `new_indepvars' `controls' `fe_controls' if `sample', fe vce(cluster 代码)
    }
    else {
        xtreg `depvar' `new_indepvars' `controls' `fe_controls', fe vce(cluster 代码)
    }
    if "`model_name'" != "" {
        estimates store `model_name'
    }
end

capture program drop assert_in_model
program define assert_in_model
    syntax, musthave(varlist)
    local miss_vars ""
    foreach v of local musthave {
        capture scalar __chk = _b[`v']
        if _rc {
            local miss_vars "`miss_vars' `v'"
        }
        scalar drop __chk
    }
    if "`miss_vars'" != "" {
        error 9
    }
end

xtset 代码 年份
if _rc {
    error 498
}

mk_basis_z, k(2)
mk_mech_z

get_mech_varlist
local gtfp_mech `r(gtfp)'

run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast `gtfp_mech'") lag("L1") model_name("T2_Mech_GTFP")

assert_in_model, musthave(L1_DFz_Level L1_DFz_Contrast L1_Level_mech_ln物流效率_c L1_Contrast_mech_ln物流效率_c L1_Level_mech_ln风险投资额_c L1_Contrast_mech_ln风险投资额_c)

local param_count = colsof(e(b))
if `param_count' < 18 {
    error 198
}

mk_basis_z, k(2)
mk_mech_z

get_mech_varlist
local energy_mech `r(energy)'
local gtfp_mech `r(gtfp)'

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T1_PA_Energy")

quietly regress ln能耗强度_c DFz_Level DFz_Contrast `controls' i.年份 i.代码
estat vif

run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T1_PB_GTFP")

quietly regress 绿色全要素BOM_c L1_DFz_Level L1_DFz_Contrast `controls' i.年份 i.代码
estat vif

capture program drop wald_dualcore_current
program define wald_dualcore_current
    version 15.1
    qui cap scalar __chk = _b[DFz_Level]
    if !_rc {
        noisily test DFz_Level = DFz_Contrast
    }
    cap scalar drop __chk
    qui cap scalar __chk = _b[L1_DFz_Level]
    if !_rc {
        noisily test L1_DFz_Level = L1_DFz_Contrast
    }
    cap scalar drop __chk
    tempname b
    matrix `b' = e(b)
    local cn : colnames `b'
    local n_pair 0
    local pL1 = length("L1_Level_mech_")
    local pC0 = length("Level_mech_")
    foreach v of local cn {
        if strpos("`v'","L1_Level_mech_")==1 {
            local base = substr("`v'", `pL1'+1, .)
            cap scalar __chk = _b[L1_Contrast_mech_`base']
            if !_rc {
                noisily test L1_Level_mech_`base' = L1_Contrast_mech_`base'
                local n_pair = `n_pair' + 1
            }
            cap scalar drop __chk
        }
        if strpos("`v'","Level_mech_")==1 & strpos("`v'","L1_")==0 {
            local base = substr("`v'", `pC0'+1, .)
            cap scalar __chk = _b[Contrast_mech_`base']
            if !_rc {
                noisily test Level_mech_`base' = Contrast_mech_`base'
                local n_pair = `n_pair' + 1
            }
            cap scalar drop __chk
        }
    }
end

local _models "T1_PA_Energy T1_PB_GTFP T2_Mech_GTFP T2_Mech_GTFP_Fixed"
foreach M of local _models {
    capture estimates restore `M'
    if !_rc {
        wald_dualcore_current
    }
}

capture confirm variable DFz_Level DFz_Contrast L1_DFz_Level L1_DFz_Contrast
if _rc {
    error 111
}

capture drop L1_Level_mech_* L1_Contrast_mech_*

local mech_base ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c

local count_success 0
foreach mech in `mech_base' {
    capture confirm variable `mech'
    if _rc {
        continue
    }
    local mech_clean = subinstr("`mech'", " ", "_", .)
    local mech_clean = subinstr("`mech_clean'", "-", "_", .)
    capture confirm variable Level_mech_`mech_clean'
    if _rc {
        continue
    }
    sort 代码 年份
    by 代码: gen double L1_Level_mech_`mech_clean' = Level_mech_`mech_clean'[_n-1]
    by 代码: gen double L1_Contrast_mech_`mech_clean' = Contrast_mech_`mech_clean'[_n-1]
    local count_success = `count_success' + 1
}

local gtfp_mech_list ""
foreach mech in `mech_base' {
    local mech_clean = subinstr("`mech'", " ", "_", .)
    local mech_clean = subinstr("`mech_clean'", "-", "_", .)
    capture confirm variable L1_Level_mech_`mech_clean'
    if !_rc {
        local gtfp_mech_list `gtfp_mech_list' L1_Level_mech_`mech_clean' L1_Contrast_mech_`mech_clean'
    }
}

if "`gtfp_mech_list'" == "" {
    error 198
}

xtreg 绿色全要素BOM_c L1_DFz_Level L1_DFz_Contrast `gtfp_mech_list' `controls' i.年份, fe vce(cluster 代码)

estimates store T2_Mech_GTFP_Fixed

foreach var in L1_DFz_Level L1_DFz_Contrast {
    capture scalar test_coef = _b[`var']
    if _rc {
        error 198
    }
}

local check_count 0
foreach var of local gtfp_mech_list {
    capture scalar test_coef = _b[`var']
    if !_rc {
        local check_count = `check_count' + 1
    }
    if `check_count' >= 3 break
}

local param_count = colsof(e(b))
if `param_count' < 18 {
    error 198
}

get_mech_varlist

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast `r(energy)'") model_name("T2_Mech_Energy")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast `r(gtfp)'") lag("L1") model_name("T2_Mech_GTFP")

capture drop region_group
gen region_group = 1 if inlist(代码, 3, 11,4)
replace region_group = 2 if inlist(代码, 1, 6, 18,17)
replace region_group = 3 if missing(region_group)

foreach g in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T3_Energy_G`g'") sample("region_group==`g'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T4_GTFP_G`g'") sample("region_group==`g'")
}

capture drop period
gen period = 1 if 年份 <= 2014
replace period = 2 if 年份 >= 2015 & 年份 <= 2018
replace period = 3 if 年份 >= 2019

foreach p in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T5_Energy_P`p'") sample("period==`p'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T6_GTFP_P`p'") sample("period==`p'")
}

capture drop keep_flag
gen keep_flag = 1
replace keep_flag = 0 if 代码==1 | 代码==3

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T7_Energy_Full")
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T7_Energy_NoCore") sample("keep_flag==1")

run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T8_GTFP_Full")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T8_GTFP_NoCore") sample("keep_flag==1")

foreach k in 1 2 3 {
    mk_basis_z, k(`k')
    mk_mech_z
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T9_Energy_k`k'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T10_GTFP_k`k'")
}

mk_basis_z, k(2)
mk_mech_z

mk_basis_z, k(2) weight_type("geo")
mk_mech_z
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T11_Energy_Geo")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T12_GTFP_Geo")

mk_basis_z, k(2) weight_type("econ")
mk_mech_z
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("T13_Energy_Econ")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T14_GTFP_Econ")

mk_basis_z, k(2)
mk_mech_z

local placebo_cities "佛山 汕头 珠海 韶关 湛江"
foreach city of local placebo_cities {
    local weight_var "`city'嵌套权重"
    capture confirm variable `weight_var'
    if _rc {
        continue
    }
    capture drop DFz_Placebo_`city' L1_DFz_Placebo_`city'
    tempvar W_std min_w max_w range_w
    quietly {
        bysort 年份: egen `min_w' = min(`weight_var')
        bysort 年份: egen `max_w' = max(`weight_var')
        gen double `range_w' = `max_w' - `min_w'
        replace `range_w' = 1 if `range_w' < 1e-9
        gen double `W_std' = ((`weight_var' - `min_w') / `range_w')^2
        gen double DFz_Placebo_`city' = lnDF_wc * `W_std'
        sort 代码 年份
        by 代码: gen double L1_DFz_Placebo_`city' = DFz_Placebo_`city'[_n-1]
    }
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast DFz_Placebo_`city'") model_name("T15_Energy_`city'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast L1_DFz_Placebo_`city'") lag("L1") model_name("T16_GTFP_`city'")
}

capture confirm variable ln碳排放_c
if !_rc {
    run_xtreg, depvar("ln碳排放_c") indepvars("DFz_Level DFz_Contrast") model_name("T17_Carbon")
}

capture confirm variable ln绿色创新_c
if !_rc {
    run_xtreg, depvar("ln绿色创新_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("T18_GreenInnov")
}

capture confirm variable 产业结构高级化
if !_rc {
    local controls_ext `controls' 产业结构高级化
    xtreg ln能耗强度_c DFz_Level DFz_Contrast `controls_ext' `fe_controls', fe vce(cluster 代码)
    estimates store T19_Energy_More
    xtreg 绿色全要素BOM_c L1_DFz_Level L1_DFz_Contrast `controls_ext' `fe_controls', fe vce(cluster 代码)
    estimates store T20_GTFP_More
}

capture confirm variable 省份代码
if !_rc {
    xtreg ln能耗强度_c DFz_Level DFz_Contrast `controls' `fe_controls', fe vce(cluster 省份代码)
    estimates store T21_Energy_Prov
}

capture which ivreg2
if _rc {
    ssc install ivreg2, replace
}

capture confirm variable L2_DFz_Level
if _rc {
    sort 代码 年份
    by 代码: gen double L2_DFz_Level = DFz_Level[_n-2]
}

capture confirm variable L2_DFz_Contrast
if _rc {
    sort 代码 年份
    by 代码: gen double L2_DFz_Contrast = DFz_Contrast[_n-2]
}

capture confirm variable L3_DFz_Level
if _rc {
    sort 代码 年份
    by 代码: gen double L3_DFz_Level = DFz_Level[_n-3]
}

capture confirm variable L3_DFz_Contrast
if _rc {
    sort 代码 年份
    by 代码: gen double L3_DFz_Contrast = DFz_Contrast[_n-3]
}

local safe_controls ln地区生产总值_c ln公路运量源数据_c ln风险投资额_c ln绿色专利_c

ivreg2 绿色全要素BOM_c (DFz_Level DFz_Contrast = L2_DFz_Level L3_DFz_Level L2_DFz_Contrast L3_DFz_Contrast) `safe_controls' `fe_controls', cluster(代码) robust first
estimates store IV2SLS_GTFP

quietly ivreg2 绿色全要素BOM_c (DFz_Level DFz_Contrast = L2_DFz_Level L3_DFz_Level L2_DFz_Contrast L3_DFz_Contrast) `safe_controls' `fe_controls', cluster(代码) robust
predict uhat_gtfp, residuals
quietly reg 绿色全要素BOM_c DFz_Level DFz_Contrast uhat_gtfp `safe_controls' `fe_controls', vce(cluster 代码)
test uhat_gtfp
local hausman_p_gtfp = r(p)
drop uhat_gtfp

ivreg2 ln能耗强度_c (DFz_Level DFz_Contrast = L2_DFz_Level L3_DFz_Level L2_DFz_Contrast L3_DFz_Contrast) `safe_controls' `fe_controls', cluster(代码) robust first
estimates store IV2SLS_Energy

quietly ivreg2 ln能耗强度_c (DFz_Level DFz_Contrast = L2_DFz_Level L3_DFz_Level L2_DFz_Contrast L3_DFz_Contrast) `safe_controls' `fe_controls', cluster(代码) robust
predict uhat_energy, residuals
quietly reg ln能耗强度_c DFz_Level DFz_Contrast uhat_energy `safe_controls' `fe_controls', vce(cluster 代码)
test uhat_energy
local hausman_p_energy = r(p)
drop uhat_energy

esttab T1_PA_Energy IV2SLS_Energy using "表_内生性检验_能耗强度.rtf", replace se star(* 0.10 ** 0.05 *** 0.01) b(%9.4f) se(%9.4f) mtitle("FE" "2SLS") stats(N r2 widstat, labels("观测数" "R²" "KP F统计量") fmt(%9.0f %9.4f %9.2f))
esttab T1_PB_GTFP IV2SLS_GTFP using "表_内生性检验_GTFP.rtf", replace se star(* 0.10 ** 0.05 *** 0.01) b(%9.4f) se(%9.4f) mtitle("FE" "2SLS") stats(N r2 widstat, labels("观测数" "R²" "KP F统计量") fmt(%9.0f %9.4f %9.2f))

capture which xsmle
if _rc {
    ssc install xsmle, replace
}

levelsof 代码, local(all_cities)
local N : word count `all_cities'
preserve
keep 代码 广州嵌套权重
duplicates drop
gen origin = 1
gen destination = 代码
gen weight = 广州嵌套权重
keep origin destination weight
tempfile gz_weights
save `gz_weights', replace
restore, preserve
keep 代码 深圳嵌套权重
duplicates drop
gen origin = 3
gen destination = 代码
gen weight = 深圳嵌套权重
keep origin destination weight
tempfile sz_weights
save `sz_weights', replace
use `gz_weights', clear
append using "`sz_weights'"

matrix W_GZ_SZ = J(`N', `N', 0)
local idx = 1
foreach city of local all_cities {
    local city_to_idx_`city' `idx'
    local ++idx
}
count
local obs = r(N)
forvalues i = 1/`obs' {
    if weight[`i'] < . {
        local orig_city = origin[`i']
        local dest_city = destination[`i']
        local w_val = weight[`i']
        local row = `city_to_idx_`orig_city''
        local col = `city_to_idx_`dest_city''
        matrix W_GZ_SZ[`row', `col'] = `w_val'
    }
}
mata:
W = st_matrix("W_GZ_SZ")
row_sums = rowsum(W)
W_normalized = J(rows(W), cols(W), 0)
for (i=1; i<=rows(W); i++) {
    if (row_sums[i] > 0) {
        W_normalized[i,] = W[i,] / row_sums[i]
    }
}
st_matrix("W_GZ_SZ", W_normalized)
end
restore

local depvar "ln能耗强度_c"
local main_regressors "ln数字金融_c ln地区生产总值_c ln公路运量源数据_c ln风险投资额_c ln绿色专利_c 绿色金融_c"
xsmle `depvar' `main_regressors', fe model(sdm) id(代码) time(年份) wmat(W_GZ_SZ) nlag(1) smatrix(W_GZ_SZ) ematrix(W_GZ_SZ) vce(robust)
estimates store SDM_full

test [Spatial]rho = 0

xtset 代码 年份
xtdescribe

capture confirm variable 广州嵌套权重
if _rc {
    error 111
}
capture confirm variable 深圳嵌套权重
if _rc {
    error 111
}

local depvar "ln能耗强度_c"
local main_regressors "ln数字金融_c ln地区生产总值_c ln公路运量源数据_c ln风险投资额_c ln绿色专利_c 绿色金融_c"
xtbalance, range(2011 2022)
marksample touse
markout `touse' `depvar' `main_regressors'
keep if `touse'

xtset 代码 年份
misstable summarize `depvar' `main_regressors'

levelsof 代码, local(all_cities)
local N : word count `all_cities'

preserve
local gz_id = 1
local sz_id = 3
keep 代码 广州嵌套权重
duplicates drop
gen origin = `gz_id'
gen destination = 代码
gen weight = 广州嵌套权重
keep origin destination weight
tempfile gz_weights
save `gz_weights', replace
restore, preserve
keep 代码 深圳嵌套权重
duplicates drop
gen origin = `sz_id'
gen destination = 代码
gen weight = 深圳嵌套权重
keep origin destination weight
tempfile sz_weights
save `sz_weights', replace
use `gz_weights', clear
append using "`sz_weights'"

matrix W_nested_full = J(`N', `N', 0)
local idx = 1
foreach city of local all_cities {
    local city_to_idx_`city' `idx'
    local ++idx
}
count
local obs = r(N)
forvalues i = 1/`obs' {
    if weight[`i'] < . {
        local orig_city = origin[`i']
        local dest_city = destination[`i']
        local w_val = weight[`i']
        local row = `city_to_idx_`orig_city''
        local col = `city_to_idx_`dest_city''
        matrix W_nested_full[`row', `col'] = `w_val'
    }
}
mata:
W = st_matrix("W_nested_full")
row_sums = rowsum(W)
W_normalized = J(rows(W), cols(W), 0)
for (i=1; i<=rows(W); i++) {
    if (row_sums[i] > 0) {
        W_normalized[i,] = W[i,] / row_sums[i]
    }
}
st_matrix("W_nested_full", W_normalized)
end
restore

capture estimates drop SDM_nested_full
xsmle `depvar' `main_regressors', fe model(sdm) id(代码) time(年份) wmat(W_nested_full) nlag(1) smatrix(W_nested_full) ematrix(W_nested_full) vce(robust)
estimates store SDM_nested_full

test [Spatial]rho = 0

capture drop lnDF_citymean lnDF_wc
bysort 代码: egen lnDF_citymean = mean(ln数字广度)
gen lnDF_wc = ln数字广度 - lnDF_citymean

mk_basis_z, k(2)
mk_mech_z

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT1_PA_Energy_数字广度")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT1_PB_GTFP_数字广度")

get_mech_varlist
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast `r(energy)'") model_name("SubT2_Mech_Energy_数字广度")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast `r(gtfp)'") lag("L1") model_name("SubT2_Mech_GTFP_数字广度")

foreach g in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT3_Energy_G`g'_数字广度") sample("region_group==`g'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT4_GTFP_G`g'_数字广度") sample("region_group==`g'")
}

foreach p in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT5_Energy_P`p'_数字广度") sample("period==`p'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT6_GTFP_P`p'_数字广度") sample("period==`p'")
}

capture drop lnDF_citymean lnDF_wc
bysort 代码: egen lnDF_citymean = mean(ln数字深度)
gen lnDF_wc = ln数字深度 - lnDF_citymean

mk_basis_z, k(2)
mk_mech_z

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT1_PA_Energy_数字深度")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT1_PB_GTFP_数字深度")

get_mech_varlist
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast `r(energy)'") model_name("SubT2_Mech_Energy_数字深度")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast `r(gtfp)'") lag("L1") model_name("SubT2_Mech_GTFP_数字深度")

foreach g in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT3_Energy_G`g'_数字深度") sample("region_group==`g'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT4_GTFP_G`g'_数字深度") sample("region_group==`g'")
}

foreach p in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT5_Energy_P`p'_数字深度") sample("period==`p'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT6_GTFP_P`p'_数字深度") sample("period==`p'")
}

capture drop lnDF_citymean lnDF_wc
bysort 代码: egen lnDF_citymean = mean(ln电子化水平)
gen lnDF_wc = ln电子化水平 - lnDF_citymean

mk_basis_z, k(2)
mk_mech_z

run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT1_PA_Energy_电子化水平")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT1_PB_GTFP_电子化水平")

get_mech_varlist
run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast `r(energy)'") model_name("SubT2_Mech_Energy_电子化水平")
run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast `r(gtfp)'") lag("L1") model_name("SubT2_Mech_GTFP_电子化水平")

foreach g in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT3_Energy_G`g'_电子化水平") sample("region_group==`g'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT4_GTFP_G`g'_电子化水平") sample("region_group==`g'")
}

foreach p in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("SubT5_Energy_P`p'_电子化水平") sample("period==`p'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("SubT6_GTFP_P`p'_电子化水平") sample("period==`p'")
}

version 17.0

capture confirm local controls
if _rc local controls
capture confirm local fe_controls
if _rc local fe_controls

capture program drop __clean_name
program define __clean_name, rclass
    syntax, name(string)
    local nm = subinstr("`name'", " ", "_", .)
    local nm = subinstr("`nm'", "-", "_", .)
    local nm = subinstr("`nm'", "（", "(", .)
    local nm = subinstr("`nm'", "）", ")", .)
    return local clean "`nm'"
end

capture program drop __default_mech_list
program define __default_mech_list, rclass
    local mech_vars ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    return local mech "`mech_vars'"
end

capture program drop mk_level_decomp
program define mk_level_decomp, rclass
    syntax [, LAG(string) MECHVARS(string) ]
    if "`lag'" == "" local lag "none"
    if "`lag'" == "L1" {
        local xL "L1_DFz_Level"
        local xC "L1_DFz_Contrast"
    }
    else {
        local xL "DFz_Level"
        local xC "DFz_Contrast"
    }
    foreach v in `xL' `xC' {
        capture confirm variable `v'
        if _rc {
            error 111
        }
    }
    xtset
    if _rc {
        error 498
    }
    if "`mechvars'" == "" {
        __default_mech_list
        local mechvars `r(mech)'
    }
    by 代码: egen mean_`xL' = mean(`xL')
    gen double dev_`xL' = `xL' - mean_`xL'
    by 代码: egen mean_`xC' = mean(`xC')
    gen double dev_`xC' = `xC' - mean_`xC'
    local list_B0 ""
    local list_BX ""
    local list_W ""
    local mundlak_means "mean_`xL' mean_`xC'"
    local created ""
    foreach m of local mechvars {
        capture confirm variable `m'
        if _rc {
            continue
        }
        local m_use "`m'"
        if "`lag'" == "L1" {
            capture confirm variable L1_`m'
            if _rc {
                sort 代码 年份
                by 代码: gen double L1_`m' = `m'[_n-1]
            }
            local m_use "L1_`m'"
        }
        __clean_name, name("`m'")
        local mc `r(clean)'
        by 代码: egen mean_`m_use' = mean(`m_use')
        gen double dev_`m_use' = `m_use' - mean_`m_use'
        local mundlak_means `mundlak_means' mean_`m_use'
        capture drop B0_Lv_mech_`mc' B1_Lv_mech_`mc' B2_Lv_mech_`mc' W_Lv_mech_`mc'
        gen double B0_Lv_mech_`mc' = mean_`xL' * mean_`m_use'
        gen double B1_Lv_mech_`mc' = mean_`xL' * dev_`m_use'
        gen double B2_Lv_mech_`mc' = dev_`xL'  * mean_`m_use'
        gen double W_Lv_mech_`mc'  = dev_`xL'  * dev_`m_use'
        capture drop B0_Ct_mech_`mc' B1_Ct_mech_`mc' B2_Ct_mech_`mc' W_Ct_mech_`mc'
        gen double B0_Ct_mech_`mc' = mean_`xC' * mean_`m_use'
        gen double B1_Ct_mech_`mc' = mean_`xC' * dev_`m_use'
        gen double B2_Ct_mech_`mc' = dev_`xC'  * mean_`m_use'
        gen double W_Ct_mech_`mc'  = dev_`xC'  * dev_`m_use'
        local list_B0 `list_B0' B0_Lv_mech_`mc' B0_Ct_mech_`mc'
        local list_BX `list_BX' B1_Lv_mech_`mc' B2_Lv_mech_`mc' B1_Ct_mech_`mc' B2_Ct_mech_`mc'
        local list_W  `list_W'  W_Lv_mech_`mc'  W_Ct_mech_`mc'
        local created `created' `m_use'
    }
    return local B0  "`list_B0'"
    return local BW  "`list_BX'"
    return local W   "`list_W'"
    return local mundlak "`mundlak_means'"
    return local created_mech "`created'"
end

capture program drop run_cre_level
program define run_cre_level
    syntax, DEPVAR(string) [LAG(string) MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    mk_level_decomp, lag("`lag'")
    local B0   `r(B0)'
    local BW   `r(BW)'
    local W    `r(W)'
    local mean `r(mean_main)'
    local dev  `r(dev_main)'
    local mund `r(mundlak)'
    xtreg `depvar' `dev' `mean' `B0' `BW' `W' `mund' `controls' i.年份, re vce(cluster 代码)
    if "`model_name'" != "" {
        estimates store `model_name'
    }
end

capture program drop run_fe_level
program define run_fe_level
    syntax, DEPVAR(string) [LAG(string)] [MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    mk_level_decomp, lag("`lag'")
    local BW `r(BW)'
    local W  `r(W)'
    local dev `r(dev_main)'
    xtreg `depvar' `dev' `BW' `W' `controls' i.年份, fe vce(cluster 代码)
    if "`model_name'" != "" {
        estimates store `model_name'
    }
end

capture program drop run_be_level
program define run_be_level
    syntax, DEPVAR(string) [LAG(string)] [MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    preserve
        mk_level_decomp, lag("`lag'")
        local B0 `r(B0)'
        collapse (mean) `depvar' `B0' `controls', by(代码)
        regress `depvar' `B0' `controls', vce(robust)
        if "`model_name'" != "" {
            estimates store `model_name'
        }
    restore
end

capture confirm local controls
if _rc local controls
capture confirm local fe_controls
if _rc local fe_controls

capture program drop __clean_name
program define __clean_name, rclass
    syntax, name(string)
    local nm = subinstr("`name'", " ", "_", .)
    local nm = subinstr("`nm'", "-", "_", .)
    local nm = subinstr("`nm'", "（", "(", .)
    local nm = subinstr("`nm'", "）", ")", .)
    return local clean "`nm'"
end

capture program drop __default_mech_list
program define __default_mech_list, rclass
    local mech_vars ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    return local mech "`mech_vars'"
end

capture program drop mk_level_decomp
program define mk_level_decomp, rclass
    syntax [, LAG(string) MECHVARS(string) ]
    if "`lag'" == "" local lag "none"
    if "`lag'" == "L1" {
        local xL "L1_DFz_Level"
        local xC "L1_DFz_Contrast"
    }
    else {
        local xL "DFz_Level"
        local xC "DFz_Contrast"
    }
    foreach v in `xL' `xC' {
        capture confirm variable `v'
        if _rc {
            error 111
        }
    }
    xtset
    if _rc {
        error 498
    }
    if "`mechvars'" == "" {
        __default_mech_list
        local mechvars `r(mech)'
    }
    by 代码: egen mean_`xL' = mean(`xL')
    gen double dev_`xL' = `xL' - mean_`xL'
    by 代码: egen mean_`xC' = mean(`xC')
    gen double dev_`xC' = `xC' - mean_`xC'
    local list_B0 ""
    local list_BX ""
    local list_W ""
    local mundlak_means "mean_`xL' mean_`xC'"
    local created ""
    foreach m of local mechvars {
        capture confirm variable `m'
        if _rc {
            continue
        }
        local m_use "`m'"
        if "`lag'" == "L1" {
            capture confirm variable L1_`m'
            if _rc {
                sort 代码 年份
                by 代码: gen double L1_`m' = `m'[_n-1]
            }
            local m_use "L1_`m'"
        }
        __clean_name, name("`m'")
        local mc `r(clean)'
        by 代码: egen mean_`m_use' = mean(`m_use')
        gen double dev_`m_use' = `m_use' - mean_`m_use'
        local mundlak_means `mundlak_means' mean_`m_use'
        capture drop B0_Lv_mech_`mc' B1_Lv_mech_`mc' B2_Lv_mech_`mc' W_Lv_mech_`mc'
        gen double B0_Lv_mech_`mc' = mean_`xL' * mean_`m_use'
        gen double B1_Lv_mech_`mc' = mean_`xL' * dev_`m_use'
        gen double B2_Lv_mech_`mc' = dev_`xL'  * mean_`m_use'
        gen double W_Lv_mech_`mc'  = dev_`xL'  * dev_`m_use'
        capture drop B0_Ct_mech_`mc' B1_Ct_mech_`mc' B2_Ct_mech_`mc' W_Ct_mech_`mc'
        gen double B0_Ct_mech_`mc' = mean_`xC' * mean_`m_use'
        gen double B1_Ct_mech_`mc' = mean_`xC' * dev_`m_use'
        gen double B2_Ct_mech_`mc' = dev_`xC'  * mean_`m_use'
        gen double W_Ct_mech_`mc'  = dev_`xC'  * dev_`m_use'
        local list_B0 `list_B0' B0_Lv_mech_`mc' B0_Ct_mech_`mc'
        local list_BX `list_BX' B1_Lv_mech_`mc' B2_Lv_mech_`mc' B1_Ct_mech_`mc' B2_Ct_mech_`mc'
        local list_W  `list_W'  W_Lv_mech_`mc'  W_Ct_mech_`mc'
        local created `created' `m_use'
    }
    return local B0 "`list_B0'"
    return local BW "`list_BX'"
    return local W "`list_W'"
    return local mundlak "`mundlak_means'"
    return local created_mech "`created'"
end

capture program drop run_cre_level
program define run_cre_level
    syntax, DEPVAR(string) [LAG(string)] [MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    mk_level_decomp, lag("`lag'")
    local B0   `r(B0)'
    local BW   `r(BW)'
    local W    `r(W)'
    local mean `r(mean_main)'
    local dev  `r(dev_main)'
    local mund `r(mundlak)'
    xtreg `depvar' `dev' `mean' `B0' `BW' `W' `mund' `controls' i.年份, re vce(cluster 代码)
    if "`model_name'" != "" estimates store `model_name'
end

capture program drop run_fe_level
program define run_fe_level
    syntax, DEPVAR(string) [LAG(string)] [MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    mk_level_decomp, lag("`lag'")
    local BW `r(BW)'
    local W  `r(W)'
    local dev `r(dev_main)'
    xtreg `depvar' `dev' `BW' `W' `controls' i.年份, fe vce(cluster 代码)
    if "`model_name'" != "" estimates store `model_name'
end

capture program drop run_be_level
program define run_be_level
    syntax, DEPVAR(string) [LAG(string)] [MODEL_name(string)]
    if "`lag'" == "" local lag "none"
    preserve
        mk_level_decomp, lag("`lag'")
        local B0 `r(B0)'
        collapse (mean) `depvar' `B0' `controls', by(代码)
        regress `depvar' `B0' `controls', vce(robust)
        if "`model_name'" != "" estimates store `model_name'
    restore
end

cap program drop mech_sig_console
program define mech_sig_console, rclass
    version 15.1
    syntax , MODEL(name) [LAGGed]
    quietly estimates restore `model'
    if _rc {
        exit 301
    }
    local mech_list ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c
    local prefL = cond("`lagged'"!="","L1_Level_mech_","Level_mech_")
    local prefC = cond("`lagged'"!="","L1_Contrast_mech_","Contrast_mech_")
    tempname df
    scalar `df' = .
    capture scalar `df' = e(df_r)
    local hasdf = (c(rc)==0)
    local sig5  = 0
    local sig10 = 0
    foreach m of local mech_list {
        local mc = subinstr("`m'"," ","_",.)
        local mc = subinstr("`mc'","-","_",.)
        capture scalar bL  = _b[`prefL'`mc']
        capture scalar seL = _se[`prefL'`mc']
        if !_rc {
            scalar tL = bL/seL
            scalar pL = cond(`hasdf', 2*ttail(`df',abs(tL)), 2*normal(-abs(tL)))
        }
        capture scalar bC  = _b[`prefC'`mc']
        capture scalar seC = _se[`prefC'`mc']
        if !_rc {
            scalar tC = bC/seC
            scalar pC = cond(`hasdf', 2*ttail(`df',abs(tC)), 2*normal(-abs(tC)))
        }
        capture {
            local flag5  = (pL<0.05 | pC<0.05)
            local flag10 = (pL<0.10 | pC<0.10)
        }
        if "`flag5'"  == "1" local sig5  = `sig5'  + 1
        if "`flag10'" == "1" local sig10 = `sig10' + 1
    }
    return scalar sig5  = `sig5'
    return scalar sig10 = `sig10'
end

tabstat ln能耗强度_c 绿色全要素BOM_c DFz_Level DFz_Contrast L1_DFz_Level L1_DFz_Contrast lnDF_wc ln地区生产总值_c ln公路运量源数据_c ln风险投资额_c ln绿色专利_c 绿色金融_c ln物流效率_c 产业结构高级化_c 第二产业占比_c 人才要素流动 资本要素流动 ln数字广度 ln数字深度 ln电子化水平, statistics(N mean sd min max skewness kurtosis) columns(statistics) longstub

summarize ln能耗强度_c 绿色全要素BOM_c DFz_Level DFz_Contrast, detail

local mech_vars "ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 人才要素流动 资本要素流动 产业结构高级化_c 第二产业占比_c"
local core1 "DFz_Level"
local core2 "DFz_Contrast"
local core1_lag "L1_DFz_Level"
local core2_lag "L1_DFz_Contrast"
local ctrl "ln地区生产总值_c ln公路运量源数据_c"
local fe "i.代码 i.年份"
local cluster "cluster(代码)"
local dep_energy "ln能耗强度_c"
local dep_gtfp "绿色全要素BOM_c"

capture program drop med_test
program define med_test
    syntax, med(varname) core(varname) dep(varname)
    xtreg `med' `core' `ctrl' `fe', fe `cluster'
    local b1 = _b[`core']
    local p1 = 2*ttail(e(df_r), abs(`b1'/_se[`core']))
    xtreg `dep' `core' `med' `ctrl' `fe', fe `cluster'
    local b2 = _b[`med']
    local p2 = 2*ttail(e(df_r), abs(`b2'/_se[`med']))
    local ind_eff = `b1'*`b2'
end

foreach m of local mech_vars {
    med_test, med(`m') core(`core1') dep(`dep_energy')
}

foreach m of local mech_vars {
    med_test, med(`m') core(`core2') dep(`dep_energy')
}

foreach m of local mech_vars {
    med_test, med(`m') core(`core1_lag') dep(`dep_gtfp')
}

foreach m of local mech_vars {
    med_test, med(`m') core(`core2_lag') dep(`dep_gtfp')
}

mk_basis_z, k(2)
mk_mech_z

foreach p in 1 2 3 {
    run_xtreg, depvar("ln能耗强度_c") indepvars("DFz_Level DFz_Contrast") model_name("Mech_Energy_P`p'") sample("period==`p'")
    run_xtreg, depvar("绿色全要素BOM_c") indepvars("DFz_Level DFz_Contrast") lag("L1") model_name("Mech_GTFP_P`p'") sample("period==`p'")
}

estimates table Mech_Energy_P1 Mech_Energy_P2 Mech_Energy_P3, b(%9.4f) se stats(N r2)
estimates table Mech_GTFP_P1 Mech_GTFP_P2 Mech_GTFP_P3, b(%9.4f) se stats(N r2)

local energy_mechs "ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 产业结构高级化_c 第二产业占比_c"
local gtfp_mechs    "ln物流效率_c ln风险投资额_c ln绿色专利_c 绿色金融_c 产业结构高级化_c 第二产业占比_c"

foreach region in 1 2 3 {
    foreach mech in `energy_mechs' {
        bootstrap, reps(1000) seed(12345): xtreg ln能耗强度_c DFz_Level DFz_Contrast `mech' ln地区生产总值_c ln公路运量源数据_c i.年份 if region_group==`region', fe
        est store boot_`region'_`mech'
        local a = _b[DFz_Level]
        local b = _b[`mech']
        local indirect = `a'*`b'
    }
}

foreach region in 1 2 3 {
    foreach mech in `gtfp_mechs' {
        bootstrap, reps(1000) seed(12345): xtreg 绿色全要素BOM_c L1_DFz_Level L1_DFz_Contrast `mech' ln地区生产总值_c ln公路运量源数据_c i.年份 if region_group==`region', fe
        est store boot_gtfp_`region'_`mech'
        local a = _b[L1_DFz_Level]
        local b = _b[`mech']
        local indirect = `a'*`b'
    }
}

foreach mech in `energy_mechs' {
    bootstrap, reps(500): xtreg `mech' DFz_Level DFz_Contrast ln地区生产总值_c ln公路运量源数据_c i.年份, fe
    local a = _b[DFz_Level]
    local a_se = _se[DFz_Level]
    bootstrap, reps(500): xtreg ln能耗强度_c DFz_Level DFz_Contrast `mech' ln地区生产总值_c ln公路运量源数据_c i.年份, fe
    local b = _b[`mech']
    local b_se = _se[`mech']
    local indirect = `a'*`b'
    local se_indirect = sqrt(`b'^2*`a_se'^2 + `a'^2*`b_se'^2)
    local z = `indirect'/`se_indirect'
    local p = 2*normal(-abs(`z'))
}



log close