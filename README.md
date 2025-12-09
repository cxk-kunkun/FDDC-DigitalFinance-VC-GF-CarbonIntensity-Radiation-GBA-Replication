

# Replication Package README

This repository contains all replication materials for the paper *[Your Paper Title]*. Running the main script reproduces all empirical results.


## 1. File Structure
```
/data
  /rawdata
    df_index.dta                    # Digital Finance (DF) index & sub-indicators (Stata)
    gf_index.dta                    # Green Finance (GF) index (Stata)
    vc_index.dta                    # VC prefecture-level panel (Stata)
    vc_micro_firm.dta               # VC micro firm data (basic unit of VC panel, Stata)
    测度中国数字普惠金融发展_数据编制与空间特....pdf  # DF source literature (includes DF measurement)
    gf_measurement_method.docx      # GF measurement methodology
    绿色金融促进城市经济高质量...272个地级市....pdf  # GF source literature
    绿色金融对农业高质量发展的...槛效应和中介效....pdf  # Supplementary GF literature
    绿色金融对数字经济绿色发展影响效应研究_谢非.pdf  # Supplementary GF literature
    绿色金融与新质生产力：促进...技术创新与环境....pdf  # Supplementary GF literature
  /processed
    panel_data.dta                  # Final processed panel data (includes pre-built weights)
/analysis
  code.do                           # Main script (run this to replicate results)
  wconstructingcode.do              # Weight construction (for inspection only)
/result
  a1.log                            # Full regression result log
README.md                           # This document
```


## 2. Data Description
### (1) Raw Data (/data/rawdata/)
- **DF data**: `df_index.dta` (DF index & sub-indicators). Its measurement and source are in `测度中国数字普惠金融发展_数据编制与空间特....pdf`.
- **GF data**: `gf_index.dta` (GF indicators). Its measurement is in `gf_measurement_method.docx`; source literature is the GF-related papers in this folder.
- **VC data**:
  - `vc_micro_firm.dta`: Micro firm-level VC data (basic unit for aggregation).
  - `vc_index.dta`: Prefecture-level VC panel (aggregated from micro data; rules in the paper).


### (2) Processed Data (/data/processed/)
`panel_data.dta` (final Stata panel data) includes:
- Core variables (DF/GF/VC indicators, energy intensity, GTFP)
- Control variables (economic development, industrial structure, etc.)
- Fixed effect identifiers (city ID, year)
- Pre-built spillover weights (from `wconstructingcode.do`; no re-run needed)


## 3. Code Description
### (1) Main Script (/analysis/code.do)
Run this script to replicate all results. It includes:
1. Import `29.dta`
2. Descriptive statistics
3. Baseline regressions (fixed effects, cluster-robust SE)
4. Heterogeneity/mechanism/robustness tests
5. Export results to `/a1.log`


### (2) Weight Construction Script (/analysis/wconstructingcode.do)
This script documents spillover weight construction (nested/geographic/economic weights).  
**Note**: Weights are pre-included in `29.dta` — this script is for inspection only (no need to run).


## 4. How to Replicate
In Stata, execute:
```stata
do analysis/code.do
```

All results will be saved to `/result/a1.log` (includes coefficients, SE, p-values, etc.).


## 5. Software Requirements
- Stata 15/16/17 (Windows/macOS compatible)
- No extra modules (built-in commands or auto-installed via `ssc install`)


## 6. Citation
Cite the paper:  
[Author Names]. (Year). *[Paper Title]*. *[Journal Name]*, [Volume(Issue)], [Pages].

Acknowledge data sources:
- DF index: Refer to `测度中国数字普惠金融发展_数据编制与空间特....pdf`
- GF index: Refer to `gf_measurement_method.docx` and GF source literature
- VC data: Refer to the paper’s data source statement