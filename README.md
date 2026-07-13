# Replication Code: Antitakeover Statutes and Firm Outcomes

This repository contains the R code used to produce all figures and tables in the paper
(*Critical Finance Review*). Raw data are **not** included (see [Data](#data-not-included) below).

## Folder Structure

```
Antitakeover_CFR/
├── Antitakeover_CFR.Rproj      # RStudio project file (root for here::here() paths)
├── README.md
├── renv.lock                   # renv lockfile (R and package versions)
├── .Rprofile                   # activates renv
├── renv/                       # renv infrastructure (library not committed)
└── Code/
    ├── Final Code/             # analysis scripts, run in numerical order
    │   ├── 01. Make Statute Adoption Table.R
    │   ├── 02. Basic DID Design Plot .R
    │   ├── 03. Treatment Timing Plot.R
    │   ├── 04. Replicate KW Table 4.R
    │   ├── 05. Compare Short and Long Models.R
    │   ├── 06a. Make Data (2) - Replication.R
    │   ├── 06b. Make Data (3) - Data Fix.R
    │   ├── 06c. Make Data (4) - Data and Design Fix.R
    │   ├── 07. Make Data Fix Charts.R
    │   ├── 08. Make Event Study Schematic Plot.R
    │   ├── 09. Event Study Plots.R
    │   ├── 10. Stacked Regressions.R
    │   ├── 10b. Stacked Regressions - Other Laws.R
    │   ├── 11. Callaway and Sant'Anna.R
    │   ├── 12. Pre-trend Tests and HonestDiD Taxonomy.R
    │   ├── 13. Moran Single Event - CS.R
    │   └── 14. Minimum Detectable Effects.R
    └── utility_fxs/            # helper functions sourced by scripts 11-14
        ├── getSE.R
        ├── get_agg_inf_func.R
        ├── get_te_dep.R
        ├── mboot.R
        ├── mboot.did.R
        ├── rambachan_roth.R
        ├── reg_did_rc.R
        └── wif.R
```

## Script → Paper Output Mapping

| Script | Output(s) |
|---|---|
| 01. Make Statute Adoption Table | `tables/enactment_table.tex` |
| 02. Basic DID Design Plot | `figures/basic_did.pdf` |
| 03. Treatment Timing Plot | `figures/treatment_timing.pdf` |
| 04. Replicate KW Table 4 | `tables/KW_Table.tex` |
| 05. Compare Short and Long Models | `figures/model_comparison_plot.pdf` |
| 06a–06c. Make Data | `Data/COMPILED/data2.rds`, `data3.rds`, `data4.rds` (analysis datasets) |
| 07. Make Data Fix Charts | `figures/datafix_compare.pdf`; `tables/data_fix.tex`, `tableA2.tex`, `tableA3.tex`, `tableA4.tex` |
| 08. Make Event Study Schematic Plot | `figures/es_schematic.pdf` |
| 09. Event Study Plots | `figures/bc_es.pdf`, `pp_es.pdf`, `figB1.pdf`, `figB2.pdf` |
| 10. Stacked Regressions | `figures/bc_stacked.pdf`, `pp_stacked.pdf`, `figB3.pdf`, `figB4.pdf` |
| 10b. Stacked Regressions - Other Laws | `figures/cs_stacked.pdf`, `fp_stacked.pdf`, `dd_stacked.pdf` |
| 11. Callaway and Sant'Anna | `figures/cs_bc.pdf`, `cs_pp.pdf`; `tables/cs_table_bc.tex`, `cs_table_pp.tex` |
| 12. Pre-trend Tests and HonestDiD Taxonomy | `tables/comparison_table.tex` |
| 13. Moran Single Event - CS | `figures/moran_cs.pdf` |
| 14. Minimum Detectable Effects | `tables/mde_table.tex` |

Note: scripts write figures/tables to a hard-coded Overleaf path
(`dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"` at the top of each script).
Edit that variable to redirect output.

## Package Management (renv)

This project uses [renv](https://rstudio.github.io/renv/) to pin R package versions
(see `renv.lock`). To reproduce the environment:

```r
# from the project root (open Antitakeover_CFR.Rproj)
renv::restore()
```

## Data (not included)

The scripts expect the following inputs, referenced relative to the project root
via `here::here()`:

- `Data/KW/maindata.dta` and `Data/KW/Enactment Dates.dta` — replication data from
  Karpoff and Wittry (2018).
- `Data/MCDONALD/LM_EDGAR_10X_Header_1994_2018.csv` — Loughran–McDonald EDGAR
  10-X header file (used for historical state-of-incorporation fixes).
- `Data/COMPILED/data2.rds`, `data3.rds`, `data4.rds` — built by scripts 06a–06c.
- Compustat/CRSP data pulled from WRDS in scripts 06a–06c, which `source()` a
  `Passwords/Password.R` file (not included) defining WRDS credentials. Create your
  own with your WRDS username/password to run the data-build scripts.
