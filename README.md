# Regime-Dependent Value-at-Risk and Risk Spillovers in Multivariate Cryptocurrency Markets

R code and data for the paper

> Soni, A., Deb, S. and Das, D. *Regime-Dependent Value-at-Risk and Risk Spillovers in Multivariate Cryptocurrency Markets.* (under review)

The paper detects common structural breaks in a five-cryptocurrency price panel with a t-SNE + CUSUM binary-segmentation procedure. It then estimates 1-day-ahead Value-at-Risk within each regime using ARMA-EGARCH and GAS models with several residual distributions, backtests the forecasts (Kupiec, Christoffersen, quadratic probability score), and measures risk spillovers with group transfer entropy.

## Repository structure

```
.
├── README.md
├── data/
│   └── Cryptocurrency6.csv       # daily closing prices, 01/12/2016 - 04/12/2021
├── R/
   ├── 00_setup.R                # paths, packages, helpers (run this first)
   ├── 01_change_points.R
   ├── 02_descriptive_stats_subsamples.R
   ├── 03a_garch_BTC.R ... 03e_garch_ETC.R
   ├── 03f_var_series_1pct.R
   ├── 04_garch_mape_rmse.R
   ├── 05_garch_without_regressors.R
   ├── 06_gas_models.R
   ├── 07_qps_garch.R
   ├── 08_qps_garch_without_regressors.R
   ├── 09_qps_gas.R
   ├── 10_actual_error_garch_gas.R
   ├── 11_tables_garch.R
   ├── 12_tables_gas.R
   ├── 13_tables_qps_ae.R
   ├── 14_figures_var_garch.R
   ├── 15_figures_var_gas.R
   ├── 16_transfer_entropy.R
   └── functions/
       ├── strucbreak.R          # t-SNE + CUSUM change-point function CW.Anchal.multi()
       ├── transfer_entropy.R    # (group) transfer entropy functions
       ├── cp_pca.R              # PCA benchmark (first principal component)
       ├── cp_pca_squared.R      # PCA benchmark (squared PC1)
       └── cp_pca_abs.R          # PCA benchmark (absolute PC1)

```

## Data

`data/Cryptocurrency6.csv` contains the daily closing prices (USD) of Bitcoin (BTC), Ethereum (ETH), Litecoin (LTC), Monero (XMR) and Ethereum Classic (ETC) from 1 December 2016 to 4 December 2021 (1,830 observations). The prices were downloaded from [Investing.com](https://in.investing.com/crypto/currencies).

| Column | Description |
|---|---|
| `Date` | trading date, format `dd/mm/yyyy` |
| `Bitcoin`, `Ethereum`, `Litecoin`, `Monero`, `Ethereum Classic` | daily closing price in USD |

All models use daily log returns, r<sub>t</sub> = log(P<sub>t</sub> / P<sub>t-1</sub>).

## How to reproduce the results

1. Install R (version 4.x) and the required packages:

   ```r
   install.packages(c("tidyverse", "ggpubr", "gridExtra", "fBasics", "readr",
                      "MLmetrics", "forecast", "rugarch", "fGarch", "Johnson",
                      "SuppDists", "PerformanceAnalytics", "xts", "cvar", "Dowd",
                      "tseries", "Rtsne", "FinTS", "PearsonDS", "gsl", "xtable",
                      "e1071", "RTransferEntropy", "GAS"))
   ```

   If a package has since been removed from CRAN, install the archived version with `remotes::install_version("<package>", version = "<version>")`. The versions used for the paper are listed in `sessionInfo.txt`.

2. Open R (or RStudio) with the repository folder as the working directory, since all paths are relative to it. Then run the scripts **in this order, in the same R session**, because later scripts use objects created by earlier ones:

   ```r
   scripts <- c("00_setup.R",
                "01_change_points.R", "02_descriptive_stats_subsamples.R",
                "03a_garch_BTC.R", "03b_garch_ETH.R", "03c_garch_LTC.R",
                "03d_garch_XMR.R", "03e_garch_ETC.R", "03f_var_series_1pct.R",
                "04_garch_mape_rmse.R", "05_garch_without_regressors.R",
                "06_gas_models.R", "07_qps_garch.R",
                "08_qps_garch_without_regressors.R", "09_qps_gas.R",
                "10_actual_error_garch_gas.R", "11_tables_garch.R",
                "12_tables_gas.R", "13_tables_qps_ae.R",
                "14_figures_var_garch.R", "15_figures_var_gas.R",
                "16_transfer_entropy.R")
   for (s in scripts) source(file.path("R", s), print.eval = TRUE)
   ```

   You can also open the scripts one by one and run them in the same order.

A full run takes several hours, mostly because of the rolling-window re-estimation of the GARCH and GAS models.

* Figures are written to `output/figures/`, and CSV tables to `output/tables/`. The folder is created automatically and is not part of the repository.
* The LaTeX versions of the tables, produced by `xtable()`, are printed to the R console.

## Where each result comes from

| Paper | Script(s) |
|---|---|
| Table 1: descriptive statistics and time-series tests, full sample | `03a`–`03e` (tests), `11_tables_garch.R` (table) |
| Figure 3: prices with the two common breaks | `01_change_points.R` (breaks), `11_tables_garch.R` (plot) |
| Section 4.1: t-SNE breaks and comparison with PCA-based breaks | `01_change_points.R`, `R/functions/strucbreak.R`, `R/functions/cp_pca*.R` |
| Table 3: Kupiec and Christoffersen tests, EGARCH | `03a`–`03e`, `11_tables_garch.R` |
| Table 4: Kupiec and Christoffersen tests, GAS | `06_gas_models.R`, `12_tables_gas.R` |
| Table 5: QPS, EGARCH and GAS | `03f_var_series_1pct.R` (1% VaR series), `07_qps_garch.R`, `09_qps_gas.R`, `13_tables_qps_ae.R` |
| Table 6 and Table A.1: subsample descriptive statistics | `02_descriptive_stats_subsamples.R` (subsamples taken from the breaks found in `01`) |
| Table A.2: AIC, EGARCH | `03a`–`03e`, `11_tables_garch.R` |
| Tables A.3–A.7: EGARCH coefficients | `03a`–`03e`, `11_tables_garch.R` |
| Table A.8: RMSE, EGARCH | `04_garch_mape_rmse.R` |
| Tables A.9–A.10: AIC and RMSE, GAS | `06_gas_models.R`, `12_tables_gas.R` |
| Figure B.1: 1% VaR forecasts, EGARCH | `03f_var_series_1pct.R`, `14_figures_var_garch.R` |
| Figure B.2: VaR forecasts, GAS | `15_figures_var_gas.R` |
| Table 7: effective group transfer entropy | `16_transfer_entropy.R` |
| Figure 4: group transfer entropy with 90% intervals | `16_transfer_entropy.R` |
| Figure 5: pairwise transfer entropy | `16_transfer_entropy.R` |

Additional scripts:

* `05_garch_without_regressors.R` and `08_qps_garch_without_regressors.R` fit the full-sample EGARCH models **without** the structural-break dummies, for comparison with the break-adjusted models.
* `10_actual_error_garch_gas.R` computes the ratio of actual to expected VaR exceedances.

## Methods in brief

* **Structural breaks:** the five price series are reduced to one dimension with t-SNE (`Rtsne`). The univariate series is split into blocks of 100 observations, a smoothed periodogram is computed for each block, and CUSUM-based binary segmentation is applied to locate two breaks. The detected breaks are at observations 300 and 1300, i.e. 26 September 2017 and 22 June 2020 (`CPPP.tsne` in `01_change_points.R`). These breaks define the subsamples SS1–SS3 used in all later scripts.
* **EGARCH:** ARMA(1,1)-EGARCH(1,1) (`rugarch`) with normal, skewed Student-t, skewed GED and Johnson SU errors. Pearson type IV (`PearsonDS`) is fitted to the standardised residuals of the normal model. Models are estimated on the full sample (with break dummies) and on each subsample.
* **GAS:** univariate GAS models (`GAS`) with normal, Student-t, skewed Student-t and asymmetric Student-t errors.
* **Backtesting:** Kupiec POF and Christoffersen independence tests, followed by the quadratic probability score.
* **Risk spillovers:** effective group transfer entropy (Dimpfl and Peter, 2018, 2019) on the 1% VaR series of each subsample.
  * The VaR series are discretised into three bins at the 5/95, 10/90 or 15/85 quantiles.
  * Effective TE is TE minus the mean TE over 100 shuffles. As in the authors' original `Shuffled_TE_n3()` routine, which produced the published results, the shuffles resample the first two source series (`TE_SHUFFLE = "original"`). Setting `TE_SHUFFLE = "all"` resamples every source series instead.
  * Standard errors come from 400 moving-block bootstrap replications with block length round(n^(1/5)).
  * `R/functions/transfer_entropy.R` is a base-R re-implementation of the authors' procedure. On the same discretised data it gives TE values identical to their original `TransEnt()` function.
  * Pairwise transfer entropy uses `RTransferEntropy::transfer_entropy()`.
  * Shuffles and bootstrap draws are random, so re-running can change the third decimal slightly.

## References for the transfer entropy method

* Dimpfl, T. and Peter, F. J. (2018). Analyzing volatility transmission using group transfer entropy. *Energy Economics*, 75, 368–376.
* Dimpfl, T. and Peter, F. J. (2019). Group transfer entropy with an application to cryptocurrencies. *Physica A*, 516, 543–551.

## License

See `LICENSE`.

## Contact

Anchal Soni, Indian Institute of Management Bangalore
Soudeep Deb (corresponding author), soudeep@iimb.ac.in
