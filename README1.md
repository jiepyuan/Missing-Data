# Repository Structure

This repository contains the empirical examples and code used in the manuscript.

## `example/`

The `example` folder contains the empirical datasets and analysis scripts used in the manuscript. It is organized into three subfolders corresponding to the three empirical examples:

* `GCM/`: Growth Curve Model (GCM)
* `SEM/`: Structural Equation Model (SEM)
* `regression/`: Regression Model

The empirical datasets included in this repository are subsets of data from:

> Metts, A., Puhlmann, L. M., Zerban, M., Kalisch, R., Zinbarg, R. E., Mineka, S., & Craske, M. G. (2025). Cross-sectional, longitudinal, and dynamic associations among big five personality traits and resilience in primarily female, upper-middle class, ethnically diverse US adolescents. *Clinical Psychological Science, 13*(3), 520–541. [https://doi.org/10.1177/21677026241281312](https://doi.org/10.1177/21677026241281312)

---

## `example/GCM/`

This folder contains the empirical example for the Growth Curve Model (GCM).

### Files

* `YEPPersonality_all_data_122423_nocarelessblv3.csv`

  Empirical dataset extracted from the study by Metts et al. (2025).

* `GCM.R`

  Fits the original dataset directly to the growth curve model without any missing-data treatment.

* `cart-grc.R`

  Applies the Classification and Regression Trees (CART) method implemented in the `mice` package for missing-data imputation and subsequently fits the growth curve model.

* `forest-grc.R`

  Applies the Random Forest imputation method implemented in the `mice` package and subsequently fits the growth curve model.

* `norm-grc.R`

  Applies the Bayesian linear regression imputation method (`norm`) implemented in the `mice` package and subsequently fits the growth curve model.

* `pmm-grc.R`

  Applies the Predictive Mean Matching (PMM) imputation method implemented in the `mice` package and subsequently fits the growth curve model.

---

## `example/SEM/`

This folder contains the empirical example for the Structural Equation Model (SEM).

### Files

* `T1 T15 Big5_YEPResSample2.csv`

  Empirical dataset extracted from the study by Metts et al. (2025).

* `SEM.R`

  Fits the original dataset directly to the structural equation model without any missing-data treatment.

* `cart-sem.R`

  Applies the Classification and Regression Trees (CART) method implemented in the `mice` package for missing-data imputation and subsequently fits the structural equation model.

* `forest-sem.R`

  Applies the Random Forest imputation method implemented in the `mice` package and subsequently fits the structural equation model.

* `norm-sem.R`

  Applies the Bayesian linear regression imputation method (`norm`) implemented in the `mice` package and subsequently fits the structural equation model.

* `pmm-sem.R`

  Applies the Predictive Mean Matching (PMM) imputation method implemented in the `mice` package and subsequently fits the structural equation model.

---

## `example/regression/`

This folder contains the empirical example for the regression model.

### Files

* `YEPPersonality_all_data_122423_nocarelessblv2.csv`

  Empirical dataset extracted from the study by Metts et al. (2025).

* `missing_data_empirical_example.R`

  Performs the regression analyses and compares parameter estimates obtained using the following missing-data handling methods:

  * Listwise deletion
  * Full Information Maximum Likelihood (FIML)
  * MICE-CART
  * MICE-Random Forest
  * MICE-Predictive Mean Matching (PMM)
  * MICE-Bayesian Linear Regression (`norm`)
