# Repository Structure

This repository is organized into two major components:

1. `example/`: empirical examples used in the manuscript.
2. `simulation/`: simulation study, including data generation, analyses, and supplementary results.

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

## `simulation/`

The `simulation` folder contains the code used for the simulation study, including data generation scripts, analysis scripts, and supplementary simulation results.

The folder is organized into three subfolders:

* `data generation/`: Code for generating the simulated datasets.
* `analysis/`: Code for analyzing the simulated datasets.
* `simulation results/`: Supplementary figures and results that are not presented in the manuscript.

---

## `simulation/data generation/`

This folder contains the code used to generate the simulated datasets for the three modeling frameworks.

It is organized into three subfolders:

* `GCM/`: Scripts for generating datasets under the Growth Curve Model (GCM).
* `SEM/`: Scripts for generating datasets under the Structural Equation Model (SEM).
* `regression/`: Scripts for generating datasets under the Regression Model.

Each subfolder contains the code used to simulate datasets according to the corresponding model specification.

---

## `simulation/analysis/`

This folder contains the analysis scripts used for the simulation study.

To avoid redundancy, only the analysis scripts for the regression framework are provided. The analysis procedures for the Structural Equation Model (SEM) and Growth Curve Model (GCM) are conceptually identical and follow the same workflow, differing only in the underlying model specification.

### Files

* `analysis_fiml.R`

  Analyzes the simulated datasets using Full Information Maximum Likelihood (FIML).

* `analysis_cart_default.R`

  Analyzes the simulated datasets using MICE with Classification and Regression Trees (CART) and the default hyperparameter settings.

* `analysis_cart_with_fine_tuning.R`

  Analyzes the simulated datasets using MICE with Classification and Regression Trees (CART), where hyperparameters are optimized through the proposed fine-tuning procedure.

* `analysis_rf_default.R`

  Analyzes the simulated datasets using MICE with Random Forest imputation and the default hyperparameter settings.

* `analysis_rf_with_fine_tuning.R`

  Analyzes the simulated datasets using MICE with Random Forest imputation, where hyperparameters are optimized through the proposed fine-tuning procedure.

### Note

The analysis scripts for the SEM and GCM frameworks are omitted because they implement the same analysis logic and fine-tuning procedures as the regression framework. The only differences concern the underlying statistical model specifications.

---

## `simulation/simulation results/`

This folder contains supplementary simulation results that are not included in the manuscript.

It is organized into three subfolders:

* `GCM/`: Supplementary figures for the Growth Curve Model simulations.
* `SEM/`: Supplementary figures for the Structural Equation Model simulations.
* `regression/`: Supplementary figures for the Regression Model simulations.

These figures provide additional simulation results that complement the findings reported in the manuscript.
