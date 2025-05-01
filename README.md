
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication: “Proper Correlation Coefficients for Nominal Random Variables”

<!-- badges: start -->
<!-- badges: end -->

This repository contains replication material for the paper “Proper
Correlation Coefficients for Nominal Random Variables” by Jan-Lukas
Wermuth. The corresponding `R` package is available in the repository
[NCor](https://github.com/jan-lukas-wermuth/NCor).

## Installation and Data Availability

In order to run the replication code, please install the `R` package
[NCor](https://github.com/jan-lukas-wermuth/NCor) first.

``` r
install.packages("devtools")
library(devtools)
install_github("jan-lukas-wermuth/NCor")
```

It may be necessary to install further packages from
[CRAN](https://cran.r-project.org). All the necessary packages are
listed at the beginning of each `R` script.

The data used in the empirical application on countries and income is
available in the [Luxembourg Income Study
Database](https://www.lisdatacenter.org). It is impossible to download
the data. Instead, every user has to create an account that allows to
work with the data in
[LISSY](https://www.lisdatacenter.org/data-access/lissy/), a
remote-execution system in which `R` code can be run.

The data for the second empirical application on countries and religions
can be downloaded from the [World Religion
Database](https://www.worldreligiondatabase.org) (only with a
subscription). After downloading, please save the file under the name
“Religionsbycountryin2020.xlsx” in the folder “data”.

## code

This folder contains the `R`code that is needed to compute all the
results in the paper. I give a short overview over the several scripts
in the folder.

- True_gammas.R: This file simulates the values for $\gamma^*$ for each
  DGP that is described in the paper and a grid of dependence
  parameters.

- CIs_Coverage.R: This script simulates empirical coverage rates of the
  confidence intervals for $\gamma^*$ for the same DGPs as above.

- IndependenceTest.R: This file simulates size and power values for the
  independence test based on $\gamma^*$ and a traditional competitor
  described in the paper. It uses the same DGPs as above.

- Coverage_plots.R: Here, the plots corresponding to the results
  obtained from CIs_Coverage.R are produced.

- Pval_plots.R: Here, the plots corresponding to the results obtained
  from IndependenceTest.R are created.

- IncomeCountryExample.R: This script

## data

## results
