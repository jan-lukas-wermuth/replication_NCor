
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
install_github("jan-lukas-wermuth/RCor")
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
  DGP that is described in the paper and a grid of dependence parameters
  $\alpha$.

- CIs_Coverage.R: This script simulates empirical coverage rates of the
  confidence intervals for $\gamma^*$ for the same DGPs as above.

- Bias.R: This script simulates the mean bias of $\widehat{\gamma}^*$
  for the same DGPs as above.

- IndependenceTest.R: This file simulates size and power values for the
  independence test based on $\gamma^*$ and a traditional competitor
  described in the paper. It uses the same DGPs as above.

- Coverage_plots.R: Here, the plots corresponding to the results
  obtained from CIs_Coverage.R are produced.

- Bias_plots.R: Here, the plots corresponding to the results obtained
  from Bias.R are produced.

- Pval_plots.R: Here, the plots corresponding to the results obtained
  from IndependenceTest.R are created.

- IncomeCountryExample.R: This script is meant to be run in LISSY, a
  remote-execution system provided by the Luxembourg Income Study (LIS)
  Database (see the remark in the section on data availability). It
  produces the estimates for $\gamma^*$ as well as its confidence
  intervals for the empirical example concerning the dependence between
  the variables country and income.

- ReligionCountryExample.R: This file computes several correlation
  coefficients for a number of border triangles. It uses data from the
  [World Religion Database](https://www.worldreligiondatabase.org) (see
  the remark in the section on data availability).

- WorldMap.R: Here, the plots corresponding to the results obtained from
  IncomeCountryExample.R and ReligionCountryExample.R are produced.

The files in the subfolder **functions** are called automatically by the
previous scripts and do not have to be run explicitly.

## data

After downloading the dataset from the [World Religion
Database](https://www.worldreligiondatabase.org), please store it in
this folder (see the remark in the section on data availability).

## results

### Plots

This folder contains all plots.

### Simulations

This folder contains all the raw simulation results in three subfolders:
**Coverage**, **IndependenceTest** and **True_gammas**.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-LIS2025" class="csl-entry">

Luxembourg Income Study (LIS) Database. 2025. Luxembourg:
<http://www.lisdatacenter.org>; LIS.

</div>

<div id="ref-WRD2025" class="csl-entry">

Zurlo, Gina A. 2025. “World Religion Database.”
<https://www.worldreligiondatabase.org>.

</div>

</div>
