# ============================================================
# Title:      True Gamma* for a Grid of Dependence Parameters
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script simulates the true value of Gamma*
#             for every value of rho, the dependence parameter
#             in the DGPs (alpha in the paper).
# ============================================================
rm(list = ls())

library(arrangements)
library(rstatix)
library(foreach)
library(readr)
library(readxl)
library(haven)
library(doParallel)
library(DescTools)
library(compositions)
library(mvtnorm)
library(devtools)
library(here)

# install_github("jan-lukas-wermuth/NCor")
library(NCor)

invisible(lapply(list.files(here("code/functions"), pattern = "\\.R$", full.names = TRUE), source))

# Parameter Specification ------------------------------------------------
MC <- 10 # Monte Carlo replications
n <- 100000 # Sample size
categories <- c("A", "B", "C")  # Categories for the response variable

# Regression Normal -------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 4
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  for (i in 1:MC) {
    print(c(rho, i))
    XY <- Gen_RegressionDGP(n, rho, i, Inf) # Generates data according to specified DGP
    gamma <- NCor(XY[,1], XY[,2], nominal = "r", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_RegNorm <- colMeans(gamma_array)
save(gammas_RegNorm, file = here("gammas_RegNorm.RData"))

# Regression Cauchy -------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^3 * 40
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  for (i in 1:MC) {
    print(c(rho, i))
    XY <- Gen_RegressionDGP(n, rho, i, 1) # Generates data according to specified DGP
    gamma <- NCor(XY[,1], XY[,2], nominal = "r", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_RegCauchy <- colMeans(gamma_array)
save(gammas_RegCauchy, file = here("gammas_RegCauchy.RData"))

# Multinomial Logit Normal ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 9
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  for (i in 1:MC) {
    print(c(rho, i))
    XY <- Gen_MultinomialDGP(n, rho, i, Inf) # Generates data according to specified DGP
    gamma <- NCor(XY[,2], XY[,1], nominal = "r", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_MultNorm <- colMeans(gamma_array)
save(gammas_MultNorm, file = here("gammas_MultNorm.RData"))

# Multinomial Logit Cauchy ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 9
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  for (i in 1:MC) {
    print(c(rho, i))
    XY <- Gen_MultinomialDGP(n, rho, i, 1) # Generates data according to specified DGP
    gamma <- NCor(XY[,2], XY[,1], nominal = "r", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_MultCauchy <- colMeans(gamma_array)
save(gammas_MultCauchy, file = here("gammas_MultCauchy.RData"))

# 3 x 3 Skewed Uniform ----------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 0.04
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  probabilities <- matrix(c(76/300 + 2*rho, 76/300 + 2*rho, 76/300 - 4*rho, 4/100 - rho, 4/100 - rho, 4/100 + 2*rho, 4/100 - rho, 4/100 - rho, 4/100 + 2*rho), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (i in 1:MC) {
    print(c(rho, i))
    contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
    gamma <- NCor(contingency_matrix, nominal = "rc", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_3x3SU <- colMeans(gamma_array)
save(gammas_3x3SU, file = here("gammas_3x3SU.RData"))

# 3 x 3 Uniform Uniform ---------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 / 36
gamma_array <- array(data = NA, dim = c(MC, length(rhos)), dimnames = list(1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos) {
  probabilities <- matrix(c(1/9 + 2*rho, 1/9 + 2*rho, 1/9 - 4*rho, 1/9 - rho, 1/9 - rho, 1/9 + 2*rho, 1/9 - rho, 1/9 - rho, 1/9 + 2*rho), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (i in 1:MC) {
    print(c(rho, i))
    contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
    gamma <- NCor(contingency_matrix, nominal = "rc", CIs = FALSE, Test = FALSE)[[1]]
    gamma_array[i, as.character(rho)] <- gamma
  }
}

gammas_3x3UU <- colMeans(gamma_array)
save(gammas_3x3UU, file = here("gammas_3x3UU.RData"))

