# ============================================================
# Title:      Bias for Gamma*-Estimator for a Grid of Dependence Parameters
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script simulates the bias of two Gamma* estimators
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

invisible(lapply(list.files(here("results/Simulations/True_gammas"), pattern = "\\.RData$", full.names = TRUE), function(x) load(x, envir = globalenv())))
invisible(lapply(list.files(here("code/functions"), pattern = "\\.R$", full.names = TRUE), source))

# Parameter Specifications ------------------------------------------------
MC <- 1000
SampleSizes <- c(50, 200, 800)
categories <- c("A", "B", "C")

# Regression Normal -------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 4
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      XY <- Gen_RegressionDGP(n, rhos[rho], i, Inf) # Generates data according to specified DGP
      ContTable <- table(XY[,1], XY[,2])
      results <- rep(NA, factorial(length(unique(XY[,1]))))
      j <- 0
      dim_r <- length(unique(XY[,1]))
      for (iperm_r in lapply(1:factorial(dim_r), function(k) arrangements::ipermutations(dim_r, dim_r)$collect()[k,])) {
        j <- j + 1
        rownames(ContTable) <- iperm_r
        cases <- rstatix::counts_to_cases(ContTable)
        gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
        results[j] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_RegNorm[rho][[1]]
    }
  }
}

save(bias_array, file = here("results/Simulations/Bias/RegNormal_Bias.RData"))

# Regression Cauchy -------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^3 * 40
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      XY <- Gen_RegressionDGP(n, rhos[rho], i, 1) # Generates data according to specified DGP
      ContTable <- table(XY[,1], XY[,2])
      results <- rep(NA, factorial(length(unique(XY[,1]))))
      j <- 0
      dim_r <- length(unique(XY[,1]))
      for (iperm_r in lapply(1:factorial(dim_r), function(k) arrangements::ipermutations(dim_r, dim_r)$collect()[k,])) {
        j <- j + 1
        rownames(ContTable) <- iperm_r
        cases <- rstatix::counts_to_cases(ContTable)
        gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
        results[j] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_RegCauchy[rho][[1]]
    }
  }
} 

save(bias_array, file = here("results/Simulations/Bias/RegCauchy_Bias.RData"))


# Multinomial Logit Normal ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 9
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      XY <- Gen_MultinomialDGP(n, rhos[rho], i, Inf) # Generates data according to specified DGP
      ContTable <- table(XY[,1], XY[,2])
      results <- rep(NA, factorial(length(unique(XY[,2]))))
      j <- 0
      dim_r <- length(unique(XY[,2]))
      for (iperm_r in lapply(1:factorial(dim_r), function(k) arrangements::ipermutations(dim_r, dim_r)$collect()[k,])) {
        j <- j + 1
        colnames(ContTable) <- iperm_r
        cases <- rstatix::counts_to_cases(ContTable)
        gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
        results[j] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_MultNorm[rho][[1]]
    }
  }
} 

save(bias_array, file = here("results/Simulations/Bias/MultNormal_Bias.RData"))

# Multinomial Logit Cauchy ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 9
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      XY <- Gen_MultinomialDGP(n, rhos[rho], i, 1) # Generates data according to specified DGP
      ContTable <- table(XY[,1], XY[,2])
      results <- rep(NA, factorial(length(unique(XY[,2]))))
      j <- 0
      dim_r <- length(unique(XY[,2]))
      for (iperm_r in lapply(1:factorial(dim_r), function(k) arrangements::ipermutations(dim_r, dim_r)$collect()[k,])) {
        j <- j + 1
        colnames(ContTable) <- iperm_r
        cases <- rstatix::counts_to_cases(ContTable)
        gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
        results[j] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_MultCauchy[rho][[1]]
    }
  }
}

save(bias_array, file = here("results/Simulations/Bias/MultCauchy_Bias.RData"))


# 3 x 3 Skewed Uniform ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 * 0.04
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  probabilities <- matrix(c(76/300 + 2*rhos[rho], 76/300 + 2*rhos[rho], 76/300 - 4*rhos[rho], 4/100 - rhos[rho], 4/100 - rhos[rho], 4/100 + 2*rhos[rho], 4/100 - rhos[rho], 4/100 - rhos[rho], 4/100 + 2*rhos[rho]), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
      dim_r <- nrow(contingency_matrix)
      dim_c <- ncol(contingency_matrix)
      rows <- factorial(dim_r)
      cols <- factorial(dim_c)
      iperm_r <- arrangements::ipermutations(dim_r, dim_r)$collect()
      iperm_c <- arrangements::ipermutations(dim_c, dim_c)$collect()
      results <- matrix(NA, nrow = rows, ncol = cols)
      for (k in 1:rows) {
        for (l in 1:cols) {
          rownames(contingency_matrix) <- iperm_r[k,]
          colnames(contingency_matrix) <- iperm_c[l,]
          cases <- rstatix::counts_to_cases(contingency_matrix)
          gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
          results[k,l] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
        }
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_3x3SU[rho][[1]]
    }
  }
} 

save(bias_array, file = here("results/Simulations/Bias/3x3SU_Bias.RData"))

# 3 x 3 Uniform Uniform ------------------------------------------------------
rhos <- (seq(0, 1, length.out = 100))^2 / 36
bias_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos))

for (rho in seq_along(rhos)){
  probabilities <- matrix(c(1/9 + 2*rhos[rho], 1/9 + 2*rhos[rho], 1/9 - 4*rhos[rho], 1/9 - rhos[rho], 1/9 - rhos[rho], 1/9 + 2*rhos[rho], 1/9 - rhos[rho], 1/9 - rhos[rho], 1/9 + 2*rhos[rho]), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (n in SampleSizes){
    print(c(rho, n))
    for (i in 1:MC){
      contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
      dim_r <- nrow(contingency_matrix)
      dim_c <- ncol(contingency_matrix)
      rows <- factorial(dim_r)
      cols <- factorial(dim_c)
      iperm_r <- arrangements::ipermutations(dim_r, dim_r)$collect()
      iperm_c <- arrangements::ipermutations(dim_c, dim_c)$collect()
      results <- matrix(NA, nrow = rows, ncol = cols)
      for (k in 1:rows) {
        for (l in 1:cols) {
          rownames(contingency_matrix) <- iperm_r[k,]
          colnames(contingency_matrix) <- iperm_c[l,]
          cases <- rstatix::counts_to_cases(contingency_matrix)
          gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
          results[k,l] <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
        }
      }
      res <- max(results)
      bias_array[as.character(n),as.character(i),as.character(rhos[rho])] <- res - gammas_3x3UU[rho][[1]]
    }
  }
}

save(bias_array, file = here("results/Simulations/Bias/3x3UU_Bias.RData"))

