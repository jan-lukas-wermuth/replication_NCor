# ============================================================
# Title:      Size and Power Simulations for Independence Tests
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script simulates the size and power values
#             for all the DGPs considered in the paper.
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

# install_github("jan-lukas-wermuth/NCor")
library(NCor)
setwd("~Dropbox/Pohle Wermuth/NominalCorrelation/replication_NCor")
invisible(lapply(list.files("code/functions", pattern = "\\.R$", full.names = TRUE), source))
results_folder <- "results/Simulations/IndependenceTest"

# Parameter Specifications ------------------------------------------------
MC <- 1000
SampleSizes <- c(50, 200, 800)
categories <- c("A", "B", "C")  # Categories for the response variable

# Regression Normal -------------------------------------------------------
rhos <- c(0, 0.13)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_reg_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_reg <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      XY <- Gen_RegressionDGP(n, rho, i, Inf) # Generates data according to specified DGP
      pval_wermuth[i] <- NCor(XY[,1], XY[,2], nominal = "r", CIs = FALSE, Test = TRUE)[[1]]$PValue
      fstat <- summary(lm(formula = Y ~ C_ind + B_ind, data = as.data.frame(cbind(Y, C_ind, B_ind))))$fstatistic
      pval_reg[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_reg_array[as.character(n),,as.character(rho)] <- pval_reg
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "RegNormal_pval_power.RData", sep = "/"))
save(pval_reg_array, file = paste(results_folder, "RegNormal_pval_reg_power.RData", sep = "/"))

# Regression Cauchy -------------------------------------------------------
rhos <- c(0, 0.24)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_reg_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_reg <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      XY <- Gen_RegressionDGP(n, rho, i, 1) # Generates data according to specified DGP
      pval_wermuth[i] <- NCor(XY[,1], XY[,2], nominal = "r", CIs = FALSE, Test = TRUE)[[1]]$PValue
      fstat <- summary(lm(formula = Y ~ C_ind + B_ind, data = as.data.frame(cbind(Y, C_ind, B_ind))))$fstatistic
      pval_reg[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_reg_array[as.character(n),,as.character(rho)] <- pval_reg
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "RegCauchy_pval_power.RData", sep = "/"))
save(pval_reg_array, file = paste(results_folder, "RegCauchy_pval_reg_power.RData", sep = "/"))

# Multinomial Logit Normal ------------------------------------------------------
rhos <- c(0, 0.135)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_reg_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_wermuth <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      XY <- Gen_MultinomialDGP(n, rho, i, Inf) # Generates data according to specified DGP
      pval_wermuth[i] <- NCor(XY[,2], XY[,1], nominal = "r", CIs = FALSE, Test = TRUE)[[1]]$PValue
      fstat <- summary(lm(formula = X ~ C_ind + B_ind, data = as.data.frame(cbind(X, C_ind, B_ind))))$fstatistic
      pval_reg[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_reg_array[as.character(n),,as.character(rho)] <- pval_reg
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "MultNorm_pval_power.RData", sep = "/"))
save(pval_reg_array, file = paste(results_folder, "MultNorm_pval_reg_power.RData", sep = "/"))

# Multinomial Logit Cauchy ------------------------------------------------------
rhos <- c(0, 0.03)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_reg_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_reg <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      XY <- Gen_MultinomialDGP(n, rho, i, 1) # Generates data according to specified DGP
      pval_wermuth[i] <- NCor(XY[,2], XY[,1], nominal = "r", CIs = FALSE, Test = TRUE)[[1]]$PValue
      fstat <- summary(lm(formula = X ~ C_ind + B_ind, data = as.data.frame(cbind(X, C_ind, B_ind))))$fstatistic
      pval_reg[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_reg_array[as.character(n),,as.character(rho)] <- pval_reg
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "MultCauchy_pval_power.RData", sep = "/"))
save(pval_reg_array, file = paste(results_folder, "MultCauchy_pval_reg_power.RData", sep = "/"))


# 3 x 3 Skewed Uniform ----------------------------------------------------
rhos <- c(0, 0.0033)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_chisq_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_gtest_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  probabilities <- matrix(c(76/300 + 2*rho, 76/300 + 2*rho, 76/300 - 4*rho, 4/100 - rho, 4/100 - rho, 4/100 + 2*rho, 4/100 - rho, 4/100 - rho, 4/100 + 2*rho), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_chisq <- rep(NA, MC)
    pval_gtest <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
      if (i == 772){pval_wermuth[i] <- 0} # i = 772 yields degenerate case
      else {pval_wermuth[i] <- NCor(contingency_matrix, nominal = "rc", CIs = FALSE, Test = TRUE)[[1]]$PValue}
      pval_chisq[i] <- chisq.test(contingency_matrix, correct = FALSE)$p.value
      pval_gtest[i] <- GTest(contingency_matrix)$p.value
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_chisq_array[as.character(n),,as.character(rho)] <- pval_chisq
    pval_gtest_array[as.character(n),,as.character(rho)] <- pval_gtest
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "3x3wermuth_pval_power.RData", sep = "/"))
save(pval_chisq_array, file = paste(results_folder, "3x3chisq_pval_power.RData", sep = "/"))
save(pval_gtest_array, file = paste(results_folder, "3x3gtest_pval_power.RData", sep = "/"))

# 3 x 3 Uniform Uniform ----------------------------------------------------
rhos <- c(0, 0.0055)
pval_wermuth_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_chisq_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array
pval_gtest_array <- array(data = NA, dim = c(length(SampleSizes), MC, length(rhos)), dimnames = list(SampleSizes, 1:MC, rhos)) # Initialize results array

Setup_cluster() # Start cluster for parallel computing
for (rho in rhos){
  probabilities <- matrix(c(1/9 + 2*rho, 1/9 + 2*rho, 1/9 - 4*rho, 1/9 - rho, 1/9 - rho, 1/9 + 2*rho, 1/9 - rho, 1/9 - rho, 1/9 + 2*rho), nrow = 3)
  prob_vector <- as.vector(probabilities)
  for (n in SampleSizes){
    pval_wermuth <- rep(NA, MC)
    pval_chisq <- rep(NA, MC)
    pval_gtest <- rep(NA, MC)
    for (i in 1:MC){
      print(c(rho, n, i))
      contingency_matrix <- Gen_3x3DGP(n, i) # Generates data according to specified DGP
      pval_wermuth[i] <- NCor(contingency_matrix, nominal = "rc", CIs = FALSE, Test = TRUE)[[1]]$PValue
      pval_chisq[i] <- chisq.test(contingency_matrix, correct = FALSE)$p.value
      pval_gtest[i] <- GTest(contingency_matrix)$p.value
    }
    pval_wermuth_array[as.character(n),,as.character(rho)] <- pval_wermuth
    pval_chisq_array[as.character(n),,as.character(rho)] <- pval_chisq
    pval_gtest_array[as.character(n),,as.character(rho)] <- pval_gtest
  }
} # 1 denotes a rejection of the null hypothesis

save(pval_wermuth_array, file = paste(results_folder, "3x3wermuth_pval_uniform_power.RData", sep = "/"))
save(pval_chisq_array, file = paste(results_folder, "3x3chisq_pval_uniform_power.RData", sep = "/"))
save(pval_gtest_array, file = paste(results_folder, "3x3gtest_pval_uniform_power.RData", sep = "/"))






