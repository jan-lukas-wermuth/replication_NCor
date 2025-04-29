# ============================================================
# Title:      Coverage Plots for Confidence Intervals for Gamma*
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script creates the coverage plots for which
#             the script CIs_Coverage.R has simulated data
# ============================================================
rm(list = ls())

library(ggplot2)
library(tidyr)
library(Cairo)
library(abind)
library(dplyr)
library(ks)
library(purrr)
library(latex2exp)


# Parameter Specification -------------------------------------------------
DGPs <- c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform")
SampleSizes <- c("50", "200", "800")
results_folder <- "/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/replication_NCor/results/Plots"
coverage_folder <- "/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/replication_NCor/results/Simulations/Coverage"

# Load Coverages ----------------------------------------------------------
load(file = paste(coverage_folder, "RegNormal_CIs.RData", sep = "/"))
coverage_wermuth_array_RegNormal <- decision_wermuth_array
load(file = paste(coverage_folder, "RegCauchy_CIs.RData", sep = "/"))
coverage_wermuth_array_RegCauchy <- decision_wermuth_array
load(file = paste(coverage_folder, "MultNorm_CIs.RData", sep = "/"))
coverage_wermuth_array_MultNormal <- decision_wermuth_array
load(file = paste(coverage_folder, "MultCauchy_CIs.RData", sep = "/"))
coverage_wermuth_array_MultCauchy <- decision_wermuth_array
load(file = paste(coverage_folder, "3x3SU_CIs.RData", sep = "/"))
coverage_wermuth_array_3x3_SU <- decision_wermuth_array
load(file = paste(coverage_folder, "3x3UU_CIs.RData", sep = "/"))
coverage_wermuth_array_3x3_UU <- decision_wermuth_array
invisible(lapply(list.files("/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/replication_NCor/results/Simulations/True_gammas", pattern = "\\.RData$", full.names = TRUE), function(x) load(x, envir = globalenv())))

# Facet_grid Preparation --------------------------------------------------
for (DGP in DGPs){
  for (n in SampleSizes) {
    if (DGP == "Regression Normal") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_RegNorm, colMeans(coverage_wermuth_array_RegNormal[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Regression Cauchy") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_RegCauchy, colMeans(coverage_wermuth_array_RegCauchy[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Multinomial Logit Normal") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_MultNorm, colMeans(coverage_wermuth_array_MultNormal[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Multinomial Logit Cauchy") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_MultCauchy, colMeans(coverage_wermuth_array_MultCauchy[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "3 x 3 Skewed Uniform") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_3x3SU, colMeans(coverage_wermuth_array_3x3_SU[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "3 x 3 Uniform Uniform") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_3x3UU, colMeans(coverage_wermuth_array_3x3_UU[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
  }
}

df_names <- ls(pattern = "^data") # Find all objects in the environment that start with "data"
df_list <- mget(df_names) # Get the actual data frames
cov_wermuth_array <- do.call(rbind, df_list) # Combine them all
cov_wermuth_array$n <- as.numeric(cov_wermuth_array$n)
cov_wermuth_array$Coverage <- as.numeric(cov_wermuth_array$Coverage)
cov_wermuth_array$gamma <- as.numeric(cov_wermuth_array$gamma)
cov_wermuth_array$n <- replace(cov_wermuth_array$n, cov_wermuth_array$n == 50, "n = 50")
cov_wermuth_array$n <- replace(cov_wermuth_array$n, cov_wermuth_array$n == 200, "n = 200")
cov_wermuth_array$n <- replace(cov_wermuth_array$n, cov_wermuth_array$n == 800, "n = 800")
cov_wermuth_array$DGP <- factor(cov_wermuth_array$DGP, levels = c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform"))
cov_wermuth_array$n <- factor(cov_wermuth_array$n, levels = c("n = 50", "n = 200", "n = 800"))
rownames(cov_wermuth_array) <- NULL

fin_plot <- ggplot2::ggplot(data = cov_wermuth_array) +
  geom_line(mapping = aes(y = Coverage, x = gamma), color = "red") +
  geom_hline(yintercept=c(0.9), col="black") +
  facet_grid(rows = vars(DGP), cols = vars(n)) +
  theme_bw() + theme(panel.spacing = unit(0.9, "lines")) + 
  ylim(c(0,1)) + xlim(c(0, 0.98)) + xlab(bquote("True Gamma"^"*")) + ylab("Empirical Coverage Rate") + 
  theme(axis.ticks = element_line(color = "black"), legend.position = "bottom")

ggsave(filename = paste(results_folder, "Coverage.pdf", sep = "/"), device = cairo_pdf, width = 15, height = 30, units = "cm")
