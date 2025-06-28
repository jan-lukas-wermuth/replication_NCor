# ============================================================
# Title:      Mean Bias Plots for Confidence Intervals for Gamma*
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script creates the bias plots for which
#             the script Bias.R has simulated data
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
library(here)


# Parameter Specification -------------------------------------------------
DGPs <- c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform")
SampleSizes <- c("50", "200", "800")

# Load Biases ----------------------------------------------------------
load(file = here("results/Simulations/Bias/RegNormal_Bias.RData"))
bias_wermuth_array_RegNormal <- bias_array
load(file = here("results/Simulations/Bias/RegCauchy_Bias.RData"))
bias_wermuth_array_RegCauchy <- bias_array
load(file = here("results/Simulations/Bias/MultNormal_Bias.RData"))
bias_wermuth_array_MultNormal <- bias_array
load(file = here("results/Simulations/Bias/MultCauchy_Bias.RData"))
bias_wermuth_array_MultCauchy <- bias_array
load(file = here("results/Simulations/Bias/3x3SU_Bias.RData"))
bias_wermuth_array_3x3_SU <- bias_array
load(file = here("results/Simulations/Bias/3x3UU_Bias.RData"))
bias_wermuth_array_3x3_UU <- bias_array
invisible(lapply(list.files(here("results/Simulations/True_gammas"), pattern = "\\.RData$", full.names = TRUE), function(x) load(x, envir = globalenv())))

# Facet_grid Preparation --------------------------------------------------
for (DGP in DGPs){
  for (n in SampleSizes) {
    if (DGP == "Regression Normal") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_RegNorm, colMeans(bias_wermuth_array_RegNormal[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Regression Cauchy") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_RegCauchy, colMeans(bias_wermuth_array_RegCauchy[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Multinomial Logit Normal") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_MultNorm, colMeans(bias_wermuth_array_MultNormal[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "Multinomial Logit Cauchy") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_MultCauchy, colMeans(bias_wermuth_array_MultCauchy[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "3 x 3 Skewed Uniform") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_3x3SU, colMeans(bias_wermuth_array_3x3_SU[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
    if (DGP == "3 x 3 Uniform Uniform") {
      assign(paste("data", DGP, n, sep = "_"), data.frame(cbind(t(rbind(gammas_3x3UU, colMeans(bias_wermuth_array_3x3_UU[n,,]))), DGP, n)))
      data.table::setnames(get(paste("data", DGP, n, sep = "_")), c("gamma", "Coverage", "DGP", "n"))
    }
  }
}

df_names <- ls(pattern = "^data") # Find all objects in the environment that start with "data"
df_list <- mget(df_names) # Get the actual data frames
bias_wermuth_array <- do.call(rbind, df_list) # Combine them all
bias_wermuth_array$n <- as.numeric(bias_wermuth_array$n)
bias_wermuth_array$Coverage <- as.numeric(bias_wermuth_array$Coverage)
bias_wermuth_array$gamma <- as.numeric(bias_wermuth_array$gamma)
bias_wermuth_array$n <- replace(bias_wermuth_array$n, bias_wermuth_array$n == 50, "n = 50")
bias_wermuth_array$n <- replace(bias_wermuth_array$n, bias_wermuth_array$n == 200, "n = 200")
bias_wermuth_array$n <- replace(bias_wermuth_array$n, bias_wermuth_array$n == 800, "n = 800")
bias_wermuth_array$DGP <- factor(bias_wermuth_array$DGP, levels = c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform"))
bias_wermuth_array$n <- factor(bias_wermuth_array$n, levels = c("n = 50", "n = 200", "n = 800"))
rownames(bias_wermuth_array) <- NULL

fin_plot <- ggplot2::ggplot(data = bias_wermuth_array) +
  geom_line(mapping = aes(y = Coverage, x = gamma), color = "red") +
  geom_hline(yintercept=c(0.9), col="black") +
  facet_grid(rows = vars(DGP), cols = vars(n)) +
  theme_bw() + theme(panel.spacing = unit(0.9, "lines")) + 
  ylim(c(-0.01,0.41)) + xlim(c(0, 1)) + xlab(bquote("True Gamma"^"*")) + ylab("Mean Bias") + 
  theme(axis.ticks = element_line(color = "black"), legend.position = "bottom")

ggsave(filename = here("results/Plots/Bias.pdf"), device = cairo_pdf, width = 15, height = 30, units = "cm")

fin_plot_wide <- ggplot2::ggplot(data = bias_wermuth_array) +
  geom_line(mapping = aes(y = Coverage, x = gamma), color = "red") +
  geom_hline(yintercept=c(0.9), col="black") +
  facet_grid(rows = vars(n), cols = vars(DGP)) +
  theme_bw() + theme(panel.spacing = unit(0.9, "lines")) + 
  ylim(c(-0.01,0.41)) + xlim(c(0, 1)) + xlab(bquote("True Gamma"^"*")) + ylab("Mean Bias") + 
  theme(axis.ticks = element_line(color = "black"), legend.position = "bottom")

ggsave(filename = here("results/Plots/Bias_wide.pdf"), device = cairo_pdf, width = 30, height = 15, units = "cm")
