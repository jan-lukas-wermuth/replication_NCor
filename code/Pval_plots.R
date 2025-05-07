# ============================================================
# Title:      Size and Power Plots for Independence Tests
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script creates the power plots for which
#             the script IndependenceTest.R has simulated data
# ============================================================
rm(list = ls())

library(ggplot2)
library(tidyr)
library(Cairo)
library(abind)
library(dplyr)
library(ks)
library(purrr)

setwd("~Dropbox/Pohle Wermuth/NominalCorrelation/replication_NCor")

# Parameter Specification -------------------------------------------------
results_folder <- "results/Plots"
sizepower_folder <- "results/Simulations/IndependenceTest"

# Load Size/Power Values for Gamma*-Test --------------------------------------------------
load(file = paste(sizepower_folder, "RegNormal_pval.RData", sep = "/"))
pval_wermuth_array_RegNormal <- as.data.frame(as.table(pval_wermuth_RegNorm)) %>% mutate(DGP = "Regression Normal") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "RegCauchy_pval.RData", sep = "/"))
pval_wermuth_array_RegCauchy <- as.data.frame(as.table(pval_wermuth_RegCauchy)) %>% mutate(DGP = "Regression Cauchy") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "MultNorm_pval.RData", sep = "/"))
pval_wermuth_array_MultNorm <- as.data.frame(as.table(pval_wermuth_MultNorm)) %>% mutate(DGP = "Multinomial Logit Normal") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "MultCauchy_pval.RData", sep = "/"))
pval_wermuth_array_MultCauchy <- as.data.frame(as.table(pval_wermuth_MultCauchy)) %>% mutate(DGP = "Multinomial Logit Cauchy") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "3x3wermuth_pval.RData", sep = "/"))
pval_wermuth_array_3x3 <- as.data.frame(as.table(pval_wermuth_3x3)) %>% mutate(DGP = "3 x 3 Skewed Uniform") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "3x3wermuth_uniform_pval.RData", sep = "/"))
pval_wermuth_array_3x3_uniform <- as.data.frame(as.table(pval_wermuth_3x3uniform)) %>% mutate(DGP = "3 x 3 Uniform Uniform") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)

# Facet_grid Preparation --------------------------------------------------
pval_wermuth_array <- rbind(pval_wermuth_array_RegNormal, pval_wermuth_array_RegCauchy, pval_wermuth_array_MultNorm, pval_wermuth_array_MultCauchy, pval_wermuth_array_3x3, pval_wermuth_array_3x3_uniform)
pval_wermuth_array$SampleSize <- as.numeric(as.character(pval_wermuth_array$SampleSize))
pval_wermuth_array$SampleSize <- replace(pval_wermuth_array$SampleSize, pval_wermuth_array$SampleSize == 50, "n = 50")
pval_wermuth_array$SampleSize <- replace(pval_wermuth_array$SampleSize, pval_wermuth_array$SampleSize == 200, "n = 200")
pval_wermuth_array$SampleSize <- replace(pval_wermuth_array$SampleSize, pval_wermuth_array$SampleSize == 800, "n = 800")
pval_wermuth_array$DGP <- factor(pval_wermuth_array$DGP, levels = c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform"))
pval_wermuth_array$SampleSize <- factor(pval_wermuth_array$SampleSize, levels = c("n = 50", "n = 200", "n = 800"))
pval_wermuth_array_0 <- pval_wermuth_array %>% subset(rho == 0)

kde_data <- pval_wermuth_array_0 %>%
  group_split(DGP, SampleSize) %>%  # Split data by both groups
  map(\(.x) {
    kde_fit <- kde.boundary(.x$Freq, xmin = 0, xmax = 1)  # Compute KDE
    data.frame(
      DGP = unique(.x$DGP), 
      SampleSize = unique(.x$SampleSize), 
      x = kde_fit$eval.points, 
      y = kde_fit$estimate
    )
  }) %>%
  list_rbind() %>%  # Combine all KDE data
  filter(x > 0.05 & x < 0.95)  # Restrict KDE range

ggplot(pval_wermuth_array_0, aes(x = Freq)) + 
  geom_histogram(breaks = seq(0, 1, by = 0.1), fill = "skyblue", color = "black", aes(y = after_stat(density))) +
  geom_line(data = kde_data, aes(x = x, y = y, group = interaction(DGP, SampleSize)), color = "red", linewidth = 1.2) +
  facet_grid(cols = vars(SampleSize), rows = vars(DGP)) +
  theme_bw() + theme(panel.spacing = unit(0.9, "lines")) +
  labs(x = "P-Value", y = "Density")
ggsave(filename = "/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/Results/Plots/Hist_PVal_0.pdf", device = cairo_pdf, width = 15, height = 30, units = "cm")

# Load Size/Power Values for global F-Test (for nom-cont) or G-Test/Chisq-Test (for nom-nom) --------------------------------------------------
rm(list = ls())
load(file = paste(sizepower_folder, "RegNormal_pval_reg.RData", sep = "/"))
pval_reg_array_RegNormal <- as.data.frame(as.table(pval_reg_array)) %>% mutate(DGP = "Regression Normal") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "RegCauchy_pval_reg.RData", sep = "/"))
pval_reg_array_RegCauchy <- as.data.frame(as.table(pval_reg_array)) %>% mutate(DGP = "Regression Cauchy") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "MultNorm_pval_reg.RData", sep = "/"))
pval_reg_array_MultNorm <- as.data.frame(as.table(pval_reg_array)) %>% mutate(DGP = "Multinomial Logit Normal") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "MultCauchy_pval_reg.RData", sep = "/"))
pval_reg_array_MultCauchy <- as.data.frame(as.table(pval_reg_array)) %>% mutate(DGP = "Multinomial Logit Cauchy") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "3x3chisq_pval.RData", sep = "/"))
pval_chisq_array_3x3 <- as.data.frame(as.table(pval_chisq_array)) %>% mutate(DGP = "3 x 3 Skewed Uniform") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)
load(file = paste(sizepower_folder, "3x3chisq_pval_uniform.RData", sep = "/"))
pval_chisq_array_3x3_uniform <- as.data.frame(as.table(pval_chisq_array)) %>% mutate(DGP = "3 x 3 Uniform Uniform") %>% rename(SampleSize = Var1, MC = Var2, rho = Var3)

# Facet_grid Preparation --------------------------------------------------
pval_reg_array <- rbind(pval_reg_array_RegNormal, pval_reg_array_RegCauchy, pval_reg_array_MultNorm, pval_reg_array_MultCauchy, pval_chisq_array_3x3, pval_chisq_array_3x3_uniform)
pval_reg_array$SampleSize <- as.numeric(as.character(pval_reg_array$SampleSize))
pval_reg_array$SampleSize <- replace(pval_reg_array$SampleSize, pval_reg_array$SampleSize == 50, "n = 50")
pval_reg_array$SampleSize <- replace(pval_reg_array$SampleSize, pval_reg_array$SampleSize == 200, "n = 200")
pval_reg_array$SampleSize <- replace(pval_reg_array$SampleSize, pval_reg_array$SampleSize == 800, "n = 800")
pval_reg_array$DGP <- factor(pval_reg_array$DGP, levels = c("Regression Normal", "Regression Cauchy", "Multinomial Logit Normal", "Multinomial Logit Cauchy", "3 x 3 Skewed Uniform", "3 x 3 Uniform Uniform"))
pval_reg_array$SampleSize <- factor(pval_reg_array$SampleSize, levels = c("n = 50", "n = 200", "n = 800"))
pval_reg_array_0 <- pval_reg_array %>% subset(rho == 0) %>% na.omit()
pval_reg_array_03 <- pval_reg_array %>% subset(rho == 0.3 | rho == 0.01)
pval_reg_array_06 <- pval_reg_array %>% subset(rho == 0.6)

kde_data <- pval_reg_array_0 %>%
  group_split(DGP, SampleSize) %>%  # Split data by both groups
  map(\(.x) {
    kde_fit <- kde.boundary(.x$Freq, xmin = 0, xmax = 1)  # Compute KDE
    data.frame(
      DGP = unique(.x$DGP), 
      SampleSize = unique(.x$SampleSize), 
      x = kde_fit$eval.points, 
      y = kde_fit$estimate
    )
  }) %>%
  list_rbind() %>%  # Combine all KDE data
  filter(x > 0.05 & x < 0.95)  # Restrict KDE range

ggplot(pval_reg_array_0, aes(x = Freq)) + 
  geom_histogram(breaks = seq(0, 1, by = 0.1), fill = "skyblue", color = "black", aes(y = after_stat(density))) +
  geom_line(data = kde_data, aes(x = x, y = y, group = interaction(DGP, SampleSize)), color = "red", linewidth = 1.2) +
  facet_grid(cols = vars(SampleSize), rows = vars(DGP)) +
  theme_bw() + theme(panel.spacing = unit(0.9, "lines")) +
  labs(x = "P-Value", y = "Density")
ggsave(filename = "/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/Results/Plots/Hist_PVal_0_reg.pdf", device = cairo_pdf, width = 15, height = 30, units = "cm")