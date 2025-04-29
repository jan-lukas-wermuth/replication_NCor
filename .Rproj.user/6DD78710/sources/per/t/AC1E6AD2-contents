# ============================================================
# Title:      Data Example Country vs Income
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script is meant to be run in LISSY, a 
#             remote-execution system in which data from the
#             Luxembourg Income Database (LIS) can be accessed.
#             It computes the correlation between the variables
#             country and income for a number of European
#             border triangles.
# ============================================================

rm(list = ls())

library(foreach)
library(doParallel)
# library(arrangements)
library(rstatix)
library(dplyr)
library(class)
library(DescTools)

RCor <- function(X, Y, alpha = 0.1, method = "gamma", IID = TRUE, Fisher = TRUE){
  if (!(is.numeric(X) && is.numeric(Y) && length(X) == length(Y))){
    stop("`X` and `Y` must be numeric vectors of the same length", call. = FALSE)
  }
  n <- length(X)
  if (method == "tau"){
    tau_info <- DescTools:::.DoCount(X, Y)
    tau <- (tau_info$C - tau_info$D) / choose(n, 2)
    tau_fis <- atanh(tau)
    X_TieProb3 <- sum((table(X)/length(X))^3)
    Y_TieProb3 <- sum((table(Y)/length(Y))^3)
    if (isTRUE(IID)){
      # Define functions
      G_XY <- Vectorize(function(x_val, y_val) (mean(X <= x_val & Y <= y_val) + mean(X <= x_val & Y < y_val) + mean(X < x_val & Y <= y_val) + mean(X < x_val & Y < y_val)) / 4)
      G_X <- Vectorize(function(x_val) (mean(X < x_val) + mean(X <= x_val)) / 2)
      G_Y <- Vectorize(function(y_val) (mean(Y < y_val) + mean(Y <= y_val)) / 2)
      
      # Calculate Marc's variance estimator
      var_hat <- 4 * mean((4 * G_XY(X, Y) - 2 * (G_X(X) + G_Y(Y)) + 1 - tau)^2)
      # Variance under independence assumption
      var_hat_ind <- 4/9 * (1 - X_TieProb3) * (1 - Y_TieProb3)
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * tau / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * tau / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      var_hat <- Tau_LRV(X, Y, tau)
      var_hat_ind <- Tau_ind_LRV(X, Y, bandwidth = "Dehling")
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * tau / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * tau / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(tau + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), tau + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(tau_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - tau^2)), tanh(tau_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - tau^2)))
    }
    res <- dplyr::tribble(~Tau, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          tau, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
  if (method == "tau_b"){
    tau_b <- stats::cor(X, Y, method = "kendall")
    tau_b_fis <- atanh(tau_b)
    if (isTRUE(IID)){
      tau_info <- DescTools:::.DoCount(X, Y)
      tau <- (tau_info$C - tau_info$D) / choose(n, 2)
      taux_info <- DescTools:::.DoCount(X, X)
      taux <- (taux_info$C - taux_info$D) / choose(n, 2)
      tauy_info <- DescTools:::.DoCount(Y, Y)
      tauy <- (tauy_info$C - tauy_info$D) / choose(n, 2)
      X_TieProb <- sum((table(X)/length(X))^2)
      Y_TieProb <- sum((table(Y)/length(Y))^2)
      X_TieProb3 <- sum((table(X)/length(X))^3)
      Y_TieProb3 <- sum((table(Y)/length(Y))^3)
      # Define functions
      G_XY <- Vectorize(function(x_val, y_val) (mean(X <= x_val & Y <= y_val) + mean(X <= x_val & Y < y_val) + mean(X < x_val & Y <= y_val) + mean(X < x_val & Y < y_val)) / 4)
      G_X <- Vectorize(function(x_val) (mean(X < x_val) + mean(X <= x_val)) / 2)
      G_Y <- Vectorize(function(y_val) (mean(Y < y_val) + mean(Y <= y_val)) / 2)
      x_neq <- Vectorize(function(x_val) mean(X != x_val))
      y_neq <- Vectorize(function(y_val) mean(Y != y_val))
      
      # Calculate Marc's variance estimator
      G_XYXY <- G_XY(X, Y)
      G_XX <- G_X(X)
      G_YY <- G_Y(Y)
      x_neqX <- x_neq(X)
      y_neqY <- y_neq(Y)
      var_tau <- 4 * mean((4 * G_XYXY - 2 * (G_XX + G_YY) + 1 - tau)^2)
      var_taux <- 4 * mean((x_neqX - taux)^2)
      var_tauy <- 4 * mean((y_neqY - tauy)^2)
      var_tautaux <- 4 * mean((4 * G_XYXY - 2 * (G_XX + G_YY) + 1 - tau) * (x_neqX - taux))
      var_tautauy <- 4 * mean((4 * G_XYXY - 2 * (G_XX + G_YY) + 1 - tau) * (y_neqY - tauy))
      var_tauxtauy <- 4 * mean((x_neqX - taux) * (y_neqY - tauy))
      var_hat <- (var_tau - tau * (var_tautaux / taux - var_tautauy / tauy) + tau^2 / 4 * (var_taux / taux^2 + var_tauy / tauy^2 + (2 * var_tauxtauy) / tauy / taux)) / (taux * tauy)
      # Variance under independence assumption
      var_hat_ind <- 4 / 9 * (1 - X_TieProb3) * (1 - Y_TieProb3) / (1 - X_TieProb) / (1 - Y_TieProb)
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * tau_b / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * tau_b / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      tau_info <- DescTools:::.DoCount(X, Y)
      tau <- (tau_info$C - tau_info$D) / choose(n, 2)
      taux_info <- DescTools:::.DoCount(X, X)
      taux <- (taux_info$C - taux_info$D) / choose(n, 2)
      tauy_info <- DescTools:::.DoCount(Y, Y)
      tauy <- (tauy_info$C - tauy_info$D) / choose(n, 2)
      X_TieProb <- sum((table(X)/length(X))^2)
      Y_TieProb <- sum((table(Y)/length(Y))^2)
      var_hat <- TauB_LRV(X, Y, tau, taux, tauy, bandwidth = "Dehling")
      var_hat_ind <- Tau_ind_LRV(X, Y, bandwidth = "Dehling") / (1 - X_TieProb) / (1 - Y_TieProb)
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * tau_b / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * tau_b / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(tau_b + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), tau_b + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(tau_b_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - tau_b^2)), tanh(tau_b_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - tau_b^2)))
    }
    res <- dplyr::tribble(~TauB, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          tau_b, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
  if (method == "tau_b_mod"){
    ties_x <- 0
    ties_y <- 0
    for (i in 3:n) {
      for (j in 2:(i - 1)) {
        for (k in 1:(j - 1)) {
          ties_x <- ties_x + ifelse(X[i] == X[j] & X[j] == X[k], 1, 0)
          ties_y <- ties_y + ifelse(Y[i] == Y[j] & Y[j] == Y[k], 1, 0)
        }
      }
    }
    ties_x <- ties_x / choose(n, 3)
    ties_y <- ties_y / choose(n, 3)
    tau_info <- DescTools:::.DoCount(X, Y)
    tau <- (tau_info$C - tau_info$D) / choose(n, 2)
    tau_b_mod <- tau / sqrt((1 - ties_x) * (1 - ties_y))
    if (isTRUE(IID)){
      var_hat_ind <- 4/9
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * tau_b_mod / sqrt(var_hat_ind))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    res <- dplyr::tribble(~TauB_Mod, ~PValueIND,
                          tau_b_mod, p_val_ind)
    return(res)
  }
  if (method == "gamma"){
    gamma_info <- DescTools:::.DoCount(X, Y)
    gamma <- (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
    gamma_fis <- atanh(gamma)
    tau <- (gamma_info$C - gamma_info$D) / choose(n, 2)
    X_TieProb <- sum((table(X)/length(X))^2)
    Y_TieProb <- sum((table(Y)/length(Y))^2)
    X_TieProb3 <- sum((table(X)/length(X))^3)
    Y_TieProb3 <- sum((table(Y)/length(Y))^3)
    XY_TieProb <- sum((table(X, Y)/length(X))^2)
    tie_prob <- X_TieProb + Y_TieProb - XY_TieProb
    if (isTRUE(IID)){
      # Define functions
      G_XY <- Vectorize(function(x_val, y_val) (mean(X <= x_val & Y <= y_val) + mean(X <= x_val & Y < y_val) + mean(X < x_val & Y <= y_val) + mean(X < x_val & Y < y_val)) / 4)
      G_X <- Vectorize(function(x_val) (mean(X < x_val) + mean(X <= x_val)) / 2)
      G_Y <- Vectorize(function(y_val) (mean(Y < y_val) + mean(Y <= y_val)) / 2)
      x_eq <- Vectorize(function(x_val) mean(X == x_val))
      y_eq <- Vectorize(function(y_val) mean(Y == y_val))
      x_eq_y_eq <- Vectorize(function(x_val, y_val) mean(X == x_val & Y == y_val))
      # Calculate Marc's variance estimator
      G_XYXY <- G_XY(X, Y)
      G_XX <- G_X(X)
      G_YY <- G_Y(Y)
      x_eqX <- x_eq(X)
      y_eqY <- y_eq(Y)
      x_eq_y_eqXY <- x_eq_y_eq(X, Y)
      var_tau <- 4 * mean((4 * G_XYXY - 2 * (G_XX + G_YY) + 1 - tau)^2)
      var_nu <- 4 * mean((x_eqX + y_eqY - x_eq_y_eqXY - tie_prob)^2)
      var_taunu <- 4 * mean((4 * G_XYXY - 2 * (G_XX + G_YY) + 1 - tau) * (x_eqX + y_eqY - x_eq_y_eqXY - tie_prob))
      var_hat <- (var_tau + gamma^2 * var_nu + 2 * gamma * var_taunu) / (1 - tie_prob)^2
      # Variance under independence assumption
      var_hat_ind <- 4 / 9 * (1 - X_TieProb3) * (1 - Y_TieProb3) / (1 - X_TieProb)^2 / (1 - Y_TieProb)^2
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * gamma / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * gamma / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      var_hat <- Gamma_LRV(X, Y, tau, tie_prob, bandwidth = "Dehling")
      var_hat_ind <- Tau_ind_LRV(X, Y, bandwidth = "Dehling") / (1 - X_TieProb)^2 / (1 - Y_TieProb)^2
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * gamma / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * gamma / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(gamma + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), gamma + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(gamma_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - gamma^2)), tanh(gamma_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - gamma^2)))
    }
    res <- dplyr::tribble(~Gamma, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          gamma, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
  if (method == "rho"){
    rho <- 12 * (n - 1) / n^3 * stats::cov(X, Y, method = "spearman")
    rho_fis <- atanh(rho)
    X_TieProb3 <- sum((table(X)/length(X))^3)
    Y_TieProb3 <- sum((table(Y)/length(Y))^3)
    if (isTRUE(IID)){
      # Define functions
      G_XY <- Vectorize(function(x_val, y_val) (mean(X <= x_val & Y <= y_val) + mean(X <= x_val & Y < y_val) + mean(X < x_val & Y <= y_val) + mean(X < x_val & Y < y_val)) / 4)
      G_X <- Vectorize(function(x_val) (mean(X < x_val) + mean(X <= x_val)) / 2)
      G_Y <- Vectorize(function(y_val) (mean(Y < y_val) + mean(Y <= y_val)) / 2)
      g_x <- Vectorize(function(x_val) mean(G_XY(x_val, Y)))
      g_y <- Vectorize(function(y_val) mean(G_XY(X, y_val)))
      
      # Calculate Marc's variance estimator
      G_XX <- G_X(X)
      G_YY <- G_Y(Y)
      var_hat <- 9 * mean((4 * (g_x(X) + g_y(Y) + G_XX * G_YY - G_XX - G_YY)  + 1 - rho)^2)
      # Variance under independence assumption
      var_hat_ind <- (1 - X_TieProb3) * (1 - Y_TieProb3)
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * rho / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * rho / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      var_hat <- SRho_LRV(X, Y, rho, bandwidth = "Dehling")
      var_hat_ind <- 9 / 4 * Tau_ind_LRV(X, Y, bandwidth = "Dehling")
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * rho / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * rho / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(rho + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), rho + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(rho_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - rho^2)), tanh(rho_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - rho^2)))
    }
    res <- dplyr::tribble(~Rho, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          rho, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
  if (method == "rho_b"){
    rho_b <- stats::cor(X, Y, method = "spearman")
    rho_b_fis <- atanh(rho_b)
    rho <- 12 * (n - 1) / n^3 * stats::cov(X, Y, method = "spearman")
    rho_x <- 12 * (n - 1) / n^3 * stats::cov(X, X, method = "spearman")
    rho_y <- 12 * (n - 1) / n^3 * stats::cov(Y, Y, method = "spearman")
    X_TieProb <- sum((table(X)/length(X))^2)
    Y_TieProb <- sum((table(Y)/length(Y))^2)
    if (isTRUE(IID)){
      # Define functions
      G_XY <- Vectorize(function(x_val, y_val) (mean(X <= x_val & Y <= y_val) + mean(X <= x_val & Y < y_val) + mean(X < x_val & Y <= y_val) + mean(X < x_val & Y < y_val)) / 4)
      G_X <- Vectorize(function(x_val) (mean(X < x_val) + mean(X <= x_val)) / 2)
      G_Y <- Vectorize(function(y_val) (mean(Y < y_val) + mean(Y <= y_val)) / 2)
      F_X <- Vectorize(function(x_val) mean(X <= x_val))
      F_X_ <- Vectorize(function(x_val) mean(X < x_val))
      F_Y <- Vectorize(function(y_val) mean(Y <= y_val))
      F_Y_ <- Vectorize(function(y_val) mean(Y < y_val))
      g_x <- Vectorize(function(x_val) mean(G_XY(x_val, Y)))
      g_y <- Vectorize(function(y_val) mean(G_XY(X, y_val)))
      
      # Calculate Marc's variance estimator
      g_xX <- g_x(X)
      g_yY <- g_y(Y)
      G_XX <- G_X(X)
      G_YY <- G_Y(Y)
      F_XX <- F_X(X)
      F_X_X <- F_X_(X)
      F_YY <- F_Y(Y)
      F_Y_Y <- F_Y_(Y)
      
      # Define functions
      min1X <- Vectorize(function(x_val) mean(pmin(F_XX, F_X(x_val))))
      min2X <- Vectorize(function(x_val) mean(pmin(F_XX, F_X_(x_val))))
      min3X <- Vectorize(function(x_val) mean(pmin(F_X_X, F_X(x_val))))
      min4X <- Vectorize(function(x_val) mean(pmin(F_X_X, F_X_(x_val))))
      min1Y <- Vectorize(function(y_val) mean(pmin(F_YY, F_Y(y_val))))
      min2Y <- Vectorize(function(y_val) mean(pmin(F_YY, F_Y_(y_val))))
      min3Y <- Vectorize(function(y_val) mean(pmin(F_Y_Y, F_Y(y_val))))
      min4Y <- Vectorize(function(y_val) mean(pmin(F_Y_Y, F_Y_(y_val))))
      
      # Calculate min functions
      min1XX <- min1X(X)
      min2XX <- min2X(X)
      min3XX <- min3X(X)
      min4XX <- min4X(X)
      min1YY <- min1Y(Y)
      min2YY <- min2Y(Y)
      min3YY <- min3Y(Y)
      min4YY <- min4Y(Y)
      var_rho <- 9 * mean((4 * (g_xX + g_yY + G_XX * G_YY - G_XX - G_YY)  + 1 - rho)^2)
      var_rhox <- 9 * mean((2 * (min1XX + min2XX + min3XX + min4XX + 2 * G_XX^2 - 4 * G_XX) + 1 - rho_x)^2)
      var_rhoy <- 9 * mean((2 * (min1YY + min2YY + min3YY + min4YY + 2 * G_YY^2 - 4 * G_YY) + 1 - rho_y)^2)
      var_rhorhox <- 9 * mean((4 * (g_xX + g_yY + G_XX * G_YY - G_XX - G_YY)  + 1 - rho) * (2 * (min1XX + min2XX + min3XX + min4XX + 2 * G_XX^2 - 4 * G_XX) + 1 - rho_x))
      var_rhorhoy <- 9 * mean((4 * (g_xX + g_yY + G_XX * G_YY - G_XX - G_YY)  + 1 - rho) * (2 * (min1YY + min2YY + min3YY + min4YY + 2 * G_YY^2 - 4 * G_YY) + 1 - rho_y))
      var_rhoxrhoy <- 9 * mean((2 * (min1XX + min2XX + min3XX + min4XX + 2 * G_XX^2 - 4 * G_XX) + 1 - rho_x) * (2 * (min1YY + min2YY + min3YY + min4YY + 2 * G_YY^2 - 4 * G_YY) + 1 - rho_y))
      var_hat <- (var_rho - rho * (var_rhorhox / rho_x + var_rhorhoy / rho_y) + 0.25 * rho^2 * (var_rhox / rho_x^2 + var_rhoy / rho_y^2 + 2 * var_rhoxrhoy / (rho_x * rho_y))) / (rho_x * rho_y)
      # Variance under independence assumption
      var_hat_ind <- 1
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * rho_b / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * rho_b / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      var_hat <- Rhob_LRV(X, Y, rho, rho_x, rho_y, bandwidth = "Dehling")
      var_hat_ind <- Rhob_ind_LRV(X, Y, bandwidth = "Dehling")
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * rho_b / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * rho_b / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(rho_b + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), rho_b + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(rho_b_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - rho_b^2)), tanh(rho_b_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - rho_b^2)))
    }
    res <- dplyr::tribble(~RhoB, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          rho_b, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
  if (method == "r"){
    r <- stats::cor(X, Y)
    r_fis <- atanh(r)
    X_std <- scale(X)
    Y_std <- scale(Y)
    if (isTRUE(IID)){
      m_40 <- mean(X_std^4)
      m_04 <- mean(Y_std^4)
      m_22 <- mean(X_std^2 * Y_std^2)
      m_31 <- mean(X_std^3 * Y_std)
      m_13 <- mean(X_std * Y_std^3)
      # Variance
      var_hat <- 0.25 * ((m_40 + 2*m_22 + m_04)*r^2 - 4*r*(m_31 + m_13) + 4*m_22)
      # Variance under independence assumption
      var_hat_ind <- 1
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * r / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * r / sqrt(var_hat))) * 2
    } else if (isFALSE(IID)){
      var_hat <- Rho_LRV(X, Y)
      var_hat_ind <- Rho_ind_LRV(X, Y, bandwidth = "Dehling")
      p_val_ind <- stats::pnorm(-abs(sqrt(n) * r / sqrt(var_hat_ind))) * 2
      p_val <- stats::pnorm(-abs(sqrt(n) * r / sqrt(var_hat))) * 2
    } else stop("Please insert a valid option for the variable `IID`!", call. = FALSE)
    if (isFALSE(Fisher)){
      CI <- c(r + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n), r + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n))
    } else if (isTRUE(Fisher)){
      CI <- c(tanh(r_fis + stats::qnorm(alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - r^2)), tanh(r_fis + stats::qnorm(1 - alpha/2)*sqrt(var_hat)/sqrt(n)/(1 - r^2)))
    }
    res <- dplyr::tribble(~R, ~CI_lower, ~CI_upper, ~PValue, ~PValueIND,
                          r, CI[1], CI[2], p_val, p_val_ind)
    return(res)
  }
}


# Load the coefficient: This is catered towards the specific income-country example! Not for general purpose. LISSY cannot load the arrangements package
NCor <- function(X, Y = NULL, alpha = 0.1, digits = 5, Inference = TRUE){
  if (is.null(Y)){
    ContTable <- X
  } else ContTable <- table(X, Y)
  if (any(ContTable %% 1 != 0)){
    round(ContTable, digits = digits)
    ContTable <- 10 ^ digits * ContTable
  }
  # Start cluster for parallel computing
  cl <- parallel::makeCluster(detectCores() - 1, type = "PSOCK")
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl)) # Need to stop the parallel computing
  
  dim_r <- nrow(ContTable)
  rows <- factorial(dim_r)
  results <- foreach::foreach(iperm_r = list(a = c(1,2,3), b = c(1,3,2), c = c(2,1,3), d = c(2,3,1), e = c(3,1,2), f = c(3,2,1)), i = 1:rows, .combine = 'c') %dopar% {
    rownames(ContTable) <- iperm_r
    cases <- rstatix::counts_to_cases(ContTable)
    gamma_info <- DescTools:::.DoCount(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])))
    (gamma_info$C - gamma_info$D) / (gamma_info$C + gamma_info$D)
  }
  if (isTRUE(Inference)){
    permutations <- matrix(c(1,1,2,2,3,3,2,3,1,3,1,2,3,2,3,1,2,1), ncol = 3)
    permutations_index <- which(results == max(results))[1]
    final_perm_row <- permutations[permutations_index,]
    rownames(ContTable) <- final_perm_row
    cases <- rstatix::counts_to_cases(ContTable)
    finalres <- RCor(as.numeric(as.vector.factor(cases[,1])), as.numeric(as.vector.factor(cases[,2])), method = "gamma")
  }
  else{
    finalres <- max(results)
  }
  return(list(finalres, allres <- results))
}


# Load all the data
df_at <- read.LIS('at21p')
df_be <- read.LIS('be21p')
df_cz <- read.LIS('cz16p')
df_fr <- read.LIS('fr20p')
df_ge <- read.LIS('de19p')
df_hu <- read.LIS('hu15p')
df_it <- read.LIS('it20p')
df_lu <- read.LIS('lu21p')
df_nl <- read.LIS('nl21p')
df_pl <- read.LIS('pl20p')
df_ro <- read.LIS('ro21p')
df_rs <- read.LIS('rs22p')
df_sk <- read.LIS('sk18p')
df_si <- read.LIS('si15p')
df_ch <- read.LIS('ch19p')

# Prepare the datasets
At <- cbind("Austria", na.omit(df_at$pitotal) / 0.903606186)
mean(as.numeric(At[,2]))
length(as.numeric(At[,2]))
Be <- cbind("Belgium", na.omit(df_be$pitotal) / 0.915711064)
mean(as.numeric(Be[,2]))
length(as.numeric(Be[,2]))
Cz <- cbind("Czechia", na.omit(df_cz$pitotal) / 13.5923206)
mean(as.numeric(Cz[,2]))
length(as.numeric(Cz[,2]))
Fr <- cbind("France", na.omit(df_fr$pitotal) / 0.877258269)
mean(as.numeric(Fr[,2]))
length(as.numeric(Fr[,2]))
Ge <- cbind("Germany", na.omit(df_ge$pitotal) / 0.825969547)
mean(as.numeric(Ge[,2]))
length(as.numeric(Ge[,2]))
Hu <- cbind("Hungary", na.omit(df_hu$pitotal) / 148.781888)
mean(as.numeric(Hu[,2]))
length(as.numeric(Hu[,2]))
It <- cbind("Italy", na.omit(df_it$pitotal) / 0.790212403)
mean(as.numeric(It[,2]))
length(as.numeric(It[,2]))
Lu <- cbind("Luxemburg", na.omit(df_lu$pitotal) / 1.0314545)
mean(as.numeric(Lu[,2]))
length(as.numeric(Lu[,2]))
Nl <- cbind("Netherlands", na.omit(df_nl$pitotal) / 0.934492964)
mean(as.numeric(Nl[,2]))
length(as.numeric(Nl[,2]))
Pl <- cbind("Poland", na.omit(df_pl$pitotal) / 2.01902978)
mean(as.numeric(Pl[,2]))
length(as.numeric(Pl[,2]))
Ro <- cbind("Romania", na.omit(df_ro$pitotal) / 2.19714983)
mean(as.numeric(Ro[,2]))
length(as.numeric(Ro[,2]))
Rs <- cbind("Serbia", na.omit(df_rs$pitotal) / 60.1672985)
mean(as.numeric(Rs[,2]))
length(as.numeric(Rs[,2]))
Sk <- cbind("Slovakia", na.omit(df_sk$pitotal) / 0.633617736)
mean(as.numeric(Sk[,2]))
length(as.numeric(Sk[,2]))
Si <- cbind("Slovenia", na.omit(df_si$pitotal) / 0.640360785)
mean(as.numeric(Si[,2]))
length(as.numeric(Si[,2]))
Sw <- cbind("Switzerland", na.omit(df_ch$pitotal) / 1.39176022)
mean(as.numeric(Sw[,2]))
length(as.numeric(Sw[,2]))



# Prepare the datasets for all the border triangles
FrSwGe <- rbind(Fr, Sw, Ge)
NlBeGe <- rbind(Nl, Be, Ge)
FrSwIt <- rbind(Fr, Sw, It)
AtSwIt <- rbind(At, Sw, It)
AtSiIt <- rbind(At, Si, It)
LuFrGe <- rbind(Lu, Fr, Ge)
LuFrBe <- rbind(Lu, Fr, Be)
LuGeBe <- rbind(Lu, Ge, Be)
AtGeSw <- rbind(At, Ge, Sw)
AtGeCz <- rbind(At, Ge, Cz)
PlGeCz <- rbind(Pl, Ge, Cz)
PlSkCz <- rbind(Pl, Sk, Cz)
CzSkAt <- rbind(Cz, Sk, At)
AtSkHu <- rbind(At, Sk, Hu)
AtSiHu <- rbind(At, Si, Hu)
RoHuRs <- rbind(Ro, Hu, Rs)

NCor(NlBeGe[,1], NlBeGe[,2])
NCor(FrSwGe[,1], FrSwGe[,2])
NCor(FrSwIt[,1], FrSwIt[,2])
NCor(AtSwIt[,1], AtSwIt[,2])
NCor(AtSiIt[,1], AtSiIt[,2])
NCor(LuFrGe[,1], LuFrGe[,2])
NCor(LuFrBe[,1], LuFrBe[,2])
NCor(LuGeBe[,1], LuGeBe[,2])
NCor(AtGeSw[,1], AtGeSw[,2])
NCor(AtGeCz[,1], AtGeCz[,2])
NCor(PlGeCz[,1], PlGeCz[,2])
NCor(PlSkCz[,1], PlSkCz[,2])
NCor(CzSkAt[,1], CzSkAt[,2])
NCor(AtSkHu[,1], AtSkHu[,2])
NCor(AtSiHu[,1], AtSiHu[,2])
NCor(RoHuRs[,1], RoHuRs[,2])

  
  
  
  
  
  