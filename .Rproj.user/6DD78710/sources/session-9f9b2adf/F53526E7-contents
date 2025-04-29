#' @title Generate data from regression DGPs
#' 
#' @description
#' Generate data according to a DGP that consists of one nominal and one continuous variable. The DGP uses a regression framework to introduce dependence.
#' 
#' 
#' @param n sample size.
#' @param rho dependence parameter.
#' @param i seed.
#' @param df degrees of freedom to be inserted into rt. df = 1 yields the Cauchy distribution and df = Inf the standard normal distribution.
#'
#' @return a bivariate sample
#' 
Gen_RegressionDGP <- function(n, rho, i, df){
  set.seed(i)
  # Generate nominal covariate and create continuous dependent variable via regression
  X <- sample(c("A", "B", "C"), n, prob = c(1/3, 1/3, 1/3), replace = TRUE)
  C_ind <- ifelse(X == "C", 1, 0)
  B_ind <- ifelse(X == "B", 1, 0)
  Y <- rho * B_ind - rho * C_ind + stats::rt(n, df = df)
  return(cbind(X, Y))
}
