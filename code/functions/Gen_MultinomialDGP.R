#' @title Generate data from multinomial DGPs
#' 
#' @description
#' Generate data according to a DGP that consists of one nominal and one continuous variable. The DGP uses a multinomial logit framework to introduce dependence.
#' 
#' 
#' @param n sample size.
#' @param rho dependence parameter.
#' @param i seed.
#' @param df degrees of freedom to be inserted into rt. df = 1 yields the Cauchy distribution and df = Inf the standard normal distribution.
#'
#' @return a bivariate sample.
#' 
Gen_MultinomialDGP <- function(n, rho, i, df){
  set.seed(i) 
  beta_1 <- c(-rho, rho)  # Coefficients for the continuous covariate
  X <- rt(n, df)  # Simulated continuous covariate
  ProbA <- exp(X * beta_1[1]) / (1 + exp(X * beta_1[1]) + exp(X * beta_1[2]))
  ProbB <- exp(X * beta_1[2]) / (1 + exp(X * beta_1[1]) + exp(X * beta_1[2]))
  ProbC <- 1 / (1 + exp(X * beta_1[1]) + exp(X * beta_1[2]))
  ProbA[is.nan(ProbA)] <- rep(1, sum(is.nan(ProbA)))
  ProbB[is.nan(ProbB)] <- rep(1, sum(is.nan(ProbB)))
  ProbC[is.nan(ProbC)] <- rep(1, sum(is.nan(ProbC)))
  # Simulate the response variable based on the calculated probabilities
  Y <- rep(NA, n)
  for (j in 1:n) {
    Y[j] <- sample(categories, size = 1, replace = TRUE, prob = cbind(ProbA[j], ProbB[j], ProbC[j]))
  }
  return(cbind(X, Y))
}
