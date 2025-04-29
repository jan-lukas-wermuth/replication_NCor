#' @title Generate data from 3x3 contingency table DGPs
#' 
#' @description
#' Generate data according to a DGP that consists of two nominal variables.
#' 
#' 
#' @param n sample size.
#' @param i seed.
#'
#' @return a contingency table
#' 
Gen_3x3DGP <- function(n, i){
  set.seed(i) 
  sampled_indices <- sample(1:9, size = n, replace = TRUE, prob = prob_vector)
  contingency_table <- table(factor(sampled_indices, levels = 1:9))
  contingency_matrix <- matrix(contingency_table, nrow = 3)
  return(contingency_matrix)
}
