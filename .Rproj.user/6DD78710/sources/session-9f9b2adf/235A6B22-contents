#' @title Helper function: Set up cluster for parallel computing
#' 
Setup_cluster <- function(){
  cl <- makeCluster(detectCores() - 1, type = "PSOCK")
  registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
}
