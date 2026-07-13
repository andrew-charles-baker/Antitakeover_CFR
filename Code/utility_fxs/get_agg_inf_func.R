get_agg_inf_func <- function(att, inffunc1, whichones, weights.agg, wifvar=NULL) {
  
  # enforce weights are in matrix form
  weights.agg <- as.matrix(weights.agg)
  inffunc1 <- as.matrix(inffunc1)
  if (!is.null(wifvar)) {
    wifvar <- as.matrix(wifvar)
  }

  # for wif replace missing values with 0
  if (!is.null(wifvar)) {
    wifvar[is.na(wifvar)] <- 0
  }
  # multiplies influence function times weights and sums to get vector of weighted IF (of length n)
  thisinffunc <- inffunc1[,whichones]%*%weights.agg
  
  # Incorporate influence function of the weights
  if (!is.null(wifvar)) {
    thisinffunc <- thisinffunc + wifvar%*%as.matrix(att[whichones])
  }
  
  # return influence function
  return(thisinffunc)
}


