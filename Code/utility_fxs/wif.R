wif <- function(tt, pg) {
  # note: weights are all of the form P(G=g|cond)/sum_cond(P(G=g|cond))
  # this is equal to P(G=g)/sum_cond(P(G=g)) which simplifies things here
  
  # effect of estimating weights in the numerator
  if1 <- map_dfc(1:ncol(tt), function(k) {
    (tt[, k] - pg[k])
  })
  # effect of estimating weights in the denominator
  if2 <- rowSums( map_dfc( 1:ncol(tt), function(k) {
    (tt[, k] - pg[k])
  }), na.rm = TRUE) %*%
    t(pg)
  
  # return the influence function for the weights
  if1 - if2
}



