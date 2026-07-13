getSE <- function(thisinffunc, DIDparams=NULL) {
  alp <- .05
  bstrap <- FALSE
  if (!is.null(DIDparams)) {
    bstrap <- DIDparams$bstrap
    alp <- DIDparams$alp
    cband <- DIDparams$cband
    n <- length(thisinffunc)
  }
  
  if (bstrap) {
    bout <- did::mboot(thisinffunc, DIDparams)
    return(bout$se)
  } else {
    return(sqrt( mean((thisinffunc)^2)/n ))
  }
}