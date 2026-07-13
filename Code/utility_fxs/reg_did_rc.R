reg_did_rc <-function(y, post, D, covariates, i.weights = NULL,
                      boot = FALSE, boot.type = "weighted", nboot = NULL,
                      inffunc = FALSE){
  # D as vector
  D <- as.vector(D)
  # post as vector
  post <- as.vector(post)
  # Sample size
  n <- length(D)
  # outcome of interested
  y <- as.vector(y)
  
  # matrix of covariates
  int.cov <- if (ncol(covariates) == 0){
    as.matrix(rep(1, n)) } else{
      model.matrix(~(.), covariates)
    }
  
  # keep all covariates with positive variance in pre and post for control units
  if(ncol(int.cov) > 1) {
    int.cov <- int.cov[, c(1, 
                           intersect(
                             which(apply(int.cov[which(D == 0 & post == 0),], 2, var) > 0.01),
                             which(apply(int.cov[which(D == 0 & post == 1),], 2, var) > 0.01)))]
  }
  
  # Weights
  if(is.null(i.weights)) {
    i.weights <- as.vector(rep(1, n))
  } else if(min(i.weights) < 0) stop("i.weights must be non-negative")
  
  
  #-----------------------------------------------------------------------------
  #Compute the Outcome regression for the control group at the pre-treatment period, using ols.
  reg.coeff.pre <- stats::coef(stats::lm(y ~ -1 + int.cov,
                                         subset = ((D==0) & (post==0)),
                                         weights = i.weights))
  if(anyNA(reg.coeff.pre)){
    stop("Outcome regression model coefficients have NA components. \n Multicollinearity of covariates is probably the reason for it.")
  }
  out.y.pre <-   as.vector(tcrossprod(reg.coeff.pre, int.cov))
  #-----------------------------------------------------------------------------
  #Compute the Outcome regression for the control group at the pre-treatment period, using ols.
  reg.coeff.post <- stats::coef(stats::lm(y ~ -1 + int.cov,
                                          subset = ((D==0) & (post==1)),
                                          weights = i.weights))
  if(anyNA(reg.coeff.post)){
    stop("Outcome regression model coefficients have NA components. \n Multicollinearity (or lack of variation) of covariates is probably the reason for it.")
  }
  out.y.post <-   as.vector(tcrossprod(reg.coeff.post, int.cov))
  #-----------------------------------------------------------------------------
  #Compute the OR DiD estimators
  # First, the weights
  w.treat.pre <- i.weights * D * (1 - post)
  w.treat.post <- i.weights * D * post
  w.cont <- i.weights * D
  
  reg.att.treat.pre <- w.treat.pre * y
  reg.att.treat.post <- w.treat.post * y
  reg.att.cont <- w.cont * (out.y.post - out.y.pre)
  
  eta.treat.pre <- mean(reg.att.treat.pre) / mean(w.treat.pre)
  eta.treat.post <- mean(reg.att.treat.post) / mean(w.treat.post)
  eta.cont <- mean(reg.att.cont) / mean(w.cont)
  
  reg.att <- (eta.treat.post - eta.treat.pre) - eta.cont
  
  #-----------------------------------------------------------------------------
  #get the influence function to compute standard error
  #-----------------------------------------------------------------------------
  # First, the influence function of the nuisance functions
  # Asymptotic linear representation of OLS parameters in pre-period
  weights.ols.pre <- i.weights * (1 - D) * (1 - post)
  wols.x.pre <- weights.ols.pre * int.cov
  wols.eX.pre <- weights.ols.pre * (y - out.y.pre) * int.cov
  XpX.inv.pre <- qr.solve(crossprod(wols.x.pre, int.cov)/n)
  asy.lin.rep.ols.pre <-  wols.eX.pre %*% XpX.inv.pre
  
  # Asymptotic linear representation of OLS parameters in post-period
  weights.ols.post <- i.weights * (1 - D) * post
  wols.x.post <- weights.ols.post * int.cov
  wols.eX.post <- weights.ols.post * (y - out.y.post) * int.cov
  XpX.inv.post <- qr.solve(crossprod(wols.x.post, int.cov)/n)
  asy.lin.rep.ols.post <-  wols.eX.post %*% XpX.inv.post
  #-----------------------------------------------------------------------------
  # Now, the influence function of the "treat" component
  # Leading term of the influence function
  inf.treat.pre <- (reg.att.treat.pre - w.treat.pre * eta.treat.pre) / mean(w.treat.pre)
  inf.treat.post <- (reg.att.treat.post - w.treat.post * eta.treat.post) / mean(w.treat.post)
  inf.treat <- inf.treat.post - inf.treat.pre
  #-----------------------------------------------------------------------------
  # Now, get the influence function of control component
  # Leading term of the influence function: no estimation effect
  inf.cont.1 <- (reg.att.cont - w.cont * eta.cont)
  # Estimation effect from beta hat (OLS using only controls)
  # Derivative matrix (k x 1 vector)
  M1 <- base::colMeans(w.cont * int.cov)
  # Now get the influence function related to the estimation effect related to beta's in post-treatment
  inf.cont.2.post <- asy.lin.rep.ols.post %*% M1
  # Now get the influence function related to the estimation effect related to beta's in pre-treatment
  inf.cont.2.pre <- asy.lin.rep.ols.pre %*% M1
  # Influence function for the control component
  inf.control <- (inf.cont.1 + inf.cont.2.post - inf.cont.2.pre) / mean(w.cont)
  #-----------------------------------------------------------------------------
  #get the influence function of the DR estimator (put all pieces together)
  reg.att.inf.func <- (inf.treat - inf.control)
  #-----------------------------------------------------------------------------
  if (boot == FALSE) {
    # Estimate of standard error
    se.reg.att <- stats::sd(reg.att.inf.func)/sqrt(n)
    # Estimate of upper boudary of 95% CI
    uci <- reg.att + 1.96 * se.reg.att
    # Estimate of lower doundary of 95% CI
    lci <- reg.att - 1.96 * se.reg.att
    #Create this null vector so we can export the bootstrap draws too.
    reg.boot <- NULL
  }
  
  if (boot == TRUE) {
    if (is.null(nboot) == TRUE) nboot = 999
    if(boot.type == "multiplier"){
      # do multiplier bootstrap
      reg.boot <- mboot.did(reg.att.inf.func, nboot)
      # get bootstrap std errors based on IQR
      se.reg.att <- stats::IQR(reg.boot) / (stats::qnorm(0.75) - stats::qnorm(0.25))
      # get symmtric critival values
      cv <- stats::quantile(abs(reg.boot/se.reg.att), probs = 0.95)
      # Estimate of upper boudary of 95% CI
      uci <- reg.att + cv * se.reg.att
      # Estimate of lower doundary of 95% CI
      lci <- reg.att - cv * se.reg.att
    } else {
      # do weighted bootstrap
      reg.boot <- unlist(lapply(1:nboot, wboot_reg_rc,
                                n = n, y = y, post = post, D = D, int.cov = int.cov, i.weights = i.weights))
      # get bootstrap std errors based on IQR
      se.reg.att <- stats::IQR((reg.boot - reg.att)) / (stats::qnorm(0.75) - stats::qnorm(0.25))
      # get symmtric critival values
      cv <- stats::quantile(abs((reg.boot - reg.att)/se.reg.att), probs = 0.95)
      # Estimate of upper boudary of 95% CI
      uci <- reg.att + cv * se.reg.att
      # Estimate of lower doundary of 95% CI
      lci <- reg.att - cv * se.reg.att
      
    }
  }
  
  
  if(inffunc == FALSE) reg.att.inf.func <- NULL
  
  ret <- (list(ATT = reg.att,
               se = se.reg.att,
               uci = uci,
               lci = lci,
               boots = reg.boot,
               att.inf.func = reg.att.inf.func))
  
  # return the list
  return(ret)
}
