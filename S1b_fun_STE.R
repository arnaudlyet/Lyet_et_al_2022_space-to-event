  # Exponential log likelihood
  exp_logl_fn <- function(data, param){
    # param: beta parameter for lambda
    lambda <- exp(param) 
    logL <- 0
    for(i in 1:nrow(data$toevent)) {
      for (j in 1:ncol(data$toevent)) {
        if(!is.na(data$toevent[i, j])) {
          tmp <- dexp(data$toevent[i, j], lambda)
        } else {
          tmp <- pexp(data$censor[j], lambda, lower.tail = F)
        }
        logL <- logL + log(tmp)
      }
    }
    return(logL)
  }
  

  # Estimate abundance from STE
  STE_estN_fn <- function(data, LOWER, UPPER){
    opt <- optim(par = max(1, log(1/mean(data$toevent, na.rm = T))), 
                 fn = exp_logl_fn, 
                 data = data, 
				 method = "Brent",
				 lower = LOWER, upper = UPPER,
                 control = list(fnscale = -1),
                 hessian = T)

    # Estimate of lambda
    estLAM <- exp(opt$par)
	
	# SE and 95% CI
	varB <- -ginv(opt$hessian)
	SE_LAM <- sqrt(varB)*estLAM    
    
	# estLAM is average density per km2
    estN <- estLAM * data$A
    
    # Delta method for variance
    form <- sprintf("~ %f * exp(x1)", data$A)
    SE_N <- deltamethod(g = as.formula(form), mean = opt$par, cov = varB, ses = T)
    
    return(list(
				estLAM = estLAM,
                SE_LAM = SE_LAM,
                LCI_LAM = estLAM - SE_LAM * 1.96,
                UCI_LAM  = estLAM + SE_LAM * 1.96,				
				estN = estN,
                SE_N = SE_N,
                LCI_N = estN - SE_N * 1.96,
                UCI_N  = estN + SE_N * 1.96) )

  }
