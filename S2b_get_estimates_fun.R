get_estimates <- function(fit_mod) {

## Density
# Log scale
N_est_log = as.numeric( fit_mod@estimates["state"]@estimates )
 N_se_log = as.numeric( sqrt( fit_mod@estimates["state"]@covMat ) )
N_lcl_log = N_est_log + (qnorm(0.025) * N_se_log)
N_ucl_log = N_est_log + (qnorm(0.975) * N_se_log)

# Real scale
N_hat = exp(N_est_log)
 N_se = N_se_log*N_hat 
 N_lcl = exp(N_lcl_log)
 N_ucl = exp(N_ucl_log)

# Detection
# Logit scale
p_est_log = as.numeric( fit_mod@estimates["det"]@estimates )
 p_se_log = as.numeric( sqrt( fit_mod@estimates["det"]@covMat ) )
p_lcl_log = p_est_log + (qnorm(0.025) * p_se_log)
p_ucl_log = p_est_log + (qnorm(0.975) * p_se_log)

# Real scale
p_hat = plogis(p_est_log)
 p_se = p_se_log*p_hat*(1-p_hat)
 p_lcl = plogis(p_lcl_log)
 p_ucl = plogis(p_ucl_log)

out <- matrix( c(N_hat, N_se, N_lcl, N_ucl,
		p_hat, p_se, p_lcl, p_ucl) ,
	nrow=2, ncol=4, byrow=TRUE, 
	dimnames = list(c("abundance", "detection"), c("EST","SE","LCL","UCL")) )

return(out)

} # END FUNCTION