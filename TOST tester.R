
# simple frequentist tost


alpha_tester <- function(n_per_group = NA,
                         SMD = NA,
                         SESOI = NA){
  mean_var_a = 0
  mean_var_b = 0 + SMD
  
  var_a = rnorm(n_per_group, mean = mean_var_a, sd = 1)
  var_b = rnorm(n_per_group, mean = mean_var_b, sd = 1)
  
  var_dif = var_a-var_b
  
  p_tost1 = t.test(var_dif, mu = SESOI, alternative = "less")$p.value
  p_tost2 = t.test(var_dif, mu = -SESOI, alternative = "greater")$p.value
  larger_p = max(c(p_tost1, p_tost2))
  
  return(larger_p)
  
}

larger_ps = pbreplicate(iterations, 
                        alpha_tester(n_per_group = 429,
                                     SMD = 0.15,
                                     SESOI = 0.2))


power = sum(larger_ps<0.05)/length(larger_ps)
power # if the SMD is not 0, but still within the SESOI bound, power is very low.