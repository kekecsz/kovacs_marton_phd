library(BayesFactor)
library(pbapply)

iterations = 1000
BF_threshold = 3

alpha_tester <- function(n_per_group = NA,
                         rscale = NA,
                         SMD = NA){
  mean_var_a = 0
  mean_var_b = 0 + SMD
  
  var_a = rnorm(n_per_group, mean = mean_var_a, sd = 1)
  var_b = rnorm(n_per_group, mean = mean_var_b, sd = 1)
  
  my_data = data.frame(value = c(var_a, var_b), group = rep(c("group_a", "group_b"), each = n_per_group))
  
  bf = ttestBF(formula = value ~ group, data = my_data, rscale=rscale)
  BayesFactor10 = matrix(bf)
  return(BayesFactor10)
  
}

BFs = pbreplicate(iterations, 
                  alpha_tester(n_per_group = 28,
                               rscale = 1,
                               SMD = 0))


power = sum(BFs<(1/BF_threshold))/length(BFs)
power ### power does not reach 0.8 with the recommended sample size by the ShinyApp

BFs = pbreplicate(iterations, 
                  alpha_tester(n_per_group = 28,
                               rscale = 1,
                               SMD = 0.21))


alpha = sum(BFs<(1/BF_threshold))/length(BFs)
alpha ### alpha is much greater than the advertised 0.05



# same but assuming that we go for a one-tailed test
# problem points are the same

alpha_tester <- function(n_per_group = NA,
                         rscale = NA,
                         SMD = NA){
  mean_var_a = 0
  mean_var_b = 0 + SMD
  
  var_a = rnorm(n_per_group, mean = mean_var_a, sd = 1)
  var_b = rnorm(n_per_group, mean = mean_var_b, sd = 1)
  
  my_data = data.frame(value = c(var_a, var_b), group = rep(c("group_a", "group_b"), each = n_per_group))
  
  bf = ttestBF(formula = value ~ group, data = my_data, rscale=rscale, nullInterval=c(-Inf,0))
  BayesFactor10 = matrix(bf)[1,1]
  return(BayesFactor10)
  
}

BFs = pbreplicate(iterations, 
                  alpha_tester(n_per_group = 28,
                               rscale = 1,
                               SMD = 0))


power = sum(BFs<(1/BF_threshold))/length(BFs)
power ### power does not reach 0.8 with the recommended sample size by the ShinyApp

BFs = pbreplicate(iterations, 
                  alpha_tester(n_per_group = 28,
                               rscale = 1,
                               SMD = 0.21))


alpha = sum(BFs<(1/BF_threshold))/length(BFs)
alpha ### alpha is much greater than the advertised 0.05


