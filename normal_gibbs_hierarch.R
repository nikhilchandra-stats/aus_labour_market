hiearachical_normal <- function(.data, col_name = "value", samples = 5000, group_name = "industry" ){
  
  n = .data %>%
    group_by(!!as.name(group_name)) %>%
    count()
  
  number_of_groups <- dim(n)[1]
  
  data2 <- .data %>% 
    rename(observations = !!as.name(col_name)) %>%
    rename(groups_analysis = !!as.name(group_name))
  
  individual_means <- data2 %>%
    group_by(.data$groups_analysis) %>%
    summarise(
      mean_values = mean(.data$observations,na.rm = TRUE),
      vars_x = var(.data$observations,na.rm = TRUE)
    ) 
  
  theta <- individual_means$mean_values
  sigma1 <- mean(individual_means$vars_x)
  mu <- mean(theta)
  tau2 <- var(theta)
  
  nu0 <- 1
  eta0 <- 1
  mu0 <- 50
  s20 <- var(data2$observations)
  t20 <- var(data2$observations)
  g20 <- var(data2$observations)
  
  THETA <- numeric(samples)
  MU <- numeric(samples)
  SIGMA <- numeric(samples)
  TAU <- numeric(samples)
  
  individual_mu <- individual_means$mean_values
  n_individual <- n$n
  group_names <- n[,1]
  
  for(i in 1:samples){
    
  # Sample Individual Means  
    for (j in 1:number_of_groups) {
      
      vtheta <- 1/(n_individual[j]/sigma2 + 1/tau2 )
      etheta <- vtheta*(individual_mu[j]*n_individual[j]/sigma2 + mu/tau2 )
      theta[j] <- rnorm(1,etheta,sqrt(vtheta))
      
    }
    
  # Sample Sigma's
  nun <- nu0 + sum(n)
  ss <- nu0*s20
  for (j in 1:number_of_groups) {
    ss <- ss + sum()
  }
    
}
  
  
}