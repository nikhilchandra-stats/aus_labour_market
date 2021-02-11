#R program for Gibbs sampling using inversion method
col_name <- "observations"
.data <- tibble(observations = rnorm(n = 1000,mean = 10,sd = 20))

gibbs_normal <- function(.data, col_name){
  
n = dim(.data)[1]

dat <- .data %>% select(as.name(col_name)) 
names(dat) <- "observations"

sample_length <- 5000
initial_mu <- dat %>% summarise(initial_mu = mean(!!as.name(col_name)))
initial_sigma <- dat %>% summarise(initial_sigma = var(!!as.name(col_name))) 

tau_initial <- 1/initial_sigma$initial_sigma[1]

sampling_matrix <- 
  data.frame( mu = rep(initial_mu$initial_mu[1],sample_length)  ) %>%
  mutate(
    
    tau = rep(tau_initial,sample_length)
  )

for(i in 2:sample_length)
{
  #sample from mu | sigma and Y
  sampling_matrix$mu[i] <- rnorm(1,
                              initial_mu$initial_mu[1],
                              (n * sampling_matrix$tau[i]) ^-1)
  
  #sample from y | x
  gamma_param2 <- dat %>%
    mutate(
      param = (observations - sampling_matrix$mu[i])^2
    ) %>%
    summarise(
      param = sum(param)
    )
  
 sampling_matrix$tau[i] <- rgamma(1,n/2, (1/2)*gamma_param2$param[1])
  
}

return(sampling_matrix)

}
