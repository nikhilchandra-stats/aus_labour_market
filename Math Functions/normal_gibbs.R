
# col_name <- "observations"
# dat_temp <- tibble(observations = rnorm(n = 5000,mean = 10,sd = 20)) %>%
#   gibbs_normal(col_name = "observations") %>%
#   sample_from_bayesian_normal()

gibbs_normal <- function(.data, col_name, samples){
  
n = dim(.data)[1]

dat <- .data %>% select(as.name(col_name)) 
names(dat) <- "observations"

sample_length <- samples
initial_mu <- .data %>% summarise(initial_mu = mean(!!as.name(col_name)))
initial_sigma <- .data %>% summarise(initial_sigma = var(!!as.name(col_name))) 

tau_initial <- 1/initial_sigma$initial_sigma[1]

tau_1 = numeric(samples)
mu_1 = numeric(samples)
observations <- dat$observations

tau_1[1] <- tau_initial
mu_1[1] <-  initial_mu$initial_mu[1]

for(i in 2:sample_length)
{
  #sample from mu | sigma and Y
  mu_1[i] <- rnorm(1,
                 initial_mu$initial_mu[1],
                 (n * tau_1[i - 1]) ^ -1
                 )
  
  #sample from y | x
  gamma_param_2_p1 <- (observations -  mu_1[i])^2
  gamma_param2 <- sum(gamma_param_2_p1)
   # gamma_param2 <- dat %>%
   #  mutate(param = (observations - mu_1[i]) ^ 2) %>%
   #  summarise(param = sum(param))
   # 

  
  tau_1[i] <- rgamma(1, n / 2, (1 / 2) * gamma_param2)
  
}

message(glue::glue("Still alive {i}"))


sampling_matrix <- 
  data.frame( mu = mu_1,tau = tau_1 ) 

return(sampling_matrix)

}

sample_from_bayesian_normal <- function(.data, samples_drawn = 5000){
  
  sampled_dat <- 
    data.frame( sampled_data =  rnorm(samples_drawn,mean = .data$mu, sd = (1/.data$tau)^(1/2) ) ) %>%
    as_tibble()
  
  return(sampled_dat)
  
}



