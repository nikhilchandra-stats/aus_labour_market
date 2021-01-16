
gibbs_sampler_normal_hierachical <- function(.data ,
                                 nu0=2,
                                 s20=1,
                                 eta0=2,
                                 t20=1,
                                 mu0,
                                 g20 ) {
  
  ### starting values
  m<-dim(.data)[1] 
  n<-sv<-ybar<-rep(NA,m) 
  for(j in 1:m) 
  { 
    ybar[j]<-.data$sample_mean[j]
    sv[j]<-(.data$sample_sd[j])^2
    n[j]<-.data$sample_n[j] 
  }
  theta<-ybar
  sigma2<-mean(sv)
  mu<-mean(theta)
  tau2<-var(theta)
  ###
  
  ### setup MCMC
  set.seed(1)
  S<-30000
  THETA<-matrix( nrow=S,ncol=m)
  MST<-matrix( nrow=S,ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:S) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      vtheta<-1/(n[j]/sigma2+1/tau2)
      etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
      theta[j]<-rnorm(1,etheta,sqrt(vtheta))
    }
    
    #sample new value of sigma2
    nun<-nu0+sum(n)
    ss<-nu0*s20;for(j in 1:m){ss<-ss+sum(( rnorm(n = n[j] ,mean = ybar[j],sd = sqrt(sv[j]) ) -theta[j])^2)}
    sigma2<-1/rgamma(1,nun/2,ss/2)
    
    #sample a new value of mu
    vmu<- 1/(m/tau2+1/g20)
    emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
    mu<-rnorm(1,emu,sqrt(vmu)) 
    
    # sample a new value of tau2
    etam<-eta0+m
    ss<- eta0*t20 + sum( (theta-mu)^2 )
    tau2<-1/rgamma(1,etam/2,ss/2)
    
    #store results
    THETA[s,]<-theta
    MST[s,]<-c(mu,sigma2,tau2)
    
  }
  
  return(list(THETA, MST) )
  
}


get_gibbs_long <- function(.data = error_dist[[2]], 
                           var_names_real = NULL ){
  
  plotting_data <- .data %>% as_tibble()
  
  var_names = paste0("theta_",seq(1,length(plotting_data),1) )
  
  plotting_data <- plotting_data %>%
    mutate(dummy = 1) %>%
    pivot_longer(-dummy, values_to = "draws", names_to = "theta") %>%
    dplyr::select(-dummy) 
  
  if( !is.null(var_names_real) ){
   
    
    for (j in 1:length(var_names) ) {
      
      plotting_data <- plotting_data %>%
        mutate(theta = 
                 ifelse(theta == paste0("V",j), var_names_real[j],theta) )
      
    }
    
    
  }
  
  return(plotting_data)
  
}
