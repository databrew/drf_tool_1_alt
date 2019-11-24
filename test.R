source('global.R')

# Log normal ci
# log_normal_statistic <- function(x, inds) {try(fitdistr(x[inds],"lognormal")$estimate, silent = TRUE)}
# bs_log_normal <- boot(data$Loss, log_normal_statistic, R = 1000)
# log_normal_ci <- boot.ci(bs_log_normal, conf=0.95, type="bca")
###############
# Simulations tab
###############
# country_name <- 'India'
# best_source  = 'EMDAT'
# damage_type = 'damage'
# 
# country_data <- country_data[country_data$country == country_name & country_data$origin == best_source & country_data$damage_type == damage_type,]
# country_frequency <- frequency_data
# country_frequency <- country_frequency[country_frequency$country == country_name & country_frequency$origin == best_source & country_frequency$damage_type == damage_type,,]
# 
# data <- country_data
# freq_data <- country_frequencyd
data <- readRDS('~/Desktop/data.rda')




dis_name = 'gamma'
data <- readRDS('~/Desktop/data.rda')


simulate_frequency <- function(data){
  
 data <- data[[1]]
 # get perils 
 temp <- data %>% group_by(peril) %>% summarise(counts = sum(value))
 present_perils <- unique(temp$peril)
 
 # subset data by present perils
 data <- data %>% filter(peril %in% present_perils)
 #### BERNULLI

 # get number of trials for bernoulli
 num_trials <- as.numeric(as.character(max(data$year))) - min(as.numeric(as.character(data$year)))
 num_trials <- num_trials + 1
 
 # loop through each peril
 data_list <- list()
 for(i in 1:length(present_perils)){
   peril_name <- present_perils[i]
   # subset by peril
   sub_data <- data[data$peril == peril_name,]
   # get bernoulli mle 
   mle_bern <- (nrow(sub_data)/num_trials)
   udis <- runif(15000, 0, 1)
   bern_data <- as.data.frame(cbind(simulation_num = 1:15000, udis = udis))
   bern_data$outcome <- ifelse(bern_data$udis < mle_bern, 1, 0)
   bern_data$peril <- peril_name
   data_list[[i]] <- bern_data
   
 }
  
 bern_data <- do.call('rbind', data_list)
 
 return(bern_data)
  
}


generate_simulation <- function(dis_name){
  if(dis_name == 'Log normal'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- rlnorm(n = 15000, meanlog = dat$`MLE 1`, sdlog = dat$`MLE 2`)
    }
  } else if (dis_name == 'Gamma'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      # check to see how much seed matters
      sim <- rgamma(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
    }
  } else if (dis_name == 'Beta'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    } else {
      sim <- rbeta(n = 15000, shape1 = dat$`MLE 1`, scale2 = dat$`MLE 2`)
    }
  }  else if (dis_name == 'Frechet'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- rfrechet(n = 15000, loc=0, scale=dat$`MLE 1`, shape=dat$`MLE 2`)
    }
  } else if (dis_name == 'Gumbel'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    } else {
      sim <- actuar::rgumbel(n = 15000, alpha = dat$`MLE 1`, scale = dat$`MLE 2`)
    }
  } else if (dis_name == 'Weibull'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- rweibull(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
    }
  } else {
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- extraDistr::rpareto(n = 15000, a = dat$`MLE 1`, b = dat$`MLE 2`)
    }
  }
  return(sim)
}







