
# this function takes a dataset an returns the detreneded data set.
detrend_linear_data <- function(dat){
  # get linear predictions
  fitted_values <- lm(data = sub_data, value ~ year)$fitted.values
  sub_data$value <- fitted_values[1] + (sub_data$value - fitted_values)
  return(sub_data)
}

# this function takes an arbitrary data set and exapands from long form to wide form.
expand_data <- function(data){
  data_combinations <- as.data.frame(expand.grid(year = unique(data$year), country = unique(data$country), peril = unique(data$peril), origin = unique(data$origin)))
  
  # homogenize variable types 
  data$damage_type <- as.character(data$damage_type)
  data$origin <- as.character(data$origin)
  data_combinations$country <- as.character(data_combinations$country)
  data_combinations$peril <- as.character(data_combinations$peril)
  # join data
  data <- left_join(data_combinations, data)
  
  return(data)
  
}

# this function simulates the frequency data based on the mle of bernoulli.
sim_bern <- function(the_right_data = NULL){
  # save(the_right_data, file = 'the_right_data.RData')
  require(tidyverse)
  # the right data should be either scaled, detrended or core data, depending on inputs
  data_list <- list()
  out <- NULL
  ok <- FALSE
  if(!is.null(the_right_data)){
    if(nrow(the_right_data) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    freq_data <- the_right_data
    num_trials <- max(freq_data$year) - min(freq_data$year)
    num_trials <- num_trials + 1
    all_perils <- unique(freq_data$peril)
    for(i in 1:length(all_perils)){
      peril_name <- all_perils[i]
      sub_dat <- freq_data %>% filter(peril == peril_name)
      if(sum(sub_dat$value) > 0){
        mle_bern <- nrow(sub_dat[sub_dat$value == 1,])/num_trials
        # mle_bern_2=fitdist(sub_dat$value, dist="binom", fix.arg=list(size=16), start=list(prob=0.1))
        
        uniform_dis <- runif(15000,0 ,1)
        uni_dat <- as.data.frame(cbind(simulation_num = 1:15000, uniform_dis = uniform_dis))
        uni_dat$value <- ifelse(uni_dat$uniform_dis < mle_bern, 1, 0)
        uni_dat$uniform_dis <- NULL
        uni_dat$simulation_num <- NULL
        uni_dat$peril <- peril_name
        uni_dat <- uni_dat[, c('peril', 'value')]
        data_list[[i]] <- uni_dat
      }
    }
   out <- do.call('rbind', data_list)
  } else {
    out <- NULL
  }
  return(out)
}

# this function fills NAs with 0, used for the frequency data
fill_na <- function(data){
  
  data_list <- list()
  for(i in 1:length(unique(data$country))){
    country_name <- unique(data$country)[i]
    sub_dat <- data[data$country == country_name,]
    sub_dat$origin[is.na(sub_dat$origin)] <- unique(sub_dat$origin)[!is.na(unique(sub_dat$origin))]
    sub_dat$damage_type[is.na(sub_dat$damage_type)] <- unique(sub_dat$damage_type)[!is.na(unique(sub_dat$damage_type))]
    sub_dat$value[is.na(sub_dat$value)] <- 0
    data_list[[i]] <- sub_dat
  }
  out <- do.call('rbind', data_list)
  out$value <- ifelse(out$value > 0, 1, 0)
  return(out)
}

# this function takes the path to the archetype data and name of archeytpe and reads in and cleans archetype data
read_in_archetype_cost_data <- function(archetype_data, archetype_names){
  # read in loss data
  # high risk middle income, storms, floods, earthquakes
  hr_mi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/hr_mi_sfe_cost.csv'), stringsAsFactors = FALSE)
 
   # change column names
  names(hr_mi_sfe) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  hr_mi_sfe$Archetype <- archetype_names[1]

  # low income drought
  li_d_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_cost.csv'),  stringsAsFactors = FALSE)
  names(li_d_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_d_cost$Archetype <-  archetype_names[4]

  # low in come drought, flood, and storm
  li_dfs_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_dfs_cost.csv'), stringsAsFactors = FALSE)
  names(li_dfs_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_dfs_cost$Archetype <-  archetype_names[6]

  # low income, storm, flood and earth quake
  li_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_cost.csv'), stringsAsFactors = FALSE)
  names(li_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_sfe_cost$Archetype <-  archetype_names[5]

  # middle income flood
  mi_f_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_cost.csv'), stringsAsFactors = FALSE)
  names(mi_f_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  mi_f_cost$Archetype <-  archetype_names[2]

  # upper midlle income storm, flood, earthquake
  umi_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_cost.csv'),  stringsAsFactors = FALSE)
  names(umi_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  umi_sfe_cost$Archetype <-  archetype_names[3]

  # combine data
  out <- rbind(hr_mi_sfe,
               li_d_cost,
               li_dfs_cost,
               li_sfe_cost,
               mi_f_cost,
               umi_sfe_cost)
  
  # name data
  out$data_type <- 'archetype_cost'
  return(out)
}

# this function takes the country name and reads in loss, cost, population.
read_in_country_data <- function(country_name) {

  # create path
  path_to_file <- paste0('data/', country_name,'/')

  # read in loss data
  loss_data <- read.csv(paste0(path_to_file, 'data_loss.csv'))
  names(loss_data) <- c('Country', 'Year', 'Peril', 'Outcome')
  loss_data$data_type <- 'Loss'

  # read in cost data
  cost_data <- read.csv(paste0(path_to_file, 'data_cost.csv'))
  names(cost_data) <- c('Country', 'Year', 'Peril', 'Outcome')
  cost_data$data_type <- 'Cost'

  # read in population data
  pop_data <-  read.csv(paste0(path_to_file, 'data_pop.csv'))
  all_data  <- plyr::rbind.fill(loss_data,
                           cost_data)

  # join with pop data
  all_data <- left_join(all_data, pop_data, by = c('Year', 'Country'))

  # read om freq for loss
  loss_freq_data <-  read.csv(paste0(path_to_file, 'freq_loss.csv'))
  loss_freq_data$data_type <- 'loss_freq'

  # read om freq for cost
  cost_freq_data <-  read.csv(paste0(path_to_file, 'freq_cost.csv'))
  cost_freq_data$data_type <- 'cost_freq'

  # combine
  all_data_freq <- rbind(loss_freq_data,
                         cost_freq_data)

  all_data_freq <- melt(all_data_freq, id.vars = c('Year','data_type'))

  names(all_data_freq)[names(all_data_freq) == 'variable'] <- 'Peril'
  names(all_data_freq)[names(all_data_freq) == 'value'] <- 'Count'
  all_data_freq$Country <- country_name

  out <- plyr::rbind.fill(all_data, all_data_freq)
  return(out)
}

# This function takes the archetype path and name and reads in frequency data for archetypes.
read_in_archetype_freq_data <- function(archetype_data, archetype_names){
  # high risk middle income storms, floods, earthquakes
  hr_mi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/hr_mi_sfe_freq.csv'))

  # change names
  names(hr_mi_sfe)[ncol(hr_mi_sfe)] <- 'Drought'
  hr_mi_sfe <- melt(hr_mi_sfe, id.vars = 'Year')
  names(hr_mi_sfe) <- c('Year', 'Peril', 'Count')
  hr_mi_sfe$Archetype <- archetype_names[[1]]

  # upper middle income storms, floods, earthquakes.
  umi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_freq.csv'))
  names(umi_sfe)[ncol(umi_sfe)] <- 'Drought'

  # change names
  umi_sfe <- melt(umi_sfe, id.vars = 'Year')
  names(umi_sfe) <- c('Year', 'Peril', 'Count')
  umi_sfe$Archetype <- archetype_names[[3]]

  # middle income floods
  mi_f <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_freq.csv'))
  names(mi_f) <-  c('Year', 'Flood', 'Drought', 'Storm', 'Earthquake')

  # change names
  mi_f <- melt(mi_f, id.vars = 'Year')
  names(mi_f) <- c('Year', 'Peril', 'Count')
  mi_f$Archetype <- archetype_names[[2]]

  # low income storms, floods, earthquakes.
  li_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_freq.csv'))
  names(li_sfe)[1] <- 'Year'
  names(li_sfe)[ncol(li_sfe)]  <-'Drought'

  # change names
  li_sfe <- melt(li_sfe, id.vars = 'Year')
  names(li_sfe) <- c('Year', 'Peril', 'Count')
  li_sfe$Archetype <- archetype_names[[5]]

  # lower income drought
  li_d <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_freq.csv'))
  names(li_d) <- c('Year', 'Drought','Storm', 'Earthquake', 'Flood')
  
  # change names
  li_d <- melt(li_d, id.vars = 'Year')
  names(li_d) <- c('Year', 'Peril', 'Count')
  li_d$Archetype <- archetype_names[[4]]

  # lower income droughts, floods, storms
  li_dfs <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_dfs_freq.csv'))
  names(li_dfs) <- 'Year'
  
  # change names
  names(li_dfs)[ncol(li_dfs)] <- 'Earthquake'
  li_dfs <- melt(li_dfs, id.vars = 'Year')
  names(li_dfs) <- c('Year', 'Peril', 'Count')
  li_dfs$Archetype <- archetype_names[[6]]

  out <- gtools::smartbind(hr_mi_sfe,
                           li_d,
                           li_dfs,
                           li_sfe,
                           mi_f,
                           umi_sfe)
  out$data_type <- 'archetype_frequency'
  return(out)
}

# this function computes the aic score ffrom the log likelihood for distributions that don't have built in R functions.
get_aic <- function(llhood){
  aic <- (-2*llhood) + 2
}

# this function takes the data and an argument whether advanced option was selected by user
# and fits a distribution for each peril and returns the mle and aic.
get_aic_mle <- function(dat, is_advanced){
  # get aic mle  by looping through perils 
  # save(dat, file = 'prefit.RData')
  
  # remove observations with zeroes.
  dat <- dat[dat$value > 0,]
  
  # get the names of the perils present in this dataset (ones with enough observations)
  temp <- dat %>% group_by(peril) %>% summarise(counts = sum(value))
  present_perils <- unique(temp$peril)
  
  # subset dat by present perils and loop through each one.
  dat <- dat %>% filter(peril %in% present_perils)
  dat_list <- list()
  for(i in 1:length(present_perils)){
    peril_name <- present_perils[i]
    sub_dat <- dat[dat$peril == peril_name,]

    # fit log normal
    log_normal <- try(fitdistrplus::fitdist(sub_dat$value,distr =  "lnorm"),silent = TRUE)
    if(class(log_normal) == 'try-error'){
      log_normal <- NULL
      log_normal_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {
      log_normal_aic <- round(log_normal$aic,2)
      if(is_advanced){
        # get bootstrap values if advanced mode
        log_normal_boot <- fitdistrplus::bootdist(log_normal, niter = 1000)
        mle_1 <- log_normal_boot[[6]]$estimate[1]
        mle_2 <- log_normal_boot[[6]]$estimate[2]
        mle_1_lower <- log_normal_boot[[5]][[3]]
        mle_2_lower <- log_normal_boot[[5]][[4]]
        mle_1_upper <- log_normal_boot[[5]][[5]]
        mle_2_upper <- log_normal_boot[[5]][[6]]
      } else {
        # get aic
        mle_1 <- log_normal$estimate[1] 
        mle_2 <- log_normal$estimate[2]
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }
    }
    # create dat frame to store aic and MLEs
    log_normal_dat <- data_frame(name = 'log_normal',
                                  aic = log_normal_aic,
                                  mle_1 = mle_1,
                                  mle_2 = mle_2,
                                 mle_1_lower =mle_1_lower,
                                 mle_2_lower = mle_2_lower,
                                 mle_1_upper = mle_1_upper,
                                 mle_2_upper = mle_2_upper)
   
    # fit the 4 parameter beta. this does not use fitdisplus and generally fails if too little observations. 
    # so if the number of observations is below 4, does not fit.
    if(nrow(sub_dat) < 4){
      beta <- NULL
      beta_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_3 <- NA
      mle_4 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_3_lower <- NA
      mle_4_lower <- NA
      mle_1_upper <- NA
      mle_2_upper <- NA
      mle_3_upper <- NA
      mle_4_upper <- NA
      # create dat frame to store aic and MLEs
      beta_dat <- data_frame(name = 'beta',
                              aic = beta_aic,
                              mle_1 = mle_1,
                              mle_2 = mle_2,
                              mle_3 = mle_3,
                              mle_4 = mle_4,
                              mle_1_lower =mle_1_lower,
                              mle_2_lower = mle_2_lower,
                              mle_3_lower = mle_3_lower,
                              mle_4_lower = mle_4_lower,
                              mle_1_upper = mle_1_upper,
                              mle_2_upper = mle_2_upper,
                              mle_3_upper = mle_3_upper,
                              mle_4_upper = mle_4_upper)
    } else {
      if(is_advanced){
        beta_dat <- bootstrap_beta(dat = sub_dat, num_iter = 100)
      } else {
        beta_dat <- bootstrap_beta(dat = sub_dat, num_iter = 1)
      }
    }
    
    # fit the gamma distribution using newton-raphson. This also does not use the fitdistplus package and generally
    # fails is not enough observations.
    if(nrow(sub_dat) < 4){
      gamma <- NULL
      gamma_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
      # create dat frame to store aic and MLEs
      gamma_dat <- data_frame(name = 'gamma',
                              aic = gamma_aic,
                              mle_1 = mle_1,
                              mle_2 = mle_2,
                              mle_1_lower =mle_1_lower,
                              mle_2_lower = mle_2_lower,
                              mle_1_upper = mle_1_upper,
                              mle_2_upper = mle_2_upper)
    } else {
      if(is_advanced){
        gamma_dat <- bootstrap_gamma(dat = sub_dat, num_iter = 1000)
      } else {
        gamma_dat <- bootstrap_gamma(dat = sub_dat, num_iter = 1)
      }
    }
    
    # fit frechet
    frechet <- try(fitdistrplus::fitdist(sub_dat$value, "frechet", start=list(lambda=0.1, mu=0.1, sigma = 0.1), method="mle"),
                   silent = TRUE)
    if(class(frechet) == 'try-error'){
      frechet <- NULL
      frechet_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
      
    } else {
      frechet_aic <- round(frechet$aic,2)
      if(is_advanced){
        # get bootstrap values
        frechet_boot <- fitdistrplus::bootdist(frechet, niter = 1000)
        mle_1 <- frechet_boot[[6]]$estimate[1]
        mle_2 <- frechet_boot[[6]]$estimate[2]
        mle_1_lower <- frechet_boot[[5]][[3]]
        mle_2_lower <- frechet_boot[[5]][[4]]
        mle_1_upper <- frechet_boot[[5]][[5]]
        mle_2_upper <- frechet_boot[[5]][[6]]
      } else {
        # get aic
        mle_1 <- frechet$estimate[1] 
        mle_2 <- frechet$estimate[2]
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }
    }
    # create dat frame to store aic and MLEs
    frechet_dat <- data_frame(name = 'frechet',
                                 aic = frechet_aic,
                                 mle_1 = mle_1,
                                 mle_2 = mle_2,
                                 mle_1_lower =mle_1_lower,
                                 mle_2_lower = mle_2_lower,
                                 mle_1_upper = mle_1_upper,
                                 mle_2_upper = mle_2_upper)
    
    
  
    # fit the gumbel distribution
    
    # dgumbel function
    dgumbel <- function(x,mu,s){ # PDF
      exp((mu - x)/s - exp((mu - x)/s))/s
    }
    
    # pgumbel function
    pgumbel <- function(q,mu,s){ # CDF
      exp(-exp(-((q - mu)/s)))
    }
    
    # qgumbel function
    qgumbel <- function(p, mu, s){ # quantile function
      mu-s*log(-log(p))
    }
    
    gumbel_fit <- try(fitdistrplus::fitdist(sub_dat$value, "gumbel", start=list(mu=0.1, s=0.1), method="mle"), silent = TRUE)
    if(class(gumbel_fit) == 'try-error'){
      gumbel_fit <- NULL
      gumbel_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
      
    } else {
     gumbel_aic <- round(gumbel_fit$aic,2)
      
      if(is_advanced){
        # get bootstrap values
        gumbel_boot <- fitdistrplus::bootdist(gumbel_fit, niter = 1000)
        mle_1 <- gumbel_boot[[6]]$estimate[1]
        mle_2 <- gumbel_boot[[6]]$estimate[2]
        mle_1_lower <- gumbel_boot[[5]][[3]]
        mle_2_lower <- gumbel_boot[[5]][[4]]
        mle_1_upper <- gumbel_boot[[5]][[5]]
        mle_2_upper <- gumbel_boot[[5]][[6]]
      } else {
        # get aic
        mle_1 <- gumbel_fit$estimate[1] 
        mle_2 <- gumbel_fit$estimate[2]
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }
    }
    # create dat frame to store aic and MLEs
    gumbel_dat <- data_frame(name = 'gumbel',
                                 aic = gumbel_aic,
                                 mle_1 = mle_1,
                                 mle_2 = mle_2,
                                 mle_1_lower =mle_1_lower,
                                 mle_2_lower = mle_2_lower,
                                 mle_1_upper = mle_1_upper,
                                 mle_2_upper = mle_2_upper)
    
    
    # fit weibull
    weibull <- try(fitdistrplus::fitdist(sub_dat$value, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
    if(class(weibull) == 'try-error'){
      weibull <- NULL
      weibull_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {
      weibull_aic <- round(weibull$aic, 2)
      
      if(is_advanced){
        # get bootstrap values
        weibull_boot <- fitdistrplus::bootdist(weibull, niter = 1000)
        mle_1 <- weibull_boot[[6]]$estimate[1]
        mle_2 <- weibull_boot[[6]]$estimate[2]
        mle_1_lower <- weibull_boot[[5]][[3]]
        mle_2_lower <- weibull_boot[[5]][[4]]
        mle_1_upper <- weibull_boot[[5]][[5]]
        mle_2_upper <- weibull_boot[[5]][[6]]
      } else {
        # get aic
        mle_1 <- weibull$estimate[1] 
        mle_2 <- weibull$estimate[2]
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }
    }
    # create dat frame to store aic and MLEs
    weibull_dat <- data_frame(name = 'weibull',
                                 aic = weibull_aic,
                                 mle_1 = mle_1,
                                 mle_2 = mle_2,
                                 mle_1_lower =mle_1_lower,
                                 mle_2_lower = mle_2_lower,
                                 mle_1_upper = mle_1_upper,
                                 mle_2_upper = mle_2_upper)
    
    # fit pareto
    pareto <- try(fitdistrplus::fitdist(sub_dat$value, "pareto", start=list(a=0.1, b=0.1), method="mle"), silent = TRUE)
    if(class(pareto) == 'try-error'){
      pareto <- NULL
      pareto_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {
      pareto_aic <- round(pareto$aic, 2)
      
      if(is_advanced){
        # get bootstrap values
        pareto_boot <- fitdistrplus::bootdist(pareto, niter = 1000)
        mle_1 <- pareto_boot[[6]]$estimate[1]
        mle_2 <- pareto_boot[[6]]$estimate[2]
        mle_1_lower <- pareto_boot[[5]][[3]]
        mle_2_lower <- pareto_boot[[5]][[4]]
        mle_1_upper <- pareto_boot[[5]][[5]]
        mle_2_upper <- pareto_boot[[5]][[6]]
      } else {
        # get aic
        mle_1 <- pareto$estimate[1] 
        mle_2 <- pareto$estimate[2]
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }
    }
    # create dat frame to store aic and MLEs
    pareto_dat <- data_frame(name = 'pareto',
                              aic = pareto_aic,
                              mle_1 = mle_1,
                              mle_2 = mle_2,
                              mle_1_lower =mle_1_lower,
                              mle_2_lower = mle_2_lower,
                              mle_1_upper = mle_1_upper,
                              mle_2_upper = mle_2_upper)
    
    # create a dat frame out of dat results
    aic_mle_dat <- bind_rows(log_normal_dat,
                          gamma_dat,
                          beta_dat,
                          frechet_dat,
                          gumbel_dat,
                          weibull_dat,
                          pareto_dat)
    
    save(aic_mle_dat, file = 'test_cols.RData')
    
    # change names of variable
    names(aic_mle_dat) <- c('distribution', 'aic', 'mle1', 'mle2', 'mle1_lower', 'mle2_lower', 'mle1_upper', 'mle2_upper',
                            'mle3', 'mle4', 'mle3_lower', 'mle4_lower', 'mle3_upper', 'mle4_upper')
    
    # capitalize and remove underscore of distribution
    aic_mle_dat$distribution <- Hmisc::capitalize(aic_mle_dat$distribution)
    aic_mle_dat$distribution <- gsub('_', ' ', aic_mle_dat$distribution)
    aic_mle_dat$aic <- round(aic_mle_dat$aic, 2)
    aic_mle_dat$mle1<- aic_mle_dat$mle1
    aic_mle_dat$mle2 <- aic_mle_dat$mle2
    
    aic_mle_dat$peril <- peril_name
    
    dat_list[[i]] <- aic_mle_dat
    print(i)
  }
  aic_dat <- do.call('rbind', dat_list)
  return(aic_dat)
}

# Function for making core data a one row per peril-year data set
transform_core_data <- function(cdx){
  if(is.null(cdx)){
    return(NULL)
  }
  if(!is.list(cdx)){
    return(NULL)
  }
  if(length(cdx) != 2){
    return(NULL)
  }
  cdx2 <- cdx[[2]]
  cdx1 <- cdx[[1]]
  cdx1 <- cdx2 %>%
    dplyr::select(-value) %>%
    left_join(cdx1) %>%
    mutate(value = ifelse(is.na(value), 0, value))
  out <- list(cdx1, cdx2)
  return(out)
}

# Transofrm core data back to long format
elongate_core_data <- function(cdx){
  if(is.null(cdx)){
    return(NULL)
  }
  if(!is.data.frame(cdx)){
    return(NULL)
  }
  if(nrow(cdx) == 0){
    return(NULL)
  }
  names(cdx)[1:2] <- tolower(names(cdx)[1:2])
  print(head(cdx))
  out <- cdx %>% gather(peril, value, names(cdx)[3:ncol(cdx)])
  return(out)
}

# function widens the data from long to wide format.
widen_core_data <- function(cdx){
  if(is.null(cdx)){
    return(cdx)
  }
  cdx <- cdx %>%
    tidyr::spread(key = peril, value = value)
  names(cdx)[1:2] <- c('Year', 'Country')
  return(cdx)
}

# this function converts values to frequency data.
values_to_frequency <- function(cdx){
  message('Converting a values table to a frequency table')
  message('--- the original values table looks like')
  print(head(cdx))
  # takes the first item in the core-data() list and returns its frequency equivalent
  if(is.null(cdx)){
    return(NULL)
  }
  these_columns <- names(cdx)[!tolower(names(cdx)) %in% c('country', 'year', 'peril')]
  for(j in 1:length(these_columns)){
    this_column <- these_columns[j]
    old_vals <- cdx[,this_column]
    new_vals <- ifelse(old_vals > 0, 1, 0)
    cdx[,this_column] <- new_vals
  }
  message('--- the new frequency table looks like')
  print(head(cdx))
  return(cdx)
}

# this function take the name of the distribution with the lowest aic score and the corresponding 
# mle and aic values, then simulates 15000 observations.
make_simulation <- function(dis_name, dat){
  if(dis_name == 'Log normal'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rlnorm(n = 15000, meanlog = dat$`mle1`, sdlog = dat$`mle2`)
      sim_lower <- rlnorm(n = 15000, meanlog = dat$`mle1_lower`, sdlog = dat$`mle2_lower`)
      sim_upper <- rlnorm(n = 15000, meanlog = dat$`mle1_upper`, sdlog = dat$`mle2_upper`)
     
    }
  } else if (dis_name == 'Gamma'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      # check to see how much seed matters
      sim <- rgamma(n = 15000, shape = dat$`mle1`, scale = dat$`mle2`)
      sim_lower <- rgamma(n = 15000, shape = dat$`mle1_lower`, scale = dat$`mle2_lower`)
      sim_upper <- rgamma(n = 15000, shape = dat$`mle1_upper`, scale = dat$`mle2_upper`)
      
    }
  } else if (dis_name == 'Beta'){
    if(any(is.na(dat$aic))){
      sim <- NA
    } else {
      sim <- rBeta_ab(n = 15000, shape1 = dat$`mle1`, shape2 = dat$`mle2`, a = dat$`mle3`, b = dat$`mle4`)
      sim_lower <- rBeta_ab(n = 15000, shape1 = dat$`mle1_lower`, shape2 = dat$`mle2_lower`, 
                            a = dat$`mle3_lower`, b = dat$`mle4_lower`)
      sim_upper <- rBeta_ab(n = 15000, shape1 = dat$`mle1_upper`, shape2 = dat$`mle2_upper`, 
                         a = dat$`mle3_upper`, b = dat$`mle4_upper`)
      
    }
  }  else if (dis_name == 'Frechet'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rfrechet(n = 15000,lambda = dat$`mle1`, mu = dat$`mle2`)
      sim_lower <- rfrechet(n = 15000,lambda = dat$`mle1_lower`, mu = dat$`mle2_lower`)
      sim_upper <- rfrechet(n = 15000,lambda = dat$`mle1_upper`, mu = dat$`mle2_upper`)
      
    }
  } else if (dis_name == 'Gumbel'){
    if(any(is.na(dat$aic))){
      sim <- NA
    } else {
      sim <- actuar::rgumbel(n = 15000, alpha = dat$`mle1`, scale = dat$`mle2`)
      sim_lower <- actuar::rgumbel(n = 15000, alpha = dat$`mle1_lower`, scale = dat$`mle2_lower`)
      sim_upper <- actuar::rgumbel(n = 15000, alpha = dat$`mle1_upper`, scale = dat$`mle2_upper`)
      
    }
  } else if (dis_name == 'Weibull'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rweibull(n = 15000, shape = dat$`mle1`, scale = dat$`mle2`)
      sim_lower <- rweibull(n = 15000, shape = dat$`mle1_lower`, scale = dat$`mle2_lower`)
      sim_upper <- rweibull(n = 15000, shape = dat$`mle1_upper`, scale = dat$`mle2_upper`)
      
    }
  } else {
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- extraDistr::rpareto(n = 15000, a = dat$`mle1`, b = dat$`mle2`)
      sim_lower <- extraDistr::rpareto(n = 15000, a = dat$`mle1_lower`, b = dat$`mle2_lower`)
      sim_upper <- extraDistr::rpareto(n = 15000, a = dat$`mle1_upper`, b = dat$`mle2_upper`)
      
    }
  }
  sim <- as.data.frame(cbind(sim, sim_lower, sim_upper))

  return(sim)
}

# this function takes the data, after all user inputs and checks to see if the data is ready to be fit.
fit_distribution <- function(the_right_data = NULL, advanced_mode){
  require(tidyverse)
  # the right data should be either scaled, detrended or core data, depending on inputs
  out <- NULL
  ok <- FALSE
  if(!is.null(the_right_data)){
    if(nrow(the_right_data) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    the_right_data <- the_right_data[the_right_data$value > 0,]
    out <- get_aic_mle(the_right_data, is_advanced = advanced_mode)
  }
  return(out)
}

# this function finds the distribution with the smallest aic score
filter_distribution <- function(fitted_distribution = NULL){
  # fitted_distribution should be a 5 column df as produced by
  # fit_distribution
  out <- NULL
  ok <- FALSE
  if(!is.null(fitted_distribution)){
    if(nrow(fitted_distribution) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    out <- fitted_distribution %>%
      group_by(peril) %>%
      mutate(min_aic = min(aic, na.rm = TRUE)) %>%
      ungroup %>%
      filter(aic == min_aic) %>%
      dplyr::distinct(peril, .keep_all = TRUE)
  } else {
    out <- NULL
  }
  return(out)
}

# this function prepares the data for simulations
prepare_simulations <- function(fitted_distribution = NULL,
                                dist_flood = NULL,
                                dist_drought = NULL,
                                dist_storm = NULL,
                                dist_earthquake = NULL){
  # fitted_distribution should be a 5 column df as produced by
  # fit_distribution
  
  if(is.null(dist_flood)){
    dist_flood = 'Gamma'  
  }
  if(is.null(dist_drought)){
    dist_drought = 'Gamma'  
  }
  if(is.null(dist_storm)){
    dist_storm = 'Gamma'  
  }
  if(is.null(dist_earthquake)){
    dist_earthquake = 'Gamma'  
  }
  
  out <- NULL
  ok <- FALSE
  if(!is.null(fitted_distribution)){
    if(nrow(fitted_distribution) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    out <- fitted_distribution %>% mutate(keep = FALSE) 
    out <- out %>%
      filter(
        (peril == 'Flood' & distribution == dist_flood) | 
          (peril == 'Drought' & distribution == dist_drought) | 
          (peril == 'Storm' & distribution == dist_storm) | 
          (peril == 'Earthquake' & distribution == dist_earthquake) 
      )
  }
  return(out)
}

# this checks if data is ready to be simulation, the simulates the data with "make_simulations" function.
# Then multiplies the simulated loss data by the simulated frequency data.
run_simulations <- function(prepared_simulation_data = NULL, prepared_frequency_data = NULL){
  # prepared_simulation_data is the format that comes
  # from the prepare_simulations function, a 5 column df as produced by
  # fit_distribution, but filtered down to only include 1 distribution for 
  # each peril, ie only 4 rows total
  # column names should be distribution, peril, mle1, mle2, aic
  out <- NULL
  ok <- FALSE
  print(head(prepared_simulation_data))
  if(!is.null(prepared_simulation_data)){
    if(nrow(prepared_simulation_data) > 0){
      ok <- TRUE
    }
  }
  
  if(ok){
    prepared_simulation_data <- prepared_simulation_data %>% filter(!is.na(aic))
    perils <- sort(unique(prepared_simulation_data$peril))
    names(prepared_frequency_data) <- c('peril', 'freq')
    # The following is fake data code
    out_list <- list()
    for(i in 1:length(perils)){
      this_peril <- perils[i]
      sub_peril <- prepared_simulation_data %>% filter(peril == this_peril)
      sub_peril_freq <- prepared_frequency_data %>% filter(peril == this_peril)
      x <-make_simulation(dis_name = sub_peril$distribution, dat = sub_peril)
      out_list[[i]] <- tibble(key = this_peril,
                              value = x$sim,
                              value_lower = x$sim_lower,
                              value_upper = x$sim_upper,
                              # freq is still not working
                              freq = sub_peril_freq$freq) %>% 
        mutate(outcome = freq * value,
               outcome_lower = freq * value_lower,
               outcome_upper = freq * value_upper)
    }
    out <- bind_rows(out_list)

    # returns a dataframe with key, value, freq, n, and outcome
  }
  return(out)
}

# Plot simulations
plot_simulations <- function(rs = NULL,
                             right_data = NULL,
                             peril = NULL,
                             overlap = NULL){
  # ran_simulations gets generated in run_simulations
  # it should be a 3 column df with columns key, value and freq
  # peril must be one of the peril types
  if(is.null(peril)){
    peril <- 'Flood'
  }
  if(is.null(overlap)){
    overlap <- c()# c('Observed data', 'Simulated data')
  }
  out <- ggplot() +
    theme_bw()
  ok <- FALSE
  rd <- right_data
  # Just keep the first part (since it's showing the values only)
  rd <- rd[[1]]
  if(!is.null(rs)){
    if(nrow(rs) > 0){
      if(!is.null(rd)){
        if(nrow(rd) > 0){
          ok <- TRUE
        }
      }
    }
  }
  
  if(ok)
    # Filter
    pd <- rs %>%
    filter(key == peril) %>%
    mutate(data_type = 'Simulated data') %>%
    dplyr::select(value, data_type)
  # Add the observed data
  observed_data <- rd %>% dplyr::select(value) %>%
    mutate(data_type = 'Observed data')
  pd <- pd %>% bind_rows(
    observed_data)  
  
  pd <- pd %>% filter(data_type %in% overlap)
  if(nrow(pd) == 0){
    out <- ggplot() +
      labs(title = paste0('No data to show for ', peril)) +
      theme_bw()
  } else {
    out <- ggplot(data = pd,
                  aes(x = value)) +
      geom_density(aes(fill = data_type),
                   alpha = 0.7) +
      scale_fill_brewer(name = 'Data type',
                        type = 'qual') +
      theme(legend.position = 'bottom') +
      theme_bw()
  }
  return(out)
}

# Function for conditional coloring, etc. in the tab panel headers
tab_maker <- function(n = 1,
                      label = 'TOOL SETTINGS',
                      input,
                      tab_data){
  label_n <- n
  if(n == 4){
    label_n <- 3.5
  }
  if(n == 5){
    label_n <- 4
  }
  
  # Get tab info
  tab_name <- input$tabs
  td <- tab_data$data
  tab_number <- td %>% filter(name == tab_name) %>% .$number
  the_text <- '&#10004;'
  if(tab_number >=n){
    the_color <- '#F05023'
    the_circle <- 'circle'
  } else {
    the_color <- '#707372'
    the_circle <- 'greycircle'
  }
  if(tab_number <= n){
    the_text <- label_n
  }
  
  HTML(paste0('<div style="width: 100%; margin: 0 auto; text-align: center"><div class="', the_circle, '">', the_text, '</div><h4 style = "width: 100%; color: ', the_color, '; margin: 0 auto; text-align: center">', label, '</h4></div>'))
}

# Function for allowing / prohibiting tab changes (based on amount of time since last change)
# (The purpose of this is to prevent the back-and-forthy weirdness)
not_too_fast <- function(old_time, new_time){
  time_difference <- as.numeric(difftime(new_time, old_time, units = 'secs'))
  if(time_difference > 0.5){
    TRUE
  } else {
    FALSE
  }
}

quant_that <- function(dat_sim,
                       dat = NULL){
  if(is.null(dat)){
    dat <- dat_sim
  }
  save(dat_sim, file = 'quant_that.RData')
  # get normal point estimate
  output <- quantile(dat_sim$value,c(0.8,0.9, 0.96,0.98,0.99))
  annual_avg = round(mean(dat_sim$value), 2)
  
  # get upper point estimate
  annual_avg_upper <- round(mean(dat_sim$value_upper), 2) 
  output_upper <- quantile(dat_sim$value_upper,c(0.8,0.9, 0.96,0.98,0.99), na.rm = TRUE)
  
  # lower point estimate
  annual_avg_lower <- round(mean(dat_sim$value_lower), 2)
  output_lower <- quantile(dat_sim$value_lower,c(0.8,0.9, 0.96,0.98,0.99), na.rm = TRUE)
 
  # create sub_data frame sub_dat to store output with chart labels
  sub_dat <- data_frame(`Annual average` = annual_avg,
                        `1 in 5 Years` = output[1],
                        `1 in 10 Years` = output[2],
                        `1 in 25 Years` = output[3],
                        `1 in 50 Years` = output[4],
                        `1 in 100 Years` = output[5],
                        `Highest historical annual loss` = max(dat$value),
                        `Most recent annual loss` = dat$value[nrow(dat)])
  
  # melt the sub_data frame to get value and variable
  sub_dat <- melt(sub_dat)
  sub_dat$value_lower <- c(annual_avg_lower, output_lower[1], output_lower[2], output_lower[3], output_lower[4],
                           output_lower[5], max(dat$value), dat$value[nrow(dat)]) 
  sub_dat$value_upper <- c(annual_avg_upper, output_upper[1], output_upper[2], output_upper[3], output_upper[4],
                           output_upper[5], max(dat$value), dat$value[nrow(dat)])
  return(sub_dat)
}

# this function tests if linear trend exists in data.
test_linear_trend <- function(dat){
  
  peril_names <- unique(dat$peril)
  result_list <- list()
  for(i in 1:length(peril_names)){
    this_peril <- peril_names[i]
    sub_dat <- dat %>% filter(peril == this_peril)
    lm_ob <- lm(data = sub_dat, value ~ year)
    t_test <- t.test(lm_ob$fitted.values, sub_dat$value)$p.value
    result_data <- data_frame(peril = this_peril,
                              p_value = t_test)
    result_list[[i]] <- result_data
  }
  out <- do.call('rbind', result_list)
  return(out)
}

# this function is used while fitting the gamma distribution
run_newton_raphson <- function(f, df, x0, eps=1e-08, maxiter=1000, ...) {
  if(!exists("ginv")) library(MASS)
  x <- x0
  t <- 0
  repeat {
    t <- t + 1
    x.new <- x - as.numeric(ginv(df(x, ...)) %*% f(x, ...))
    if(mean(abs(x.new - x)) < eps | t >= maxiter) {
      
      if(t >= maxiter) warning("Maximum number of iterations reached!")
      break
    }
    x <- x.new
  }
  out <- list(solution=x.new, value=f(x.new, ...), iter=t)
  return(out)
}

# Derivative of the gamma log-likelihood
dl <- function(theta, X, n) {
  
  alpha <- theta[1]
  beta <- theta[2]
  o1 <- -n * log(beta) - n * digamma(alpha) + sum(log(X))
  o2 <- -n * alpha / beta + n * mean(X) / beta**2
  return(c(o1, o2))
  
}

# Second derivative of the gamma log-likelihood
ddl <- function(theta, X, n) {
  
  alpha <- theta[1]
  beta <- theta[2]
  o11 <- -n * trigamma(alpha)
  o12 <- -n / beta
  o22 <- -n * (2 * mean(X) / beta**3 - alpha / beta**2)
  return(matrix(c(o11, o12, o12, o22), 2, 2, byrow=TRUE))
  
}

# Gamma method of moments estimation
gamma_mme <- function(X) {
  m <- mean(X)
  v <- var(X)
  return(c(m**2 / v, v / m))
}

# fits the gamma distribution using newton raphson
gamma_fit <- function(dat, num_iter){
  x <- dat$value
  n <- length(x)
  mme <- gamma_mme(x)                             # method of moments estimator
  mle <- run_newton_raphson(dl, ddl, mme, X=x, n=n)$solution  # maximum likelihood estimator
  alpha = mle[1]
  beta = mle[2]
  
  # sufficient stats:
  xbar <- mean(x)
  logxbar <- mean(log(x))
  rhs <- log(xbar) - logxbar
  # init
  ll_hood <- sum(dgamma(x,shape = alpha,scale=beta,log=TRUE))
  return(list(ll_hood = ll_hood, alpha = alpha, beta = beta))
}

# function for implementing bootstrap using gamma_mle_ll
bootstrap_gamma <- function(dat, num_iter){
  gamma_results <- list()
  for(i in 1:num_iter){
    set.seed(i)
    dat <- dat %>% dplyr::filter(value > 0)
    if(num_iter == 1){
      sample_index <- 1:nrow(dat)
    } else {
      sample_index <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
    }
    # get new data 
    new_data <- dat[sample_index,]
    newton_rapshon <-try(gamma_fit(new_data), silent = TRUE)
    if(class(newton_rapshon) == 'try-error'){
      gamma_data <- data_frame(aic = NA, mle1 = NA, mle2 = NA)
    } else {
      ll_hood <- newton_rapshon$ll_hood
      mle1 <- newton_rapshon$alpha
      mle2 <- newton_rapshon$beta
      aic <- get_aic(ll_hood)
      gamma_data <- data_frame(aic = aic, 
                               mle1 = mle1, 
                               mle2 = mle2)
    }
    gamma_results[[i]] <- gamma_data
  }
  fitted_data <- do.call('rbind', gamma_results)
  # get aic at the 50th percentile
  aic <- quantile(fitted_data$aic, 0.5, na.rm = TRUE)
  # get mle1 and mle2, lower mid and upper
  mles_1 <- quantile(fitted_data$mle1, c(0.025, 0.5, 0.975), na.rm = TRUE)
  mles_2 <- quantile(fitted_data$mle2, c(0.025, 0.5, 0.975), na.rm = TRUE)
  # store results in dataframe
  out <- data_frame(name = 'Gamma', 
                    aic = aic,
                    mle_1 = mles_1[2],
                    mle_2 = mles_2[2],
                    mle_3 = NA,
                    mle_4 = NA,
                    mle_1_lower = mles_1[1],
                    mle_2_lower = mles_2[1],
                    mle_3_lower = NA,
                    mle_4_lower = NA,
                    mle_1_upper = mles_1[3],
                    mle_2_upper = mles_2[3],
                    mle_3_upper = NA,
                    mle_4_upper = NA)
  return(out)
}

# density function for four parameter beta
dBeta_ab <-function(x, shape1=2, shape2=3, a = 0, b=1, params = list(shape1, shape2, a, b),...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  out <- (x>=a & x<=b) * dbeta((x-a)/(b-a),shape1,shape2)/(b-a)
  return(out)
}

# probability distribution function for 4 parameter beta
pBeta_ab <- function(q, shape1=2, shape2=3, a = 0, b=1, params = list(shape1=2, shape2 = 5, a = 0, b = 1),...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  out <- pbeta((q-a)/(b-a),shape1,shape2)
  return(out)
}

# quantile function for four parameter beta
qBeta_ab <- function(p, shape1=2, shape2=3, a = 0, b=1, params = list(shape1=2, shape2 = 5, a = 0, b = 1),...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  out <- (b-a)*qbeta(p,shape1,shape2) + a
  return(out)
}

# random number generator for 4 parameter beta
rBeta_ab <- function(n, shape1=2, shape2=3, a = 0, b = 1, params = list(shape1, shape2, a, b),...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  X <- rbeta(n,shape1,shape2)
  out <- (b-a)*X + a
  return(out)
}

# maximum likelihood for 4 parameter beta
eBeta_ab <- function(X,w, method ="numerical.MLE",...){
  n <- length(X)
  w <- rep(1,n)
  {
    if(method != "numerical.MLE") warning(paste("method ", method, " is not avaliable, use numerial.MLE instead."))  
    method = "numerical.MLE"  
    
    d <- max(X)-min(X)
    est.par <- wmle(X=X, w=w, distname = "Beta_ab",
                    initial=list(shape1=3,shape2=3,a=min(X)-0.1*d,b=max(X)+0.1*d),
                    lower=list(shape1=1,shape2=1,a=-Inf,b=max(X)),
                    upper=list(shape1=Inf,shape2=Inf,a=min(X),b=Inf))
    
    # est.par.se <- try(sqrt(diag(solve(attributes(est.par)$nll.hessian))),silent=TRUE)
    # if(class(est.par.se) == "try-error") {
    est.par.se <- rep(NA, length(est.par))
    # } 
  } 
  attributes(est.par)$ob <- X
  attributes(est.par)$weights <- w
  attributes(est.par)$distname <- "Beta_ab"
  attributes(est.par)$method <- method
  attributes(est.par)$par.name <- c("shape1","shape2","a","b")
  attributes(est.par)$par.type <- c("shape","shape","boundary","boundary")
  attributes(est.par)$par.vals <- c(est.par$shape1, est.par$shape2, est.par$a, est.par$b)
  attributes(est.par)$par.s.e <-  est.par.se  
  class(est.par) <- "eDist"
  return(est.par)
}


## (weighted) (log) likelihood function for 4 parameter beta
lBeta_ab <- function(X, w, shape1=2, shape2 =3, a = 0, b = 1,  params = list(shape1, shape2, a, b), logL = TRUE,...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  n <- length(X)
  if(missing(w)){
    w <- rep(1,n)
  } else {
    w <- n*w/sum(w)
  }
  #     ll <- sum(w*((shape1-1)*log(X-a)+(shape2-1)*log(b-X)-log(beta(shape1,shape2))-(shape1+shape2-1)*log(b-a)))
  ll <- sum(w*log(dBeta_ab(x=X,params = params)))
  l <- exp(ll)
  if(logL) {return(ll)} else{return(l)}
}

# (weighted) score vectors for 4 parameter beta
sBeta_ab <- function(X, w, shape1=2, shape2 =3, a = 0, b = 1,  params = list(shape1, shape2, a, b),...){
  if(!missing(params)){
    shape1 <- params$shape1
    shape2 <- params$shape2
    a <- params$a
    b <- params$b
  }
  
  n <- length(X)
  if(missing(w)){
    w <- rep(1,n)
  } else {
    w <- n*w/sum(w)
  }
  
  score1 <- sum(w*(digamma(shape1+shape2)-digamma(shape1)+log(X-a)-log(b-a)))
  score2 <- sum(w*(digamma(shape1+shape2)-digamma(shape2)+log(b-X)-log(b-a)))
  score3 <- sum(w*((shape1+shape2-1)/(b-a)-(shape1-1)/(X-a)))
  score4 <- sum(w*((shape2-1)/(b-X)-(shape1+shape2-1)/(b-a)))
  
  score <- c(score1,score2,score3,score4)
  names(score) <- c("shape1","shape2","a","b")
  return(score)
}

# weighted maximum likelihood (we keep the weights constant) for 4 parameter beta
wmle <- function(X, w, distname, initial, lower, upper, loglik.fn, score.fn, obs.info.fn){
    n <- length(X)
    
    w <- rep(1,n)
    
    w <- n*w/sum(w)
    par.name <- names(initial)
    
    # prepare log-likelihood function (ll)
    
    if(exists(paste("l",distname,sep=""), mode = "function")) {
      ldist <- get(paste("l",distname,sep=""))
      ll <- function(params) {ldist(X,w,params=as.list(params))}
    } else {
      ddist <- get(paste("d",distname,sep=""))
      ll <- function(params) {sum(w*log(ddist(x=X,params=as.list(params))))}
    }
    
    nll <- function(par) {-ll(as.list(par))}
    
    # prepare gradient of log-likelihood function (ll.gr, score function)
    
    if(exists(paste("s",distname,sep=""), mode = "function")) {
      gr <- TRUE
      sdist <- get(paste("s",distname,sep=""))
      ll.gr <- function(params) {sdist(X,w,params=params)}
    } else {
      gr <- FALSE
    }
    
    # prepare hessian of log-likelihood function (ll.hess, score function)
    if(exists(paste("i",distname,sep=""), mode = "function")) {
      hess <- TRUE
      idist <- get(paste("i",distname,sep=""))
      ll.hess <- function(params) {-idist(X,w,params=params)}
    } else {
      hess <- FALSE
    }
    
    # transform parameter space
    # set rules to transform all bounded intervals to whole real line
    num.par <- max(length(lower),length(upper))
    trans.fn <- vector("list",num.par)
    trans.fn.deriv <- vector("list",num.par)
    trans.fn.dd <- vector("list",num.par)    
    inv.trans.fn <- vector("list",num.par)
    
    for(k in 1:num.par) {
      if(lower[[k]] == -Inf & upper[[k]] == Inf) {
        trans.fn[[k]] <- function(y){y}
        trans.fn.deriv[[k]] <- function(y) {1}
        trans.fn.dd[[k]] <- function(y) {0}
        inv.trans.fn[[k]] <- function(y){y}
      } else if(lower[[k]] != -Inf & upper[[k]] == Inf) {
        trans.fn[[k]] <- function(y){log(y-lower[[k]])}
        body(trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn[[k]])[2],fixed=T))[[1]]
        
        trans.fn.deriv[[k]] <- function(y){1/(y-lower[[k]])}
        body(trans.fn.deriv[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.deriv[[k]])[2],fixed=T))[[1]]
        
        trans.fn.dd[[k]] <- function(y){-1/(lower[[k]]-y)^2}
        body(trans.fn.dd[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.dd[[k]])[2],fixed=T))[[1]]
        
        inv.trans.fn[[k]] <- function(y){lower[[k]]+exp(y)}
        body(inv.trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(inv.trans.fn[[k]])[2],fixed=T))[[1]]
        
      } else if(lower[[k]] == -Inf & upper[[k]] != Inf) {
        trans.fn[[k]] <- function(y){log(upper[[k]]-y)}
        body(trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn[[k]])[2],fixed=T))[[1]]
        
        trans.fn.deriv[[k]] <- function(y){-1/(upper[[k]]-y)}
        body(trans.fn.deriv[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.deriv[[k]])[2],fixed=T))[[1]]
        
        trans.fn.dd[[k]] <- function(y){-1/(upper[[k]]-y)^2}
        body(trans.fn.dd[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.dd[[k]])[2],fixed=T))[[1]]
        
        inv.trans.fn[[k]] <- function(y){upper[[k]]-exp(y)}
        body(inv.trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(inv.trans.fn[[k]])[2],fixed=T))[[1]]
        
      } else if(lower[[k]] != -Inf & upper[[k]] != Inf) {
        trans.fn[[k]] <- function(y){logit((upper[[k]]-y)/(upper[[k]]-lower[[k]]))}
        body(trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn[[k]])[2],fixed=T))[[1]]
        
        trans.fn.deriv[[k]] <- function(y){ ifelse(y==upper[[k]] | y==lower[[k]], -Inf, 1/(lower[[k]]-y)+1/(y-upper[[k]]))}
        body(trans.fn.deriv[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.deriv[[k]])[2],fixed=T))[[1]]
        
        trans.fn.dd[[k]] <- function(y){ ifelse(y==upper[[k]], -Inf, ifelse(y==lower[[k]], Inf, 1/(lower[[k]]-y)^2-1/(y-upper[[k]])^2))}
        body(trans.fn.dd[[k]])[[2]] <- parse(text=gsub("k",k,body(trans.fn.dd[[k]])[2],fixed=T))[[1]]
        
        inv.trans.fn[[k]] <- function(y){upper[[k]]-invlogit(y)*(upper[[k]]-lower[[k]])}
        body(inv.trans.fn[[k]])[[2]] <- parse(text=gsub("k",k,body(inv.trans.fn[[k]])[2],fixed=T))[[1]]
        
      } else {
        stop("the boundary parameters are not set appropriately!")
      }
    }
    
    # function transform point from original parameter space to transformed space
    do.call.list <- function(what, args,...) {do.call(what=what,args=list(args),...)}
    trans.par.fn <- function(point){
      out <- mapply(do.call.list, trans.fn, point)
      names(out) <- names(unlist(point))
      return(as.list(out))
    }
    #     trans.par.fn(initial)
    #     trans.par.fn(unlist(initial))
    
    # trans.initial - transformed initial point
    trans.initial <- trans.par.fn(initial)
    
    # function transform point from transformed parameter space to original space
    inv.trans.par.fn <- function(point){
      out <- mapply(do.call.list, inv.trans.fn, point)
      names(out) <- names(unlist(point))
      return(as.list(out))
    }
    #     inv.trans.par.fn(trans.initial)
    #     inv.trans.par.fn(unlist(trans.initial))
    
    # log-likelihood function for transformed parameters    
    trans.ll <- function(trans.arg) { return(ll(inv.trans.par.fn(trans.arg)))}
    trans.nll <- function(trans.arg) {-trans.ll(trans.arg)}
    
    
    # gradient of ll function for transformed parameters 
    if(gr==TRUE){
      trans.ll.gr <- function(trans.arg) { 
        inv.trans.arg <- vector("list",length = num.par)
        deriv <- NULL
        for(k in 1:num.par){
          inv.trans.arg[[k]] <- as.numeric(inv.trans.fn[[k]](trans.arg[[k]]))
          deriv[k] <- as.numeric(trans.fn.deriv[[k]](inv.trans.arg[[k]]))
        }
        names(inv.trans.arg) <- names(initial)
        return(ll.gr(params <-  as.list(inv.trans.arg))/deriv)
      }
      trans.nll.gr <- function(trans.arg) {-trans.ll.gr(trans.arg)}
      
      # hessian of ll function for transformed parameters 
      if(hess==TRUE){
        trans.ll.hess <- function(trans.arg) { 
          inv.trans.arg <- vector("list",length = num.par)
          deriv <- NULL
          dd <- NULL
          for(k in 1:num.par){
            inv.trans.arg[[k]] <- as.numeric(inv.trans.fn[[k]](trans.arg[[k]]))
            deriv[k] <- as.numeric(trans.fn.deriv[[k]](inv.trans.arg[[k]]))
            dd[k] <- as.numeric(trans.fn.dd[[k]](inv.trans.arg[[k]]))
          }
          names(inv.trans.arg) <- names(initial)
          return( (ll.hess(params <- as.list(inv.trans.arg)) - diag(ll.gr(params <-  as.list(inv.trans.arg))/deriv*dd)) / 
                    deriv%*%t(deriv) )
        }
        trans.nll.hess <- function(trans.arg) {-trans.ll.hess(trans.arg)}
      }
      
    }
    
    ## maximize ll (minimize nll)
    available.methods <- c("BFGS", "CG", "Nelder-Mead", "L-BFGS-B", "nlm", "nlminb")
    
    convergence <- FALSE
    approx.rst.list <- NULL
    for(method in available.methods) {
      if(gr == TRUE){
        if(hess == TRUE) {
          rst <- try(suppressWarnings(optimx::optimx( par=unlist(trans.initial), fn=trans.nll, 
                                                      gr=trans.nll.gr, hess=trans.nll.hess,
                                                      method = method, hessian = TRUE)))
        } else {
          rst <- try(suppressWarnings(optimx::optimx( par=unlist(trans.initial), fn=trans.nll, 
                                                      gr=trans.nll.gr, 
                                                      method = method, hessian = TRUE)))
        }
      } else {
        rst <- try(suppressWarnings(optimx::optimx( par=unlist(trans.initial), fn=trans.nll, 
                                                    method = method, hessian = TRUE)))
      }
      
      if(any(class(rst)=="try-error")) next
      
      if(with(rst,convcode!=0)) {approx.rst.list <- rbind(approx.rst.list, rst);next}
      
      trans.est.par <- coef(rst)
      est.par <- as.vector(by(1:num.par, 1:num.par,FUN= function(k){inv.trans.fn[[k]](trans.est.par[[k]])}))
      names(est.par) <- par.name
      
      if(!all(is.finite(est.par))) next
      
      if(with(rst, any(!is.na(c(kkt1,kkt2))) & all(kkt1,kkt2,na.rm=TRUE))) {
        convergence <- TRUE
        break
      } else {
        approx.rst.list <- rbind(approx.rst.list, rst)
      }
    }
    
    if(!convergence) {
      rst <- approx.rst.list[with(approx.rst.list, value==min(value, NA.rm=T)),]
      
      trans.est.par <- coef(rst)
      est.par <- as.vector(by(1:num.par, 1:num.par,FUN= function(k){inv.trans.fn[[k]](trans.est.par[[k]])}))
      names(est.par) <- par.name
    }
    
    est.par <- as.list(est.par)
    
    if(gr & is.numeric(attributes(rst)$details[[2]])){
      deriv <- as.vector(by(1:num.par, 1:num.par,FUN= function(k){trans.fn.deriv[[k]](est.par[[k]])}))
      
      trans.nll.gr <- attributes(rst)$details[[2]]
      nll.gr <- trans.nll.gr*deriv
      names(nll.gr) <- par.name
    } else {
      nll.gr <- try(suppressWarnings(grad(nll, unlist(est.par))),silent=TRUE)
      if(any(class(nll.gr)=="try-error")) {nll.gr <- rep(NA, num.par)}
      names(nll.gr) <- par.name
    }
    
    
    if(hess) {
      deriv <- as.vector(by(1:num.par, 1:num.par,FUN= function(k){trans.fn.deriv[[k]](est.par[[k]])}))
      dd <- as.vector(by(1:num.par, 1:num.par,FUN= function(k){trans.fn.dd[[k]](est.par[[k]])}))
      
      trans.nll.hess <- attributes(rst)$details[[3]]
      nll.hessian <- diag(trans.nll.gr*dd) + trans.nll.hess * (deriv%*%t(deriv))
      colnames(nll.hessian) <- rownames(nll.hessian) <- par.name
    } else{
      nll.hessian <- pracma::hessian(nll,unlist(est.par))
      colnames(nll.hessian) <- rownames(nll.hessian) <- par.name
    }
    
    
    attributes(est.par)$nll.hessian <- nll.hessian
    attributes(est.par)$nll.gr <- nll.gr
    attributes(est.par)$nll <- rst$value
    
    attributes(est.par)$optim.fn <- paste0("optimx-",attributes(rst)$details[[1]])
    
    return(est.par)  
  }      

# Auxiliary Functions 
# logit: a mathematical function transforming [0,1] to [-Inf, Inf]
logit <- 
  function(x){
    log(x/(1-x))
  }

# invlogit: Inverse of logit function; transform [-Inf, Inf] to [0,1]
invlogit <- 
  function(x){
    1/(1+exp(-x))
  }

# function for implementing bootstrap using gamma_mle_ll
bootstrap_beta <- function(dat, num_iter){
  beta_results <- list()
  for(i in 1:num_iter){
    set.seed(i)
    dat <- dat %>% dplyr::filter(value > 0)
    if(num_iter == 1){
      sample_index <- 1:nrow(dat)
    } else {
      sample_index <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
    }
    # get new data
    new_data <- dat[sample_index,]
    beta_ab_params <-  try(eBeta_ab(new_data$value), silent = TRUE)
    if(class(beta_ab_params) == 'try-error'){
      beta_data <- data_frame(aic = NA,
                              mle1 = NA,
                              mle2 = NA,
                              mle3 = NA,
                              mle4 = NA)
    } else {
      ll_hood <- lBeta_ab(X = new_data$value, params = list(shape1 = beta_ab_params$shape1,shape2= beta_ab_params$shape2,
                                                            a = 0, b = beta_ab_params$b))
      mle1 <- beta_ab_params$shape1
      mle2 <- beta_ab_params$shape2
      mle3 <- 0
      mle4 <- beta_ab_params$b
      aic <- get_aic(llhood = ll_hood)
      beta_data <- data_frame(aic = aic,
                              mle1 = mle1,
                              mle2 = mle2,
                              mle3 = mle3,
                              mle4 = mle4)
    }
    beta_results[[i]] <- beta_data
  }
  # save(beta_results, file = 'beta_results.RData')
  fitted_data <- do.call('rbind', beta_results)
  # get aic at the 50th percentile
  aic <- quantile(fitted_data$aic, 0.5, na.rm = TRUE)
  # get mle1 and mle2, lower mid and upper
  mles_1 <- quantile(fitted_data$mle1, c(0.025, 0.5, 0.975), na.rm = TRUE)
  mles_2 <- quantile(fitted_data$mle2, c(0.025, 0.5, 0.975), na.rm = TRUE)
  mles_3 <- quantile(fitted_data$mle3, c(0.025, 0.5, 0.975), na.rm = TRUE)
  mles_4 <- quantile(fitted_data$mle4, c(0.025, 0.5, 0.975), na.rm = TRUE)
  # store results in dataframe
  out <- data_frame(name = 'Beta',
                    aic = aic,
                    mle_1 = mles_1[2],
                    mle_2 = mles_2[2],
                    mle_3 = mles_3[2],
                    mle_4 = mles_4[2],
                    mle_1_lower = mles_1[1],
                    mle_2_lower = mles_2[1],
                    mle_3_lower = mles_3[1],
                    mle_4_lower = mles_4[1],
                    mle_1_upper = mles_1[3],
                    mle_2_upper = mles_2[3],
                    mle_3_upper = mles_3[3],
                    mle_4_upper = mles_4[3])
  return(out)
}

# for output 4, creates funding gap curve
get_gap_curve <- function(sim_vector, budget = budget){
  funding_gap_curve <- as.data.frame(quantile(sim_vector,seq(0.5,0.98,by=0.002), na.rm = TRUE))
  funding_gap_curve$x <- rownames(funding_gap_curve)
  rownames(funding_gap_curve) <- NULL
  names(funding_gap_curve)[1] <- 'y'
  
  # remove percent and turn numeric
  funding_gap_curve$x <- gsub('%', '', funding_gap_curve$x)
  funding_gap_curve$x <- as.numeric(funding_gap_curve$x)/100
  
  # divide y by 100k, so get data in millions
  funding_gap_curve$x <- (1 - funding_gap_curve$x)
  # funding_gap_curve$y <- funding_gap_curve$y/scale_by
  
  names(funding_gap_curve)[2] <- 'Probability of exceeding loss'
  names(funding_gap_curve)[1] <- 'Funding gap'
  funding_gap_curve$`Funding gap` <- -funding_gap_curve$`Funding gap`
  funding_gap_curve$`Funding gap` <- funding_gap_curve$`Funding gap` + budget
  return(funding_gap_curve)
}