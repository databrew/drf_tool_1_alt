# normalize data
normalize_data <- function(x, add_decimal, dec = NULL){
  min_x <- min(x)
  max_x <- max(x)
  z <- (x - min_x)/(max_x - min_x)
  if(add_decimal){
    min_z <- min(z)
    max_z <- max(z)
    z[z==max_z] <- max_z - dec
    z[z==min_z] <- min_z + dec
    
  }
  return(z)
}

get_best_data <- function(data){
  # define a for loop
  data_list <- list()
  
  for(i in 1:length(countries)){
    country_name <- countries[i]
    sub_dat <- data[data$country == country_name,]
    if(length(unique(sub_dat$origin)) > 1) {
      num_emdat <- nrow(sub_dat[sub_dat$origin == 'EMDAT',])
      num_des <- nrow(sub_dat[sub_dat$origin == 'DesInventar',])
      if(num_emdat >= num_des){
        sub_dat$best_data[sub_dat$origin == 'EMDAT'] <- TRUE
        sub_dat$best_data[sub_dat$origin == 'DesInventar'] <- FALSE
        
      } else {
        sub_dat$best_data[sub_dat$origin == 'EMDAT'] <- FALSE
        sub_dat$best_data[sub_dat$origin == 'DesInventar'] <- TRUE
      }
    } else {
      sub_dat$best_data <- TRUE
    }
    data_list[[i]] <- sub_dat
  }
  
  # get dataframe back wih new variable
  data <- do.call('rbind', data_list)
  
  # # keep only best data
  # data <- data[data$best_data == TRUE,]
  # data$best_data <- NULL
  return(data)
  
}

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

# data <- cost_freq
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



read_in_archetype_cost_data <- function(archetype_data, archetype_names){


  # read in loss data
  hr_mi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/hr_mi_sfe_cost.csv'), stringsAsFactors = FALSE)
  # change column names
  names(hr_mi_sfe) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  hr_mi_sfe$Archetype <- archetype_names[1]

  li_d_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_cost.csv'),  stringsAsFactors = FALSE)
  names(li_d_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_d_cost$Archetype <-  archetype_names[4]

  li_dfs_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_dfs_cost.csv'), stringsAsFactors = FALSE)
  names(li_dfs_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_dfs_cost$Archetype <-  archetype_names[6]

  li_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_cost.csv'), stringsAsFactors = FALSE)
  names(li_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  li_sfe_cost$Archetype <-  archetype_names[5]

  mi_f_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_cost.csv'), stringsAsFactors = FALSE)
  names(mi_f_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  mi_f_cost$Archetype <-  archetype_names[2]

  umi_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_cost.csv'),  stringsAsFactors = FALSE)
  names(umi_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Outcome')
  umi_sfe_cost$Archetype <-  archetype_names[3]

  out <- rbind(hr_mi_sfe,
               li_d_cost,
               li_dfs_cost,
               li_sfe_cost,
               mi_f_cost,
               umi_sfe_cost)
  out$data_type <- 'archetype_cost'



  return(out)


}

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

  # joine with pop data
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

read_in_archetype_freq_data <- function(archetype_data, archetype_names){
  # read in loss data
  hr_mi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/hr_mi_sfe_freq.csv'))

  # change names
  names(hr_mi_sfe)[ncol(hr_mi_sfe)] <- 'Drought'
  hr_mi_sfe <- melt(hr_mi_sfe, id.vars = 'Year')
  names(hr_mi_sfe) <- c('Year', 'Peril', 'Count')
  hr_mi_sfe$Archetype <- archetype_names[[1]]

  # read in loss data
  umi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_freq.csv'))
  names(umi_sfe)[ncol(umi_sfe)] <- 'Drought'

  # change names
  umi_sfe <- melt(umi_sfe, id.vars = 'Year')
  names(umi_sfe) <- c('Year', 'Peril', 'Count')
  umi_sfe$Archetype <- archetype_names[[3]]

  # read in loss data
  mi_f <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_freq.csv'))
  names(mi_f) <-  c('Year', 'Flood', 'Drought', 'Storm', 'Earthquake')

  mi_f <- melt(mi_f, id.vars = 'Year')
  names(mi_f) <- c('Year', 'Peril', 'Count')
  mi_f$Archetype <- archetype_names[[2]]

  # read in loss data
  li_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_freq.csv'))
  names(li_sfe)[1] <- 'Year'
  names(li_sfe)[ncol(li_sfe)]  <-'Drought'

  # change names
  li_sfe <- melt(li_sfe, id.vars = 'Year')
  names(li_sfe) <- c('Year', 'Peril', 'Count')
  li_sfe$Archetype <- archetype_names[[5]]

  # read in loss data
  li_d <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_freq.csv'))
  names(li_d) <- c('Year', 'Drought','Storm', 'Earthquake', 'Flood')
  # change names
  li_d <- melt(li_d, id.vars = 'Year')
  names(li_d) <- c('Year', 'Peril', 'Count')
  li_d$Archetype <- archetype_names[[4]]

  # read in loss data
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


# fit gumbel function
fit_gumbel <- function(x){
  
  
  
  # fit gumble 
  gumbel_fit <- fitdistrplus::fitdist(x, "gumbel", start=list(mu=1, s=1), method="mle")
  
  return(gumbel_fit)
}

# create a plot for a generalized bar graph.
plot_bar <- function(temp_dat, bar_color, border_color, alpha, plot_title){
  # get data based on country input
  ggplot(temp_dat, aes(variable, value)) +
    geom_bar(stat = 'identity', fill = bar_color, color = border_color, alpha = alpha) +
    labs(x = 'Return period',
         y = 'Estimated annual loss (million USD',
         title = plot_title)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12)) 
  
}



get_aic_mle <- function(dat){
  # get aic mle  by looping through perils 

  dat <- dat[dat$value > 0,]
  temp <- dat %>% group_by(peril) %>% summarise(counts = sum(value))
  present_perils <- unique(temp$peril)
  
  # subset dat by present perils
  dat <- dat %>% filter(peril %in% present_perils)
  dat_list <- list()
  for(i in 1:length(present_perils)){
    peril_name <- present_perils[i]
    sub_dat <- dat[dat$peril == peril_name,]

    log_normal <- try(fitdistr(sub_dat$value, "lognormal"),silent = TRUE)
    if(class(log_normal) == 'try-error'){
      log_normal <- NULL
      log_normal_aic <- NA
      log_normal$estimate[1] <- NA
      log_normal$estimate[2] <- NA
      # log_norma$upper_mle_1 <- NA
      # log_normal$lower_mle_1 <- NA
      # log_norma$upper_mle_2 <- NA
      # log_normal$lower_mle_2 <- NA
      #
    } else {
      # get aic
      log_normal_aic <- round(AIC(log_normal), 4)
      #
      # # upper and lower bound for each estimate
      # log_normal_upper_mle_1 <- log_normal$estimate[1] + log_normal$sd[1]
      # log_normal_lower_mle_1 <- log_normal$estimate[1] - log_normal$sd[1]
      #
      # log_normal_upper_mle_2 <- log_normal$estimate[2] + log_normal$sd[2]
      # log_normal_lower_mle_2 <- log_normal$estimate[2] - log_normal$sd[2]
      
      # if there is an error, fill object with NA
     
      
      # get MLE
      log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
     
    }
    # create dat frame to store aic and MLEs
    log_normal_dat <- data_frame(name = 'log_normal',
                                  aic = log_normal_aic,
                                  mle_1 = log_normal$estimate[1],
                                  mle_2 = log_normal$estimate[2])
    # upper_mle_1 = log_normal_upper_mle_1,
    # lower_mle_1 = log_normal_lower_mle_1,
    # upper_mle_2 = log_normal_upper_mle_2,
    # lower_mle_2 = log_normal_lower_mle_2)
    
    beta <- try(eBeta_ab(sub_dat$value, method = "numerical.MLE"), silent = TRUE)
    if(class(beta) == 'try-error'){
      beta <- NULL
      beta_aic <- NA
      beta$shape1 <- NA
      beta$shape2 <- NA
      beta_mle <- c(beta$shape1, beta$shape2)
    } else {
      beta_ll <- lBeta_ab(X = sub_dat$value, params = beta, logL = TRUE)
      beta_aic <- -(2*beta_ll + 2)
      beta_mle <- c(beta$shape1, beta$shape2)
      
      # beta_aic <- round(beta$aic, 4)
     
    }
    beta_dat <- data_frame(name = 'beta',
                            aic = round(beta_aic, 4),
                            mle_1 = beta_mle[1],
                            mle_2 = beta_mle[2])
    
    
    
    # EQUATION FOR AIC
    # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.
    
    # fit gamma
    # gamma <- fitdistr(sub_dat$value, 'gamma')
    gamma <- try(fitdistrplus::fitdist(sub_dat$value, "gamma", start=list(shape=0.5, scale=1), method="mme"), silent = TRUE)
    
    if(class(gamma) == 'try-error'){
      gamma <- NULL
      gamma_aic <- NA
      gamma$estimate[1] <- NA
      gamma$estimate[2] <- NA
      
    } else {
      # get aic
      gamma_aic <- round(gamma$aic, 4)

      # upper and lower bound for each estimate
      # gamma_upper_mle_1 <- gamma$estimate[1] + gamma$sd[1]
      # gamm_lower_mle_1 <- gamma$estimate[1] - gamma$sd[1]
      #
      # gamma_upper_mle_2 <- gamma$estimate[2] + gamma$sd[2]
      # gamma_lower_mle_2 <- gamma$estimate[2] - gamma$sd[2]
      #
      # get mle
      gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
    }
    gamma_dat <- data_frame(name = 'gamma',
                             aic = gamma_aic,
                             mle_1 = gamma$estimate[1],
                             mle_2 = gamma$estimate[2])

    # upper_mle_1 = log_normal_upper_mle_1,
    # lower_mle_1 = log_normal_lower_mle_1,
    # upper_mle_2 = log_normal_upper_mle_2,
    # lower_mle_2 = log_normal_lower_mle_2)
    
    
    
    # fit frechet
    # dfrechet(sub_dat$value, lambda = 1, mu = 1, sigma = 1, log = TRUE)
    frechet <- try(fitdistrplus::fitdist(sub_dat$value, "frechet", start=list(scale=0.1, shape=0.1), method="mle"),
                   silent = TRUE)
    if(class(frechet) == 'try-error'){
      frechet <- NULL
      frechet_aic <- NA
      frechet$estimate[1] <- NA
      frechet$estimate[2] <- NA
      
    } else {
      frechet_aic <- round(frechet$aic, 4)
      # get mle
      frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
    }
    frechet_dat <- data_frame(name = 'frechet',
                               aic = frechet_aic,
                               mle_1 = frechet$estimate[1],
                               mle_2 = frechet$estimate[2])
    
    
    
    # git gumbel
    gumbel_fit <- try(fit_gumbel(sub_dat$value), silent = TRUE)
    if(class(gumbel_fit) == 'try-error'){
      gumbel_fit <- NULL
      gumbel_aic <- NA
      gumbel_fit$estimate[1] <- NA
      gumbel_fit$estimate[2] <- NA
      
    } else {
      gumbel_aic <- round(gumbel_fit$aic, 4)
      # get mle
      gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
    }
    gumbel_dat <- data_frame(name = 'gumbel',
                              aic = gumbel_aic,
                              mle_1 = gumbel_fit$estimate[1],
                              mle_2 = gumbel_fit$estimate[2])
    
    
    
    # fit weibull
    weibull <- try(fitdistrplus::fitdist(sub_dat$value, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
    if(class(weibull) == 'try-error'){
      weibull <- NULL
      weibull_aic <- NA
      weibull$estimate[1] <- NA
      weibull$estimate[2] <- NA
      
    } else {
      weibull_aic <- round(weibull$aic, 4)
     
      
      # get mle
      weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
      
    }
    weibull_dat <- data_frame(name = 'weibull',
                               aic = weibull_aic,
                               mle_1 = weibull$estimate[1],
                               mle_2 = weibull$estimate[2])
    
    
    
    # fit pareto
    pareto <-try(ParetoPosStable::pareto.fit(sub_dat$country, estim.method = 'MLE'), silent = TRUE)
    if(class(pareto) == 'try-error'){
      pareto <- NULL
      pareto_aic <- NA
      pareto$estimate[1] <- NA
      pareto$estimate[2] <- NA
      
    } else {
      pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
      
      # get mle
      pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
     
    }
    pareto_dat <- data_frame(name = 'pareto',
                              aic = pareto_aic,
                              mle_1 = pareto$estimate[[1]],
                              mle_2 = pareto$estimate[[2]])
    
    
    
    
    # create a dat frame out of dat results
    aic_mle_dat <- rbind(log_normal_dat,
                          gamma_dat,
                          beta_dat,
                          frechet_dat,
                          gumbel_dat,
                          weibull_dat,
                          pareto_dat)
    
    # change names of variable
    names(aic_mle_dat) <- c('distribution', 'aic', 'mle1', 'mle2')
    
    # capitalize and remove underscore of distribution
    aic_mle_dat$distribution <- Hmisc::capitalize(aic_mle_dat$distribution)
    aic_mle_dat$distribution <- gsub('_', ' ', aic_mle_dat$distribution)
    aic_mle_dat$aic <- round(aic_mle_dat$aic, 2)
    aic_mle_dat$mle1<- aic_mle_dat$mle1
    aic_mle_dat$mle2 <- aic_mle_dat$mle2
    
    aic_mle_dat$peril <- peril_name
    
    dat_list[[i]] <- aic_mle_dat
    
  }
  
  aic_dat <- do.call('rbind', dat_list)
  
  return(aic_dat)
  
}


# create a generalized line plot
plot_line <- function(temp_dat, 
                      line_color, 
                      line_size, 
                      alpha, 
                      exhibit_2,
                      largest_loss_num,
                      largest_loss_year,
                      plot_title){
  
  if(!exhibit_2){
    p <-  ggplot(temp_dat, aes(x, y)) +
      geom_line(size = line_size, color = line_color, alpha = alpha) +
      labs(x = 'Probability of Exceeding Loss',
           y = 'Funding gap (million USD)',
           title = plot_title) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 12)) 
    
  } else {
    # get data based on country input
    p <- ggplot(temp_dat, aes(x, y)) +
      geom_line(size = 1.5, color = 'blue') +
      labs(x = 'Probability of Exceeding Loss',
           y = 'Value of loss (million USD)',
           title = plot_title,
           caption = paste0('dotted line represents largest loss',' (', largest_loss_year, ')'))  +
      geom_hline(yintercept  = largest_loss_num, linetype= 2) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 12)) 
    
  }
  
  return(p)
  
}

# create plot to visualize simulationdata data
plot_sim <- function(temp_dat){

  p <- ggplot(temp_dat, aes(SimulatedNNDISLoss)) +
    geom_histogram(alpha = 0.6) +
    labs(x="Simulated Loss", 
         y="Frequency",
         title='distribution of simulated loss',
         subtitle = '15k simulations') +
    theme_databrew()
  return(p)
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

widen_core_data <- function(cdx){
  if(is.null(cdx)){
    return(cdx)
  }
  cdx <- cdx %>%
    tidyr::spread(key = peril, value = value)
  names(cdx)[1:2] <- c('Year', 'Country')
  return(cdx)
}

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

make_simulation <- function(dis_name, dat){
  if(dis_name == 'Log normal'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rlnorm(n = 15000, meanlog = dat$`mle1`, sdlog = dat$`mle2`)
    }
  } else if (dis_name == 'Gamma'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      # check to see how much seed matters
      sim <- rgamma(n = 15000, shape = dat$`mle1`, scale = dat$`mle2`)
    }
  } else if (dis_name == 'Beta'){
    if(any(is.na(dat$aic))){
      sim <- NA
    } else {
      sim <- rbeta(n = 15000, shape1 = dat$`mle1`, scale2 = dat$`mle2`)
    }
  }  else if (dis_name == 'Frechet'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rfrechet(n = 15000, loc=0, scale=dat$`mle1`, shape=dat$`mle2`)
    }
  } else if (dis_name == 'Gumbel'){
    if(any(is.na(dat$aic))){
      sim <- NA
    } else {
      sim <- actuar::rgumbel(n = 15000, alpha = dat$`mle1`, scale = dat$`mle2`)
    }
  } else if (dis_name == 'Weibull'){
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- rweibull(n = 15000, shape = dat$`mle1`, scale = dat$`mle2`)
    }
  } else {
    if(any(is.na(dat$aic))){
      sim <- NA
    }  else {
      sim <- extraDistr::rpareto(n = 15000, a = dat$`mle1`, b = dat$`mle2`)
    }
  }
  return(sim)
}



fit_distribution <- function(the_right_data = NULL){
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
    out <- get_aic_mle(the_right_data)
    # save(out, file = 'out.RData')
  }
  return(out)
}

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
    if(nrow(prepared_simulation_data) != 4){
      message('Prepared simulation data should have more than 4 rows')
    }
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
                              value = x,
                              # freq is still not working
                              freq = sub_peril_freq$freq) %>% 
        mutate(outcome = freq * value)
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

# Plot simulations
tabulate_simulations <- function(ran_simulations = NULL,
                                 peril = 'Flood'){
  # ran_simulations gets generated in run_simulations
  # it should be a 3 column df with columns key, value and freq
  # peril must be one of the peril types
  
  out <- NULL
  ok <- FALSE
  if(!is.null(ran_simulations)){
    if(nrow(ran_simulations) > 0){
      ok <- TRUE
    }
  }
  
  if(ok){
    # Filter
    pd <- ran_simulations %>%
      filter(key == peril)
    # Fake calculations
    out <- pd %>%
      group_by(key) %>%
      summarive(value = mean(value, na.rm = TRUE))
  } else {
    out <- NULL
  }
  return(out)
}

simulate_ci <- function(right_data = NULL){
  # the right data should be either scaled, detrended or core data, depending on inputs
  # what is returned is 2 rows for each peril, the lower and upper
  out <- NULL
  ok <- FALSE
  if(!is.null(the_right_data)){
    if(nrow(the_right_data) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    perils <- c('Flood', 'Drought', 'Storm', 'Earthquake')
    out <- expand.grid(key = perils,
                       type = c('Lower', 'Upper'))
    out <- out %>%
      mutate(mle1 <- rnorm(n = nrow(out)),
             mle2 = rnorm(n = nrow(out)))
  }
  return(out)
}

generate_ci <- function(simulated_ci = NULL){
  # this takes the output of simulate_ci and prepares plotting
  # values for confidence intervals
  out <- NULL
  ok <- FALSE
  if(!is.null(simulated_ci)){
    if(nrow(simulated_ci) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    out <- tibble(x = 1:100,
                  upr = 1:100,
                  lwr = (1:100) - 5)
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
  num_years = 16
  output <- quantile(dat_sim$value,c(0.8,0.9, 0.96,0.98,0.99))
  annual_avg = round(sum(dat$value)/num_years, 2)
  
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
  return(sub_dat)
}

bootstrap_cis <- function(grd){
  
  if(is.null(grd)){
    message('grd is null in the bootstrap functionality. returning null')
    return(NULL)
  }
  if(nrow(grd[[1]]) == 0){
    message('grd has no rows in the bootstrap functionality. returning null')
    return(NULL)
  }
  
  loss_data <- grd[[1]]
  freq_data <- grd[[2]]
  # # Collapse, combine accross all perils
  # loss_data <- loss_data %>% group_by(year, country) %>% summarise(value = sum(value))
  loss_list <- freq_list <- list()
  perils <- sort(unique(loss_data$peril))
  counter <- 0
  for(j in 1:length(perils)){
    for(i in 1:1000){
      counter <- counter + 1
      this_peril <- perils[j]
      # Loss data
      this_loss_data <- loss_data %>% filter(peril == this_peril)
      loss_indices <- sample(1:nrow(this_loss_data), size = nrow(this_loss_data), replace = T)
      new_loss_data <- this_loss_data[loss_indices,]
      loss_list[[counter]] <- new_loss_data %>% mutate(county = i)
      
      # Freq data
      this_freq_data <- freq_data %>% filter(peril == this_peril)
      freq_indices <- sample(1:nrow(this_freq_data), size = nrow(this_freq_data), replace = T)
      new_freq_data <- this_freq_data[freq_indices,]
      freq_list[[counter]] <- new_freq_data %>% mutate(county = i)
    }
  }
  
  # Multiply the freq and the loss data
  combined_list <- list()
  for(i in 1:length(loss_list)){
    this_loss_data <- loss_list[[i]]
    this_freq_data <- freq_list[[i]]
    out <- this_loss_data %>%
      dplyr::select(-year) %>%
      mutate(value = value * this_freq_data$value)
    combined_list[[i]] <- out
  }
  # Collapse the combined_list dataframes so that they combine all perils together
  # WORKING IN HERE, NOT DONE
  collapsed <- bind_rows(combined_list)
  collapsed
    mutate(dummy = 1) %>%
    group_by(county) %>%
    mutate(cs = cumsum(dummy)) %>% ungroup
  collapsed <- collapsed %>% group_by(country, cs) %>%
    summarise(value = sum(value))
  
  quant_list <- list()
  for(i in 1:length(combined_list)){
    x <- quant_that(combined_list[[i]])
    quant_list[[i]] <- x
  }
  quant <- bind_rows(quant_list)
  done <- quant %>%
    group_by(variable) %>%
    summarise(lwr = quantile(value, 0.025),
              upr = quantile(value, 0.975))
  
  quant_that(dat_sim = combined_list[[2]])
  combined <- bind_rows(combined_list) %>%
    group_by(peril) %>%
    arrange(value)

}

