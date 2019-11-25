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


fill_na <- function(data){
  
  data$best_data <- NULL
  data_list <- list()
  for(i in 1:length(unique(data$country))){
    country_name <- unique(data$country)[i]
    sub_dat <- data[data$country == country_name,]
    sub_dat$origin[is.na(sub_dat$origin)] <- unique(sub_dat$origin)[!is.na(unique(sub_dat$origin))]
    sub_dat$damage_type[is.na(sub_dat$damage_type)] <- unique(sub_dat$damage_type)[!is.na(unique(sub_dat$damage_type))]
    sub_dat$value[is.na(sub_dat$value)] <- 0
    data_list[[i]] <- sub_dat
    print(i)
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



fit_distribution <- function(data){
  # get aic mle  by looping through perils 
  data <- data[[1]]
  
  data <- data[data$value > 0,]
  temp <- data %>% group_by(peril) %>% summarise(counts = sum(value))
  present_perils <- unique(temp$peril)
  
  # subset data by present perils
  data <- data %>% filter(peril %in% present_perils)
  data_list <- list()
  for(i in 1:length(present_perils)){
    peril_name <- present_perils[i]
    sub_data <- data[data$peril == peril_name,]
    
    log_normal <- try(fitdistr(sub_data$value, "lognormal"),silent = TRUE)
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
      message('log normal AIC is ', log_normal_aic)
      
      # get MLE
      log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
      message('log normal mle is ', log_normal_mle)
    }
    # create data frame to store aic and MLEs
    log_normal_data <- data_frame(name = 'log_normal',
                                  aic = log_normal_aic,
                                  mle_1 = log_normal$estimate[1],
                                  mle_2 = log_normal$estimate[2])
    # upper_mle_1 = log_normal_upper_mle_1,
    # lower_mle_1 = log_normal_lower_mle_1,
    # upper_mle_2 = log_normal_upper_mle_2,
    # lower_mle_2 = log_normal_lower_mle_2)
    
    beta <- try(eBeta_ab(sub_data$value, method = "numerical.MLE"), silent = TRUE)
    if(class(beta) == 'try-error'){
      beta <- NULL
      beta_aic <- NA
      beta$shape1 <- NA
      beta$shape2 <- NA
      beta_mle <- c(beta$shape1, beta$shape2)
    } else {
      beta_ll <- lBeta_ab(X = sub_data$value, params = beta, logL = TRUE)
      beta_aic <- -(2*beta_ll + 2)
      beta_mle <- c(beta$shape1, beta$shape2)
      
      # beta_aic <- round(beta$aic, 4)
      message('beta AIC is ', beta_aic)
      message('beta mle is ', beta_mle)
    }
    beta_data <- data_frame(name = 'beta',
                            aic = round(beta_aic, 4),
                            mle_1 = beta_mle[1],
                            mle_2 = beta_mle[2])
    
    
    
    # EQUATION FOR AIC
    # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.
    
    # fit gamma
    # gamma <- fitdistr(sub_data$value, 'gamma')
    gamma <- try(fitdistrplus::fitdist(sub_data$value, "gamma", start=list(shape=0.5, scale=1), method="mle"), silent = TRUE)
    
    if(class(gamma) == 'try-error'){
      gamma <- NULL
      gamma_aic <- NA
      gamma$estimate[1] <- NA
      gamma$estimate[2] <- NA
      
    } else {
      # get aic
      gamma_aic <- round(gamma$aic, 4)
      message('gamma AIC is ', gamma_aic)
      
      # upper and lower bound for each estimate
      # gamma_upper_mle_1 <- gamma$estimate[1] + gamma$sd[1]
      # gamm_lower_mle_1 <- gamma$estimate[1] - gamma$sd[1]
      #
      # gamma_upper_mle_2 <- gamma$estimate[2] + gamma$sd[2]
      # gamma_lower_mle_2 <- gamma$estimate[2] - gamma$sd[2]
      #
      # get mle
      gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
      message('gamme mle is ', gamma_mle)
    }
    gamma_data <- data_frame(name = 'gamma',
                             aic = gamma_aic,
                             mle_1 = gamma$estimate[1],
                             mle_2 = gamma$estimate[2])
    # upper_mle_1 = log_normal_upper_mle_1,
    # lower_mle_1 = log_normal_lower_mle_1,
    # upper_mle_2 = log_normal_upper_mle_2,
    # lower_mle_2 = log_normal_lower_mle_2)
    
    
    
    # fit frechet
    # dfrechet(sub_data$value, lambda = 1, mu = 1, sigma = 1, log = TRUE)
    frechet <- try(fitdistrplus::fitdist(sub_data$value, "frechet", start=list(scale=0.1, shape=0.1), method="mle"),
                   silent = TRUE)
    if(class(frechet) == 'try-error'){
      frechet <- NULL
      frechet_aic <- NA
      frechet$estimate[1] <- NA
      frechet$estimate[2] <- NA
      
    } else {
      frechet_aic <- round(frechet$aic, 4)
      message('frechet AIC is ', frechet_aic)
      # get mle
      frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
      message('frechet mle is ', frechet_mle)
    }
    frechet_data <- data_frame(name = 'frechet',
                               aic = frechet_aic,
                               mle_1 = frechet$estimate[1],
                               mle_2 = frechet$estimate[2])
    
    
    
    # git gumbel
    gumbel_fit <- try(fit_gumbel(sub_data$value), silent = TRUE)
    if(class(gumbel_fit) == 'try-error'){
      gumbel_fit <- NULL
      gumbel_aic <- NA
      gumbel_fit$estimate[1] <- NA
      gumbel_fit$estimate[2] <- NA
      
    } else {
      gumbel_aic <- round(gumbel_fit$aic, 4)
      message('gumbel AIC is ', gumbel_aic)
      # get mle
      gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
      message('gumbel mle is ', gumbel_mle)
    }
    gumbel_data <- data_frame(name = 'gumbel',
                              aic = gumbel_aic,
                              mle_1 = gumbel_fit$estimate[1],
                              mle_2 = gumbel_fit$estimate[2])
    
    
    
    # fit weibull
    weibull <- try(fitdistrplus::fitdist(sub_data$value, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
    if(class(weibull) == 'try-error'){
      weibull <- NULL
      weibull_aic <- NA
      weibull$estimate[1] <- NA
      weibull$estimate[2] <- NA
      
    } else {
      weibull_aic <- round(weibull$aic, 4)
      message('weibull AIC is ', weibull_aic)
      
      # get mle
      weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
      message('weibull mle is ', weibull_mle)
    }
    weibull_data <- data_frame(name = 'weibull',
                               aic = weibull_aic,
                               mle_1 = weibull$estimate[1],
                               mle_2 = weibull$estimate[2])
    
    
    
    # fit pareto
    pareto <-try(ParetoPosStable::pareto.fit(sub_data$value, estim.method = 'MLE'), silent = TRUE)
    if(class(pareto) == 'try-error'){
      pareto <- NULL
      pareto_aic <- NA
      pareto$estimate[1] <- NA
      pareto$estimate[2] <- NA
      
    } else {
      pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
      message('pareto AIC is ', pareto_aic)
      # get mle
      pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
      message('pareto mle is ', pareto_mle)
    }
    pareto_data <- data_frame(name = 'pareto',
                              aic = pareto_aic,
                              mle_1 = pareto$estimate[[1]],
                              mle_2 = pareto$estimate[[2]])
    
    
    
    
    # create a data frame out of data results
    aic_mle_data <- rbind(log_normal_data,
                          gamma_data,
                          beta_data,
                          frechet_data,
                          gumbel_data,
                          weibull_data,
                          pareto_data)
    
    # change names of variable
    names(aic_mle_data) <- c('Distribution', 'AIC', 'MLE 1', 'MLE 2')
    
    # capitalize and remove underscore of Distribution
    aic_mle_data$Distribution <- Hmisc::capitalize(aic_mle_data$Distribution)
    aic_mle_data$Distribution <- gsub('_', ' ', aic_mle_data$Distribution)
    aic_mle_data$AIC <- round(aic_mle_data$AIC, 2)
    aic_mle_data$`MLE 1`<- round(aic_mle_data$`MLE 1`, 2)
    aic_mle_data$`MLE 2` <- round(aic_mle_data$`MLE 2`, 2)
    
    aic_mle_data$peril <- peril_name
    
    data_list[[i]] <- aic_mle_data
    
  }
  
  aic_data <- do.call('rbind', data_list)
  
  return(aic_data)
  
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
         title='Distribution of simulated loss',
         subtitle = '15k simulations') +
    theme_databrew()
  return(p)
}
