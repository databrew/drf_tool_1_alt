

fit_distribution <- function(data, peril){
  
  # subset by peril
  data <- data[data$Peril == peril,]
  log_normal <- try(fitdistr(data$Loss, "lognormal"),silent = TRUE)
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
  
  beta <- try(eBeta_ab(data$Loss, method = "numerical.MLE"), silent = TRUE)
  if(class(beta) == 'try-error'){
    beta <- NULL
    beta_aic <- NA
    beta$shape1 <- NA
    beta$shape2 <- NA
    beta_mle <- c(beta$shape1, beta$shape2)
  } else {
    beta_ll <- lBeta_ab(X = data$Loss, params = beta, logL = TRUE)
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
  # gamma <- fitdistr(data$Loss, 'gamma')
  gamma <- try(fitdistrplus::fitdist(data$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle"), silent = TRUE)
  
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
  # dfrechet(data$Loss, lambda = 1, mu = 1, sigma = 1, log = TRUE)
  frechet <- try(fitdistrplus::fitdist(data$Loss, "frechet", start=list(scale=0.1, shape=0.1), method="mle"), 
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
  gumbel_fit <- try(fit_gumbel(data$Loss), silent = TRUE)
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
  weibull <- try(fitdistrplus::fitdist(data$Loss, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
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
  pareto <-ParetoPosStable::pareto.fit(data$Loss, estim.method = 'MLE')
  if(class(pareto) == 'try-error'){
    pareto <- NULL
    pareto_aic <- NA
    pareto_fit$estimate[1] <- NA
    pareto_fit$estimate[2] <- NA
    
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
  
  return(aic_mle_data)
}

get_min_aic <- function(data, peril){
  
  if(peril == 'Drought'){
    dat <- data[[1]]
  } else if(peril == 'Earthquake'){
    dat <- data[[2]]
  } else if(peril == 'Flood'){
    dat <- data[[3]]
  } else if (peril == 'Storm'){
    dat <- data[[4]]
  }
  
  # get index for minimum aic
  ind_drought <- which(dat$AIC == min(dat$AIC, na.rm = T))
  # # subset by best aic index
  dat <- dat[ind_drought,]    
  best_dis <- dat$Distribution
  return(best_dis)
}

run_simulations <- function(dat, peril, prob_dis){
  
  if(peril == 'Drought'){
    dat <- dat[[1]]
    if(best_dis == prob_dis){
      # for now remove beta
      # dat <- dat[dat$Distribution != 'Beta',]
      # get index for minimum aic
      aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
      # # subset my index
      dat <- dat[aic_min_ind,]  
    } else {
      # dat <- dat[dat$Distribution != 'Beta',]
      dat <- dat[dat$Distribution ==  prob_dis,]
    }
  } else if(peril == 'Earthquake'){
    dat <- dat[[2]]
    if(best_dis == prob_dis){
      # for now remove beta
      # dat <- dat[dat$Distribution != 'Beta',]
      # get index for minimum aic
      aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
      # # subset my index
      dat <- dat[aic_min_ind,]  
    } else {
      # dat <- dat[dat$Distribution != 'Beta',]
      dat <- dat[dat$Distribution ==  prob_dis,]
    }
  } else if(peril == 'Flood'){
    dat <- dat[[3]]
    if(best_dis == prob_dis){
      # for now remove beta
      # dat <- dat[dat$Distribution != 'Beta',]
      # get index for minimum aic
      aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
      # # subset my index
      dat <- dat[aic_min_ind,]  
    } else {
      # dat <- dat[dat$Distribution != 'Beta',]
      dat <- dat[dat$Distribution ==  prob_dis,]
    }
  } else if (peril == 'Storm'){
    if(best_dis == prob_dis){
      # for now remove beta
      # dat <- dat[dat$Distribution != 'Beta',]
      # get index for minimum aic
      aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
      # # subset my index
      dat <- dat[aic_min_ind,]  
    } else {
      # dat <- dat[dat$Distribution != 'Beta',]
      dat <- dat[dat$Distribution ==  prob_dis,]
    }
    dat <- dat[[4]]
  }
  
  
  # set conditions for each distribution
  if(dat$Distribution == 'Log normal'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- rlnorm(n = 15000, meanlog = dat$`MLE 1`, sdlog = dat$`MLE 2`)
    }
  } else if (dat$Distribution == 'Gamma'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      # check to see how much seed matters
      sim <- rgamma(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
    }
  } else if (dat$Distribution == 'Beta'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    } else {
      sim <- rbeta(n = 15000, shape1 = dat$`MLE 1`, scale2 = dat$`MLE 2`)
    }
  }  else if (dat$Distribution == 'Frechet'){
    if(any(is.na(dat$AIC))){
      sim <- NA
    }  else {
      sim <- rfrechet(n = 15000, loc=0, scale=dat$`MLE 1`, shape=dat$`MLE 2`)
    }
  } else if (dat$Distribution == 'Gumbel'){
    if(any(is.na(dat$AIC))){
      sim <- NA 
    } else {
      sim <- actuar::rgumbel(n = 15000, alpha = dat$`MLE 1`, scale = dat$`MLE 2`)
    }
  } else if (dat$Distribution == 'Weibull'){
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
