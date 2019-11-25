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

run_simulations <- function(prepared_simulation_data = NULL){
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
    message('RUN_SIMULATIONS IS USING FAKE METHODOLOGY')
    perils <- sort(unique(prepared_simulation_data$peril))
    
    # The following is fake data code
    out_list <- list()
    for(i in 1:length(perils)){
      this_peril <- perils[i]
      sub_peril <- prepared_simulation_data %>% filter(peril == this_peril)
      x <-make_simulation(dis_name = sub_peril$distribution, dat = sub_peril)
      out_list[[i]] <- tibble(key = this_peril,
                         value = x,
                         # freq is still not working
                         freq = sample(0:1, size = 15000, replace = TRUE)) %>% mutate(n = 1:15000) %>%
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