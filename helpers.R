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
    # right now, just giving some fake data
    out <- expand.grid(distribution = c('Log-Normal',
                                        'Normal',
                                        'Random',
                                        'Uniform',
                                        'Backwards',
                                        'Forwards'),
                       peril = c('Flood', 'Drought', 'Storm', 'Earthquake'))
    out$mle1 <- rnorm(n = nrow(out))
    out$mle2 <- rnorm(n = nrow(out))
    out$aic <- sample(1:100000, size = nrow(out))
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
      group_by(distribution, peril) %>%
      mutate(min_aic = min(aic, na.rm = TRUE)) %>%
      ungroup %>%
      filter(aic == min_aic) %>%
      dplyr::distinct(distribution, peril, .keep_all = TRUE)
  }
  return(out)
}

prepare_simulations <- function(fitted_distribution = NULL,
                            dist_flood = 'Gamma',
                            dist_drought = 'Gamma',
                            dist_storm = 'Gamma',
                            dist_earthquake = 'Gamma'){
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
  out <- NULL
  ok <- FALSE
  if(!is.null(prepared_simulation_data)){
    if(nrow(prepared_simulation_data) > 0){
      ok <- TRUE
    }
  }

  if(ok){
    if(nrow(prepared_simulation_data) != 4){
      message('Prepared simulation data should have exactly 4 rows!!!')
    }
    perils <- sort(unique(prepared_simulation_data$peril))
    
    # The following is fake data code
    out_list <- list()
    for(i in 1:length(perils)){
      this_peril <- perils[i]
      out_list[[i]] <- tibble(key = this_peril,
                         value = rnorm(n = 15000),
                         freq = sample(0:1, size = 15000, replace = TRUE)) %>% mutate(n = 1:15000) %>%
        mutate(outcome = freq * value)
    }
    out <- bind_rows(out_list)
  }
  return(out)
}

# Plot simulations
plot_simulations <- function(ran_simulations = NULL,
                             peril = 'Flood'){
  # ran_simulations gets generated in run_simulations
  # it should be a 3 column df with columns key, value and freq
  # peril must be one of the peril types
  
  out <- ggplot()
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
    out <- ggplot(data = pd,
           aes(x = value)) +
      geom_density()
  } else {
    out <- ggplot()
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