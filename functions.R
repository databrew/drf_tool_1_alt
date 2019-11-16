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


read_in_archetype_cost_data <- function(archetype_data, archetype_names){
  
  
  # read in loss data
  hr_mi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/hr_mi_sfe_cost.csv'), stringsAsFactors = FALSE)
  # change column names
  names(hr_mi_sfe) <- c('Archetype', 'Year', 'Peril', 'Affected')
  hr_mi_sfe$Archetype <- archetype_names[2]
  
  li_d_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_cost.csv'),  stringsAsFactors = FALSE)
  names(li_d_cost) <- c('Archetype', 'Year', 'Peril', 'Affected')
  li_d_cost$Archetype <-  archetype_names[5]
  
  li_dfs_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_dfs_cost.csv'), stringsAsFactors = FALSE)
  names(li_dfs_cost) <- c('Archetype', 'Year', 'Peril', 'Affected')
  li_dfs_cost$Archetype <-  archetype_names[7]
  
  li_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_cost.csv'), stringsAsFactors = FALSE)
  names(li_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Affected')
  li_sfe_cost$Archetype <-  archetype_names[6]
  
  mi_f_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_cost.csv'), stringsAsFactors = FALSE)
  names(mi_f_cost) <- c('Archetype', 'Year', 'Peril', 'Affected')
  mi_f_cost$Archetype <-  archetype_names[3]
  
  umi_sfe_cost <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_cost.csv'),  stringsAsFactors = FALSE)
  names(umi_sfe_cost) <- c('Archetype', 'Year', 'Peril', 'Affected')
  umi_sfe_cost$Archetype <-  archetype_names[4]
  
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
  hr_mi_sfe$Archetype <- archetype_names[[2]]
  
  # read in loss data
  umi_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/umi_sfe_freq.csv'))
  names(umi_sfe)[ncol(umi_sfe)] <- 'Drought'
  
  # change names
  umi_sfe <- melt(umi_sfe, id.vars = 'Year')
  names(umi_sfe) <- c('Year', 'Peril', 'Count')
  umi_sfe$Archetype <- archetype_names[[4]]
  
  # read in loss data
  mi_f <- read.csv(paste0('data/Archetypes/',archetype_data, '/mi_f_freq.csv'))
  names(mi_f) <-  c('Year', 'Flood', 'Drought', 'Storm', 'Earthquake')
  
  mi_f <- melt(mi_f, id.vars = 'Year')
  names(mi_f) <- c('Year', 'Peril', 'Count')
  mi_f$Archetype <- archetype_names[[3]]
  
  # read in loss data
  li_sfe <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_sfe_freq.csv'))
  names(li_sfe)[1] <- 'Year'
  names(li_sfe)[ncol(li_sfe)]  <-'Drought'
  
  # change names
  li_sfe <- melt(li_sfe, id.vars = 'Year')
  names(li_sfe) <- c('Year', 'Peril', 'Count')
  li_sfe$Archetype <- archetype_names[[6]]
  
  # read in loss data
  li_d <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_d_freq.csv'))
  names(li_d) <- c('Year', 'Drought','Storm', 'Earthquake', 'Flood')
  # change names
  li_d <- melt(li_d, id.vars = 'Year')
  names(li_d) <- c('Year', 'Peril', 'Count')
  li_d$Archetype <- archetype_names[[5]]
  
  # read in loss data
  li_dfs <- read.csv(paste0('data/Archetypes/',archetype_data, '/li_dfs_freq.csv'))
  names(li_dfs) <- 'Year'
  # change names
  names(li_dfs)[ncol(li_dfs)] <- 'Earthquake'
  li_dfs <- melt(li_dfs, id.vars = 'Year')
  names(li_dfs) <- c('Year', 'Peril', 'Count')
  li_dfs$Archetype <- archetype_names[[7]]
  
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
