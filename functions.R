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
