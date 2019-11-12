##########
# Sri Lanka
##########

# read in loss data
sri_lanka_loss <- read.csv('data/Sri Lanka/data_loss.csv')

names(sri_lanka_loss) <- c('Country', 'Year', 'Peril', 'Loss')

# read in cost data
sri_lanka_cost <- read.csv('data/Sri Lanka/data_cost.csv')

names(sri_lanka_cost) <- c('Country', 'Year', 'Peril', 'Affected')

# read in population data
sri_lanka_pop <- read.csv('data/Sri Lanka/data_pop.csv')

# load frequency data
sri_lanka_freq <- read.csv('data/Sri Lanka/data_freq.csv')
names(sri_lanka_freq)[1] <- 'Year'

flood_data <- sri_lanka_loss[sri_lanka_loss$Peril == 'Flood',]
freq_data <- sri_lanka_freq
freq_data <- freq_data[, c('Year','Flood')]
num_trials <- as.numeric(as.character(max(freq_data$Year))) - min(as.numeric(as.character(freq_data$Year)))
num_trials <- num_trials + 1

mle_bern <- sum(nrow(freq_data[freq_data$Flood == 1,])/num_trials)

uniform_dis <- runif(15000, 0, 1)
sim_freq_data <- as.data.frame(cbind(simulation_num = 1:15000, uniform_dis = uniform_dis))
# create a variable to show success (mle?uniform_dis, then success)
sim_freq_data$outcome <- ifelse(sim_freq_data$uniform_dis < mle_bern, 1, 0)

frequency_distribution_bernoulli <- reactive({
  # will have other options for different scales later
  freq_data <- scale_by_pop()
  # temporarily do simulation
  freq_data <- freq_data[complete.cases(freq_data),]
  # sum of success (disaster) over sum if trials (years). 6 success in 8 years
  # get trials max year minus min year
  num_trials <- as.numeric(as.character(max(freq_data$Year))) - min(as.numeric(as.character(freq_data$Year)))
  num_trials <- num_trials + 1
  mle_bern <- sum(nrow(freq_data)/num_trials)
 

})