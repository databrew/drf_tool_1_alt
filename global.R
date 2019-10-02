
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggthemes)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(dplyr)
library(shinyjqui)
library(bsplus)
library(htmltools)
library(shinyBS)
library(aTSA)
library(MASS)
library(databrew)
library(reshape2)
library(fitdistrplus)
library(actuar)
library(extraDistr)
library(ParetoPosStable)
library(Hmisc)
# library(ExtDist)


# run the functions script when this script is called by app.R
source('functions.R')
# remove scientific notation in plots
options(scipen = '999')

##########
# create vectors to store choices for inputs in the app.r script
##########

# create a countries vector 
countries <- c('Afghanistan')

# create a vector of countries without scaling data
# no_scale_countries <- c('Malaysia')

# define a vector of countries and currencies to be used in the dropdown (add more later)
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')

# create a placeholder for disturbution types
basic_parametric <- c('Log normal', 'Beta', 'Gamma', 
                      'Frechet', 'Gumbel', 'Weilbull',
                      'Poisson', ' Bernoulli')
advanced_parametric <- c('Log normal', 'Beta', 'Gamma', 
                         'Frechet', 'Gumbel', 'Weibull',
                         'Pareto')

##########
# Afghanistan
##########

# read in prepopulated raw data 
raw_data_af <- read.csv('data/Afghanistan/raw_data_all.csv', header = FALSE)

# rename columns
names(raw_data_af) <- c('Country', 'Year', 'Peril', 'Loss')

# round
raw_data_af$Loss <- round(raw_data_af$Loss, 2)

##########
# Somalia
##########

# set scaling number
scale_by = 1000000

# read in prepopulated raw data 
raw_data_som <- read.csv('data/Somalia/raw_data_all.csv', header = TRUE)

# rename columns
names(raw_data_som) <- c('Country', 'Year', 'Peril', 'Loss')

# scale data to work
raw_data_som$Loss_scaled <- raw_data_som$Loss/scale_by

# round
raw_data_som$Loss <- round(raw_data_som$Loss, 2)



##########
# malaysia
##########
# set scaling number
# scale_by = 1000

# read in prepopulated raw data 
raw_data_malay <- read.csv('data/Malaysia/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_malay) <- c('Country', 'Year', 'Peril', 'Loss')

# divide loss by 1000
# raw_data_malay$Loss_scaled <- raw_data_malay$Loss/scale_by

# round
raw_data_malay$Loss <- round(raw_data_malay$Loss, 2)


##########
# senegal
##########

# set scaling number
# scale_by = 1000

# read in prepopulated raw data 
raw_data_sen <- read.csv('data/Senegal/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_sen) <- c('Country', 'Year', 'Peril', 'Loss')

# remove rows with 0
raw_data_sen <- raw_data_sen[raw_data_sen$Loss != 0,]

# scale data down
# raw_data_sen$Loss_scaled <- raw_data_sen$Loss/scale_by

# round
raw_data_sen$Loss <- round(raw_data_sen$Loss, 2)



