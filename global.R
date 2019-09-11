# library(lubridate)
# library(officer)
# library(tidyr)
# library(dplyr)
# library(zoo)
# library(plotly)
# library(leaflet)
# library(ggplot2)
# library(sp)
# library(rgdal)
# library(rgeos)
# library(maptools)
# library(rmapshaper)
# library(mapview)
# library(webshot)
# library(htmltools)
# library(quantmod)
# library(gapminder)
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



source('functions.R')
options(scipen = '999')

countries <- c('Afghanistan', 
               #'Somalia', 
               'Malaysia', 
               'Senegal')

# create a vector of countries without scaling data
no_scale_countries <- c('Afghanistan', 'Malaysia')


# define a vector of countries and currencies to be used in the dropdown (add more later)
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')

# create a placeholder for disturbution types
basic_parametric <- c('Lognormal', 'Beta', 'Gamma', 
                      'Frechet', 'Gumbel', 'Weilbull',
                      'Poisson', ' Bernoulli')
advanced_parametric <- c('Lognormal', 'Beta', 'Gamma', 
                         'Frechet', 'Gumbel', 'Weilbull',
                         'Pareto', 'Poisson', ' Bernoulli')


##########
# Afghanistan
##########

# read in prepopulated raw data 
raw_data_af <- read.csv('data/Afghanistan/raw_data_all.csv', header = FALSE)

# rename columns
names(raw_data_af) <- c('Country', 'Year', 'Peril', 'Loss')

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


##########
# malaysia
##########
# set scaling number
scale_by = 1000

# read in prepopulated raw data 
raw_data_malay <- read.csv('data/Malaysia/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_malay) <- c('Country', 'Year', 'Peril', 'Loss')

# divide loss by 1000
raw_data_malay$Loss_scaled <- raw_data_malay$Loss/scale_by


##########
# senegal
##########

# set scaling number
scale_by = 1000

# read in prepopulated raw data 
raw_data_sen <- read.csv('data/Senegal/raw_data_flood.csv', header = TRUE)

# rename columns
names(raw_data_sen) <- c('Country', 'Year', 'Peril', 'Loss')

# remove rows with 0
raw_data_sen <- raw_data_sen[raw_data_sen$Loss != 0,]

# scale data down
raw_data_sen$Loss_scaled <- raw_data_sen$Loss/scale_by



