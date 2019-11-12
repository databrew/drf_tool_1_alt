
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(pracma)
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
library(boot)
# library(ExtDist)


# run the functions script when this script is called by app.R
source('functions.R')
# remove scientific notation in plots
options(scipen = '999')

##########
# create vectors to store choices for inputs in the app.r script
##########

# create a countries vector 
countries <- c('','Sri Lanka', 'South Africa', 'Philippines', 'Mozambique')

# create a vector of archetype data
archetypes <- c('',
                'High risk middle income country, exposed to storms, floods, and earthquakes',
                'Middle income country, exposed to floods',
                'Upper-middle income country, exposed to storms, floods, and earthquakes',
                'Drought-prone low income country',
                'Low income conutry, exposed to storms, floods, and earthquakes',
                'Low income country, exposed to droughts, floods, and storms')

# create a vector for scaling choices
scaled_data <- c('Population', 'GDP', 'Inflation')

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
# Archetype data
##########

# Hig risk middle income country, storms, floods, and earthquakes
high_risk_mid_income_storms_floods_earthquakes <- read.csv('data/Archetypes/high_risk_middle_income_storms_floods_earthquakes.csv')

names(high_risk_mid_income_storms_floods_earthquakes) <- c('Archetype', 'Year', 'Peril', 'Affected')

# low income droughts
low_income_droughts <- read.csv('data/Archetypes/low_income_drought.csv')

names(low_income_droughts) <- c('Archetype', 'Year', 'Peril', 'Affected')

# low income droughts, floods, storms
low_income_droughts_floods_storms <- read.csv('data/Archetypes/low_income_storms_floods_earthquakes.csv')

names(low_income_droughts) <- c('Archetype', 'Year', 'Peril', 'Affected')

# low income storms, floods, earthquakes
low_income_storms_floods_earthquakes <- read.csv('data/Archetypes/low_income_storms_floods_earthquakes.csv')

names(low_income_storms_floods_earthquakes) <- c('Archetype', 'Year', 'Peril', 'Affected')

# middle income flood
middle_income_flood <- read.csv('data/Archetypes/middle_income_flood.csv')

names(middle_income_flood) <- c('Archetype', 'Year', 'Peril', 'Affected')

# upper middle income storms floods earthquakes
upper_middle_income_storms_floods_earthquakes <- read.csv('data/Archetypes/upper_middle_income_storms_floods_earthquakes.csv')

names(upper_middle_income_storms_floods_earthquakes) <- c('Archetype', 'Year', 'Peril', 'Affected')


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

# make a data frame to store information about data
sri_lanka_info <- data_frame(loss = TRUE, cost = TRUE, population = TRUE, inflation = FALSE, gdp = FALSE)

##########
# South Africa
##########

# read in loss data
south_africa_loss <- read.csv('data/South Africa/data_loss.csv')

names(south_africa_loss) <- c('Country', 'Year', 'Peril', 'Loss')

# read in cost data
south_africa_cost <- read.csv('data/South Africa/data_cost.csv')

names(south_africa_cost) <- c('Country', 'Year', 'Peril', 'Affected')

# read in population data
south_africa_pop <- read.csv('data/South Africa/data_pop.csv')

# make a data frame to store information about data
south_africa_info <- data_frame(loss = TRUE, cost = TRUE, population = TRUE, inflation = FALSE, gdp = FALSE)

##########
# Philippines
##########

# read in loss data
philippines_loss <- read.csv('data/Philippines/data_loss.csv')

names(philippines_loss) <- c('Country', 'Year', 'Peril', 'Loss')

# read in cost data
philippines_cost <- read.csv('data/Philippines/data_cost.csv')

names(philippines_cost) <- c('Country', 'Year', 'Peril', 'Affected')

# read in population data
philippines_pop <- read.csv('data/Philippines/data_pop.csv')

# make a data frame to store information about data
philippines_info <- data_frame(loss = TRUE, cost = TRUE, population = TRUE, inflation = FALSE, gdp = FALSE)

##########
# Mozambique
##########

# read in loss data
mozambique_loss <- read.csv('data/Mozambique/data_loss.csv')

names(mozambique_loss) <- c('Country', 'Year', 'Peril', 'Loss')

# read in cost data
mozambique_cost <- read.csv('data/Mozambique/data_cost.csv')

names(mozambique_cost) <- c('Country', 'Year', 'Peril', 'Affected')

# read in population data
mozambique_pop <- read.csv('data/Mozambique/data_pop.csv')

# make a data frame to store information about data
mozambique_info <- data_frame(loss = TRUE, cost = TRUE, population = TRUE, inflation = FALSE, gdp = FALSE)

#### add more countries



