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
library(shinyjs)
library(rowr)
library(Kendall)
library(tidyr)
# library(ExtDist)



# run the functions script when this script is called by app.R
source('functions.R')
# remove scientific notation in plots
options(scipen = '999')

##########
# create vectors to store choices for inputs in the app.r script
##########

# create a countries vector 
countries <- c("Afghanistan","Algeria","Angola","Azerbaijan","Bangladesh", "Bosnia and Herzegovina", "Bulgaria", "Costa Rica","Djibouti", "El Salvador",
               "Ethiopia", "Guatemala", "Haiti","Honduras", "India","Indonesia",  "Iran (Islamic Republic Of)", "Jamaica","Kenya","Kyrgyzstan", "Laos",                     
                "Madagascar","Mexico", "Morocco","Mozambique","Myanmar", "Namibia","Nepal", "Nicaragua", "Niger (the)" ,"Nigeria", "Pakistan", "Peru","Philippines (the)",
               "Romania", "Somalia", "South Africa","Sri Lanka","Tajikistan","Viet Nam", "Yemen")

# create a vector of archetype data
archetypes <- c('High risk middle income country, exposed to storms, floods, and earthquakes',
                'Middle income country, exposed to floods',
                'Upper-middle income country, exposed to storms, floods, and earthquakes',
                'Drought-prone low income country',
                'Low income conutry, exposed to storms, floods, and earthquakes',
                'Low income country, exposed to droughts, floods, and storms')

# create a vector for scaling choices
scaled_data <- c('No scale','population', 'gdp', 'inflation')

# define a vector of countries and currencies to be used in the dropdown (add more later)
currencies <- c('USD', 'Other')

# create a placeholer for other currency codes
other_currencies <- c('EUR', 'CNY')

# create a placeholder for disturbution types
basic_parametric <- c('Log normal', 'Beta', 'Gamma', 
                      'Frechet', 'Gumbel', 'Weilbull')
advanced_parametric <- c('Log normal', 'Beta', 'Gamma', 
                         'Frechet', 'Gumbel', 'Weibull',
                         'Pareto')


###########
# read in country data
###########

# emdat
emdat_data <- read.csv('data/Countries/emdat_country.csv', stringsAsFactors = FALSE)

# add column for data origin
emdat_data$origin <- 'EM_DAT'

# desinventar
des_data <- read.csv('data/Countries/desinventer_country.csv', stringsAsFactors = FALSE)

# add column for data origin
des_data$origin <- 'DesInventar'

# read in ocha data
# desinventar
ocha_data <- read.csv('data/Countries/ocha_data.csv', stringsAsFactors = FALSE)

# add column for data origin
ocha_data$origin <- 'OCHA'

ocha_data$Total.affected <- NA

# combine data
country_data <- rbind(emdat_data, des_data, ocha_data)
rm(emdat_data, des_data, ocha_data)

# rename columns
names(country_data) <- c('country', 'year', 'peril', 'affected', 'damage', 'origin')

# melt data to collapse damage type
country_data <- melt(country_data, id.vars = c('country', 'year', 'peril', 'origin'))

names(country_data)[5] <- 'damage_type'

# read in best data key
best_data <- read.csv('data/Countries/best_data_emdat_investar_ocha.csv', stringsAsFactors = FALSE)

names(best_data) <- c('country', 'desinventar', 'em_dat', 'ocha', 'affected', 'damage')

# remove columns
best_data$em_dat <- best_data$ocha <- best_data$desinventar <- NULL

best_data <- melt(best_data, id.vars = 'country')
names(best_data)[2:3] <- c('damage_type', 'best_source')
# 
# best_data_cost <- best_data %>% filter(damage_type == 'affected')
# best_data_damage <- best_data %>% filter(damage_type == 'damage')
# rm(best_data)
##########
# determing best source based on years available
##########

# # split data into affected and total damage
cost_data <- country_data[country_data$damage_type == 'affected',]
damage_data <-  country_data[country_data$damage_type == 'damage',]
rm(country_data)
# 
# cost_data <- left_join(cost_data, best_data_cost, c('country', 'damage_type'))
# 
# damage_data <- left_join(damage_data, best_data_cost, c('country', 'damage_type'))
# rm(best_data_cost, best_data_damage)

# # get best data
# cost_data <- get_best_data(cost_data)
# damage_data <- get_best_data(damage_data)

# get frequency data
cost_freq <- expand_data(cost_data)
damage_freq <- expand_data(damage_data)
cost_freq <- fill_na(cost_freq)
damage_freq <- fill_na(damage_freq)

# combine data
frequency_data <- rbind(cost_freq, damage_freq)
country_data <- rbind(damage_data, cost_data)
rm(cost_freq, damage_freq, cost_data, damage_data)


##########
# read in scaling data
##########
population_data <- read.csv('data/Scale/population_data.csv')
gdp_data <- read.csv('data/Scale/gdp.csv')
inflation_data <- read.csv('data/Scale/inflation.csv')

# join data 
scale_data <- full_join(population_data, inflation_data)
scale_data <- full_join(scale_data, gdp_data)


##########
# read in archetyp data
#########

# get all archetype cost data into one data frame
archetype_cost_data <- read_in_archetype_cost_data('cost_data', archetype_names = archetypes)
names(archetype_cost_data) <- c('archetype', 'year', 'peril', 'outcome', 'data_type')

# ge all archetype frequency data into data frame
archetype_freq_data <- read_in_archetype_freq_data('freq_data', archetype_names = archetypes)
names(archetype_freq_data) <- c('year', 'peril', 'count','archetype', 'data_type')


####################
# create welcome modal amd tab maker
###################

# Define the text for the welcome modal
welcome_modal <-
  fluidPage(
    p('This Tool has been developed in conjunction with the World Bank in order to develop capacity of the World Bank partner countries
        on key decisions they must take during disaster risk financing. The Tool is intended for use as outlined in the Introduction (About tab) and 
        should not be used for any other purposes. The tool should not be used to inform real financial decisions.'),
    br(),
    p("Information in the Tool is provided for educational purposes only and does not constitute legal or scientific advice or service. The World Bank makes no warranties or 
        representations, express or implied as to the accuracy or reliability of the Tool or the data contained therein. A user of the Tool should seek qualified expert advice for specific diagnosis 
        and analysis of a particular project. Any ose thereof or reliance thereon is at the sole and independent discretion and responsibility of the user. No conclusions or inferences 
        drawn from the Tool should be attributed to the World Bank, its Board of Executive Directors, its management, or any of its member countries."),
    br(),
    p('This tool does not imply and judgement of endorsement on the part of the World Bank. In no event will GAD or the World Bank be liable for any form of damage arising from the 
        application or misapplication of the tool, or any other associated materials.'),
    fluidRow(column(12, align = 'center', actionButton('accept', 'Accept')))
)

# Define text for the about page
about_page <-
  fluidPage(
    h1('About'),
    fluidRow(
      column(12,
             tags$p(class = "intro",
                    "The development of this Tool was led by the Disaster Risk Financing and Insurance 
                                       Program (DRFIP), a partnership of the World Bank Group's Finance Competitiveness and 
                                       Innovation Global Practice and the Global Facility for Disaster Reduction and Recovery 
                                       (GFDRR)."), 
             tags$br("The World Bank invests substantial resources in the development of its models, 
                                        modelling methodologies and databases. This Tool contains proprietary and confidential 
                                        information and is intended for the exclusive use of World Bank partners with whom this 
                                        Tool has been shared. Any user is subject to the restrictions of the confidentiality 
                                        provisions set forth in license and other nondisclosure agreements.")))
  )


# Function for conditional coloring, etc. in the tab panel headers
tab_maker <- function(n = 1,
                      label = 'TOOL SETTINGS',
                      input,
                      tab_data){
  label_n <- n
  if(n == 4){
    label_n <- 3.5
  }
  if(n == 5){
    label_n <- 4
  }
  
  # Get tab info
  tab_name <- input$tabs
  td <- tab_data$data
  tab_number <- td %>% filter(name == tab_name) %>% .$number
  the_text <- '&#10004;'
  if(tab_number >=n){
    the_color <- '#F05023'
    the_circle <- 'circle'
  } else {
    the_color <- '#707372'
    the_circle <- 'greycircle'
  }
  if(tab_number <= n){
    the_text <- label_n
  }
  
  HTML(paste0('<div style="width: 100%; margin: 0 auto; text-align: center"><div class="', the_circle, '">', the_text, '</div><h4 style = "width: 100%; color: ', the_color, '; margin: 0 auto; text-align: center">', label, '</h4></div>'))
}

# Function for allowing / prohibiting tab changes (based on amount of time since last change)
# (The purpose of this is to prevent the back-and-forthy weirdness)
not_too_fast <- function(old_time, new_time){
  time_difference <- as.numeric(difftime(new_time, old_time, units = 'secs'))
  if(time_difference > 0.5){
    TRUE
  } else {
    message('too fast!')
    FALSE
  }
}
