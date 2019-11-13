
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