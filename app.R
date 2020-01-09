# load additional shiny libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# Source the data set-up and functions built for analysis
source('functions.R')
source('global.R')

#Create a dictionary of tab names / numbers
tab_dict <- data_frame(number = 1:5,
                       name = toupper(c('Tool settings',
                                        'Data',
                                        'Risk profile fitting',
                                        'Simulations',
                                        'Output')))

n_tabs <- nrow(tab_dict)
# define the header with pictures
header <- dashboardHeader(title = tags$a(tags$img(src='logos_together.png',height='60',width='200', alt = 'World Bank Group')))
# set sidebar names 
sidebar <- dashboardSidebar(
  sidebarMenu(
    width = 350,
    id = 'side_tab',
    menuItem(
      text=paste("Emergecny Funding", "\n","Assessment"),
      tabName="main",
      icon=icon("crosshairs")),
    menuItem(
      text = 'About the Tool',
      tabName = 'about',
      icon = icon("info-circle")),
    menuItem(
      text = 'Training',
      tabName = 'training',
      icon = icon("book-open"))
  )
)
# start the body
body <- dashboardBody(
  tags$head(tags$style(HTML(".small-box {height: 40px}"))),
  tags$head(tags$style(HTML('.nav-tabs a {cursor: default}'))),
  valueBoxOutput(outputId = "vb"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  hidden(
    lapply(seq(n_tabs), function(i) {
      div(
        class = "page",
        id = paste0("step", i),
        "Step", i
      )
    })
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(theme = 'custom.css',
                fluidRow(
                  column(12, align = 'center',
                         actionButton("prevBtn", "Previous", icon = icon("arrow-left")),
                         actionButton("nextBtn", "Continue",
                                      style = "color: white;
                     background-color: #009FDA;
                     font-weight: bold;
                     position: relative;
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px",
                                      icon = icon("arrow-right")))
                ),
                tabsetPanel(
                  id = 'tabs',
                  tabPanel(
                    
                    title = uiOutput('tool_settings_ui'),
                    #icon("calendar"),
                    value = 'TOOL SETTINGS',
                    
                    fluidPage(
                      bsPopover(id = "tabs", title = '',
                                content = 'Use the "Continue" button at the top of the page to navigate.',
                                placement = "bottom", trigger = "hover", options = list(container ='body')),
                      
                      h3('Please select your preferred settings'),
                      fluidRow(
                        radioButtons("advanced", "Select a type of setting. If you are an advanced user, please select the advanced settings option below for more statistical flexibility.",
                                     choices = c('Basic', 'Advanced'),
                                     selected = 'Basic',
                                     inline = TRUE),
                        bsPopover(id = "advanced", title = '',
                                  content = 'For more statistical options and to view the Simulations tab, "Advanced"', trigger = "hover", options = list(container ='body'))
                      ),
                      
                      fluidRow(
                        radioButtons("data_type", "Select Data Type",
                                     choices = c('Country', 'Archetype'), selected = 'Country', inline = TRUE),
                        bsPopover(id = "data_type", title = '',
                                  content = 'If you pick "Country", you will be able to use real data at the national level. With "Archetype", you will instead use data which is representative of many countries.',
                                  placement = "top", trigger = "hover", options = list(container ='body'))
                      ),
                      uiOutput('damage_type_ui'),
                      
                      bsPopover(id = "damage_type", title = '', content = "Select whether you would like to view the loss as cost per person or as total damage. If you choose cost per person, you will later be prompted for some currency information.",
                                placement = "top", trigger = "hover", options = list(container ='body')),
                      fluidRow(
                        column(3,
                               uiOutput('cost_per_person_ui')
                        ),
                        column(3,
                               uiOutput('rate_ui')),
                        column(3,
                               uiOutput('currency_ui')),
                        column(3,
                               uiOutput('code_ui'))
                      ),
                      uiOutput('country_ui'),
                      fluidRow(uiOutput('data_source_ui'))
                      
                    )),
                  tabPanel(uiOutput('data_ui'),
                           value = 'DATA',
                           
                           fluidPage(
                             fluidRow(uiOutput('select_scale_ui')),
                             fluidRow(
                               h4('Review Data Samples')
                             ),
                             fluidRow(
                               helpText('To edit the data, double click on a value in the table below')
                               ## Keeping this in case the WB wants to go back to uploading data
                               # radioButtons('upload_or_auto',
                               #              'Do you wish to replace pre-loaded data with user-supplied data?',
                               #              choiceValues = c('Pre-loaded data',
                               #                               'User-supplied data'),
                               #              choiceNames = c('No', 'Yes'),
                               #              selected = 'Pre-loaded data',
                               #              inline = TRUE),
                               # bsPopover(id = "upload_or_auto", title = '',
                               #           content = 'As an alternative to using the data built in to the app, you can upload your own data.',
                               #           placement = "top", trigger = "hover", options = list(container ='body'))
                             ),
                             fluidRow(
                               column(9,
                                      h4('Peril data')),
                               column(3,
                                      fluidRow(
                                        downloadButton("download_peril_data",
                                                       "Download Peril Data"),
                                        bsPopover(id = "download_peril_data", title = '',
                                                  content = 'Download the peril raw data so as to view or edit on your computer, outside of this application.',
                                                  placement = "top", 
                                                  trigger = "hover", 
                                                  options = list(container ='body'))
                                      ))
                             ),
                             fluidRow(
                               column(12,
                                      fluidRow(
                                        selectInput('view_data', 'View peril data', choices = c('Total Damages or Number of people affected', 'Frequency')),
                                        bsPopover(id = "view_data", title = '',
                                                  content = 'One can choose to view the data in terms of Total damages or Number of people affected (based on the users choice from the "tool settings" tab) and the frequency of the loss-related events.',
                                                  placement = "top", trigger = "hover", options = list(container ='body'))
                                      ),
                                      DT::dataTableOutput('raw_data_table'))),
                             br(),
                             fluidRow(
                               column(9,
                                      h4('Scaling data')),
                               column(3,
                                      fluidRow(
                                        downloadButton("download_scaled_data",
                                                       "Download Scaled Data"),
                                        bsPopover(id = "download_scaled_data", title = '',
                                                  content = 'Download the scaled raw data so as to view or edit on your computer, outside of this application.',
                                                  placement = "top", 
                                                  trigger = "hover", 
                                                  options = list(container ='body'))))),
                             fluidRow(
                               column(12,
                                      DT::dataTableOutput('raw_scaled_data')
                               )
                             )
                             
                           )),
                  tabPanel(
                    title = uiOutput('input_ui'),
                    value = 'RISK PROFILE FITTING',
                    #  start new row that encompasses inputs for country, download buttons, damage type, and currency
                    fluidPage(
                      fluidRow(column(12,
                                      uiOutput('select_peril_ui'))),
                      fluidRow(
                        uiOutput('peril_ui')
                      ),
                      uiOutput('trend_test_ui'),
                      fluidRow(column(12,
                                      uiOutput('prob_dis_flood'),
                                      uiOutput('prob_dis_earthquake'),
                                      uiOutput('prob_dis_drought'),
                                      uiOutput('prob_dis_storm')))
                      
                    )),
                  tabPanel(
                    uiOutput('simulations_ui'),
                    value = 'SIMULATIONS',
                    fluidPage(
                      fluidRow(
                        column(6,
                               fluidRow(
                                 checkboxGroupInput('overlap_choices',
                                                    'Show:',
                                                    choices = c('Observed data',
                                                                'Simulated data'),
                                                    selected = c('Observed data',
                                                                 'Simulated data'),
                                                    inline = TRUE),
                                 bsPopover(id = "overlap_choices", title = '',
                                           content = 'The chart below will show one or both of the simulated and observed data',
                                           placement = "top", 
                                           trigger = "hover", 
                                           options = list(container ='body')))),
                        column(6,
                               fluidRow(
                                 selectInput('peril_simulation',
                                             'Peril:',
                                             choices = c('Flood',
                                                         'Drought',
                                                         'Earthquake',
                                                         'Storm')),
                                 bsPopover(id = "peril_simulation", title = '',
                                           content = 'Select one specific peril to examine details on the fit with the distributions below',
                                           placement = "top", 
                                           trigger = "hover", 
                                           options = list(container ='body'))
                               ))
                      ),
                      fluidRow(
                        box(
                          title = 'Simulation chart',
                          width = 6,
                          status = 'primary',
                          fluidRow(
                            plotOutput('simulation_plot'),
                            bsPopover(id = "simulation_plot", title = '',
                                      content = "This chart shows the historic distribution of loss from perils.",
                                      placement = "top", trigger = "hover", options = list(container ='body'))
                          )),
                        box(
                          title = 'Simulation table',
                          width = 6,
                          status = 'primary',
                          fluidRow(
                            DT::dataTableOutput('simulation_table'),
                            bsPopover(id = "simulation_table", title = '',
                                      content = "This table shows the AIC scores for each parametric distribution. NAs are a result of the non convergence in the optimization algorithm. The table also shows the Maximum Likelihood Estimators for each distribution.",
                                      placement = "top", trigger = "hover", options = list(container ='body'))
                          ))
                      ),
                      fluidRow(
                        column(6,
                               plotOutput('rag_ratings'))
                        
                      ),
                      fluidRow(
                        column(6,
                               plotOutput('dist_plot')),
                        column(6,
                               plotOutput('grouped_plot'))
                      )
                    )
                  ),
                  tabPanel(
                    title = uiOutput('output_ui'),
                    value = 'OUTPUT',
                    uiOutput('output_top_ui'),
                    fluidPage(
                      br(),
                      checkboxInput('ci',
                                    'CI under construction (only generated in advanced mode)',
                                    value = FALSE),
                      fluidRow(
                        box(title = "",
                            checkboxInput(inputId = 'is_table_1',
                                          label = 'Show table instead of chart',
                                          value = FALSE),
                            uiOutput('output1')),
                        box(title = "",
                            fluidRow(column(6,
                                            numericInput('severe', 'Define the probability (%) for a severe event' , value = 25),
                                            bsPopover(id = "severe", title = '',
                                                      content = 'This should be the estimated percentage likelihood of a severe event taking place',
                                                      placement = "top", 
                                                      trigger = "hover", 
                                                      options = list(container ='body'))),
                                     column(6,
                                            numericInput('extreme', 'Define the probability (%) for an extreme event' , value = 10),
                                            bsPopover(id = "extreme", title = '',
                                                      content = 'This should be the percentage likelihood of an extreme event taking place',
                                                      placement = "top", 
                                                      trigger = "hover", 
                                                      options = list(container ='body')))),
                            fluidRow(
                              column(12,
                                     checkboxInput(inputId = 'is_table_3',
                                                   label = 'Show table instead of chart',
                                                   value = FALSE),
                                     uiOutput('output3')
                              )
                            )
                        )),
                      fluidRow(
                        box(title = "",
                            fluidRow(
                              column(3,
                                     numericInput('budget', 'Budget (in millions)', value = 0),
                                     bsPopover(id = "budget", title = '',
                                               content = 'The budget will be used for calculating the likelihood of exceeding funding, etc',
                                               placement = "top", 
                                               trigger = "hover", 
                                               options = list(container ='body')))
                            ),
                            fluidRow(plotOutput('loss_exceedance_plotly'),
                                     bsPopover(id = "loss_exceedance_plotly", title = 'Exhibit 2', content = "This graph shows the probability of a year taking place that exceeds the aggregate annual loss amount on the y-axis. The probability of exceeding the available budget is represented by the probability where the available budget line and the loss exceedance curve cross.",
                                               placement = "top", trigger = "hover", options = list(container ='body')))),
                        box(title = "",
                            fluidRow(
                              column(3,
                                     numericInput('exceed_budget', 'Exceed funding gap/surplus by', value = 0),
                                     bsPopover(id = "exceed_budget", title = '',
                                               content = 'This is the amount by which the funding gap can be exceeded',
                                               placement = "top", 
                                               trigger = "hover", 
                                               options = list(container ='body'))),
                              column(12,
                                     plotOutput('loss_exceedance_gap_plotly')
                                     
                              ),
                              bsPopover(id = 'loss_exceedance_gap_plotly', title = 'Exhibit 4', content = "The graph shows the probability of experiencing different sized funding gaps/surpluses. When the line is above the x-axis, it indicates a funding surplus - if below, it indicates a funding deficit.",
                                        placement = "top", trigger = "hover", options = list(container ='body'))
                            ))),
                      fluidRow(
                        column(12,
                               align = 'center',
                               actionButton('check_another',
                                            label = 'Check another',
                                            icon = icon('backspace'),
                                            style='font-size:180%'))
                      ))
                  )),
                br()
      )),
    tabItem(
      tabName = 'about',
      about_page
    ),
    tabItem(
      tabName = 'training',
      fluidPage(h1('Training'),
                p('Placeholder page.'))
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output, session) {
  
  shinyjs::disable(selector = '.nav-tabs a')
  
  # Reactive tab data
  tab_data <- reactiveValues(data = tab_dict)
  
  # To download the raw peril data based on country or archetype selected
  output$download_peril_data <- downloadHandler(
    filename = function() {
      paste("perild_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      usde <- use_core_data_edited()
      if(usde){
        cored <- core_data_edited$data
      } else {
        cored <- core_data()
      }
      
      view_data <- input$view_data
      editit <- FALSE
      if(view_data != 'Frequency'){
        data <- cored[[1]]
      } else {
        data <- cored[[2]]
      }
      if(is.null(data)){
        the_data <- tibble(` ` = "No data available")
      }
      if(nrow(data) <=3){
        the_data  <- tibble(` ` = "Not enough observations available")
      } else {
        the_data <- widen_core_data(data)
      }
      write.csv(the_data, file, row.names = FALSE)
    }
  )
  
  # download the scaled data (population, inflation, gdp)
  output$download_scaled_data <- downloadHandler(
    filename = function() {
      paste("scaled_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      psd <- prepare_scale_data()
      if(is.null(psd)){
        the_data <- tibble(` ` = 'No data available')
      } else {
        if(input$data_type == 'Archetype'){
          the_data <- tibble(` ` = 'No data available')
        } else {
          out <- prepare_scale_data()
          out$population_factor <- out$gdp_factor <- out$gdp_ok <- out$inflation_factor <- out$inflation_ok <- NULL
          names(out) <- c('Country', 'Year', 'Population', 'Inflation',
                          'GDP')
          the_data <- out
        }
      }
      write.csv(the_data, file, row.names = FALSE)
    }
  )
  
  ##########
  # LANDING PAGE
  ##########
  # Beginning observeEvent functions for Landing page and showing the 'simulations' tab if advanced user is selected
  # creates a pop up page that the user must accept to acces the app
  set.seed(122)
  histdata <- rnorm(1)
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "", easyClose = FALSE, footer = NULL,
      
      fluidPage(
        fluidRow(
          column(4, align = 'center',
                 img(src='drf.png', align = "left", height = '50px')),
          column(4, align = 'center',
                 img(src='gfdrr.png', align = "middle", height = '50px')),
          column(4, align = 'right',
                 img(src='EU_Flag.png', align = "right", height = '50px'))
        ),
        fluidRow(
          column(12, align = 'center',
                 h2('Welcome to World Bank Group!'))
        ),
        fluidRow(
          column(12, align = 'center',
                 p('Please tell us your name, email and create a password so we can get started.'))
        ),
        fluidRow(
          column(6,
                 textInput('first_name', '',
                           placeholder = 'First name')),
          column(6,
                 textInput('last_name', '',
                           placeholder = 'Last name'))
        ),
        fluidRow(column(12, textInput('email', '', placeholder = 'Email'))),
        fluidRow(column(12, textInput('password', '', placeholder = 'Password'))),
        fluidRow(column(12, textInput('confirm_password', '', placeholder = 'Confirm password'))),
        fluidRow(
          column(12, align = 'center',
                 actionButton('get_started', 'Get started'))
        )
      )
    )
    )
  })
  
  # Observe the "Get started" button on the log-in page and remove the modal
  observeEvent(input$get_started, {
    removeModal()
    showModal(modalDialog(
      title = "Disaster Risk Financing Tool 1", easyClose = FALSE, footer = NULL,
      welcome_modal
    ))
  })
  observeEvent(input$accept,{
    removeModal()
  })
  
  # Define a reactive value which is the currently selected tab number
  rv <- reactiveValues(page = 1)
  
  # Disble the forward, back buttons depending on posiiton
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < n_tabs)
    hide(selector = ".page")
  })
  
  # Keep a counter of time since of the last tab change
  tab_time <- reactiveVal(value = Sys.time())
  
  # Define function for changing the tab number in one direction or the
  # other as a function of forward/back clicks
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  # Observe the forward/back clicks, and update rv$page accordingly
  observeEvent(input$prevBtn, {
    navPage(-1)
  })
  observeEvent(input$nextBtn, {
    navPage(1)
  })
  
  # Observe any changes to rv$page, and update the selected tab accordingly
  observeEvent(rv$page, {
    # Do the time calculations
    tt <- tab_time()
    ok <- not_too_fast(old_time = tt,
                       new_time = Sys.time())
    if(ok){
      tab_number <- rv$page
      td <- tab_data$data
      tab_name <- td %>% filter(number == tab_number) %>% .$name
      updateTabsetPanel(session, inputId="tabs", selected=tab_name)
      tab_time(Sys.time())
    }
  })
  
  # Observe any click on the tab menu, and update accordingly the rv$page object
  observeEvent(input$tabs, {
    # Do the time calculations
    tt <- tab_time()
    ok <- not_too_fast(old_time = tt,
                       new_time = Sys.time())
    if(ok){
      tab_name <- input$tabs
      td <- tab_data$data
      tab_number <- td %>% filter(name == tab_name) %>% .$number
      message(paste0('Selected tab is ', tab_name, '. Number: ', tab_number))
      rv$page <- tab_number
      tab_time(Sys.time())
    }
  })
  
  ##########
  # TOOL SETTINGS
  ##########
  
  #  an output that renders UI based on an input. If data type is country, then countries are shown, else, archetypes.
  output$country_ui <- renderUI({
    country_picked <- input$data_type == 'Country'
    if(country_picked){
      the_input <- selectInput("country",
                               "Choose a country",
                               choices = countries,
                               selected = 'Bangladesh')
      the_input <- fluidRow(
        the_input,
        bsPopover(id = "country", title = '',
                  content = 'Select a country to examine real data from that country. If you wish to use "Archetype" data instead of data specific to one country, go above and change your choice in the "Select Data Type" menu',
                  placement = "top", trigger = "hover", options = list(container ='body'))
      )
    } else {
      the_input <- selectInput('archetype', 'Choose an archetype',
                               choices = archetypes)
      the_input <- 
        fluidRow(the_input,
                 bsPopover(id = "archetype", title = '',
                           content = 'Select an archetype that matches the characteristics of the country or countries of interest to you',
                           placement = "top", trigger = "hover", options = list(container ='body')))
    }
    the_input
  })
  
  # An output that depends on which type of damage is selected. 
  output$damage_type_ui <- renderUI({
    if(input$data_type == 'Archetype'){
      out <- radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                          'Select how you want to view the loss',
                          choices = c('Cost per person'),
                          selected = 'Cost per person',
                          inline = TRUE)
    } else {
      out <- radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                          'Select how you want to view the loss',
                          choices = c('Total damage', 'Cost per person'),
                          selected = 'Total damage',
                          inline = TRUE)
    }
    out <- fluidRow(
      out,
      bsPopover(id = "damage_type", title = '',
                content = "Select whether you would like to view the loss as cost per person or as total damage. If archetype is chosen, you can only do the former.",
                placement = "top", trigger = "hover", options = list(container ='body'))
    )
    out
  })
  
  
  # define a counting object to be used to determine when the 'simulations' tab should be shown
  counter <- reactiveVal(value = 0)
  
  observeEvent(input$advanced,{
    
    cc <- counter()
    new_cc <- cc + 1
    counter(new_cc)
    # if the counter is even, hide tab (this is the default because the counter starts at zero)
    if(cc %% 2 == 0){
      hideTab(inputId = 'tabs', target = 'SIMULATIONS')
      x <- tab_dict[c(1:3,5),]
      x$number <- 1:4
      tab_data$data <- x
    } else {
      # if the counter is odd, show the tab
      showTab(inputId = 'tabs', target = 'SIMULATIONS')
      tab_data$data <- tab_dict
    }
  })
  
  ##########
  # DATA TAB - SUBSETTING AND MANIPULATING DATA FOR TABLES
  ##########
  
  # get a reactive object that selects country data (list) based on imput, if country perils are selected/
  selected_country <- reactive({
    if(is.null(input$country)){
      NULL
    } else {
      # store input country
      country_name <- input$country
      
      # get a list called country data
      country_data <- country_data[country_data$country ==country_name,]
      return(country_data)
    }
  })
  
  # create reactive object for getting frequency data from selected country, if country perils are selected.
  country_frequency <- reactive({
    if(is.null(selected_country())){
      NULL
    } else {
      country_name <- input$country
      best_data <- best_data_source()
      freq_data <- frequency_data[frequency_data$country == country_name,]
      return(freq_data)
    }
  })
  
  # create a reactive object to get archetype data if selected.
  selected_archetype <- reactive({
    if(is.null(input$archetype)){
      NULL
    } else {
      archetype <- input$archetype
      archetype_data <- archetype_cost_data[archetype_cost_data == archetype,]
      return(archetype_data)
    }
  })
  
  # create reactive object to get archetype frequency if selected. 
  archetype_frequency <- reactive({
    if(is.null(selected_archetype())){
      NULL
    } else {
      archetype <- input$archetype
      freq_data <- archetype_freq_data[archetype_freq_data$archetype == archetype,]
      return(freq_data)
    }
  })
  
  # create a reactive object to retrieve the best source
  best_data_source <- reactive({
    if(is.null(input$damage_type) | is.null(selected_country())){
      NULL
    } else {
      if(input$data_type == 'Archetype'){
        return(NULL)
      } else {
        country_data <- selected_country()
        country_name <- unique(country_data$country)
        best_source <- best_data %>% filter(country == country_name)
        damage_type <- input$damage_type
        if(damage_type == 'Total damage'){
          source_name <- best_source %>% filter(damage_type == 'damage') %>% .$best_source
        } else {
          source_name <- best_source %>% filter(damage_type == 'affected') %>% .$best_source
        }
        source_name
      }
    }
  })
  
  # create a UI output to display best source
  output$data_source_ui <- renderUI({
    sn <- best_data_source()
    if(is.null(sn)){
      NULL
    } else {
      h4(paste0('The best data source for the chosen parameters is: ',
                sn))
    }
  })
  
  # prepare loss (total damage) data if total damage was selected. Subset by best data source
  prepare_loss_data <- reactive({
    data_source <- best_data_source()
    if(is.null(data_source) | is.null(country_frequency())){
      NULL
    } else {
      best_source <- best_data_source()
      country_data <- selected_country()
      country_frequency <- country_frequency()
    
      # subset data by best source
      country_data <- country_data[country_data$origin == best_source & country_data$damage_type == 'damage',]
      country_frequency <- country_frequency[country_frequency$origin == best_source & country_frequency$damage_type == 'damage',]
      
      # remove uneeded variables
      country_data$origin <- country_data$damage_type <- country_data$best_data <- NULL
      country_frequency$origin <- country_frequency$damage_type <-  NULL
      
      # store in list loss and frequency data in a list
      data <- list()
      data[[1]] <- country_data
      data[[2]] <- country_frequency
      return(data)
    }
  })
  
  # retrieve the scaling data for the country selected 
  prepare_scale_data <- reactive({
    if(is.null(input$country)){
      NULL
    } else {
      country_name <- input$country
      scale_data <- scale_data[scale_data$Country == country_name,]
      names(scale_data) <- c('country', 'year', 'population','population_factor','inflation',
                             'inflation_factor', 'gdp', 'gdp_factor', 'inflation_ok', 'gdp_ok')
      return(scale_data)
    }
  })
  
  # Createa UI output based on if cost per person was chosen - if so create input for currency
  output$currency_ui <- renderUI({
    if(is.null(input$damage_type)){
      NULL
    } else {
      if(input$damage_type == 'Total damage'){
        NULL
      } else {
        fluidRow(
          radioButtons('currency',
                       'Choose a currency',
                       choices = currencies,
                       selected = 'USD',
                       inline = TRUE),
          bsPopover(id = "currency", title = '',
                    content = "If other is chosen, please select a currency code and exchange rate.",
                    placement = "top", trigger = "hover", options = list(container ='body'))
        )}
    }
  })
  
  # Create a UI output based on if cost per person was chosen - if so create input for cost per person
  output$cost_per_person_ui <- renderUI({
    if(is.null(input$damage_type)){
      NULL
    } else {
      if(input$damage_type == 'Total damage') {
        NULL
      } else {
        numericInput('cost_per_person',
                     'Enter cost per person USD',
                     min = 1,
                     max = 1000,
                     step = 10,
                     value = 50)
      }
    }
  })
  
  # Create a UI output based on if cost per person was chosen - if so create input for currency code
  output$code_ui <- renderUI({
    if(is.null(input$damage_type) | is.null(input$currency)){
      NULL
    } else {
      if(input$damage_type != 'Cost per person'){
        return(NULL)
      }
      if(input$currency == 'USD' | input$damage_type == 'Total damage'){
        selectInput('code',
                    'Choose a currency code',
                    choices = 'USD',
                    selected = 'USD')
      } else {
        selectInput('code',
                    'Choose a currency code',
                    choices = other_currencies,
                    selected = other_currencies[1])
      }
    }
  })
  
  # Create a UI output based on if cost per person was chosen - if so create input for rate
  output$rate_ui <- renderUI({
    if(is.null(input$damage_type) | is.null(input$currency) | is.null(input$damage_type == 'Total damage')) {
      NULL
    } else {
      if(input$damage_type != 'Cost per person'){
        return(NULL)
      }
      if(input$currency == 'USD'){
        numericInput('rate',
                     'Enter conversion rate',
                     min = 1,
                     max = 1,
                     step = 1,
                     value = 1)
      } else {
        numericInput('rate',
                     'Enter conversion rate',
                     min = 0,
                     max = 100,
                     step = 1,
                     value = 1)
      }
    }
  })
 
  # prepare cost per person data if damage type chosen is 'cost per person'
  prepare_cost_data <- reactive({
    best_source <- best_data_source()
    rate <- input$rate
    code <- input$code
    cost <- input$cost_per_person
    country_data <- selected_country()
    cf <- country_frequency()
    ok <- TRUE
    data_source <- best_data_source()
    if(is.null(data_source) | is.null(cf) | is.null(input$damage_type) |
       is.null(best_source) | is.null(rate) | is.null(code) | is.null(cost) | is.null(country_data)){
      ok <- FALSE
    }
    if(!is.null(input$damage_type)){
      if( input$damage_type == 'Total damage'){
        ok <- FALSE
      }
    }
    if(ok){
      
      # get frequency data
      country_frequency <- country_frequency()
     
      # subset by best source
      country_data <- country_data[country_data$origin == best_source & country_data$damage_type == 'affected',]
      country_frequency <- country_frequency[country_frequency$origin == best_source & country_frequency$damage_type == 'affected',]
      
      # remove unneeded columns
      country_data$origin <- country_data$damage_type <- country_data$best_data <- NULL
      country_frequency$origin <- country_frequency$damage_type <-  NULL
      
      # multiply the people affected by cost
      country_data$value <- country_data$value*cost
      
      # store loss and frequency data in list
      data <- list()
      data[[1]] <- country_data
      data[[2]] <- country_frequency
      return(data)
    } else {
      NULL
    }
  })
  
  # prepare archetype data if archetypes are chosen. Archetypes are by default cost per person method
  prepare_archetype_data <- reactive({
    
    # get cost per person inputs
    rate <- input$rate
    code <- input$code
    cost <- input$cost_per_person
    archetype_data <- selected_archetype()
    archetype_frequency <- archetype_frequency()
    ok <- TRUE
    if(is.null(archetype_frequency) | is.null(input$damage_type) | 
       is.null(rate) | is.null(code) | is.null(cost) | is.null(archetype_data)){
      ok <- FALSE
    }
    if(!is.null(input$damage_type)){
      if( input$damage_type == 'Total damage'){
        ok <- FALSE
      }
    }
    if(ok){
      # remove unneeded columns
      archetype_data$data_type <- archetype_data$damage_type <- archetype_data$best_data <- NULL
      archetype_frequency$data_type <- archetype_frequency$damage_type <-  NULL
      
      # multiply people affected by cost
      archetype_data$value <- archetype_data$value*cost
     
      # store archetype loss and frequency in list
      data <- list()
      data[[1]] <- archetype_data
      data[[2]] <- archetype_frequency
      return(data)
    } else {
      NULL
    }
  })
  
  # Create a UI output that gives the user a choice on what data to scale by based on if advanced user.
  # If advanced, can choose all three. If not, choice is restricted to population.
  output$select_scale_ui <- renderUI({
    if(input$data_type == 'Archetype' | is.null(prepare_scale_data())){
      NULL
    } else {
      if(input$advanced == 'Basic'){
        out <- selectInput('select_scale',
                           'Scale data by:',
                           choices = 'POPULATION',
                           selected = 'POPULATION')
      } else {
        data <- prepare_scale_data()
        
        use_inflation <- isTRUE(unique(data$inflation_ok))
        use_gdp <- isTRUE(unique(data$gdp_ok))
        if(use_inflation & use_gdp){
          scaled_choices <- c('Population', 'Inflation', 'GDP')
        } else if(!use_inflation & use_gdp){
          scaled_choices <- c('Population', 'GDP')
        } else if(use_inflation & !use_gdp){
          scaled_choices <- c('Population', 'Inflation')
        } else if(!use_inflation & !use_gdp){
          scaled_choices <- c('Population')
        }
        
        scaled_choices <- toupper(c(scaled_choices, ' No scaling'))
        out <- selectInput('select_scale',
                           'Choose from preloaded scaling data',
                           choices = scaled_choices,
                           selected = scaled_choices[1])
      }
      fluidRow(
        out,
        bsPopover(id = "select_scale", title = '',
                  content = 'Choose how you would like to see your data "scaled". (the default is by population)',
                  placement = "top", trigger = "hover", options = list(container ='body'))
      ) 
    }
  })
  
  # Create a reactive object that gathers all the information from inputs so far and creates a list of two dataframes:
  # with homogenized format so that they can be passed to analysis functions.
  core_data_edited <- reactiveValues(data = data.frame())
  use_core_data_edited <- reactiveVal(value = FALSE)
  core_data <- reactive({
    dat <- NULL
    ad <- prepare_archetype_data()
    ld <- prepare_loss_data()
    cored <- prepare_cost_data()
    dt <- input$data_type
    dmt <- input$damage_type  
    ok <- TRUE
    min_obs <- 3
    if(is.null(dt)){
      ok <- FALSE
    }
    possible_tables <- c('cost', 'loss', 'arch')
    if(is.null(ad)){
      possible_tables <- possible_tables[possible_tables != 'arch']
    }
    if(is.null(ld)){
      possible_tables <- possible_tables[possible_tables!= 'loss']
    }
    if(is.null(cored)){
      possible_tables <- possible_tables[possible_tables != 'cost']
    }
    if(length(possible_tables) == 0){
      ok <- FALSE
    }
    if(ok){
      if(dt == 'Country'){
        if(dmt == 'Total damage'){
          dat <- ld
        } else {
          # Country and cost, rather than country and total damage
          dat <- cored
        }
      } else {
        # Not country, but archetype
        dat <- ad
      }
    } else {
      out <- NULL
    }
    # At this point, if we've made to here, there is an object called data
    out <- NULL
    if(!is.null(dat)){
      out <- dat
      out <- transform_core_data(out)
    }
    out
  })
  
  # create data table to view that data selected by the user
  output$raw_data_table <- DT::renderDataTable({
    
    # determine if the data was edited by the user
    usde <- use_core_data_edited()
    if(usde){
      cored <- core_data_edited$data
    } else {
      cored <- core_data()
    }
    
    view_data <- input$view_data
    editit <- FALSE
    if(view_data != 'Frequency'){
      data <- cored[[1]]
      editit <- TRUE
    } else {
      data <- cored[[2]]
    }
    if(is.null(data)){
      return(NULL)
    }
    if(nrow(data) <=3){
      message('Not enough obs')
      return(NULL)
    } else {
      out <- widen_core_data(data)
      if(editit){
        editit <- list(target = 'cell', disable = list(columns = 0:1))
      }
      datatable(out, rownames = FALSE,
                editable = editit, options = list(lengthChange = FALSE, dom = 't')) %>%
        formatCurrency(c("Drought",
                         "Earthquake",
                         "Flood",
                         "Storm"),currency = "", interval = 3, mark = ",")
    }
  })
  proxy <- dataTableProxy('raw_data_table')
  
  
  
  # Capture the edits to the raw_data_table
  observeEvent(input$raw_data_table_cell_edit, {
    # Indicate that we should instead use edited core data, instead of the default
    message('Woh, edited the core data. Now we are going to use core_data_edited$data instead of core_data()')
    use_core_data_edited(TRUE)
    # Capture edits
    info = input$raw_data_table_cell_edit
    i = info$row
    j = info$col
    v = info$value
    # Update core data accordingly
    cdx <- core_data()
    cdx1 <- cdx[[1]]
    cdx2 <- cdx[[2]]
    cdx1 <- widen_core_data(cdx1)
    cdx1[i,j+1] <- as.numeric(v)
    cdx1 <- elongate_core_data(cdx1)
    cdx2 <- values_to_frequency(cdx1)
    edited <- list(cdx1, cdx2)
    core_data_edited$data <- edited
    
  })
  
  # create table to visualize the country's scaling data
  output$raw_scaled_data <- renderDataTable({
    psd <- prepare_scale_data()
    if(is.null(psd)){
      NULL
    } else {
      if(input$data_type == 'Archetype'){
        out <- NULL
      } else {
        out <- prepare_scale_data()
        out$population_factor <- out$gdp_factor <- out$gdp_ok <- out$inflation_factor <- out$inflation_ok <- NULL
        names(out) <- c('Country', 'Year', 'Population', 'Inflation',
                        'GDP')
        out <- out %>% dplyr::select(Year, Country, Population,
                                     Inflation, GDP)
        datatable(out, rownames = FALSE, options = list(lengthChange = FALSE, dom = 't')) %>%
          formatCurrency(c("Population", 
                           "Inflation",
                           "GDP"),currency = "", interval = 3, mark = ",")
      }
    }
  })
  
  # reactive object to scale data
  scale_data_reactive <- reactive({
    final_list <- list()
    usde <- use_core_data_edited()
    if(usde){
      cored <- core_data_edited$data
    } else {
      cored <- core_data()
    }
    sd <- prepare_scale_data()
    is_archetype <- input$data_type == 'Archetype'
    is_advanced <- input$advanced == 'Advanced'
    ss <- input$select_scale
    out <- NULL
    ok <- TRUE
    if(is.null(sd)){
      return(NULL)
    }
    if(is.null(cored)){
      return(NULL)
    }
    if(is_archetype){
      message('---no archetype, nulling out')
      return(NULL)
    }
    if(is.null(ss)){
      return(NULL)
    }
    if(is.list(cored)){
      second_part <- cored[[2]]
      cored <- cored[[1]]
      
    } else {
      return(NULL)
    }
    
    if(is_advanced){
      if(ss == 'POPULATION'){
        # store frequenc
        if(is.null(cored) | is.null(sd)){
          out <- NULL
        } else {
          combined_data <- left_join(cored, sd)
          combined_data$value <- combined_data$population_factor*combined_data$value
          out <- combined_data
          out <- out %>% dplyr::select(year:value)
          final_list[[1]] <- out
          final_list[[2]] <- second_part
          return(final_list)
          
        }
      } else if(ss == 'GDP'){
        if(is.null(cored) | is.null(sd)){
          out <- NULL
        } else {
          combined_data <- left_join(cored, sd)
          combined_data$value <- combined_data$gdp_factor*combined_data$value
          out <- combined_data
          out <- out %>% dplyr::select(year:value)
          final_list[[1]] <- out
          final_list[[2]] <- second_part
          return(final_list)
          
        }
      } else if(ss == 'INFLATION'){
        if(is.null(cored) | is.null(sd)){
          out <- NULL
        } else {
          
          combined_data <- left_join(cored, sd)
          combined_data$value <- combined_data$inflation_factor*combined_data$value
          out <- combined_data
          out <- out %>% dplyr::select(year:value)
          final_list[[1]] <- out
          final_list[[2]] <- second_part
          return(final_list)
        }
      } else {
        out <- cored
        out <- out %>% dplyr::select(year:value)
        final_list[[1]] <- out
        final_list[[2]] <- second_part
        return(final_list)
      }
    } else { # not advanced user
      if(is.null(cored) | is.null(sd)){
        out <- NULL
      } else {
        combined_data <- left_join(cored, sd)
        combined_data$value <- combined_data$population_factor*combined_data$value
        out <- combined_data
      }
      out <- out %>% dplyr::select(year:value)
      final_list[[1]] <- out
      final_list[[2]] <- second_part
      return(final_list)
    }
    out <- out %>% dplyr::select(year:value)
    final_list[[1]] <- out
    final_list[[2]] <- second_part
    return(final_list)
    
  })
  
  significant_trends <- reactiveVal(FALSE)
  
  output$trend_test_ui <- renderUI({
    
    sdr <- scale_data_reactive()
    sdr <- sdr[[1]]
    ok <- FALSE
    if(!is.null(sdr)){
      if(nrow(sdr) > 0){
        ok <- TRUE
      }
    }
    if(!ok){
      return(NULL)
    } else {
      test_data <- sdr
      # split test_data by peril type
      test_storm <-try(MannKendall(test_data$Outcome[test_data$peril == 'Storm'])$sl, silent = TRUE)
      test_drought <-try(MannKendall(test_data$Outcome[test_data$peril == 'Drought'])$sl, silent = TRUE)
      test_earthquake <-try(MannKendall(test_data$Outcome[test_data$peril == 'Earthquake'])$sl, silent = TRUE)
      test_flood <-try(MannKendall(test_data$Outcome[test_data$peril == 'Flood'])$sl, silent = TRUE)
      
      # concatanate
      all_perils <- data_frame(peril = c('Storm', 'Drought', 'Earthquake', 'Flood'),
                               p_value = c(test_storm,test_drought, test_earthquake, test_flood))
      all_perils$p_value <- as.numeric(all_perils$p_value)
      
      if(input$advanced == 'Basic'){
        return(NULL)
      } else {
        peril_names <- all_perils$peril[all_perils$p_value < 0.05 & !is.na(all_perils$p_value)]
        if(identical(peril_names, character(0))){
          significant_trends(FALSE)
          fluidPage(
            helpText('All perils had no significant trend or not enough observations')
          )
          
        } else {
          significant_trends(TRUE)
          peril_names <- paste(peril_names, collapse = ',')
          fluidPage(
            selectInput('trend_test', paste0('Trends were found in ', paste0(peril_names, collapse = ', '), '. Correct linear trends?'), choices = c('Yes', 'No')))
          
        }
      }
    }
    
  })
  
  execute_trend_test <- reactive({
    
    sdr <- scale_data_reactive()
    sdr <- sdr[[1]]
    ok <- FALSE
    if(!is.null(sdr)){
      if(nrow(sdr) > 0){
        ok <- TRUE
      }
    }
    if(!ok){
      return(NULL)
    } else {
      test_data <- sdr
      # split test_data by peril type
      test_storm <-try(MannKendall(test_data$Outcome[test_data$peril == 'Storm'])$sl, silent = TRUE)
      test_drought <-try(MannKendall(test_data$Outcome[test_data$peril == 'Drought'])$sl, silent = TRUE)
      test_earthquake <-try(MannKendall(test_data$Outcome[test_data$peril == 'Earthquake'])$sl, silent = TRUE)
      test_flood <-try(MannKendall(test_data$Outcome[test_data$peril == 'Flood'])$sl, silent = TRUE)
      
      # concatanate
      all_perils <- data_frame(peril = c('Storm', 'Drought', 'Earthquake', 'Flood'),
                               p_value = c(test_storm,test_drought, test_earthquake, test_flood))
      all_perils$p_value <- as.numeric(all_perils$p_value)
      
      
      if(input$advanced == 'Basic'){
        return(NULL)
      } else {
        return(all_perils)
      }
    }
    
  })
  
  
  
  # reactive object to correct for trend
  # trend_perils <- all_perils
  # trend_perils$p_value[trend_perils$peril == 'Storm'] <- 0.03
  
  correct_trend <- reactive({
    if(is.null(input$trend_test) | is.null(execute_trend_test())){
      return(NULL)
    } else {
      
      if(input$trend_test  == 'No'){
        return(NULL)
      }
      trend_perils <- execute_trend_test()
      peril_type <- trend_perils$peril[trend_perils$p_value <= 0.05 & !is.na(trend_perils$p_value)]
      
      # split data into 3 groups - loss trend perils, loss other perils, and other data
      trend_data <- scale_data_reactive()
      second_part <- trend_data[[2]]
      trend_data <- trend_data[[1]]
      trend_data_peril <-  trend_data[trend_data$peril %in% peril_type,]
      
      # apply trend to trend data for both trends
      data_list <- list()
      for(i in 1:length(peril_type)){
        peril <- peril_type[i]
        sub_data <- trend_data[trend_data$peril == peril,]
        sub_data$trend_value <- detrend(sub_data$value)
        data_list[[i]] <- sub_data
      }
      
      trended_data <- do.call('rbind', data_list)
      trend_data$value[trend_data$peril %in% peril_type] <- trended_data$trend_value
      final_data <- list()
      final_data[[1]] <- trend_data
      final_data[[2]] <- second_part
      return(final_data)
    }
    
    
    
  })
  
  
  ################
  # Data tab
  ################
  # the problem here is that there are some reactives we are not nullifying out and need to spread data for frequency
  # create a data table  
  
  
  # output for scaling data if available - the 3rd, 4th, and 5th index in the data list are scaling data. Population is 3rd
  # gdp 4th, inflation 5th
  
  # reactive object to create right data
  get_right_data <- reactive({
    usde <- use_core_data_edited()
    if(usde){
      cored <- core_data_edited$data
    } else {
      cored <- core_data()
    }
    scale_dat <- scale_data_reactive()
    trend_dat <- correct_trend()
    is_trend <- input$trend_test
    out <- NULL
    # if(is.null(core_dat) & is.null(scale_dat) & is.null(trend_dat)){
    #   NULL
    # }
    if(input$data_type == 'Archetype'){
      out <- cored
    } else {
      # capture whether there are significant trends
      if(is.null(is_trend)){
        out <- scale_dat
      } else {
        if(is_trend == 'Yes'){
          out <- trend_dat
        } else {
          out <- scale_dat
        }
      }
    }
    return(out)
  })
  
  simulate_bernoulli <- reactive({
    rd <- get_right_data()
    # save(rd, file = 'right_data.RData')
    rd <- rd[[2]]
    temp<- sim_bern(rd)
    return(temp)
  })
  
  fitted_distribution <- reactive({
    is_advanced <- input$advanced == 'Advanced'
    
    rd <- get_right_data()
    rd <- rd[[1]]
    temp <- fit_distribution(rd, advanced_mode = is_advanced)
    return(temp)
  })
  
  filtered_distribution <- reactive({
    fd <- fitted_distribution()
    filter_distribution(fd)
  })
  
  output$peril_ui <- renderUI({
    is_advanced <- input$advanced == 'Advanced'
    fd <- filtered_distribution()
    fdx <- fitted_distribution()
    # Filter to keep only those non na values
    fdx_ok <- FALSE
    if(!is.null(fdx)){
      if(nrow(fdx) > 0){
        fdx_ok <- TRUE
        fdx <- fdx %>%
          filter(!is.na(aic))
      }
    }
    if(is.null(fd)){
      return(NULL)
    } else {
      chosen_flood <- fd %>% filter(peril == 'Flood') %>% .$distribution
      chosen_earthquake <- fd %>% filter(peril == 'Earthquake') %>% .$distribution
      chosen_drought <- fd %>% filter(peril == 'Drought') %>% .$distribution
      chosen_storm <- fd %>% filter(peril == 'Storm') %>% .$distribution
      
      if(!is_advanced){
        flood_choices <- chosen_flood
        earthquake_choices <- chosen_earthquake
        drought_choices <- chosen_drought
        storm_choices <- chosen_storm
      } else {
        if(fdx_ok){
          flood_choices <- fdx %>% filter(peril == 'Flood') %>% .$distribution
          earthquake_choices <- fdx %>% filter(peril == 'Earthquake') %>% .$distribution
          drought_choices <- fdx %>% filter(peril == 'Drought') %>% .$distribution
          storm_choices <- fdx %>% filter(peril == 'Storm') %>% .$distribution
        } else {
          flood_choices <- earthquake_choices <- drought_choices <- storm_choices <- advanced_parametric
        }
        
      }
      message('flood choices is ', flood_choices)
      message('chosen flood ', chosen_flood)
      
      no_go <- c()
      if(length(chosen_flood) == 0){
        no_go <- c(no_go, 'Flood')
        flood_go <- br()
      } else {
        flood_go <- fluidRow(
            radioButtons('dist_flood_input',
                         'Distribution for flood',
                         choices = flood_choices,
                         selected = chosen_flood,
                         inline = TRUE),
          bsPopover(id = "dist_flood_input", title = '',
                    content = 'The selected distribution is the "best" distribution for the observed flood damage. In advanced mode, however, you can select other distributions as well.',
                    placement = "top", 
                    trigger = "hover", 
                    options = list(container ='body'))
        )
      }
      if(length(chosen_earthquake) == 0){
        no_go <- c(no_go, 'Earthquake')
        earthquake_go <- br()
      } else {
        earthquake_go <- fluidRow(
          radioButtons('dist_earthquake_input',
                       'Distribution for earthquake',
                       choices = earthquake_choices,
                       selected = chosen_earthquake,
                       inline = TRUE),
          bsPopover(id = "dist_earthquake_input", title = '',
                    content = 'The selected distribution is the "best" distribution for the observed earthquake damage. In advanced mode, however, you can select other distributions as well.',
                    placement = "top", 
                    trigger = "hover", 
                    options = list(container ='body'))
        )
      }
      if(length(chosen_drought) == 0){
        no_go <- c(no_go, 'Drought')
        drought_go <- br()
      } else {
        drought_go <- fluidRow(
          radioButtons('dist_drought_input',
                       'Distribution for drought',
                       choices = drought_choices,
                       selected = chosen_drought,
                       inline = TRUE),
          bsPopover(id = "dist_drought_input", title = '',
                    content = 'The selected distribution is the "best" distribution for the observed drought damage. In advanced mode, however, you can select other distributions as well.',
                    placement = "top", 
                    trigger = "hover", 
                    options = list(container ='body'))
          
        )
      }
      if(length(chosen_storm) == 0){
        no_go <- c(no_go, 'Storm')
        storm_go <- br()
      } else {
        storm_go <- fluidRow(
          radioButtons('dist_storm_input',
                       'Distribution for storm',
                       choices = storm_choices,
                       selected = chosen_storm,
                       inline = TRUE),
          bsPopover(id = "dist_storm_input", title = '',
                    content = 'The selected distribution is the "best" distribution for the observed storm damage. In advanced mode, however, you can select other distributions as well.',
                    placement = "top", 
                    trigger = "hover", 
                    options = list(container ='body'))
        )
      }
      no_go <- paste0(no_go, collapse = ', ')
      if(length(no_go) > 0){
        ht <- helpText(paste0('No distributions could fit the data for the following perils: ', no_go))
      } else {
        ht <- br()
      }
      
      fluidPage(
        fluidRow(ht),
        flood_go,
        drought_go,
        storm_go,
        earthquake_go
      )
    }
  })
  
  prepared_simulations <- reactive({
    fd <- fitted_distribution()
    x <- prepare_simulations(fd, dist_flood = input$dist_flood_input,
                             dist_drought = input$dist_drought_input,
                             dist_storm = input$dist_storm_input,
                             dist_earthquake = input$dist_earthquake_input)
    message('x is')
    print(head(x))
    x
  })
  
  ran_simulations <- reactive({

    bs <- simulate_bernoulli()
    ps <- prepared_simulations()
    # save(bs, file = 'bs.RData')
    # save(ps, file = 'ps.RData')
    
    x <- run_simulations(ps, bs)
    return(x)
  })
  
  
  # output$delete <- DT::renderDataTable({
  #   corrected_data <- ran_simulations()
  #   datatable(corrected_data, rownames = FALSE)
  # }, options = list(pageLength = 5, autoWidth = TRUE, rownames= FALSE
  # ))
  
  output$simulation_plot <- renderPlot({
    rs <- ran_simulations()
    rd <- get_right_data()
    ips <- input$peril_simulation
    ioc <- input$overlap_choices
    plot_simulations(rs = rs,
                     right_data = rd,
                     peril = ips,
                     overlap = ioc)
  })
  
  output$simulation_table <- DT::renderDataTable({
    fd <- fitted_distribution()
    ok <- FALSE
    if(!is.null(fd)){
      if(nrow(fd) > 0){
        ok <- TRUE
      }
    }
    if(ok){
      selected_peril <- input$peril_simulation
      fd <- fd %>%
        filter(peril == selected_peril) %>%
        dplyr::select(-peril)
      fd <- fd[, c('distribution', 'aic', 'mle1', 'mle2')]
      fd$mle1 <- round(fd$mle1, 2)
      fd$mle2 <- round(fd$mle2, 2)
      
      names(fd) <- c('Distribution',
                     'AIC',
                     'MLE 1',
                     'MLE 2')
        return(datatable(fd, rownames = FALSE, options = list(lengthChange = FALSE, dom = 't')))
    } else{
      return(NULL)
    }
  })
  
  ################
  # Output tab
  ################
  output$select_peril_ui <- renderUI({
    dat_sim <- ran_simulations()
    if(is.null(dat_sim)){
      return(NULL)
    }
    dat_sim <- dat_sim %>% filter(!is.na(value))

    peril_choices <- unique(dat_sim$key)
    
    
    fluidRow(
      checkboxGroupInput('select_peril',
                         label = 'Perils to view on output page',
                         choices = peril_choices,
                         selected = peril_choices,
                         inline = TRUE),
      bsPopover(id = "select_peril", title = '',
                content = 'Select all perils which you would like to view',
                placement = "top", 
                trigger = "hover", 
                options = list(container ='body'))
    )
  })
  
  gather_perils <- reactive({
    selected_perils <- input$select_peril
    if(is.null(selected_perils)){
      NULL
    } else {
      dat_sim <- ran_simulations()
      dat_sim <- dat_sim %>% filter(!is.na(outcome))
      # filter selected_perils
      filtered_sims <- dat_sim %>%
        filter(key %in% selected_perils) %>%
        mutate(dummy = 1) %>%
        group_by(key) %>%
        mutate(cs = cumsum(dummy)) %>%
        ungroup() %>%
        dplyr::select(-dummy) %>%
        mutate(key = 'all_perils') %>%
        group_by(key, cs) %>%
        summarise(value = sum(outcome, na.rm = TRUE),
                  value_lower = sum(outcome_lower),
                  value_upper = sum(outcome_upper)) %>%
        dplyr::select(-cs)
      return(filtered_sims)
    }
  })
  
  gather_data <- reactive({
    selected_perils <- input$select_peril
    if(is.null(selected_perils)){
      NULL
    } else {
      dat <- get_right_data()
      dat <- dat[[1]]

      # filter selected_perils
      filtered_dat <- dat %>%
        filter(peril %in% selected_perils) %>%
        group_by(year) %>%
        summarise(value = sum(value))
      
      return(filtered_dat)
    }
  })
  # # make reactive object to store probability of exceeding budget
  probability_of_exceeding <- reactive({
    
    budget <- input$budget
    if(is.null(budget) | is.null(gather_perils())){
      NULL
    } else {
      
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # get budget
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5,0.98,by=0.002), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      
      # find where budget equals curve
      prob_exceed <- output$Probability[which.min(abs(output$`Total Loss` - budget))]
      return(prob_exceed)
      
      
    }
    
    
    
  })
  #
  # create a reactive object that takes new input
  probability_of_exceeding_surplus_deficit <- reactive({
    
    budget <- input$budget
    if(is.null(budget) |  is.null(gather_perils())){
      NULL
    } else {
      
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # get budget
      exceed_budget <- input$exceed_budget
      budget <- exceed_budget + budget
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5,0.98,by=0.002), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      # save(output, budget, file = 'prob_exceed.RData')
      # find where budget equals curve
      prob_exceed_surplus_deficit <- output$Probability[which.min(abs(output$`Total Loss` - budget))]
      return(prob_exceed_surplus_deficit)
      
    }
    
  })
  #
  #
  #
  # # OUTPUT 1
  #
  
  annual_loss_data <- reactive({
    
    budget <- input$budget
    gp <- gather_perils()
    gd <- gather_data()
    selected_perils <- input$select_peril
    if(is.na(budget) | is.null(gp) | is.null(gd) | is.null(selected_perils)){
      return(NULL)
    } else {
      
      dat <- gather_data()
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # remove obsevations with 0, if any
      dat <- dat[dat$value > 0,]
      dat <- dat[order(dat$year, decreasing = FALSE),]
      # get budget
      
      sub_dat <- quant_that(dat_sim = dat_sim,
                            dat = dat)
      
      sub_dat$variable <- factor(sub_dat$variable, levels = c('1 in 5 Years', '1 in 10 Years', '1 in 25 Years', '1 in 50 Years', '1 in 100 Years',
                                                         'Annual average', 'Highest historical annual loss', 'Most recent annual loss'))
      
      sub_dat$value <- round(sub_dat$value, 2)
      # save(sub_dat, file = 'p_dat.RData')
      return(sub_dat)
    }
  })
  
  output$output1 <- renderUI({
    is_table <- input$is_table_1
    if(is_table){
      DT::dataTableOutput('annual_loss_tably')
    } else {
      fluidRow(
        plotOutput('annual_loss_plotly'),
        bsPopover(id = "annual_loss_plotly", title = 'Exhibit 1',
                  content = "This graph shows the estimated annual loss across all selected perils. A return period of 1 in 5 years is the estimated annual loss expected to happen every five years (ie 20% probability). Similarly, a period of 1 in 10 years is the estimated annual loss expected to happen every 10 years (ie 10% probability).",
                  placement = "top", trigger = "hover", options = list(container ='body'))
      )
    }
  })
  
  output$annual_loss_gap_tably <- DT::renderDataTable({
    x <- annual_loss_gap_data()
    save(x, file = 'alg.RData')
    if(!is.null(x)){
      x$value <- round(x$value)
      x$value_lower <- round(x$value_lower)
      x$value_upper <- round(x$value_upper)
      names(x) <- c('Variable', 'Loss', 'Lower bound', 'Upper bound')
      # names(x) <- Hmisc::capitalize(names(x))
      datatable(x, rownames = FALSE,
                editable = FALSE, options = list(lengthChange = FALSE, dom = 't')) %>%
        formatCurrency(c("Loss", "Lower bound", "Upper bound"),currency = "", interval = 3, mark = ",")
      # DT::datatable(x)
    }
  })
  
  output$output3 <- renderUI({
    is_table <- input$is_table_3
    if(is_table){
      DT::dataTableOutput('annual_loss_gap_tably')
    } else {
      fluidRow(
        plotOutput('annual_loss_gap_plotly'),
        bsPopover(id = 'annual_loss_gap_plotly', title = 'Exhibit 3', content = "The funding gap is the difference between the available federal budget and the estimated annual loss at the return period. A loss value below the red budget line represents an estimated surplus (if above, it would be a deficit).",
                  placement = "top", trigger = "hover", options = list(container ='body'))
      )
    }
  })
  
  
  output$annual_loss_tably <- DT::renderDataTable({
    x <- annual_loss_data()
    if(!is.null(x)){
      x$value <- round(x$value)
      x$value_lower <- round(x$value_lower)
      x$value_upper <- round(x$value_upper)
      names(x) <- c('Variable', 'Loss', 'Lower bound', 'Upper bound')
      # names(x) <- Hmisc::capitalize(names(x))
      datatable(x, rownames = FALSE,
                editable = FALSE, options = list(lengthChange = FALSE, dom = 't')) %>%
        formatCurrency(c("Loss", "Lower bound", "Upper bound"),currency = "", interval = 3, mark = ",")
    }
  })
  
  
  ### OUTPUT 1
  output$annual_loss_plotly <- renderPlot({
    budget <- input$budget
    plot_dat <- annual_loss_data()
    sp <- input$select_peril
    
    if(is.null(plot_dat) | is.null(sp)){
      return(NULL)
    }
    
    is_archetype <- input$data_type == 'Archetype'
    # get country input for plot title
    if(is_archetype){
      plot_title <- paste0('Estimate of Annual Loss by ', sp, ' for ', '\n',input$archetype )
    } else {
      plot_title <- paste0('Estimate of Annual Loss by ', sp, ' for ','\n', input$country )
    }
    
    if(input$ci){
      # Plot
      g <- ggplot(plot_dat, aes(x=variable,
                                y=value/scale_size,
                                text = value)) +
        geom_bar(stat = 'identity',
                 fill = '#5B84B1FF',
                 col = '#FC766AFF',
                 alpha = 0.6) +
        geom_hline(yintercept = budget) +
        theme_bw(base_size = 14,
                 base_family = 'Ubuntu')  +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1)) +
        xlab('') + ylab('(in millions)') +
        ggtitle(plot_title) + geom_errorbar(aes(x = variable,
                          ymin = value_lower/scale_size,
                          ymax = value_upper/scale_size))
    } else {
      # Plot
      g <- ggplot(plot_dat, aes(x=variable,
                                y=value/scale_size,
                                text = value)) +
        geom_bar(stat = 'identity',
                 fill = '#5B84B1FF',
                 col = '#FC766AFF',
                 alpha = 0.6) +
        geom_hline(yintercept = budget) +
        theme_bw(base_size = 14,
                 base_family = 'Ubuntu')  +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1)) +
        xlab('') + ylab('(in millions)') +
        ggtitle(plot_title)
      
    }
    
    return(g)
    
   
   
  })
  
  ## OUTPUT 3
  output$loss_exceedance_plotly <- renderPlot({
    
    prob_exceed <- probability_of_exceeding()
    budget <- input$budget
    sp <- input$select_peril
    
    
    if(is.null(prob_exceed) | is.na(budget) | is.null(gather_data()) | is.null(sp)){
      NULL
    } else {
      
      dat <- gather_data()
      dat_sim <- gather_perils()
      # load(file = 'dat.RData')
      # 
      # load(file = 'dat_sim.RData')
      dat <- dat[order(dat$year, decreasing = FALSE),]
      largest_loss_num <- round(max(dat$value), 2)
      largest_loss_year <- dat$year[dat$value == max(dat$value)]
      largest_loss_num <- largest_loss_num/scale_size
      
      # find where budget equals curve
      
      # get country input for plot title
      is_archetype <- input$data_type == 'Archetype'
      # get country input for plot title
      if(is_archetype){
        plot_title <- paste0('Loss exceedance curve by ', sp, '\n', ' for ', input$archetype  )
      } else {
        plot_title <- paste0('Loss exceedance curve by ', sp, '\n',' for ', input$country )
      }      
      # caption 
      exceed_budget <- paste0('Probability of exceeding budget = ', prob_exceed)
      plot_title <- paste0(plot_title, ' : ', exceed_budget)
      
      # budget <- input$budget
      output <- as.data.frame(quantile(dat_sim$value,seq(0.5,0.98,by=0.002), na.rm = TRUE))
      output$x <- rownames(output)
      rownames(output) <- NULL
      names(output)[1] <- 'y'
      
      # remove percent and turn numeric
      output$x <- gsub('%', '', output$x)
      output$x <- as.numeric(output$x)
      output$x <- output$x/100
      names(output)[1] <- 'Total Loss'
      names(output)[2] <- 'Probability'
      output$Probability <- 1 - output$Probability
      output$`Total Loss lower` <-  quantile(dat_sim$value_lower,seq(0.5,0.98,by=0.002), na.rm = TRUE)
      output$`Total Loss upper` <-  quantile(dat_sim$value_upper,seq(0.5,0.98,by=0.002), na.rm = TRUE)
      
      plot_dat <- output
      # get budget
      if(input$ci){
        
        g <- ggplot(plot_dat, aes(Probability, `Total Loss`/scale_size)) +
          geom_line(col = 'blue', size = 1, alpha = 0.7) +
          geom_line(aes(Probability, `Total Loss lower`/scale_size), linetype = 'dotted') +
          geom_line(aes(Probability, `Total Loss upper`/scale_size), linetype = 'dotted') +
          scale_x_reverse() +
          ggtitle(plot_title) +
          labs(y = '') +
          geom_hline(yintercept = largest_loss_num) +
          geom_hline(yintercept = budget) +
          geom_vline(xintercept = prob_exceed, linetype = 'dotted') +
          annotate('text', label = 'Budget', x = 0.45, y = budget, vjust = -1) +
          annotate('text', label = 'Largest loss', x = 0.45, y = largest_loss_num, vjust = -1) +
          theme_bw(base_size = 14,
                   base_family = 'Ubuntu')

        
      } else {
        g <- ggplot(plot_dat, aes(Probability, `Total Loss`/scale_size)) +
          geom_line(col = 'blue', size = 1, alpha = 0.7) +
          scale_x_reverse() +
          ggtitle(plot_title) +
          geom_hline(yintercept = largest_loss_num) +
          geom_hline(yintercept = budget) +
          geom_vline(xintercept = prob_exceed, linetype = 'dotted') +
          labs(y = '') +
          annotate('text', label = 'Budget', x = 0.45, y = budget, vjust = -1) +
          annotate('text', label = 'Largest loss', x = 0.45, y = largest_loss_num, vjust = -1) +
          theme_bw(base_size = 14,
                   base_family = 'Ubuntu')
        
      }
    return(g)
    }
  
  })
  
  # ############ OUTPUT 3
  annual_loss_gap_data <- reactive({
    budget <- input$budget
    if(is.na(budget) |  is.null(gather_perils()) | is.null(gather_data())){
      NULL
    } else {
      dat <- gather_data()
      dat_sim <- gather_perils()
      dat_sim <- dat_sim %>% filter(!is.na(value))
      # remove obsevations with 0, if any
      dat <- dat[dat$value > 0,]
      dat <- dat[order(dat$year, decreasing = FALSE),]
      is_archetype <- input$data_type == 'Archetype'
      severe <- input$severe
      extreme <- input$extreme
      severe <- severe/100
      severe <- 1-severe
      extreme <- extreme/100
      extreme <- 1-extreme
      

      output <- quantile(dat_sim$value,c(severe, extreme))
      annual_avg <- mean(dat_sim$value)
      
      output_lower <- quantile(dat_sim$value_lower,c(severe, extreme), na.rm = TRUE)
      annual_avg_lower <- mean(dat_sim$value_lower)
      
      output_upper <- quantile(dat_sim$value_upper,c(severe, extreme), na.rm = TRUE)
      annual_avg_upper <- mean(dat_sim$value_upper)
      # create data frame dat to store output with chart labels
      sub_plot_dat <- data_frame(`Average` = annual_avg,
                                 `Severe` = output[1],
                                 `Extreme` = output[2])
      
      
      # melt the data frame to get value and variable
      sub_plot_dat <- melt(sub_plot_dat)
      sub_plot_dat$value_lower <- c(annual_avg_lower, output_lower[1], output_lower[2])
      sub_plot_dat$value_upper <- c(annual_avg_upper, output_upper[1], output_upper[2])
      
      return(sub_plot_dat)
    }
  })
  
  output$annual_loss_gap_plotly <- renderPlot({
    
    plot_dat <- annual_loss_gap_data()
    sp <- input$select_peril
    if(is.null(plot_dat) | is.null(sp)){
      return(NULL)
    }
    
    budget <- input$budget
    is_archetype <- input$data_type == 'Archetype'
    # get country input for plot title
    if(is_archetype){
      plot_title <- paste0('Estimate of Annual Loss by severity for ', sp, '\n',input$archetype )
    } else {
      plot_title <- paste0('Estimate of Annual Loss by severity for ', sp, '\n',input$country )
    }
    
    if(input$ci){

      g <- ggplot(plot_dat, aes(x=variable,
                                y=value/scale_size,
                                text = value)) +
        geom_bar(stat = 'identity',
                 fill = '#5B84B1FF',
                 col = '#FC766AFF',
                 alpha = 0.6) +
        theme_bw(base_size = 14,
                 base_family = 'Ubuntu')  +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1)) +
        xlab('') + ylab('(in millions)') +
        geom_errorbar(aes(x = variable, ymin = value_lower/scale_size, ymax = value_upper/scale_size)) +

        ggtitle(plot_title)

      
    }else {
      # plot
      g <- ggplot(plot_dat, aes(x=variable,
                                y=value/scale_size,
                                text = value)) +
        geom_bar(stat = 'identity',
                 fill = '#5B84B1FF',
                 col = '#FC766AFF',
                 alpha = 0.6) +
        labs(y = '') +
        theme_bw(base_size = 14,
                 base_family = 'Ubuntu')  +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1)) +
        xlab('') + ylab('(in millions)') +
        
        ggtitle(plot_title)
    }
    
    return(g)
    
  })
  
  output$loss_exceedance_gap_plotly <- renderPlot({
    
    prob_exceed_suprplus_deficit <- probability_of_exceeding_surplus_deficit()
    budget <- input$budget
    data_type <- input$data_type
    sp <- input$select_peril
    
    if(is.null(prob_exceed_suprplus_deficit) | is.na(budget) | is.null(data_type) | is.null(gather_data())){
      NULL
    } else {
      
      dat <- gather_data()
      
      
      dat_sim <- gather_perils()
      
      dat_sim <- dat_sim %>% filter(!is.na(value))
      dat <- dat[order(dat$year, decreasing = FALSE),]
      largest_loss_num <- max(dat$value)
      largest_loss_year <- dat$year[dat$value == max(dat$value)]
      exceed_budget <- input$exceed_budget
      
      # get country input for plot title
      if(budget == 0){
        is_archetype <- input$data_type == 'Archetype'
        # get country input for plot title
        if(is_archetype){
          plot_title <- paste0('Estimate of Annual Funding gap by', sp, ' for ', '\n',input$archetype )
        } else {
          plot_title <- paste0('Estimate of Annual Funding gap by ', sp, ' for ','\n', input$country )
        }
        
      } else {
        is_archetype <- input$data_type == 'Archetype'
        # get country input for plot title
        if(is_archetype){
          plot_title <- paste0('Estimate of Annual Funding gap by', sp, ' for ', '\n',input$archetype )
        } else {
          plot_title <- paste0('Estimate of Annual Funding gap by ', sp, ' for ','\n', input$country )
        }
        
        exceed_surplus_deficit <- paste0('Probability of exceeding funding gap/surplus by \n', exceed_budget, ' is ', prob_exceed_suprplus_deficit)
        plot_title <- paste0(plot_title, ' : ', exceed_surplus_deficit)
        
      }
      
      # save(dat_sim, plot_title, largest_loss_num, exceed_budget, largest_loss_year, file = 'output_4.RData')
      
      # get best distirbution
      get_gap_curve <- function(sim_vector, budget = budget){
        funding_gap_curve <- as.data.frame(quantile(sim_vector,seq(0.5,0.98,by=0.002), na.rm = TRUE))
        funding_gap_curve$x <- rownames(funding_gap_curve)
        rownames(funding_gap_curve) <- NULL
        names(funding_gap_curve)[1] <- 'y'
        
        # remove percent and turn numeric
        funding_gap_curve$x <- gsub('%', '', funding_gap_curve$x)
        funding_gap_curve$x <- as.numeric(funding_gap_curve$x)/100
        
        # divide y by 100k, so get data in millions
        funding_gap_curve$x <- (1 - funding_gap_curve$x)
        # funding_gap_curve$y <- funding_gap_curve$y/scale_by
        
        names(funding_gap_curve)[2] <- 'Probability of exceeding loss'
        names(funding_gap_curve)[1] <- 'Funding gap'
        funding_gap_curve$`Funding gap` <- -funding_gap_curve$`Funding gap`
        funding_gap_curve$`Funding gap` <- funding_gap_curve$`Funding gap` + budget
        return(funding_gap_curve)
      }
     curve <- get_gap_curve(dat_sim$value, budget = budget)
     curve_lower <- get_gap_curve(dat_sim$value_lower, budget = budget)
     curve_upper <- get_gap_curve(dat_sim$value_upper, budget = budget)
     
     curve$value_lower <- curve_lower$`Funding gap`
     curve$value_upper <- curve_upper$`Funding gap`
     

      if(input$ci){
        
        g <-  ggplot(curve, aes(`Probability of exceeding loss`, `Funding gap`/scale_size)) +
          geom_line(col = 'blue', size = 1, alpha = 0.7) +
          geom_line(aes(`Probability of exceeding loss`, value_lower/scale_size), linetype = 'dotted') +
          geom_line(aes(`Probability of exceeding loss`, value_upper/scale_size), linetype = 'dotted') +
          scale_x_reverse(position = 'top') +
          ylab('') +
          geom_hline(yintercept = 0, size = 2) +
          ggtitle(plot_title) +
          theme_bw(base_size = 14,
                   base_family = 'Ubuntu')
        # g

      } else {
        g <-  ggplot(curve, aes(`Probability of exceeding loss`, `Funding gap`/scale_size)) +
          geom_line(col = 'blue', size = 1, alpha = 0.7) +
          scale_x_reverse(position = 'top') +
          ylab('') +
          geom_hline(yintercept = 0, size = 2) +
          ggtitle(plot_title) +
          theme_bw(base_size = 14,
                   base_family = 'Ubuntu')
        
      }
     return(g)
    }
    
  })
  #   # #
  #
  
  # UIs for panel headers
  output$tool_settings_ui <- renderUI({
    tab_maker(n = 1, label = 'TOOL SETTINGS',
              input = input,
              tab_data = tab_data)
  })
  
  output$data_ui <- renderUI({
    tab_maker(n = 2, label = 'DATA',
              input = input,
              tab_data = tab_data)
  })
  output$input_ui <- renderUI({
    tab_maker(n = 3, label = 'RISK PROFILE FITTING',
              input = input,
              tab_data = tab_data)
  })
  output$simulations_ui <- renderUI({
    tab_maker(n = 4, label = 'SIMULATIONS',
              input = input,
              tab_data = tab_data)
  })
  output$output_ui <- renderUI({
    tab_maker(n = 5, label = 'OUTPUT',
              input = input,
              tab_data = tab_data)
  })
  
  output$output_top_ui <- renderUI({
    is_archetype <- input$data_type == 'Archetype'
    
    the_country <- input$country
    the_perils <- input$select_peril
    the_damage_type <- input$damage_type
    
    the_country_ok <- FALSE
    the_perils_ok <- FALSE
    the_damage_type_ok <- FALSE
    
    
    if(!is.null(the_country)){
      the_country_ok <- TRUE
    }
    
    if(!is.null(the_perils)){
      if(length(the_perils) > 0){
        the_perils_ok <- TRUE
      }
    }
    
    cpp <- FALSE
    if(!is.null(the_damage_type)){
      the_damage_type_ok <- TRUE
      if(the_damage_type == 'Cost per person'){
        cpp <- TRUE
      }
    }
    
    oks <- c(the_country_ok, the_perils_ok, the_damage_type_ok)
    if(length(which(oks)) == 0){
      out <- fluidPage(
        fluidRow(
          h4(
            paste0('Risk & Disaster Analysis Output')
          )
        )
      )
    } else {
      if(the_country_ok){
        if(is_archetype){
          the_country <- 'Archetype'
        }
        vb1 <- valueBox(value = tags$p(paste0('Country: ', the_country, collapse = ''), style = "font-size: 40%;"),
                        subtitle = '',
                        width = 3)
      } else {
        vb1 <- valueBox(value = '', subtitle = '',
                        width = 3)
      }
      
      if(the_perils_ok){
        fs <- 40 / ((length(the_perils))^(1/4))
        vb2 <- valueBox(value = tags$p(paste0('Perils: ', paste0(the_perils, collapse = ', '), collapse = ''), style = paste0("font-size: ", fs, "%;")),
                        subtitle = '',
                        width = 3)
      } else {
        vb2 <- valueBox(value = '', subtitle = '',
                        width = 3)
      }
      
      if(the_damage_type_ok){
        if(cpp){
          the_val <- input$cost_per_person
          cpp_subtitle <- 'Cost per person:'
        } else {
          the_val <- 'damage'
          cpp_subtitle <- 'Total'
        }
        vb3 <- valueBox(value = tags$p(paste0(cpp_subtitle, ' ', the_val, collapse = ''), style = "font-size: 40%;"),
                        subtitle = '',
                        width = 3)
      } else {
        vb3 <- valueBox(value = '', subtitle = '',
                        width = 3)
      }
      
      
      out <- fluidPage(
        fluidRow(
          column(3,
                 h4(
                   paste0('Risk & Disaster Analysis Output for:')
                 )),
          vb1,
          vb2
        )
      )
    }
  })
  observeEvent(input$check_another,{
    updateTabsetPanel(session, inputId="tabs", selected='TOOL SETTINGS')
  })
  
  # output$delete <- renderUI({
  #   grd <- get_right_data()
  #   message('grd is')
  #   print(head(grd))
  #   h3('Test')
  #   save(grd, file = 'grd.RData')
  # })
}

shinyApp(ui, server)