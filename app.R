library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# Source the data set-up
source('functions.R')
source('global.R')
source('helpers.R')

# # Create a dictionary of tab names / numbers
tab_dict <- data_frame(number = 1:5,
                       name = toupper(c('Tool settings',
                                        'Data',
                                        'Input',
                                        'Simulations',
                                        'Output')))
n_tabs <- nrow(tab_dict)


header <- dashboardHeader(title = tags$a(href='https://www.worldbank.org/',
                                         tags$img(src='logo.png',height='60',width='200', alt = 'World Bank Group')))


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'side_tab',
    menuItem(
      text="Risk & Disaster",
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

body <- dashboardBody(
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
                tabsetPanel(
                  id = 'tabs',
                  tabPanel(
                    
                    title = uiOutput('tool_settings_ui'),
                    #icon("calendar"), 
                    value = 'TOOL SETTINGS',
                    
                    fluidPage(
                      h3('Please select your preferred settings'),
                      fluidRow(
                        radioButtons("advanced", "Select a type of setting. If you are an advanced user, please select the advanced settings option below for more statistical flexibility.",
                                     choices = c('Basic', 'Advanced'),
                                     selected = 'Basic',
                                     inline = TRUE)
                      ),
                      
                      fluidRow(
                        radioButtons("data_type", "Select Data Type",
                                     choices = c('Country', 'Archetype'), selected = 'Country', inline = TRUE)
                      ),
                      fluidRow(uiOutput('damage_type_ui')),
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
                      fluidRow(uiOutput('country_ui')),
                      fluidRow(uiOutput('data_source_ui')),
                      
                    )),
                  tabPanel(uiOutput('data_ui'),
                           value = 'DATA',
                           
                           fluidPage(
                             fluidRow(
                               h4('Review Data Samples')
                             ),
                             fluidRow(
                               radioButtons('upload_or_auto',
                                            'Do you wish to replace pre-loaded data with user-supplied data?',
                                            choiceValues = c('Pre-loaded data',
                                                             'User-supplied data'),
                                            choiceNames = c('No', 'Yes'),
                                            selected = 'Pre-loaded data',
                                            inline = TRUE)
                             ),
                             fluidRow(
                               column(9,
                                      h4('Peril data')),
                               column(3,
                                      downloadButton("download_peril_data",
                                                     "Download Peril Data"))
                             ),
                             fluidRow(
                               column(12,
                                      selectInput('view_data', 'View peril data', choices = c('Loss/cost', 'Frequency')),
                                      DT::dataTableOutput('raw_data_table'))),
                             br(),
                             fluidRow(
                               column(9,
                                      h4('Scaling data')),
                               column(3,
                                      downloadButton("download_scaled_data",
                                                     "Download Scaled Data"))),
                             fluidRow(
                               column(12,
                                      DT::dataTableOutput('raw_scaled_data')
                               )
                             )
                             
                           ),
                           
                           fluidRow(
                             # The bsPopover function takes the input name (advanced, referring to the veriable name of the advanced settings input)
                             # and creates popup boxes with any content specified in 'content' argument
                             bsPopover(id = "advanced", title = '', 
                                       content = "For more statistical options and view the 'Simulations' tab, select the 'Use Advanced Settings Button'", 
                                       placement = "middle", trigger = "hover", options = list(container ='body')),
                             bsPopover(id = "upload_data", title = '', 
                                       content = "To use your own dataset, push the 'Upload Peril Data' button", 
                                       placement = "middle", trigger = "hover", options = list(container ='body')),
                             
                             bsPopover(id = "download_data", title = '', 
                                       content = "Download the peril data for the country selected", 
                                       placement = "middle", trigger = "hover", options = list(container ='body')),
                             
                             bsPopover(id = "country", title = '', 
                                       content = "If you wish to upload your own data, please select the upload peril button", 
                                       placement = "middle", trigger = "hover", options = list(container ='body')),
                             
                             
                           ),
                           
                           # popups for the inputs damage_type, currency.Need to add one on the server side for prob_dis
                           bsPopover(id = "run_tool", title = '', 
                                     content ="After selecting all inputs, click this button to run the tool. It will generate simulation based on the best fitted distribution.", 
                                     placement = "right", trigger = "hover", options = list(container ='body')),
                           bsPopover(id = "damage_type", title = '', 
                                     content = "Select whether you would like to view the loss as cost per person or as total damage. If you choose cost per person, please enter the cost. In this mode you must use USD", 
                                     placement = "middle", trigger = "hover", options = list(container ='body')),
                           bsPopover(id = "currency", title = '', 
                                     content = "If other is chosen, please select a currency code and exchange rate.", 
                                     placement = "middle", trigger = "hover", options = list(container ='body'))
                           
                  ),
                  tabPanel(
                    
                    title = uiOutput('input_ui'),
                    value = 'INPUT',
                    
                    #  start new row that encompasses inputs for country, download buttons, damage type, and currency
                    fluidPage(
                      fluidRow(column(12,
                                      uiOutput('peril_ui'))),
                      
                      fluidRow(uiOutput('select_scale_ui')),
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
                               checkboxGroupInput('overlap_choices',
                                                  'Show:',
                                                  choices = c('Observed data',
                                                              'Simulated data'),
                                                  selected = c('Observed data',
                                                              'Simulated data'))),
                        column(6,
                               selectInput('peril_simulation',
                                           'Peril',
                                           choices = c('Flood',
                                                       'Drought',
                                                       'Earthquake',
                                                       'Storm')))
                      ),
                      fluidRow(
                        box(
                          title = 'Simulation chart',
                          width = 6,
                          status = 'primary',
                          plotOutput('simulation_plot')),
                        box(
                          title = 'Simulation table',
                          width = 6,
                          status = 'primary',
                          DT::dataTableOutput('simulation_table'))
                      ),
                      fluidRow(
                        column(6,
                               plotOutput('rag_ratings'))
                        
                      ),
                      bsPopover(id = "hist_plot", title = '', 
                                content = "This chart shows the historic distribution of 'Loss' from perils.", 
                                placement = "middle", trigger = "hover", options = list(container ='body')),
                      
                      bsPopover(id = "sim_plot", title = '', 
                                content = "This chart shows the simulated distribution of 'Loss' using the best fit distribution, or if in advanced settings, the distribution chosen on the previous page. 15k simulations, with 1k representing one full year", 
                                placement = "bottom", trigger = "hover", options = list(container ='body')),
                      
                      bsPopover(id = "aic_table", title = '', 
                                content = "This table shows the AIC scores for each parametric distribution. NAs are a result of the non convergence in the optimization algorithm. The table also shows the Maximum Likelihood Estimators for each distribution.", 
                                placement = "bottom", trigger = "hover", options = list(container ='body')),
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
                    
                    h4('Risk & Disaster Analysis Output'),
                    fluidPage(
                      DT::dataTableOutput('delete'),
                      br(),
                      fluidRow(
                        column(3,
                               div(class = 'well',
                                   numericInput('budget', 'Budget', value = 0),
                                   checkboxInput('ci', 
                                                 'Show confidence intervals',
                                                 value = FALSE))),
                        column(3,
                               div(class = 'well',
                                   numericInput('exceed_budget', 'Exceed funding gap/surplus by', value = 0)))
                      ),
                      # 
                      bsPopover(id = "annual_loss_plotly", title = 'Exhibit 1', 
                                content = "This graph shows the estimated annual loss across all selected perils. A return period of 1 in 5 years is the estimated annual loss expected to happend every five years (ie 20% probability). Similarly, a period of 1 in 10 years is the estimated annual loss expectedto happen every 10 years (ie 10% probability.", 
                                placement = "middle", trigger = "hover", options = list(container ='body')),
                      bsPopover(id = "loss_exceedance_plotly", title = 'Exhibit 2', content = "This graph shows the probability of a year taking place that exceeds the aggregate annual loss amount on the y-axis. The probability of exceeding the available budget is represented by the probability where the available budget line and the loss exceedance curve cross.",
                                placement = "left", trigger = "hover", options = list(container ='body')),
                      bsPopover(id = 'annual_loss_gap_plotly', title = 'Exhibit 3', content = "The funding gap is the difference between the available federal budget and the estimated annual loss at the return period. A loss value below the red budget line represents an estimated surplus (if above, it would be a deficit)",
                                placement = "middle", trigger = "hover", options = list(container ='body')),
                      bsPopover(id = 'loss_exceedance_gap_plotly', title = 'Exhibit 4', content = "The graph shows the probability of experiencing different sized funding gaps/surpluses. When the line is above the x-axis, it indicates a funding surplus - if below, it indicates a funding deficit.",
                                placement = "left", trigger = "hover", options = list(container ='body')),
                      
                      fluidRow(
                        
                        box(title = "Estimated Average Annual Loss by Time Period",
                            plotOutput('annual_loss_plotly')),
                        box(title = "Estimated Average Annual Loss by Severity",
                            plotOutput('annual_loss_gap_plotly'))),
                      fluidRow(
                        box(title = "Loss Exceedance Curve",
                            plotOutput('loss_exceedance_plotly')),
                        box(title = "Funding Gap",
                            plotOutput('loss_exceedance_gap_plotly'))))
                  )),
                br(),
                actionButton("prevBtn", "< Previous"),
                actionButton("nextBtn", "Continue",
                             style = "color: white; 
                     background-color: #009FDA; 
                     font-weight: bold;
                     position: relative; 
                     text-align:center;
                     border-radius: 6px;
                     border-width: 2px")
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
  
  # Reactive tab data
  tab_data <- reactiveValues(data = tab_dict)
  
  # Reactive dataset
  input_data <- reactive({
    user_supplied <- input$upload_or_auto
    if(user_supplied != 'User-supplied data'){
      NULL
    } else {
      message('Going to try to read user-supplied data!')
      inFile <- input$upload_csv
      if (is.null(inFile)){
        NULL 
      } else {
        out <- read.csv(inFile$datapath)
        if(is.null(out)){
          NULL
        } else {
          out
        }
      }
    }
  })
  
  
  # Download controls
  output$download_peril_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      the_data <- selected_damage_type() # ASK JOE about this
      write.csv(the_data, file)
    }
  )
  output$download_scaled_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      the_data <- raw_scaled_data()
      write.csv(the_data, file)
    }
  )
  
  
  
  ##########
  # Beginning observeEvent functions for Landing page and showing the 'simulations' tab if advanced user is selected
  # creates a pop up page that the user must accept to acces the app
  set.seed(122)
  histdata <- rnorm(1)
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
    
    # UNCOMMENT TO SHOW MODAL
    # # event will be called when histdata changes, which only happens once, when it is initially calculated
    # showModal(modalDialog(
    #   title = "", easyClose = FALSE, footer = NULL,
    #   
    #   fluidPage(
    #     fluidRow(
    #       column(12, align = 'center',
    #              h2('Welcome to World Bank Group!'))
    #     ),
    #     fluidRow(
    #       column(12, align = 'center',
    #              p('Please tell us your name, email and create a password so we can get started.'))
    #     ),
    #     fluidRow(
    #       column(6,
    #              textInput('first_name', '',
    #                        placeholder = 'First name')),
    #       column(6,
    #              textInput('last_name', '',
    #                        placeholder = 'Last name'))
    #     ),
    #     fluidRow(column(12, textInput('email', '', placeholder = 'Email'))),
    #     fluidRow(column(12, textInput('password', '', placeholder = 'Password'))),
    #     fluidRow(column(12, textInput('confirm_password', '', placeholder = 'Confirm password'))),
    #     fluidRow(
    #       column(12, align = 'center',
    #              actionButton('get_started', 'Get started'))
    #     )
    #   )
    # )
    # )
  })
  
  # Observe the "Get started" button on the log-in page and remove the modal
  observeEvent(input$get_started, {
    removeModal()
    
    # UNCOMMENT BEFORE DEPLOY    
    # showModal(modalDialog(
    #   title = "Disaster Risk Financing Tool 1", easyClose = FALSE, footer = NULL,
    #   welcome_modal
    # ))
  })
  observeEvent(input$accept,{
    removeModal()
  })
  
  
  # Upload menu
  observeEvent(input$upload_or_auto, {
    iu <- input$upload_or_auto
    message('iu is ', iu)
    if(iu == 'User-supplied data'){
      showModal(modalDialog(
        title = "Upload your own data", easyClose = TRUE,
        fluidPage(uiOutput('ui_upload_type'),
                  uiOutput('ui_upload_type_text'),
                  uiOutput('ui_upload_data'))
      ))
    } 
    
  })
  
  # Define a reactive value which is the currently selected tab number
  rv <- reactiveValues(page = 1)
  
  # Make tab dict reactive
  
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
    # # Do the time calculations
    # tt <- tab_time()
    # ok <- not_too_fast(old_time = tt,
    #                    new_time = Sys.time())
    # if(ok){
    # Update rv$page
    navPage(-1)
    # tab_time(Sys.time())
    # }
    
  })
  observeEvent(input$nextBtn, {
    # Do the time calculations
    # tt <- tab_time()
    # ok <- not_too_fast(old_time = tt,
    #                    new_time = Sys.time())
    # if(ok){
    # Update rv$page
    navPage(1)
    # tab_time(Sys.time())
    # }
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
  
  
  output$country_ui <- renderUI({
    country_picked <- input$data_type == 'Country'
    if(country_picked){
      message('Joe: country, not archetype, selected.')
      the_input <- selectInput("country", 
                                 "Choose a country",
                                 choices = countries,
                                 selected = countries[1])
    } else {
      message('Joe: archetype, not country, selected.')
      the_input <- selectInput('archetype', 'Choose an archetype',
                               choices = archetypes)
    }
    the_input
    
  })

  output$damage_type_ui <- renderUI({
    
    if(input$data_type == 'Archetype'){
      radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                   'Select how you want to view the loss',
                   choices = c('Cost per person'),
                   selected = 'Cost per person',
                   inline = TRUE)
    } else {
      radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                   'Select how you want to view the loss',
                   choices = c('Total damage', 'Cost per person'),
                   selected = 'Total damage',
                   inline = TRUE)
    }
    
  })
  
  
  # define a counting object to be used to determine when the 'simulations' tab should be shown
  counter <- reactiveVal(value = 0)
  
  # Define the upload type options if upload is user-supplied
  output$ui_upload_type <- renderUI({
    if(input$upload_or_auto == 'User-supplied data'){
      radioButtons('upload_type',
                   'Type of data',
                   choices = c('Loss', 'Cost', 'Population'),
                   inline = TRUE,
                   selected = 'Loss')
    } else {
      NULL
    }
  })
  
  output$ui_upload_type_text <- renderUI({
    if(input$upload_or_auto == 'User-supplied data'){
      typey <- input$upload_type
      if(is.null(typey)){
        return(NULL)
      } else {
        if(typey == 'Loss'){
          helpText('Upload a csv with the following 4 column headers: "Country", "Year", "Peril", "Loss')
        } else if(typey == 'Cost'){
          helpText('Upload a csv with the following 4 column headers: "Country", "Year", "Peril", "Affected')
        } else if(typey == 'Population'){
          helpText('Upload a csv with the following 3 column headers: "Country", "Year", "Population"')
        }
      }
    } else {
      NULL
    }
  })
  
  output$ui_upload_data <- renderUI({
    if(input$upload_or_auto == 'User-supplied data'){
      fileInput(inputId = 'upload_csv', label = 'Choose a CSV file',accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")
      )
    } else {
      NULL
    }
    
  })
  
  observeEvent(input$advanced,{
    
    cc <- counter()
    message('current counter is ', cc)
    new_cc <- cc + 1
    counter(new_cc)
    message('new counter is ', new_cc)
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
  
  ################
  # User input tab 
  ################
  
  # get a reactive object that selects country data (list) based on imput
  selected_country <- reactive({
    if(is.null(input$country)){
      NULL
    } else {
      # store input country
      country_name <- input$country
      
      # get a list called country data
      country_data <- country_data[country_data$country ==country_name,]
      
      # If user-supplied, overwrite some data
      user_supplied <- input$upload_or_auto
      if(user_supplied == 'User-supplied data'){
        the_data <- input_data()
        if(!is.null(the_data)){
          country_data <- list()
          
          # Define which index to overwrite
          typey <- input$upload_type
          if(typey == 'Loss'){
            the_index <- 1
          } else if(typey == 'Cost'){
            the_index <- 2
          } else if(typey == 'Population'){
            the_index <- 3
          }
          message('the_index is ')
          print(the_index)
          
          # Overwrite the auto data with user-supplied data
          country_data[[the_index]] <- the_data
          
          message('selected_country() is ')
          print(head(country_data[[1]]))
          print(head(country_data[[2]]))
          print(head(country_data[[3]]))
        }
      }
      
      return(country_data)
      
      
      
    }
    
    
  })
  
  # create reactive object for country_data frequency
  country_frequency <- reactive({
    if(is.null(selected_country())){
      NULL
    } else {
      country_name <- input$country
      best_data <- input$data_source
      freq_data <- frequency_data[frequency_data$country == country_name,]
      return(freq_data)
    }
  })
  
  # create a reactive object for archetypes 
  selected_archetype <- reactive({
    message('input$archetype is ', input$archetype)
    if(is.null(input$archetype)){
      NULL
    } else {
      archetype <- input$archetype
      archetype_data <- archetype_cost_data[archetype_cost_data == archetype,]
      return(archetype_data)
    }
    
  })
  
  # create reactive object for country_data frequency
  archetype_frequency <- reactive({
    if(is.null(selected_archetype())){
      NULL
    } else {
      archetype <- input$archetype
      freq_data <- archetype_freq_data[archetype_freq_data$archetype == archetype,]
      return(freq_data)
    }
  })
  
  output$data_source_ui <- renderUI({
    if(is.null(input$data_type) | is.null(selected_country())){
      NULL
    } else {
      if(input$data_type == 'Archetype'){
        return(NULL)
      } else {
        
        country_data <- selected_country()
        if(length(unique(country_data$origin)) > 1){
          best_source <- unique(country_data$origin[which(country_data$best_data == TRUE)])
          selectInput('data_source', 
                      'Choose data source (default is best)', 
                      choices = c('EMDAT', 'DesInventar'),
                      selected = best_source)
        } else {
          best_source <- unique(country_data$origin[which(country_data$best_data == TRUE)])
          selectInput('data_source', 
                      'Choose data source (default is best)', 
                      choices = best_source,
                      selected = best_source)
        }
        
      }
    }
  })
  
  
  prepare_loss_data <- reactive({
    if(is.null(input$data_source) | is.null(country_frequency())){
      NULL
    } else {
      best_source <- input$data_source
      country_data <- selected_country()
      country_frequency <- country_frequency()
      
      # subset data by best source
      country_data <- country_data[country_data$origin == best_source & country_data$damage_type == 'damage',]
      country_frequency <- country_frequency[country_frequency$origin == best_source & country_frequency$damage_type == 'damage',]
      
      # remove emdat and 
      country_data$origin <- country_data$damage_type <- country_data$best_data <- NULL
      country_frequency$origin <- country_frequency$damage_type <-  NULL
      
      # store in list
      data <- list()
      data[[1]] <- country_data
      data[[2]] <- country_frequency
      
      
      return(data)
    }
    
  })
  
  
  prepare_scale_data <- reactive({
    if(is.null(input$country)){
      NULL
    } else {
      country_name <- input$country
      scale_data <- scale_data[scale_data$Country == country_name,]
      names(scale_data) <- c('country', 'year', 'population', 'inflation', 'gdp')
      return(scale_data)
    }
  })
  
  
  # create renderui for currency
  output$currency_ui <- renderUI({
    if(is.null(input$damage_type)){
      NULL
    } else {
      if(input$damage_type == 'Total damage'){
        NULL
      } else {
        
        radioButtons('currency',
                     'Choose a currency',
                     choices = currencies,
                     selected = 'USD',
                     inline = TRUE)
      }
    }
  })
  
  # create a uioutput for when data type == cost per person
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
  
  # create uioutput code for cases where the user choses a currency other than USD
  output$code_ui <- renderUI({
    if(is.null(input$damage_type) | is.null(input$currency)){
      NULL
    } else {
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
  
  # create uioutput rate for cases where the user choses a currency other than USD
  output$rate_ui <- renderUI({
    if(is.null(input$damage_type) | is.null(input$currency) | is.null(input$damage_type == 'Total damage')) {
      NULL
    } else {
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
  # 
  # country_name <- 'Sri Lanka'
  # country_data <- country_data[country_data$country == country_name,]
  # country_frequency <- freq_data
  # country_frequency <- country_frequency[country_frequency$country == country_name,]
  
  prepare_cost_data <- reactive({
    best_source <- input$data_source
    rate <- input$rate
    code <- input$code
    cost <- input$cost_per_person
    country_data <- selected_country()
    cf <- country_frequency()
    ok <- TRUE
    if(is.null(input$data_source) | is.null(cf) | is.null(input$damage_type) |
       is.null(best_source) | is.null(rate) | is.null(code) | is.null(cost) | is.null(country_data)){
      ok <- FALSE
    }
    
    if(!is.null(input$damage_type)){
      if( input$damage_type == 'Total damage'){
        ok <- FALSE
      }
    }
    if(ok){
      
      country_frequency <- country_frequency()
      message('Here is what our stuff looks like:')
      message('---best_source is ', best_source)
      message('---rate is ', rate)
      message('---code is ', code)
      message('---cost is ', cost)
      # subset data by best source
      country_data <- country_data[country_data$origin == best_source & country_data$damage_type == 'affected',]
      country_frequency <- country_frequency[country_frequency$origin == best_source & country_frequency$damage_type == 'affected',]
      
      # remove emdat and 
      country_data$origin <- country_data$damage_type <- country_data$best_data <- NULL
      country_frequency$origin <- country_frequency$damage_type <-  NULL
      
      country_data$value <- country_data$value*cost
      print(head(country_data))
      # store in list
      data <- list()
      data[[1]] <- country_data
      data[[2]] <- country_frequency
      return(data)
    } else {
      NULL
    }
  })
  
  # archetype_name <- 'High risk middle income country, exposed to storms, floods, and earthquakes'
  # archetype_data <- archetype_cost_data[archetype_cost_data$archetype == archetype_name,]
  # archetype_frequency <- archetype_freq_data
  # archetype_frequency <- archetype_frequency[archetype_frequency$archetype == archetype_name,]
  
  prepare_archetype_data <- reactive({
    if(is.null(input$data_source) | is.null(archetype_frequency()) | is.null(input$damage_type)){
      NULL
    } else {
      best_source <- input$data_source
      # rate <- input$rate
      # code <- input$code
      # cost <- input$cost_per_person
      archetype_data <- selected_archetype()
      archetype_frequency <- archetype_frequency()
      cost = 50
      
      # remove emdat and
      archetype_frequency$data_type <- archetype_data$data_type <-  NULL
      
      archetype_data$outcome <- archetype_data$outcome*cost
      
      message(head(archetype_data), 'this is good')
      
      # store in list
      data <- list()
      data[[1]] <- archetype_data
      data[[2]] <- archetype_frequency
      
      
      return(data)
    }
    
  })
  
  
  
  # render ui for scale, only if advance
  output$select_scale_ui <- renderUI({
    if(input$data_type == 'Archetype' | is.null(prepare_scale_data())){
      NULL
    } else {
      if(input$advanced == 'Basic'){
        selectInput('select_scale', 
                    'Scaled data by population',
                    choices = 'Population',
                    selected = 'Population')
      } else {
        data <- prepare_scale_data()
        scaled_choices <- names(data)[names(data) %in% scaled_data]
        scaled_choices <- Hmisc::capitalize(c(scaled_choices, ' No scaling'))
        selectInput('select_scale', 
                    'Choose from preloaded scaling data',
                    choices = scaled_choices,
                    selected = scaled_choices[1])
      }
      
    }
  })
  
  
  # create a uioutput for peril type  - this is dependent on the country selected.
  output$peril_type_ui <- renderUI({
    if(is.null(selected_country())){
      NULL
    } else {
      # get country data
      data  <- selected_country()
      temp <- data
      # get the peril names for choices in peril input
      peril_names <- as.character(unique(temp$Peril))
      checkboxGroupInput('peril_type', 
                         'Choose a peril',
                         choices = peril_names,
                         selected = peril_names,
                         inline = TRUE)
    }
    
  })
  
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
      message('here')
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
    # save(data, file = 'data.RData')
    out <- NULL
    if(!is.null(dat)){
      out <- dat
    }
    out
  })
  output$raw_data_table <- DT::renderDataTable({
    cored <- core_data()
    view_data <- input$view_data
    if(view_data != 'Frequency'){
      data <- cored[[1]]
    } else {
      data <- cored[[2]]
    }
    if(nrow(data) <=3){
      message('Not enough obs')
      return(NULL)
    } else {
      data
    }
  })
  
  # reactive object to scale data
  scale_data_reactive <- reactive({

    cored <- core_data()
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
    # message('cored looks like this')
    # print(cored)
    if(is.list(cored)){
      cored <- cored[[1]]
    } else {
      return(NULL)
    }
  
  if(is_advanced){
    if(ss == 'Population'){
      # store frequenc
      if(is.null(cored) | is.null(sd)){
        out <- NULL
      } else {
        combined_data <- inner_join(cored, sd)
        out <- combined_data %>%  group_by(peril) %>% arrange(-year) %>% mutate(scaled_factor = population[1]/population) %>%
          mutate(value = value * scaled_factor)
      }
    } else if(ss == 'GDP'){
      out <- cored
    } else if(ss == 'Inflation'){
      out <- cored
    } else {
      out <- cored
    }
  } else { # not advanced user
    message('cored is-----------------')
    print(head(cored))
    message('ss is-------------')
    print(head(sd))
    if(is.null(cored) | is.null(sd)){
      out <- NULL
    } else {
      combined_data <- inner_join(cored, sd)
      out <- combined_data %>%  group_by(peril) %>% arrange(-year) %>% mutate(scaled_factor = population[1]/population) %>%
        mutate(value = value * scaled_factor)
    }
    out
  }
})
  
  

  
 
  
  # data <- data_list
  
  
  # reactive object to test for a trend
  # peril_names <- all_perils
  # peril_names$p_value[peril_names$peril == 'Storm'] <- 0.03
  
  significant_trends <- reactiveVal(FALSE)
  
  output$trend_test_ui <- renderUI({
    
    sdr <- scale_data_reactive()
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
        # split data by peril type
        test_storm <-try(MannKendall(data$Outcome[data$peril == 'Storm'])$sl, silent = TRUE)
        test_drought <-try(MannKendall(data$Outcome[data$peril == 'Drought'])$sl, silent = TRUE)
        test_earthquake <-try(MannKendall(data$Outcome[data$peril == 'Earthquake'])$sl, silent = TRUE)
        test_flood <-try(MannKendall(data$Outcome[data$peril == 'Flood'])$sl, silent = TRUE)

        # concatanate
        all_perils <- data_frame(peril = c('Storm', 'Drought', 'Earthquake', 'Flood'),
                                 p_value = c(test_storm,test_drought, test_earthquake, test_flood))
        all_perils$p_value <- as.numeric(all_perils$p_value)
      
      message('sdr is ')
      print(head(sdr))
      
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
  
  
  
  
  # reactive object to correct for trend
  # trend_perils <- all_perils
  # trend_perils$p_value[trend_perils$peril == 'Storm'] <- 0.03
  
  correct_trend <- reactive({
    if(is.null(input$trend_test)){
      return(NULL)
    } else {
      
      if(input$trend_test  == 'No'){
        return(NULL)
      }
        trend_perils <- execute_trend_test()
        peril_type <- trend_perils$peril[trend_perils$p_value <= 0.05 & !is.na(trend_perils$p_value)]
        
        # split data into 3 groups - loss trend perils, loss other perils, and other data
        trend_data <- data[[1]]
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
        final_data[[2]] <- data[[1]]
        return(data)
      }
      
    
    
  })
  
  
  
  
  ################
  # Data tab
  ################
  # the problem here is that there are some reactives we are not nullifying out and need to spread data for frequency 
  # create a data table  
  
  
  # output for scaling data if available - the 3rd, 4th, and 5th index in the data list are scaling data. Population is 3rd
  # gdp 4th, inflation 5th
  output$raw_scaled_data <- renderDataTable({
    psd <- prepare_scale_data()
    if(is.null(psd)){
      NULL
    } else {
      if(input$data_type == 'Archetype'){
        # data <- data_frame('Scaling data unavailable for data selected')
        # names(data) <- NULL
        # out <- data
        out <- NULL
        # datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      } else {
        out <- prepare_scale_data()
        # datatable(data, options = list(dom='t',ordering=F))
      }
      out
    }
    
  })
  
  # reactive object to create right data 
  get_right_data <- reactive({
    core_dat <- core_data()
    scale_dat <- scale_data_reactive()
    trend_dat <- correct_trend()
    is_trend <- input$trend_test
    out <- NULL
    # if(is.null(core_dat) & is.null(scale_dat) & is.null(trend_dat)){
    #   NULL
    # }
    if(input$data_type == 'Archetype'){
      out <- core_dat
      out <- out[[1]]
      names(out) <- c('archetype', 'year', 'peril', 'value')
      message('archetype selected, so core_dat!!!')
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
    
    save(out, file = 'out.RData')
    return(out)
  })
  
  
  
  fitted_distribution <- reactive({
    rd <- get_right_data()
    temp <- fit_distribution(rd)
    message('fit_distribution outcome')
    head(temp)
  })
  
  filtered_distribution <- reactive({
    fd <- fitted_distribution()
    save(fd, file = 'fitted_distribution.RData')
    message('the fitted_distribution() reactive is:')
    print(head(fd))
    save(fd, file = 'fdx.RData')
    filter_distribution(fd)
  })
  
  
  
  output$peril_ui <- renderUI({
    is_advanced <- input$advanced == 'Advanced'
    fd <- filtered_distribution()
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
        flood_choices <- earthquake_choices <- drought_choices <- storm_choices <- advanced_parametric
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
                       inline = TRUE))
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
                       inline = TRUE)
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
                       inline = TRUE)
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
                       inline = TRUE)
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
    message('fd here is :')
    print(head(fd))
    x <- prepare_simulations(fd, dist_flood = input$dist_flood_input, 
                        dist_drought = input$dist_drought_input,
                        dist_storm = input$dist_storm_input,
                        dist_earthquake = input$dist_earthquake_input)
    message('x is')
    print(head(x))
    x
  })
  
  ran_simulations <- reactive({
    ps <- prepared_simulations()
    message('PS IS')
    print(head(ps))
    x <- run_simulations(ps)
    save(x, file = 'x.RData')
  })
  
  
  output$delete <- DT::renderDataTable({
    corrected_data <- ran_simulations()
    save(corrected_data, file = 'corrected_data.RData')
    corrected_data
  })
  
  output$simulation_plot <- renderPlot({
    rs <- ran_simulations()
    rd <- get_right_data()
    plot_simulations(rs = rs,
                     right_data = rd,
                     peril = input$peril_simulation,
                     overlap = input$overlap_choices)
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
      names(fd) <- c('Distribution',
                     'AIC',
                     'MLE 1',
                     'MLE 2')
      return(fd)
    } else{
      return(NULL)
    }
  })
  # 
  #   data <- readRDS('~/Desktop/data.rda')
  #   
  #   fit_dis <- reactive({
  #     if(is.null(correct_trend())){
  #       NULL
  #     } else {
  #       data <- correct_trend()
  #       out <- fit_distribution(data)
  #       return(out)
  #     }
  #     
  #   })
  #   
  #   # make a reactive object that grabs the name of the best distribution - this is the default for basic users. Advanced users can choose a new one
  #   get_best_dis <- reactive({
  #     if(is.null(fit_dis())){
  #       return(NULL)
  #     } else {
  #       # get aic_mle_data
  #       dat <- fit_dis()
  #       peril_names <- unique(dat$peril)
  #       data_list <- list()
  #       for(i in 1:length(peril_names)){
  #         peril_name <- peril_names[i]
  #         sub_dat <- dat[dat$peril == peril_name,]
  #         aic_min_ind <- which(sub_dat$AIC == min(sub_dat$AIC, na.rm = T))
  #         sub_dat <- sub_dat[aic_min_ind,]
  #         sub_dat$best_dis <- sub_dat$Distribution
  #         data_list[[i]] <- sub_dat
  #         
  #       }
  #       
  #       out <- do.call('rbind', data_list)
  #       return(out)
  #   
  #     }
  #   })
  #   
  #   
  #   # ui for prob_dis - right now the output is dependent on the best distribution
  #   # if advanced is selected the distribution has multiple choices, otherwise it defaults to best
  #   output$prob_dis_drought_ui <- renderUI({
  #     gg <- get_best_dis()
  #     if(is.null(gg)){
  #       return(NULL)
  #     } else {
  #       # get aic_mle_data
  #       dat <- get_best_dis()
  #       
  #       if(input$advanced == 'Advanced'){
  #         selectInput('prob_dis_drought', 'Choose distribution (default is best fit)',
  #                     choices = advanced_parametric,
  #                     selected = best_dis)
  #       } else {
  #         selectInput('prob_dis_drought', 'Default is best fit',
  #                     choices = best_dis,
  #                     selected = best_dis)
  #       }
  #     }
  #     
  #   })
  #   
  #   utput$prob_dis_earthquake_ui <- renderUI({
  #     gg <- get_best_dis()
  #     if(is.null(gg)){
  #       return(NULL)
  #     } else {
  #       # get aic_mle_data
  #       dat <- get_best_dis()
  #       
  #       if(input$advanced == 'Advanced'){
  #         selectInput('prob_dis_earthquake', 'Choose distribution (default is best fit)',
  #                     choices = advanced_parametric,
  #                     selected = best_dis)
  #       } else {
  #         selectInput('prob_dis_earthquake', 'Default is best fit',
  #                     choices = best_dis,
  #                     selected = best_dis)
  #       }
  #     }
  #     
  #   })
  #   
  #   utput$prob_dis_flood_ui <- renderUI({
  #     gg <- get_best_dis()
  #     if(is.null(gg)){
  #       return(NULL)
  #     } else {
  #       # get aic_mle_data
  #       dat <- get_best_dis()
  #       
  #       if(input$advanced == 'Advanced'){
  #         selectInput('prob_dis_flood', 'Choose distribution (default is best fit)',
  #                     choices = advanced_parametric,
  #                     selected = best_dis)
  #       } else {
  #         selectInput('prob_dis_flood', 'Default is best fit',
  #                     choices = best_dis,
  #                     selected = best_dis)
  #       }
  #     }
  #     
  #   })
  #   
  #   utput$prob_dis_storm_ui <- renderUI({
  #     gg <- get_best_dis()
  #     if(is.null(gg)){
  #       return(NULL)
  #     } else {
  #       # get aic_mle_data
  #       dat <- get_best_dis()
  #       
  #       if(input$advanced == 'Advanced'){
  #         selectInput('prob_dis_storm', 'Choose distribution (default is best fit)',
  #                     choices = advanced_parametric,
  #                     selected = best_dis)
  #       } else {
  #         selectInput('prob_dis_storm', 'Default is best fit',
  #                     choices = best_dis,
  #                     selected = best_dis)
  #       }
  #     }
  #     
  #   })
  #   
  
  
  #   
  # Log normal ci
  # log_normal_statistic <- function(x, inds) {try(fitdistr(x[inds],"lognormal")$estimate, silent = TRUE)}
  # bs_log_normal <- boot(data$Loss, log_normal_statistic, R = 1000)
  # log_normal_ci <- boot.ci(bs_log_normal, conf=0.95, type="bca")
  #   ###############
  #   # Simulations tab
  #   ###############
  # 
  #   get_aic_mle <- reactive({
  #     if(is.null(correct_trend())){
  #       NULL
  #     } else {
  #       data <- correct_trend()
  #       message(head(data), 'get_aic_mle data here')
  #     }
  # 
  #   })
  # 
  #   get_aic_mle <- reactive({
  #     if(is.null(selected_damage_type())){
  #       return(NULL)
  #     } else {
  # 
  #       # data <- country_data
  #       data <- selected_damage_type()
  #       # remove obsevations with 0, if any
  #       data <- data[data$Loss > 0,]
  # 
  #       ##########
  #       # fit lognormal
  #       #########
  #       log_normal <- try(fitdistr(data$Loss, "lognormal"),silent = TRUE)
  #       if(class(log_normal) == 'try-error'){
  #         log_normal <- NULL
  #         log_normal_aic <- NA
  #         log_normal$estimate[1] <- NA
  #         log_normal$estimate[2] <- NA
  #         # log_norma$upper_mle_1 <- NA
  #         # log_normal$lower_mle_1 <- NA
  #         # log_norma$upper_mle_2 <- NA
  #         # log_normal$lower_mle_2 <- NA
  #         #
  #       } else {
  #         # get aic
  #         log_normal_aic <- round(AIC(log_normal), 4)
  #         #
  #         # # upper and lower bound for each estimate
  #         # log_normal_upper_mle_1 <- log_normal$estimate[1] + log_normal$sd[1]
  #         # log_normal_lower_mle_1 <- log_normal$estimate[1] - log_normal$sd[1]
  #         #
  #         # log_normal_upper_mle_2 <- log_normal$estimate[2] + log_normal$sd[2]
  #         # log_normal_lower_mle_2 <- log_normal$estimate[2] - log_normal$sd[2]
  # 
  #         # if there is an error, fill object with NA
  #         message('log normal AIC is ', log_normal_aic)
  # 
  #         # get MLE
  #         log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
  #         message('log normal mle is ', log_normal_mle)
  #       }
  #       # create data frame to store aic and MLEs
  #       log_normal_data <- data_frame(name = 'log_normal',
  #                                     aic = log_normal_aic,
  #                                     mle_1 = log_normal$estimate[1],
  #                                     mle_2 = log_normal$estimate[2])
  #       # upper_mle_1 = log_normal_upper_mle_1,
  #       # lower_mle_1 = log_normal_lower_mle_1,
  #       # upper_mle_2 = log_normal_upper_mle_2,
  #       # lower_mle_2 = log_normal_lower_mle_2)
  # 
  #       beta <- try(eBeta_ab(data$Loss, method = "numerical.MLE"), silent = TRUE)
  #       if(class(beta) == 'try-error'){
  #         beta <- NULL
  #         beta_aic <- NA
  #         beta$shape1 <- NA
  #         beta$shape2 <- NA
  #         beta_mle <- c(beta$shape1, beta$shape2)
  #       } else {
  #         beta_ll <- lBeta_ab(X = data$Loss, params = beta, logL = TRUE)
  #         beta_aic <- -(2*beta_ll + 2)
  #         beta_mle <- c(beta$shape1, beta$shape2)
  # 
  #         # beta_aic <- round(beta$aic, 4)
  #         message('beta AIC is ', beta_aic)
  #         message('beta mle is ', beta_mle)
  #       }
  #       beta_data <- data_frame(name = 'beta',
  #                               aic = round(beta_aic, 4),
  #                               mle_1 = beta_mle[1],
  #                               mle_2 = beta_mle[2])
  # 
  # 
  # 
  #       # EQUATION FOR AIC
  #       # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.
  # 
  #       # fit gamma
  #       # gamma <- fitdistr(data$Loss, 'gamma')
  #       gamma <- try(fitdistrplus::fitdist(data$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle"), silent = TRUE)
  # 
  #       if(class(gamma) == 'try-error'){
  #         gamma <- NULL
  #         gamma_aic <- NA
  #         gamma$estimate[1] <- NA
  #         gamma$estimate[2] <- NA
  # 
  #       } else {
  #         # get aic
  #         gamma_aic <- round(gamma$aic, 4)
  #         message('gamma AIC is ', gamma_aic)
  # 
  #         # upper and lower bound for each estimate
  #         # gamma_upper_mle_1 <- gamma$estimate[1] + gamma$sd[1]
  #         # gamm_lower_mle_1 <- gamma$estimate[1] - gamma$sd[1]
  #         #
  #         # gamma_upper_mle_2 <- gamma$estimate[2] + gamma$sd[2]
  #         # gamma_lower_mle_2 <- gamma$estimate[2] - gamma$sd[2]
  #         #
  #         # get mle
  #         gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
  #         message('gamme mle is ', gamma_mle)
  #       }
  #       gamma_data <- data_frame(name = 'gamma',
  #                                aic = gamma_aic,
  #                                mle_1 = gamma$estimate[1],
  #                                mle_2 = gamma$estimate[2])
  #       # upper_mle_1 = log_normal_upper_mle_1,
  #       # lower_mle_1 = log_normal_lower_mle_1,
  #       # upper_mle_2 = log_normal_upper_mle_2,
  #       # lower_mle_2 = log_normal_lower_mle_2)
  # 
  # 
  # 
  #       # fit frechet
  #       # dfrechet(data$Loss, lambda = 1, mu = 1, sigma = 1, log = TRUE)
  #       frechet <- try(fitdistrplus::fitdist(data$Loss, "frechet", start=list(scale=0.1, shape=0.1), method="mle"),
  #                      silent = TRUE)
  #       if(class(frechet) == 'try-error'){
  #         frechet <- NULL
  #         frechet_aic <- NA
  #         frechet$estimate[1] <- NA
  #         frechet$estimate[2] <- NA
  # 
  #       } else {
  #         frechet_aic <- round(frechet$aic, 4)
  #         message('frechet AIC is ', frechet_aic)
  #         # get mle
  #         frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
  #         message('frechet mle is ', frechet_mle)
  #       }
  #       frechet_data <- data_frame(name = 'frechet',
  #                                  aic = frechet_aic,
  #                                  mle_1 = frechet$estimate[1],
  #                                  mle_2 = frechet$estimate[2])
  # 
  # 
  # 
  #       # git gumbel
  #       gumbel_fit <- try(fit_gumbel(data$Loss), silent = TRUE)
  #       if(class(gumbel_fit) == 'try-error'){
  #         gumbel_fit <- NULL
  #         gumbel_aic <- NA
  #         gumbel_fit$estimate[1] <- NA
  #         gumbel_fit$estimate[2] <- NA
  # 
  #       } else {
  #         gumbel_aic <- round(gumbel_fit$aic, 4)
  #         message('gumbel AIC is ', gumbel_aic)
  #         # get mle
  #         gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
  #         message('gumbel mle is ', gumbel_mle)
  #       }
  #       gumbel_data <- data_frame(name = 'gumbel',
  #                                 aic = gumbel_aic,
  #                                 mle_1 = gumbel_fit$estimate[1],
  #                                 mle_2 = gumbel_fit$estimate[2])
  # 
  # 
  # 
  #       # fit weibull
  #       weibull <- try(fitdistrplus::fitdist(data$Loss, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
  #       if(class(weibull) == 'try-error'){
  #         weibull <- NULL
  #         weibull_aic <- NA
  #         weibull$estimate[1] <- NA
  #         weibull$estimate[2] <- NA
  # 
  #       } else {
  #         weibull_aic <- round(weibull$aic, 4)
  #         message('weibull AIC is ', weibull_aic)
  # 
  #         # get mle
  #         weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
  #         message('weibull mle is ', weibull_mle)
  #       }
  #       weibull_data <- data_frame(name = 'weibull',
  #                                  aic = weibull_aic,
  #                                  mle_1 = weibull$estimate[1],
  #                                  mle_2 = weibull$estimate[2])
  # 
  # 
  # 
  #       # fit pareto
  #       pareto <-ParetoPosStable::pareto.fit(data$Loss, estim.method = 'MLE')
  #       if(class(pareto) == 'try-error'){
  #         pareto <- NULL
  #         pareto_aic <- NA
  #         pareto_fit$estimate[1] <- NA
  #         pareto_fit$estimate[2] <- NA
  # 
  #       } else {
  #         pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
  #         message('pareto AIC is ', pareto_aic)
  #         # get mle
  #         pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
  #         message('pareto mle is ', pareto_mle)
  #       }
  #       pareto_data <- data_frame(name = 'pareto',
  #                                 aic = pareto_aic,
  #                                 mle_1 = pareto$estimate[[1]],
  #                                 mle_2 = pareto$estimate[[2]])
  # 
  # 
  # 
  # 
  #       # create a data frame out of data results
  #       aic_mle_data <- rbind(log_normal_data,
  #                             gamma_data,
  #                             beta_data,
  #                             frechet_data,
  #                             gumbel_data,
  #                             weibull_data,
  #                             pareto_data)
  # 
  #       # change names of variable
  #       names(aic_mle_data) <- c('Distribution', 'AIC', 'MLE 1', 'MLE 2')
  # 
  #       # capitalize and remove underscore of Distribution
  #       aic_mle_data$Distribution <- Hmisc::capitalize(aic_mle_data$Distribution)
  #       aic_mle_data$Distribution <- gsub('_', ' ', aic_mle_data$Distribution)
  #       aic_mle_data$AIC <- round(aic_mle_data$AIC, 2)
  #       aic_mle_data$`MLE 1`<- round(aic_mle_data$`MLE 1`, 2)
  #       aic_mle_data$`MLE 2` <- round(aic_mle_data$`MLE 2`, 2)
  # 
  #       return(aic_mle_data)
  #     }
  # 
  # 
  # 
  #   })
  
  # 
  # # create table for aic
  # output$aic_table <- renderDataTable({
  #   aic_mle_data <- get_aic_mle()
  #   DT::datatable(aic_mle_data, options = list(dom = 't'))
  # }) 
  # 
  # # 
  # # create a reactive object that takes the aic_mle_data runs simulations on the 
  # # best distribution (found from min aic)
  # run_best_simulation <- reactive({
  #   set.seed(11)
  #   
  #   if(is.null(input$prob_dis)){
  #     return(NULL)
  #   } else {
  #     best_dis <- get_best_dis()
  #     # get aic_mle_data
  #     dat <- get_aic_mle()
  #     
  #     if(best_dis == input$prob_dis){
  #       # for now remove beta
  #       # dat <- dat[dat$Distribution != 'Beta',]
  #       # get index for minimum aic
  #       aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
  #       # # subset my index
  #       dat <- dat[aic_min_ind,]  
  #     } else {
  #       # dat <- dat[dat$Distribution != 'Beta',]
  #       dat <- dat[dat$Distribution ==  input$prob_dis,]
  #     }
  #     
  #     # set conditions for each distribution
  #     if(dat$Distribution == 'Log normal'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       }  else {
  #         sim <- rlnorm(n = 15000, meanlog = dat$`MLE 1`, sdlog = dat$`MLE 2`)
  #       }
  #     } else if (dat$Distribution == 'Gamma'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       }  else {
  #         # check to see how much seed matters
  #         sim <- rgamma(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
  #       }
  #     } else if (dat$Distribution == 'Beta'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       } else {
  #         sim <- rbeta(n = 15000, shape1 = dat$`MLE 1`, scale2 = dat$`MLE 2`)
  #       }
  #     }  else if (dat$Distribution == 'Frechet'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       }  else {
  #         sim <- rfrechet(n = 15000, loc=0, scale=dat$`MLE 1`, shape=dat$`MLE 2`)
  #       }
  #     } else if (dat$Distribution == 'Gumbel'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA 
  #       } else {
  #         sim <- actuar::rgumbel(n = 15000, alpha = dat$`MLE 1`, scale = dat$`MLE 2`)
  #       }
  #     } else if (dat$Distribution == 'Weibull'){
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       }  else {
  #         sim <- rweibull(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
  #       }
  #     } else {
  #       if(any(is.na(dat$AIC))){
  #         sim <- NA
  #       }  else {
  #         sim <- extraDistr::rpareto(n = 15000, a = dat$`MLE 1`, b = dat$`MLE 2`)
  #       }
  #     }
  #     return(sim)
  #   }
  #   
  # })
  # 
  # # create a ouput plot that draws a density of the distribution over the 
  # # histogram of raw data
  # output$hist_plot <- renderPlot({
  #   data <- selected_damage_type()
  #   g <- ggplot(data, aes(data$Loss)) +
  #     geom_histogram(bins = 5, fill = 'black', color = 'black', alpha = 0.6) + 
  #     labs(x = 'Loss', 
  #          y = 'Counts') +
  #     theme_clean() +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1, size = 12),
  #           axis.text.y = element_text(size = 12),
  #           axis.title = element_text(size = 12)) 
  #   return(g)
  #   
  # })
  # output$sim_plot <- renderPlot({
  #   if(is.null(input$prob_dis)){
  #     return(NULL)
  #   } else {
  #     dat_sim <- run_best_simulation()
  #     dat_sim <- as.data.frame(dat_sim)
  #     names(dat_sim) <- 'Simulated loss'
  #     g =  ggplot(dat_sim, aes(`Simulated loss`)) +
  #       geom_density(fill = 'black', color = 'black', alpha = 0.5) +
  #       labs(y = 'Density') +
  #       theme_clean() +
  #       theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1, size = 12),
  #             axis.text.y = element_text(size = 12),
  #             axis.title = element_text(size = 12)) 
  #     return(g)
  #   }
  #   
  # })
  # 
  # ################
  # # Output tab
  # ################
  # 
  # # make reactive object to store probability of exceeding budget
  # probability_of_exceeding <- reactive({
  # 
  #   if(is.null(selected_damage_type())){
  #     NULL
  #   } else {
  #     dat_sim <- run_best_simulation()
  #     if(any(is.na(dat_sim))){
  #       return(NULL)
  #     } else {
  # 
  #       # get budget
  #       budget <- input$budget
  #       peril_exceedance_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002), na.rm = TRUE))
  #       peril_exceedance_curve$x <- rownames(peril_exceedance_curve)
  #       rownames(peril_exceedance_curve) <- NULL
  #       names(peril_exceedance_curve)[1] <- 'y'
  # 
  #       # remove percent and turn numeric
  #       peril_exceedance_curve$x <- gsub('%', '', peril_exceedance_curve$x)
  #       peril_exceedance_curve$x <- as.numeric(peril_exceedance_curve$x)
  #       peril_exceedance_curve$x <- peril_exceedance_curve$x/100
  #       names(peril_exceedance_curve)[1] <- 'Total Loss'
  #       names(peril_exceedance_curve)[2] <- 'Probability'
  #       peril_exceedance_curve$Probability <- 1 - peril_exceedance_curve$Probability
  # 
  #       # find where budget equals curve
  #       prob_exceed <- peril_exceedance_curve$Probability[which.min(abs(peril_exceedance_curve$`Total Loss` - budget))]
  #       return(prob_exceed)
  #     }
  #   }
  # 
  # 
  # })
  # 
  # # create a reactive object that takes new input 
  # probability_of_exceeding_suplus_deficit <- reactive({
  #   
  #   if(is.null(selected_damage_type())){
  #     NULL
  #   } else {
  #     dat_sim <- run_best_simulation()
  #     if(any(is.na(dat_sim))){
  #       return(NULL)
  #     } else {
  #       
  #       # get budget
  #       budget <- input$budget
  #       exceed_budget <- input$exceed_budget
  #       budget <- exceed_budget + budget
  #       peril_exceedance_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002), na.rm = TRUE))
  #       peril_exceedance_curve$x <- rownames(peril_exceedance_curve)
  #       rownames(peril_exceedance_curve) <- NULL
  #       names(peril_exceedance_curve)[1] <- 'y'
  #       
  #       # remove percent and turn numeric
  #       peril_exceedance_curve$x <- gsub('%', '', peril_exceedance_curve$x)
  #       peril_exceedance_curve$x <- as.numeric(peril_exceedance_curve$x)
  #       peril_exceedance_curve$x <- peril_exceedance_curve$x/100
  #       names(peril_exceedance_curve)[1] <- 'Total Loss'
  #       names(peril_exceedance_curve)[2] <- 'Probability'
  #       peril_exceedance_curve$Probability <- 1 - peril_exceedance_curve$Probability
  #       
  #       # find where budget equals curve
  #       prob_exceed_surplus_deficit <- peril_exceedance_curve$Probability[which.min(abs(peril_exceedance_curve$`Total Loss` - budget))]
  #       return(prob_exceed_surplus_deficit)
  #     }
  #   }
  #   
  #   
  # })
  # 
  # 
  # 
  # # OUTPUT 1
  
  dat <- out
  dat_sim = x
  output$annual_loss_plotly <- renderPlot({

    dat <- get_right_data()
    dat_sim <- ran_simulations()
    # remove obsevations with 0, if any
    dat <- dat[dat$value > 0,]
    dat <- dat[order(dat$year, decreasing = FALSE),]
        # get budget
        budget <- input$budget

        # remove obsevations with 0, if any
        dat <- dat[dat$value > 0,]
        dat <- dat[order(dat$year, decreasing = FALSE),]
        is_archetype <- input$data_type == 'Archetype'
        # get country input for plot title
        if(is_archetype){
          plot_title <- 'Archetype'
        } else {
          plot_title <- input$country
        }
        
        data_list <- list()
        for(i in 1:length(unique(dat_sim$key))){
          peril_name <- unique(dat_sim$key)[i]
          sub_peril <- dat_sim %>% filter(dat_sim$key ==peril_name)
          sub_dat <- dat %>% filter(peril == peril_name)
          output <- quantile(dat_sim$value,c(0.8,0.9, 0.96,0.98,0.99))

          # create sub_data frame sub_dat to store output with chart labels
          sub_dat <- data_frame(`Annual average` = annual_avg,
                            `1 in 5 Years` = output[1],
                            `1 in 10 Years` = output[2],
                            `1 in 25 Years` = output[3],
                            `1 in 50 Years` = output[4],
                            `1 in 100 Years` = output[5],
                            `Highest historical annual loss` = max(sub_dat$value),
                            `Most recent annual loss` = sub_dat$value[nrow(sub_dat)])
          
          # melt the sub_data frame to get value and variable
          sub_dat <- melt(sub_dat)
          
          sub_dat$variable <- factor(sub_dat$variable, levels = c('1 in 5 Years', '1 in 10 Years', '1 in 25 Years', '1 in 50 Years', '1 in 100 Years',
                                                          'Annual average', 'Highest historical annual loss', 'Most recent annual loss'))
          sub_dat$value <- round(sub_dat$value, 2)
          
          # melt the data frame to get value and variable
          sub_plot_dat <- melt(sub_plot_dat)
          data_list[[i]] <- sub_plot_dat
        }
        
        plot_dat <- do.call('rbind', data_list)
        
      

        if(input$ci){
          y_min <- dat$value - mean(dat$value)
          y_min <- ifelse(y_min < 0, 0, y_min)

          y_max <-  dat$value + mean(dat$value)
          # Plot
          g <- ggplot(dat, aes(x=variable,
                               y=value,
                               text = value)) +
            geom_bar(stat = 'identity',
                     fill = '#5B84B1FF',
                     col = '#FC766AFF',
                     alpha = 0.6) +
            geom_errorbar(aes(x=variable, ymin=y_min, ymax=y_max), color="black", width=0.5) +
            geom_hline(yintercept = budget) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')  +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1)) +
            xlab('') + ylab('') +
            ggtitle(plot_title)
        } else {
          # Plot
          g <- ggplot(dat, aes(x=variable,
                               y=value,
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
            xlab('') + ylab('') +
            ggtitle(plot_title)
        }


        return(g)
      

  })
  # 
  # 
  # ############ OUTPUT 2
  # 
  # 
  output$loss_exceedance_plotly <- renderPlot({
    if(is.null(selected_damage_type()) | is.na(input$budget)){
      NULL
    } else {
      dat_sim <- run_best_simulation()
      if(any(is.na(dat_sim))){
        return(NULL)
      } else {

        data <- selected_damage_type()

        country_name <- unique(data$Country)

        data <- data[order(data$Year, decreasing = FALSE),]
        largest_loss_num <- max(data$Loss)
        largest_loss_year <- data$Year[data$Loss == max(data$Loss)]

        # find where budget equals curve
        prob_exceed <- probability_of_exceeding()
        # get country input for plot title
        plot_title <- input$country
        exceed_budget <- paste0('Probability of exceeding budget = ', prob_exceed)
        plot_title <- paste0(plot_title, ' : ', exceed_budget)

        # get budget
        budget <- input$budget
        peril_exceedance_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002), na.rm = TRUE))
        peril_exceedance_curve$x <- rownames(peril_exceedance_curve)
        rownames(peril_exceedance_curve) <- NULL
        names(peril_exceedance_curve)[1] <- 'y'

        # remove percent and turn numeric
        peril_exceedance_curve$x <- gsub('%', '', peril_exceedance_curve$x)
        peril_exceedance_curve$x <- as.numeric(peril_exceedance_curve$x)
        peril_exceedance_curve$x <- peril_exceedance_curve$x/100
        names(peril_exceedance_curve)[1] <- 'Total Loss'
        names(peril_exceedance_curve)[2] <- 'Probability'
        peril_exceedance_curve$Probability <- 1 - peril_exceedance_curve$Probability

        if(input$ci){
          dat <- peril_exceedance_curve
          dat$y_min <- dat$`Total Loss`- mean(dat$`Total Loss`)
          dat$y_min <- ifelse(dat$y_min < 0, 0, dat$y_min)

          dat$y_max <-  dat$`Total Loss` + mean(dat$`Total Loss`)
          g <- ggplot(dat, aes(Probability, `Total Loss`)) +
            geom_line(col = 'blue', size = 1, alpha = 0.7) +
            geom_line(aes(Probability, y_min), linetype = 'dotted') +
            geom_line(aes(Probability, y_max), linetype = 'dotted') +
            scale_x_reverse() +
            ggtitle(plot_title) +
            geom_hline(yintercept = largest_loss_num) +
            geom_hline(yintercept = budget) +
            geom_vline(xintercept = prob_exceed, linetype = 'dotted') +
            annotate('text', label = 'Budget', x = 0.45, y = budget, vjust = -1) +
            annotate('text', label = 'Largest loss', x = 0.45, y = largest_loss_num, vjust = -1) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')

        } else {
          g <- ggplot(peril_exceedance_curve, aes(Probability, `Total Loss`)) +
            geom_line(col = 'blue', size = 1, alpha = 0.7) +
            scale_x_reverse() +
            ggtitle(plot_title) +
            geom_hline(yintercept = largest_loss_num) +
            geom_hline(yintercept = budget) +
            geom_vline(xintercept = prob_exceed, linetype = 'dotted') +
            annotate('text', label = 'Budget', x = 0.45, y = budget, vjust = -1) +
            annotate('text', label = 'Largest loss', x = 0.45, y = largest_loss_num, vjust = -1) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')

        }


        return(g)

      }
    }


  })
  # 
  # 
  # ############ OUTPUT 3
 
  output$annual_loss_gap_plotly <- renderPlot({
    dat <- out
    dat_sim = x
        dat <- get_right_data()
        dat_sim <- ran_simulations()
        # remove obsevations with 0, if any
        dat <- dat[dat$value > 0,]
        dat <- dat[order(dat$year, decreasing = FALSE),]
        is_archetype <- input$data_type == 'Archetype'
        # get country input for plot title
        if(is_archetype){
          plot_title <- 'Archetype'
        } else {
          plot_title <- input$country
        }

        data_list <- list()
        for(i in 1:length(unique(dat_sim$key))){
          peril_name <- unique(dat_sim$key)[i]
          sub_peril <- dat_sim %>% filter(dat_sim$key ==peril_name)
          sub_dat <- dat %>% filter(peril == peril_name)
          output <- quantile(dat_sim$value,c(0.8,0.9, 0.96,0.98,0.99))
          annual_avg <- mean(sub_dat$value)
          # create data frame dat to store output with chart labels
          sub_plot_dat <- data_frame(`Average` = annual_avg,
                                 `Severe` = output[2],
                                 `Extreme` = output[5])
          
          # melt the data frame to get value and variable
          sub_plot_dat <- melt(sub_plot_dat)
          data_list[[i]] <- sub_plot_dat
        }
        
        plot_dat <- do.call('rbind', data_list)
        
        

        if(input$ci){
          y_min <- plot_dat$value - mean(plot_dat$value)
          y_min <- ifelse(y_min < 0, 0, y_min)

          y_max <-  plot_dat$value + mean(plot_dat$value)
          # plot
          g <- ggplot(plot_dat, aes(x=variable,
                               y=value,
                               text = value)) +
            geom_bar(stat = 'identity',
                     fill = '#5B84B1FF',
                     col = '#FC766AFF',
                     alpha = 0.6) +
            geom_errorbar(aes(x=variable, ymin=y_min, ymax=y_max), color="black", width=0.5) +

            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')  +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1)) +
            xlab('') + ylab('') +

            ggtitle(plot_title)

        }else {
          # plot
          g <- ggplot(plot_dat, aes(x=variable,
                               y=value,
                               text = value)) +
            geom_bar(stat = 'identity',
                     fill = '#5B84B1FF',
                     col = '#FC766AFF',
                     alpha = 0.6) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')  +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1)) +
            xlab('') + ylab('') +

            ggtitle(plot_title)

        }

        return(g)

  })
  # 
  # # OUTPUT 4
  dat <- out
  dat_sim <- x
  output$loss_exceedance_gap_plotly <- renderPlot({

        # prob_exceed_suprplus_deficit <- probability_of_exceeding_suplus_deficit()
        # data <- selected_damage_type()
    dat <- out
    dat_sim = x
    dat <- get_right_data()
    dat_sim <- ran_simulations()
        dat <- dat[order(dat$year, decreasing = FALSE),]
        largest_loss_num <- max(dat$value)
        largest_loss_year <- dat$Year[dat$loss == max(dat$loss)]
        budget <- input$budget
        exceed_budget <- input$exceed_budget

        # # get country input for plot title
        # if(input$budget == 0){
        #   plot_title <- input$country
        # 
        # } else {
        #   plot_title <- input$country
        #   exceed_surplus_deficit <- paste0('Probability of exceeding funding gap/surplus by \n', exceed_budget, ' is ', prob_exceed_suprplus_deficit)
        #   plot_title <- paste0(plot_title, ' : ', exceed_surplus_deficit)
        # 
        # }

        is_archetype <- input$data_type == 'Archetype'
        # get country input for plot title
        if(is_archetype){
          plot_title <- 'Archetype'
        } else {
          plot_title <- input$country
        }
        
        data_list <- list()
        for(i in 1:length(unique(dat_sim$key))){
          peril_name <- unique(dat_sim$key)[i]
          sub_peril <- dat_sim %>% filter(dat_sim$key ==peril_name)
          sub_dat <- dat %>% filter(peril == peril_name)
          # get best distirbution
          funding_gap_curve <- as.data.frame(quantile(dat_sim$value,seq(0.5,0.98,by=0.002)))
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
          data_list[[i]] <- funding_gap_curve
        }
        
        plot_dat <- do.call('rbind', data_list)
        
        
        if(input$ci){
          dat <- plot_dat
          dat$y_min <- dat$`Funding gap`- mean(dat$`Funding gap`)
          dat$y_min <- dat$y_min
          # dat$y_min <- ifelse(y_min > 0, 0, y_min)
          dat$y_max <-  dat$`Funding gap` + mean(dat$`Funding gap`)
          g <-  ggplot(dat, aes(`Probability of exceeding loss`, `Funding gap`)) +
            geom_line(col = 'blue', size = 1, alpha = 0.7) +
            geom_line(aes(`Probability of exceeding loss`, y_min), linetype = 'dotted') +
            geom_line(aes(`Probability of exceeding loss`, y_max), linetype = 'dotted') +
            scale_x_reverse(position = 'top') +
            geom_hline(yintercept = 0, size = 2) +
            ggtitle(plot_title) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')
          g
        } else {
          g <-  ggplot(funding_gap_curve, aes(`Probability of exceeding loss`, `Funding gap`)) +
            geom_line(col = 'blue', size = 1, alpha = 0.7) +
            scale_x_reverse(position = 'top') +
            geom_hline(yintercept = 0, size = 2) +
            ggtitle(plot_title) +
            theme_bw(base_size = 14,
                     base_family = 'Ubuntu')
        }


        g

      
  })
  # 
  # DONT DO ANYTHING WITH BERNOULLI UNTILL YOU GET MORE INFO
  # # The basic user will not see this, only the advanced user
  # frequency_distribution_bernoulli <- reactive({
  #   # will have other options for different scales later
  #   freq_data <- scale_by_pop()
  #   # temporarily do simulation 
  #   freq_data <- freq_data[complete.cases(freq_data),]
  #   # sum of success (disaster) over sum if trials (years). 6 success in 8 years
  #   # get trials max year minus min year
  #   num_trials <- as.numeric(as.character(max(freq_data$Year))) - min(as.numeric(as.character(freq_data$Year)))
  #   num_trials <- num_trials + 1
  #   mle_bern <- sum(nrow(freq_data)/num_trials)
  #   uniform_dis <- runif(1000, 0, 1)
  #   sim_freq_data <- as.data.frame(cbind(simulation_num = 1:1000, uniform_dis = uniform_dis))
  #   # create a variable to show success (mle?uniform_dis, then success)
  #   sim_freq_data$outcome <- ifelse(sim_freq_data$uniform_dis < mle_bern, 'success', 'fail')
  #   
  # })
  # 
  # DONT USE YET
  
  # 
  # get_poisson({
  #   # poisson <- fitdistr(data$Loss, "Poisson")
  #   # poisson_aic <- AIC(poisson)
  #   
  # })
  
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
    tab_maker(n = 3, label = 'INPUT',
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
  # output$delete <- DT::renderDataTable({
  #   sdr <- scale_data_reactive()
  #   sdr
  # })
}

shinyApp(ui, server)