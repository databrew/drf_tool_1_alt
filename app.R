library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# Source the data set-up
source('functions.R')
source('global.R')

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
                                placement = "top", trigger = "hover", options = list(container ='body')),
                      
                      h3('Please select your preferred settings'),
                      fluidRow(
                        radioButtons("advanced", "Select a type of setting. If you are an advanced user, please select the advanced settings option below for more statistical flexibility.",
                                     choices = c('Basic', 'Advanced'),
                                     selected = 'Basic',
                                     inline = TRUE),
                        bsPopover(id = "advanced", title = '',
                                  content = 'For more statistical options and to view the Simulations tab, "Advanced"', trigger = "hover", options = list(container ='body')),
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
                      fluidRow(uiOutput('data_source_ui')),
                      
                    )),
                  tabPanel(uiOutput('data_ui'),
                           value = 'DATA',
                           
                           fluidPage(
                             fluidRow(uiOutput('select_scale_ui')),
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
                                            inline = TRUE),
                               bsPopover(id = "upload_or_auto", title = '',
                                         content = 'As an alternative to using the data built in to the app, you can upload your own data.',
                                         placement = "top", trigger = "hover", options = list(container ='body'))
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
                                        selectInput('view_data', 'View peril data', choices = c('Loss/cost', 'Frequency')),
                                        bsPopover(id = "view_data", title = '',
                                                  content = 'One can choose to view the data in terms of loss/cost or the frequency of the loss-related events.',
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
                    value = 'INPUT',
                    
                    #  start new row that encompasses inputs for country, download buttons, damage type, and currency
                    fluidPage(
                      fluidRow(column(12,
                                      uiOutput('select_peril_ui'))),
                      fluidRow(
                        # column(12,
                                      uiOutput('peril_ui')
                                      # )
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
                    # uiOutput('delete'),
                    
                    
                    uiOutput('output_top_ui'),
                    fluidPage(
                      # DT::dataTableOutput('delete'),
                      br(),
                      fluidRow(
                        column(3,
                               div(class = 'well',
                                   numericInput('budget', 'Budget', value = 0),
                                   bsPopover(id = "budget", title = '',
                                             content = 'The budget will be used for calculating the likelihood of exceeding funding, etc',
                                             placement = "top", 
                                             trigger = "hover", 
                                             options = list(container ='body')),
                                   checkboxInput('ci',
                                                 'Show confidence intervals',
                                                 value = FALSE))),
                        column(3,
                               div(class = 'well',
                                   numericInput('exceed_budget', 'Exceed funding gap/surplus by', value = 0),
                                   bsPopover(id = "exceed_budget", title = '',
                                             content = 'This is the amount by which the funding gap can be exceeded',
                                             placement = "top", 
                                             trigger = "hover", 
                                             options = list(container ='body')))),
                        column(3,
                               div(class = 'well',
                                   numericInput('severe', 'Define the probability for a severe event' , value = 10),
                                   bsPopover(id = "severe", title = '',
                                             content = 'This should be the estimated percentage likelihood of a severe event taking place',
                                             placement = "top", 
                                             trigger = "hover", 
                                             options = list(container ='body')))),
                        column(3,
                               div(class = 'well',
                                   numericInput('extreme', 'Define the probability for an extreme event' , value = 1),
                                   bsPopover(id = "extreme", title = '',
                                             content = 'This should be the percentage likelihood of an extreme event taking place',
                                             placement = "top", 
                                             trigger = "hover", 
                                             options = list(container ='body'))))
                        
                      ),
                      fluidRow(
                        box(title = "Estimated Average Annual Loss by Time Period",
                            checkboxInput(inputId = 'is_table_1',
                                          label = 'Show table instead of chart',
                                          value = FALSE),
                            uiOutput('output1')),
                        box(title = "Estimated Average Annual Loss by Severity",
                            checkboxInput(inputId = 'is_table_3',
                                          label = 'Show table instead of chart',
                                          value = FALSE),
                            uiOutput('output3'))),
                      fluidRow(
                        box(title = "Loss Exceedance Curve",
                            fluidRow(plotOutput('loss_exceedance_plotly'),
                                     bsPopover(id = "loss_exceedance_plotly", title = 'Exhibit 2', content = "This graph shows the probability of a year taking place that exceeds the aggregate annual loss amount on the y-axis. The probability of exceeding the available budget is represented by the probability where the available budget line and the loss exceedance curve cross.",
                                               placement = "top", trigger = "hover", options = list(container ='body')))),
                        box(title = "Funding Gap",
                            fluidRow(
                              plotOutput('loss_exceedance_gap_plotly'),
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
                br(),
                
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
  
  # observeEvent(input$tabs,{
  #   # shinyjs::enable(selector = '.nav-tabs a')
  #   # Get the tab info
  #   tab_number <- rv$page
  #   td <- tab_data$data
  #   tab_name <- td %>% filter(number == tab_number) %>% .$name
  #   message('tab_number: ', tab_number)
  #   message('tab_name: ', tab_name)
  #   message('td is ')
  #   print(td)
  #   keep <- tab_number + c(-1, 1)
  #   keep <- keep[keep > 0 & keep < 5] # this needs to be adjusted
  #   keep_td <- td[keep,]
  #   for(i in 1:nrow(keep_td)){
  #     disable_id <- keep_td$name[i]
  #     message('Trying to disable: ', disable_id)
  #     shinyjs::disable(id = disable_id)
  #   }
  #   # shinyjs::toggleState(id = )
  #   # shinyjs::disable(selector = '.nav-tabs a')
  # })
  
  
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
      write.csv(the_data, file)
    }
  )
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
      write.csv(the_data, file)
      
    }
  )
  
  
  
  ##########
  # Beginning observeEvent functions for Landing page and showing the 'simulations' tab if advanced user is selected
  # creates a pop up page that the user must accept to acces the app
  set.seed(122)
  histdata <- rnorm(1)
  # #   ## Uncomment before deploy
  #
  # observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
  #  
  #  
  #   # event will be called when histdata changes, which only happens once, when it is initially calculated
  #   showModal(modalDialog(
  #     title = "", easyClose = FALSE, footer = NULL,
  #
  #     fluidPage(
  #       fluidRow(
  #         column(6, align = 'center',
  #                img(src='logo1.png', align = "left", height = '50px')),
  #         column(6, align = 'center',
  #                img(src='logo2.jpg', align = "right", height = '50px'))
  #       ),
  #       fluidRow(
  #         column(12, align = 'center',
  #                h2('Welcome to World Bank Group!'))
  #       ),
  #       fluidRow(
  #         column(12, align = 'center',
  #                p('Please tell us your name, email and create a password so we can get started.'))
  #       ),
  #       fluidRow(
  #         column(6,
  #                textInput('first_name', '',
  #                          placeholder = 'First name')),
  #         column(6,
  #                textInput('last_name', '',
  #                          placeholder = 'Last name'))
  #       ),
  #       fluidRow(column(12, textInput('email', '', placeholder = 'Email'))),
  #       fluidRow(column(12, textInput('password', '', placeholder = 'Password'))),
  #       fluidRow(column(12, textInput('confirm_password', '', placeholder = 'Confirm password'))),
  #       fluidRow(
  #         column(12, align = 'center',
  #                actionButton('get_started', 'Get started'))
  #       )
  #     )
  #   )
  #   )
  # })
  
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
  
  
  output$country_ui <- renderUI({
    country_picked <- input$data_type == 'Country'
    if(country_picked){
      the_input <- selectInput("country",
                               "Choose a country",
                               choices = countries,
                               selected = countries[1])
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
  
  # observeEvent(input$tabs,{
  #   disable('simulations_ui')
  # })
  
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
          
          # Overwrite the auto data with user-supplied data
          country_data[[the_index]] <- the_data
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
      best_data <- best_data_source()
      freq_data <- frequency_data[frequency_data$country == country_name,]
      return(freq_data)
    }
  })
  
  # create a reactive object for archetypes
  selected_archetype <- reactive({
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
  
  output$data_source_ui <- renderUI({
    sn <- best_data_source()
    if(is.null(sn)){
      NULL
    } else {
      h4(paste0('The best data source for the chosen parameters is: ',
                sn))
    }
  })
  
  
  
  
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
      names(scale_data) <- c('country', 'year', 'population','population_factor','inflation',
                             'inflation_factor', 'gdp', 'gdp_factor', 'inflation_ok', 'gdp_ok')
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
  
  # create uioutput rate for cases where the user choses a currency other than USD
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
  #
  # country_name <- 'Sri Lanka'
  # country_data <- country_data[country_data$country == country_name,]
  # country_frequency <- freq_data
  # country_frequency <- country_frequency[country_frequency$country == country_name,]
  
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
      
      country_frequency <- country_frequency()
      # message('Here is what our stuff looks like:')
      # message('---best_source is ', best_source)
      # message('---rate is ', rate)
      # message('---code is ', code)
      # message('---cost is ', cost)
      # subset data by best source
      country_data <- country_data[country_data$origin == best_source & country_data$damage_type == 'affected',]
      country_frequency <- country_frequency[country_frequency$origin == best_source & country_frequency$damage_type == 'affected',]
      
      # remove emdat and
      country_data$origin <- country_data$damage_type <- country_data$best_data <- NULL
      country_frequency$origin <- country_frequency$damage_type <-  NULL
      
      country_data$value <- country_data$value*cost
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
      
      # message('Here is what our stuff looks like:')
      # message('---best_source is ', best_source)
      # message('---rate is ', rate)
      # message('---code is ', code)
      # message('---cost is ', cost)
      # subset data by best source
      
      
      # remove emdat and
      archetype_data$data_type <- archetype_data$damage_type <- archetype_data$best_data <- NULL
      archetype_frequency$data_type <- archetype_frequency$damage_type <-  NULL
      
      archetype_data$value <- archetype_data$value*cost
      # save(archetype_data, file = 'archetype_data.RData')
      # save(archetype_frequency, file = 'archetype_frequency.RData')
      # store in list
      data <- list()
      data[[1]] <- archetype_data
      data[[2]] <- archetype_frequency
      return(data)
    } else {
      NULL
    }
  })
  
  
  
  # render ui for scale, only if advance
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
                  content = 'Choose how you would like to see your data "scaled".',
                  placement = "top", trigger = "hover", options = list(container ='body'))
      ) 
    }
  })
  
  
  # # create a uioutput for peril type  - this is dependent on the country selected.
  # output$peril_type_ui <- renderUI({
  #   if(is.null(selected_country())){
  #     NULL
  #   } else {
  #     # get country data
  #     data  <- selected_country()
  #     temp <- data
  #     # get the peril names for choices in peril input
  #     peril_names <- as.character(unique(temp$Peril))
  #     checkboxGroupInput('peril_type',
  #                        'Choose a peril',
  #                        choices = peril_names,
  #                        selected = peril_names,
  #                        inline = TRUE)
  #   }
  #  
  # })
  
  
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
  output$raw_data_table <- DT::renderDataTable({
    
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
                editable = editit)
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
        out$population_factor <- out$gdp_factor <- out$gdp_ok <- out$inflation_factor <- out$inflation_ok <- NULL
        names(out) <- c('Country', 'Year', 'Population', 'Inflation',
                        'GDP')
        # datatable(data, options = list(dom='t',ordering=F))
      }
      datatable(out, rownames = FALSE)
    }
    
  })
  
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
    rd <- rd[[2]]
    temp<- sim_bern(rd)
  })
  
  fitted_distribution <- reactive({
    rd <- get_right_data()
    rd <- rd[[1]]
    fit_distribution(rd)
    
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
      names(fd) <- c('Distribution',
                     'AIC',
                     'MLE 1',
                     'MLE 2')
      return(datatable(fd, rownames = FALSE))
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
        summarise(value = sum(outcome, na.rm = TRUE)) %>%
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
  probability_of_exceeding_suplus_deficit <- reactive({
    
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
    if(is.na(budget) | is.null(gp) | is.null(gd)){
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
    if(!is.null(x)){
      names(x) <- Hmisc::capitalize(names(x))
      DT::datatable(x)
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
      names(x) <- Hmisc::capitalize(names(x))
      DT::datatable(x)
    }
  })
  
  output$annual_loss_plotly <- renderPlot({
    budget <- input$budget
    plot_dat <- annual_loss_data()
    
    
    if(is.null(plot_dat)){
      return(NULL)
    }
    
    is_archetype <- input$data_type == 'Archetype'
    # get country input for plot title
    if(is_archetype){
      plot_title <- 'Archetype'
    } else {
      plot_title <- input$country
    }
    
    # Plot
    g <- ggplot(plot_dat, aes(x=variable,
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
    
    if(input$ci){
      message('For now, no CIs for this chart')
      # gd <- get_right_data()
      # message('Confidence selected: going to calculate bootstrapped cis')
      # message('gather_data() looks like: ')
      # print(head(gd))
      # # run the bootstrap
      # ci_data <- bootstrap_cis(grd = gd)
    # g <- g +
    #   geom_errorbar(data = ci_data,
    #                 aes(x = variable,
    #                     ymin = lwr,
    #                     ymax = upr))
    }
    return(g)
  })
  
  output$loss_exceedance_plotly <- renderPlot({
    
    prob_exceed <- probability_of_exceeding()
    budget <- input$budget
    
    if(is.null(prob_exceed) | is.na(budget) | is.null(gather_data())){
      NULL
    } else {
      
      dat <- gather_data()
      
      
      dat_sim <- gather_perils()
      country_name <- unique(dat$country)
      
      dat <- dat[order(dat$year, decreasing = FALSE),]
      largest_loss_num <- round(max(dat$value), 2)
      largest_loss_year <- dat$year[dat$value == max(dat$value)]
      
      # find where budget equals curve
      
      # get country input for plot title
      plot_title <- input$country
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
      
      
      
      plot_dat <- output
      # get budget
      if(input$ci){
        plot_dat$y_min <- plot_dat$`Total Loss`- mean(plot_dat$`Total Loss`)
        plot_dat$y_min <- ifelse(plot_dat$y_min < 0, 0, plot_dat$y_min)
        
        plot_dat$y_max <-  plot_dat$`Total Loss` + mean(plot_dat$`Total Loss`)
        g <- ggplot(plot_dat, aes(Probability, `Total Loss`)) +
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
        g <- ggplot(plot_dat, aes(Probability, `Total Loss`)) +
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
    
    
    
    
  })
  #   # # #
  #   # # #
  # ############ OUTPUT 3
  #
  
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
      annual_avg <- mean(dat$value)
      # create data frame dat to store output with chart labels
      sub_plot_dat <- data_frame(`Average` = annual_avg,
                                 `Severe` = output[1],
                                 `Extreme` = output[2])
      
      
      # melt the data frame to get value and variable
      sub_plot_dat <- melt(sub_plot_dat)
      return(sub_plot_dat)
    }
  })
  
  output$annual_loss_gap_plotly <- renderPlot({
    
    plot_dat <- annual_loss_gap_data()
    if(is.null(plot_dat)){
      return(NULL)
    }
    
    budget <- input$budget
    is_archetype <- input$data_type == 'Archetype'
    # get country input for plot title
    if(is_archetype){
      plot_title <- 'Archetype'
    } else {
      plot_title <- input$country
    }
    
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
  
  output$loss_exceedance_gap_plotly <- renderPlot({
    
    prob_exceed_suprplus_deficit <- probability_of_exceeding_suplus_deficit()
    budget <- input$budget
    data_type <- input$data_type
    
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
          plot_title <- 'Archetype'
        } else {
          plot_title <- input$country
        }
        
      } else {
        plot_title <- input$country
        exceed_surplus_deficit <- paste0('Probability of exceeding funding gap/surplus by \n', exceed_budget, ' is ', prob_exceed_suprplus_deficit)
        plot_title <- paste0(plot_title, ' : ', exceed_surplus_deficit)
        
      }
      
      
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
      
      
      plot_dat <- funding_gap_curve
      
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
        dat <- plot_dat
        
        g <-  ggplot(dat, aes(`Probability of exceeding loss`, `Funding gap`)) +
          geom_line(col = 'blue', size = 1, alpha = 0.7) +
          scale_x_reverse(position = 'top') +
          geom_hline(yintercept = 0, size = 2) +
          ggtitle(plot_title) +
          theme_bw(base_size = 14,
                   base_family = 'Ubuntu')
      }
      g
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
          vb2,
          vb3
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