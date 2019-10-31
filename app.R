library(shiny)
library(DT)
library(plotly)
library(htmltools)
library(shiny)
source('global.R')

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(theme = "bootstrap.css",
            # begin html tags for app aesthetic - picture for banner included
            tags$head(tags$style(".title {background:url('banner.jpg'); 
                                 background-repeat: no-repeat;
                                 background-size: 50% 200%; color: white;
                                 font-family: Optima, Segoe, 'Segoe UI', Candara, Calibri, Arial, sans-serif;
                                 font-size: 24px;
                                 font-style: normal;
                                 font-variant: normal;
                                 font-weight: 2000;
                                 line-height: 26.4px}")), #tag end 
            tags$head(tags$style(
              type="text/css",
              "image img {max-width: 100%; width: auto; height: auto}"
            )), # tag end
            tags$head(tags$style(HTML('
                                      .modal.in .modal-dialog{
                                      width:100%; 
                                      height:100%;
                                      margin:0px;
                                      }
                                      .modal-content{
                                      width:100%;
                                      height:100%;}'
            ))),# tag end
            
            # begin title panel - this sets the parameters for the banner photo as well ass the title. 
            titlePanel(windowTitle = "Disaster Risk Financin Tool 1)",
                       title =
                         div(
                           img(
                             src = "",
                             height = 0,
                             width = 0,
                             style = "margin:100px 100px"
                           ),
                           class = 'title'
                         ) # div end               
            ), # titlePanel end 
            
            # being navbar - the is includes the entire UI - the only higher levels are 'gluidPage' and 'ShinyUI'
            navbarPage(
              id = "Navbar",
              fluid = TRUE,
              theme = shinythemes::shinytheme("spacelab"), # includes theme
              footer = helpText(#, feedback button
                "Please send feedback to",
                a(href="blah", target="_blank", "here")
              ), # end footer for feedback 
              tags$style(type="text/css", "body {padding-top: 0px;}"),
              
              # begins the about section (tab)
              tabPanel("About",
                       fluidRow(# starting row
                         #column(3,div(tags$style(
                         #type="text/css",
                         # "#image img {max-width: auto; max-height: auto}"
                         # ),div(img(src = "Sidepanel.png"), id = "image")),tags$br()),
                         # Set column width to 12, the max number, taking up the entire page.
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
                                        provisions set forth in license and other nondisclosure agreements."))), # ending row
                       # beginning a new row
                       fluidRow(# set more HTML aesthetics
                         tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                 tags$p("To get started navigate to the 
                                        User inputs tab to choose a country. If an advanced user, please select the 
                                        Advanced Settings option below for more statistical flexibility.")
                         )
                       ),# end row
                       # New row - meaning under the last row, create a checkbox for advanced setting
                       fluidRow(
                         awesomeCheckbox("advanced", "Use Advanced Settings")
                       )), # end tab panel for about section
              
              # new tab for user inputs
              tabPanel("User inputs",
                       
                       fluidRow(
                         column(3,
                                div(class = 'well',
                                    selectInput('data_type', 'Choose the data type', choices = c('Country', 'Archetype'), selected = 'Country'
                                                  )))
                       ),
                       #  start new row that encompasses inputs for country, download buttons, damage type, and currency
                       fluidRow(
                         column(6, # one third of page
                                div(class = "well",
                                    # input for country
                                    uiOutput('country'),
                                    br(), 
                                    # a uioutput that gives choices for perils based on country input
                                    uiOutput('peril_type'),
                                    uiOutput('prob_dis'),
                                    radioButtons('upload_or_auto',
                                                 'What kind of data do you want to use?',
                                                 choices = c('Pre-loaded data',
                                                             'User-supplied data'),
                                                 selected = 'Pre-loaded data',
                                                 inline = TRUE),
                                    uiOutput('ui_upload_type'),
                                    uiOutput('ui_upload_type_text'),
                                    uiOutput('ui_upload_data')
                                )),
                         
                         
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
                         
                         # start a column that takes almost half the page. This column contains inputs for damage_type, currency, and the probability distribution.
                         
                         column(6, 
                                div(class = 'well',
                                    uiOutput('damage_type'),
                                    radioButtons('currency',
                                                 'Choose a currency',
                                                 choices = currencies,
                                                 selected = 'USD',
                                                 inline = TRUE)
                                    
                                ),
                                column(3,
                                       uiOutput('cost_per_person')),
                                column(3, 
                                       uiOutput('rate')),
                                column(3, 
                                       uiOutput('code')))
                         
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
                       
                       
              ), # end user inputs tab panel
              tabPanel('Data',
                       fluidRow(
                         column(6,
                                div(class = 'well',
                                    h4('Peril data'),
                                    awesomeCheckbox('further_detrend', 'Remove Trends From Data', value = FALSE, status = 'danger'),
                                    DT::dataTableOutput('raw_data_table'),
                                    br(),
                                    # download buttons
                                    fluidRow(
                                      column(12,
                                             downloadButton("download_peril_data",
                                                            "Download Peril Data"))
                                      
                                    )
                                )),
                         column(6,
                                div(class = "well",
                                    h4('Scaling data'),
                                    selectInput('select_scale', 
                                                'Choose from preloaded scaling data',
                                                choices = scaled_data,
                                                selected = scaled_data[1]),
                                    DT::dataTableOutput('raw_scaled_data'),
                                    br(),
                                    fluidRow(
                                      column(12,
                                             downloadButton("download_scaled_data",
                                                            "Download Scaled Data"))
                                      
                                    ))
                         )
                       ),
                       # popus for the upload the further detrend inputs
                       bsPopover(id = "upload", title = '', 
                                 content = "Upload user data and scale by either Population, GDP, or Inflation", 
                                 placement = "middle", trigger = "hover", options = list(container ='body')),
                       bsPopover(id = "further_detrend", title = '', 
                                 content = "If there is no available data to scale by, select this to detrend the data in a linear fashion.", 
                                 placement = "middle", trigger = "hover", options = list(container ='body'))
              ),
              
              # being the section (tab panel) for simulations
              tabPanel(title = 'Simulations',
                       value = 'simulations',
                       # being a row that that two plots: the histogram 
                       fluidRow(
                         column(6,
                                box(
                                  title = 'Peril data',
                                  width = 12,
                                  status = 'primary',
                                  plotOutput('hist_plot'))),
                         column(6,
                                box(
                                  title = 'Simulation data',
                                  width = 12,
                                  status = 'primary',
                                  plotOutput('sim_plot')))
                       ),
                       fluidRow(
                         br(),
                         
                         column(12,
                                box(
                                  title = 'AIC',
                                  width = 12,
                                  status = 'primary',
                                  DT::dataTableOutput('aic_table')))
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
              ),
              
              tabPanel("Output",
                       
                       fluidRow(
                         column(3,
                                div(class = 'well',
                                    numericInput('budget', 'Budget', value = 0),
                                    checkboxInput('ci', 
                                                  'Show confidence intervals',
                                                  value = FALSE)))
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
                         # output 1
                         column(6,div(class = "well",tags$h5(tags$b("Estimated Average Annual Loss by Time Period")),
                                      tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                              tags$ol(class = "intro-divider")
                                      ), plotOutput('annual_loss_plotly'))),
                         # output 2
                         column(6,div(class = "well",tags$h5(tags$b("Loss Exceedance Curve")),
                                      tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                              tags$ol(class = "intro-divider")
                                      ),  plotlyOutput('loss_exceedance_plotly')))
                       ),
                       
                       fluidRow(
                         # output 3
                         column(6,div(class = "well",tags$h5(tags$b("Estimated Average Annual Loss by Severity")),
                                      tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                              tags$ol(class = "intro-divider")
                                      ), plotOutput('annual_loss_gap_plotly'))),
                         # output 4
                         column(6,div(class = "well",tags$h5(tags$b("Funding Gap")),
                                      tags$hr(style="border-color: red;border-top: 3px solid #F511BC;"),
                                      plotlyOutput('loss_exceedance_gap_plotly')))
                       )
                      
                      )
            )
  )
)

server <- function(input, output) {
  
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
      the_data <- selected_damage_type()
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
  
  
  ################
  # about tab
  ################
  
  set.seed(122)
  histdata <- rnorm(500)
  
  ##########
  # Beginning observeEvent functions for Landing page and showing the 'simulations' tab if advanced user is selected
  # creates a pop up page that the user must accept to acces the app
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "Disaster Risk Financing Tool 1", easyClose = FALSE, footer = modalButton('Accept'),
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
        application or misapplication of the tool, or any other associated materials.')
    ))
  })
  
  output$country <- renderUI({
    
      if(input$data_type == 'Country'){
        selectInput("country", 
                    "Choose a country",
                    choices = countries,
                    selected = '')
      } else {
        selectInput("archetype", 
                    "Choose an archetype",
                    choices = archetypes,
                    selected = '')
      }
      
  })
  
  output$damage_type <- renderUI({
    
      if(input$data_type == 'Archetype'){
        radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                     'Choose how you want to view the loss',
                     choices = c('Cost per person'),
                     selected = 'Cost per person',
                     inline = FALSE)
      } else {
        radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                     'Choose how you want to view the loss',
                     choices = c('Total damage', 'Cost per person'),
                     selected = 'Total damage',
                     inline = FALSE)
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
      hideTab(inputId = 'Navbar', target = 'simulations')
    } else {
      # if the counter is odd, show the tab
      showTab(inputId = 'Navbar', target = 'simulations')
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
      if(input$country == ''){
        NULL
      } else {
        # store input country
        country_name <- input$country
        
        # get a list called country data
        country_data <- list()
        
        # the first spot in the list is for loss data, second spot for cost data, and third spot for population. If available, 4th will be inflation, 5th sill be gdp
        if(country_name == 'Sri Lanka'){
          country_data[[1]] <- sri_lanka_loss
          country_data[[2]] <- sri_lanka_cost
          country_data[[3]] <- sri_lanka_pop
        } else if (country_name == 'South Africa'){
          country_data[[1]] <- south_africa_loss
          country_data[[2]] <- south_africa_cost
          country_data[[3]] <- south_africa_pop
        } else if (country_name == 'Philippines'){
          country_data[[1]] <- philippines_loss
          country_data[[2]] <- philippines_cost
          country_data[[3]] <- philippines_pop
        } else if(country_name == 'Mozambique') {
          country_data[[1]] <- mozambique_loss
          country_data[[2]] <- mozambique_cost
          country_data[[3]] <- mozambique_pop    
        } 
        
        # If user-supplied, overwrite some data
        user_supplied <- input$upload_or_auto
        if(user_supplied == 'User-supplied data'){
          the_data <- input_data()
          if(!is.null(the_data)){
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
          }
        }
        
        message('selected_country() is ')
        print(head(country_data[[1]]))
        print(head(country_data[[2]]))
        print(head(country_data[[3]]))
        return(country_data)
      }
      }
      
  
  })
  
  # create a reactive object for archetypes 
  selected_archetype <- reactive({
    if(is.null(input$archetype)){
      NULL
    } else if(input$archetype == ''){
      NULL
    } else if(input$archetype == 'High risk middle income country, exposed to storms, floods, and earthquakes'){
      archetype_data <- high_risk_mid_income_storms_floods_earthquakes
    } else if(input$archetype == 'Middle income country, exposed to floods') {
      archetype_data <- middle_income_flood
    } else if(input$archetype == 'Upper-middle income country, exposed to storms, floods, and earthquakes') {
      archetype_data <- upper_middle_income_storms_floods_earthquakes
    } else if(input$archetype == 'Drought-prone low income country') {
      archetype_data <- low_income_droughts
    } else if(input$archetype == 'Low income conutry, exposed to storms, floods, and earthquakes') {
      archetype_data <- low_income_storms_floods_earthquakes
    } else if(input$archetype == 'Low income country, exposed to droughts, floods, and storms'){
      archetype_data <- low_income_droughts_floods_storms
    }
      return(archetype_data)
  })
  
  # create a reactive object to get country info
  selected_country_info <- reactive({
    country_name <- input$country
    # get country info - a dataframe 
    if(country_name == 'Sri Lanka'){
      country_info <- sri_lanka_info
    } else if (country_name == 'South Africa'){
      country_info <- south_africa_info
    } else if (country_name == 'Philippines'){
      country_info <- philippines_info
    } else if(country_name == 'Mozambique') {
      country_info <- mozambique_info
    } 
    return(country_info)
  })
  
  # create a uioutput for peril type  - this is dependent on the country selected.
  output$peril_type <- renderUI({
    # get country data
    data  <- selected_country()
    temp <- data[[1]]
    # get the peril names for choices in peril input
    peril_names <- as.character(unique(temp$Peril))
    peril_names <- append('All', peril_names)
    selectInput('peril_type', 
                'Choose a peril',
                choices = peril_names,
                selected = 'All')
  })
  
  # create a uioutput for when data type == cost per person
  output$cost_per_person <- renderUI({
    if(is.null(input$damage_type)){
      return(NULL)
    } else {
      if(input$damage_type == 'Total damage') {
        return(NULL)
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
  output$code <- renderUI({
    if(is.null(input$damage_type)){
      return(NULL)
    } else {
      if(input$currency == 'USD' | input$damage_type == 'Total damage'){
        return(NULL)
      } else {
        selectInput('code',
                    'Choose a currency code', 
                    choices = other_currencies, 
                    selected = NULL)
        
      }
    }
    
    
  })
  
  # create uioutput rate for cases where the user choses a currency other than USD
  output$rate <- renderUI({
    if(is.null(input$damage_type)) {
      return(NULL)
    } else {
      if(input$currency == 'USD' | input$damage_type == 'Total damage'){
        return(NULL)
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
  
  # create a reactive object that gets data based on damage type - use the list of dataframes - the first index is loss data, second cost per person
  selected_damage_type <- reactive({
    # get country data
    if(is.null(input$peril_type) | is.null(input$damage_type)){
      NULL
    } else {
      data  <- selected_country()
      
      cost <- input$cost_per_person
      peril_type <- input$peril_type
      # determine if we are doing total damage or cost per person 
      damage_type <- input$damage_type
      message(damage_type)
      if(damage_type == 'Cost per person'){
        data <- data[[2]] # get cost per person data
        names(data)[which(names(data) == 'Affected')] <- 'Loss' # change the name for generalization 
        data$Loss <- data$Loss*cost # multiple loss (in this case people affected) by cost input
        if(peril_type != 'All'){
          data <- data[data$Peril == peril_type,]
        }
      } else {
        data <- data[[1]]
        if(peril_type != 'All'){
          data <- data[data$Peril == peril_type,]
        }
      }
      return(data)
    }
  })
  
  
  ################
  # Data tab
  ################
  
  # create a data table  
  output$raw_data_table <- DT::renderDataTable({
    if(is.null(selected_damage_type())){
      NULL
    } else {
      min_obs = 4
      data <- selected_damage_type()
      message('selected_damage_type() is: ')
      print(head(data))
      num_obs <- nrow(data)
      if(num_obs < min_obs){
        data <- data_frame('Not enough observations to run simulations')
        names(data) <- NULL
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      } else {
        datatable(data, options = list(dom='t',ordering=F))
      }
    }
   
  })
  
  # output for scaling data if available - the 3rd, 4th, and 5th index in the data list are scaling data. Population is 3rd
  # gdp 4th, inflation 5th
  output$raw_scaled_data <- renderDataTable({
    select_scale <- input$select_scale
    data <- selected_country()
    data_info <- selected_country_info()
    
    # check if scaled data exists
    if(select_scale == 'Population'){
      if(data_info$population) {
        data <- data[[3]]
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      } else {
        data <- data_frame('Population data is not available for this country')
        names(data) <- NULL
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      }
    } else if(select_scale == 'GDP'){
      if(data_info$gdp) {
        data <- data[[4]]
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      } else {
        data <- data_frame('GDP data is not available for this country')
        names(data) <- NULL
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      }
    } else if(select_scale == 'Inflation'){
      if(data_info$gdp) {
        data <- data[[5]]
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      } else {
        data <- data_frame('Inflation data is not available for this country')
        names(data) <- NULL
        datatable(data, rownames = FALSE, colnames = NULL, options = list(dom='t',ordering=F))
      }
    }
  })
  
  # make a reactive object that grabs the name of the best distribution - this is the default for basic users. Advanced users can choose a new one
  get_best_dis <- reactive({
    if(is.null(get_aic_mle())){
      return(NULL)
    } else {
      # get aic_mle_data
      dat <- get_aic_mle()
      # for now remove beta
      # dat <- dat[dat$Distribution != 'Beta',]
      # get index for minimum aic
      aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
      # # subset by best aic index
      dat <- dat[aic_min_ind,]    
      best_dis <- dat$Distribution
      return(best_dis)
    }
  })
  
  # ui for prob_dis - right now the output is dependent on the best distribution
  # if advanced is selected the distribution has multiple choices, otherwise it defaults to best
  output$prob_dis <- renderUI({
    # get aic_mle_data
    dat <- get_aic_mle()
    # get index for minimum aic
    aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
    # # subset my index
    dat <- dat[aic_min_ind,]    
    best_dis <- dat$Distribution
    
    if(input$advanced){
      selectInput('prob_dis', 'Choose distribution (default is best fit)', 
                  choices = advanced_parametric,
                  selected = best_dis)
    } else {
      selectInput('prob_dis', 'Default is best fit', 
                  choices = best_dis,
                  selected = best_dis)
    }
  })
  
  ################
  # Simulations tab 
  ################
  
  get_aic_mle <- reactive({
    if(is.null(selected_damage_type())){
      return(NULL)
    } else {
      # data <- country_data
      data <- selected_damage_type()
      # remove obsevations with 0, if any
      data <- data[data$Loss > 0,]
      message(head(data))
      
      ##########
      # fit lognormal
      #########
      log_normal <- try(fitdistr(data$Loss, "lognormal"),silent = TRUE)
      if(class(log_normal) == 'try-error'){
        log_normal <- NULL
        log_normal_aic <- NA
        log_normal$estimate[1] <- NA
        log_normal$estimate[2] <- NA
        
      } else {
        # get aic
        log_normal_aic <- round(AIC(log_normal), 4)
        
        # if there is an error, fill object with NA
        message('log normal AIC is ', log_normal_aic)
        
        # get MLE 
        log_normal_mle <- paste0(log_normal$estimate[1], ' ', log_normal$estimate[2])
        message('log normal mle is ', log_normal_mle)
      }
      # create data frame to store aic and MLEs
      log_normal_data <- data_frame(name = 'log_normal',
                                    aic = log_normal_aic, 
                                    mle_1 = log_normal$estimate[1],
                                    mle_2 = log_normal$estimate[2])
      
      beta <- try(eBeta_ab(data$Loss, method = "numerical.MLE"), silent = TRUE)
      if(class(beta) == 'try-error'){
        beta <- NULL
        beta_aic <- NA
        beta$shape1 <- NA
        beta$shape2 <- NA
        beta_mle <- c(beta$shape1, beta$shape2)
      } else {
        beta_ll <- lBeta_ab(X = data$Loss, params = beta, logL = TRUE)
        beta_aic <- -(2*beta_ll + 2) 
        beta_mle <- c(beta$shape1, beta$shape2)
        
        # beta_aic <- round(beta$aic, 4)
        message('beta AIC is ', beta_aic)
        message('beta mle is ', beta_mle)
      }
      beta_data <- data_frame(name = 'beta',
                              aic = round(beta_aic, 4), 
                              mle_1 = beta_mle[1],
                              mle_2 = beta_mle[2])
      
      
      
      # EQUATION FOR AIC 
      # -2*loglikihood + k*npar, where k is generally 2 and npar is number of parameters in the model.
      
      # fit gamma
      # gamma <- fitdistr(data$Loss, 'gamma')
      gamma <- try(fitdistrplus::fitdist(data$Loss, "gamma", start=list(shape=0.5, scale=1), method="mle"), silent = TRUE)
      
      if(class(gamma) == 'try-error'){
        gamma <- NULL
        gamma_aic <- NA
        gamma$estimate[1] <- NA
        gamma$estimate[2] <- NA
        
      } else {
        # get aic
        gamma_aic <- round(gamma$aic, 4)
        message('gamma AIC is ', gamma_aic)
        
        # get mle 
        gamma_mle <- paste0(gamma$estimate[1], ' ', gamma$estimate[2])
        message('gamme mle is ', gamma_mle)
      }
      gamma_data <- data_frame(name = 'gamma',
                               aic = gamma_aic, 
                               mle_1 = gamma$estimate[1],
                               mle_2 = gamma$estimate[2])
      
      
      
      # fit frechet
      # dfrechet(data$Loss, lambda = 1, mu = 1, sigma = 1, log = TRUE)
      frechet <- try(fitdistrplus::fitdist(data$Loss, "frechet", start=list(scale=0.1, shape=0.1), method="mle"), 
                     silent = TRUE)
      if(class(frechet) == 'try-error'){
        frechet <- NULL
        frechet_aic <- NA
        frechet$estimate[1] <- NA
        frechet$estimate[2] <- NA
        
      } else {
        frechet_aic <- round(frechet$aic, 4)
        message('frechet AIC is ', frechet_aic)
        # get mle 
        frechet_mle <- paste0(frechet$estimate[1], ' ', frechet$estimate[2])
        message('frechet mle is ', frechet_mle) 
      }
      frechet_data <- data_frame(name = 'frechet',
                                 aic = frechet_aic, 
                                 mle_1 = frechet$estimate[1],
                                 mle_2 = frechet$estimate[2])
      
      
      
      # git gumbel
      gumbel_fit <- try(fit_gumbel(data$Loss), silent = TRUE)
      if(class(gumbel_fit) == 'try-error'){
        gumbel_fit <- NULL
        gumbel_aic <- NA
        gumbel_fit$estimate[1] <- NA
        gumbel_fit$estimate[2] <- NA
        
      } else {
        gumbel_aic <- round(gumbel_fit$aic, 4)
        message('gumbel AIC is ', gumbel_aic)
        # get mle
        gumbel_mle <- paste0(gumbel_fit$estimate[1], ' ', gumbel_fit$estimate[2])
        message('gumbel mle is ', gumbel_mle)
      }
      gumbel_data <- data_frame(name = 'gumbel',
                                aic = gumbel_aic, 
                                mle_1 = gumbel_fit$estimate[1],
                                mle_2 = gumbel_fit$estimate[2])
      
      
      
      # fit weibull
      weibull <- try(fitdistrplus::fitdist(data$Loss, "weibull", start=list(shape=0.1, scale=1), method="mle"), silent = TRUE)
      if(class(weibull) == 'try-error'){
        weibull <- NULL
        weibull_aic <- NA
        weibull$estimate[1] <- NA
        weibull$estimate[2] <- NA
        
      } else {
        weibull_aic <- round(weibull$aic, 4)
        message('weibull AIC is ', weibull_aic)
        
        # get mle
        weibull_mle <- paste0(weibull$estimate[1], ' ', weibull$estimate[2])
        message('weibull mle is ', weibull_mle)
      }
      weibull_data <- data_frame(name = 'weibull',
                                 aic = weibull_aic, 
                                 mle_1 = weibull$estimate[1],
                                 mle_2 = weibull$estimate[2])
      
      
      
      # fit pareto
      pareto <-ParetoPosStable::pareto.fit(data$Loss, estim.method = 'MLE')
      if(class(pareto) == 'try-error'){
        pareto <- NULL
        pareto_aic <- NA
        pareto_fit$estimate[1] <- NA
        pareto_fit$estimate[2] <- NA
        
      } else { 
        pareto_aic <- round(-(2*pareto$loglik) + 2, 4)
        message('pareto AIC is ', pareto_aic)
        # get mle
        pareto_mle <- paste0(pareto$estimate[1], ' ', pareto$estimate[2])
        message('pareto mle is ', pareto_mle)
      }
      pareto_data <- data_frame(name = 'pareto',
                                aic = pareto_aic, 
                                mle_1 = pareto$estimate[[1]],
                                mle_2 = pareto$estimate[[2]])
      
      
      
      
      # create a data frame out of data results
      aic_mle_data <- rbind(log_normal_data,
                            gamma_data,
                            beta_data,
                            frechet_data,
                            gumbel_data,
                            weibull_data,
                            pareto_data)
      
      # change names of variable
      names(aic_mle_data) <- c('Distribution', 'AIC', 'MLE 1', 'MLE 2')
      
      # capitalize and remove underscore of Distribution
      aic_mle_data$Distribution <- Hmisc::capitalize(aic_mle_data$Distribution)
      aic_mle_data$Distribution <- gsub('_', ' ', aic_mle_data$Distribution)
      aic_mle_data$AIC <- round(aic_mle_data$AIC, 2)
      aic_mle_data$`MLE 1`<- round(aic_mle_data$`MLE 1`, 2)
      aic_mle_data$`MLE 2` <- round(aic_mle_data$`MLE 2`, 2)
      
      return(aic_mle_data)
    }
    
  })
  
  # create table for aic
  output$aic_table <- renderDataTable({
    aic_mle_data <- get_aic_mle()
    DT::datatable(aic_mle_data, options = list(dom = 't'))
  }) 
  
  # 
  # create a reactive object that takes the aic_mle_data runs simulations on the 
  # best distribution (found from min aic)
  run_best_simulation <- reactive({
    set.seed(11)
    
    if(is.null(input$prob_dis)){
      return(NULL)
    } else {
      best_dis <- get_best_dis()
      # get aic_mle_data
      dat <- get_aic_mle()
      
      if(best_dis == input$prob_dis){
        # for now remove beta
        # dat <- dat[dat$Distribution != 'Beta',]
        # get index for minimum aic
        aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
        # # subset my index
        dat <- dat[aic_min_ind,]  
      } else {
        # dat <- dat[dat$Distribution != 'Beta',]
        dat <- dat[dat$Distribution ==  input$prob_dis,]
      }
      
      # set conditions for each distribution
      if(dat$Distribution == 'Log normal'){
        if(any(is.na(dat$AIC))){
          sim <- NA
        }  else {
          sim <- rlnorm(n = 15000, meanlog = dat$`MLE 1`, sdlog = dat$`MLE 2`)
        }
      } else if (dat$Distribution == 'Gamma'){
        if(any(is.na(dat$AIC))){
          sim <- NA
        }  else {
          # check to see how much seed matters
          sim <- rgamma(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
        }
      } else if (dat$Distribution == 'Beta'){
        if(any(is.na(dat$AIC))){
          sim <- NA
        } else {
          sim <- rbeta(n = 15000, shape1 = dat$`MLE 1`, scale2 = dat$`MLE 2`)
        }
      }  else if (dat$Distribution == 'Frechet'){
        if(any(is.na(dat$AIC))){
          sim <- NA
        }  else {
          sim <- rfrechet(n = 15000, loc=0, scale=dat$`MLE 1`, shape=dat$`MLE 2`)
        }
      } else if (dat$Distribution == 'Gumbel'){
        if(any(is.na(dat$AIC))){
          sim <- NA 
        } else {
          sim <- actuar::rgumbel(n = 15000, alpha = dat$`MLE 1`, scale = dat$`MLE 2`)
        }
      } else if (dat$Distribution == 'Weibull'){
        if(any(is.na(dat$AIC))){
          sim <- NA
        }  else {
          sim <- rweibull(n = 15000, shape = dat$`MLE 1`, scale = dat$`MLE 2`)
        }
      } else {
        if(any(is.na(dat$AIC))){
          sim <- NA
        }  else {
          sim <- extraDistr::rpareto(n = 15000, a = dat$`MLE 1`, b = dat$`MLE 2`)
        }
      }
      return(sim)
    }
    
  })
  
  # create a ouput plot that draws a density of the distribution over the 
  # histogram of raw data
  output$hist_plot <- renderPlot({
    data <- selected_damage_type()
    g <- ggplot(data, aes(data$Loss)) +
      geom_histogram(bins = 5, fill = 'black', color = 'blue', alpha = 0.6) + 
      labs(x = 'Loss', 
           y = 'Counts') +
      theme_clean() +
      theme(axis.text.x = element_text(angle = 0, size = 15),
            axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 15)) 
    return(g)
    
  })
  output$sim_plot <- renderPlot({
    if(is.null(input$prob_dis)){
      return(NULL)
    } else {
      dat_sim <- run_best_simulation()
      dat_sim <- as.data.frame(dat_sim)
      names(dat_sim) <- 'Simulated loss'
      g =  ggplot(dat_sim, aes(`Simulated loss`)) +
        geom_density(fill = 'black', color = 'blue', alpha = 0.5) +
        labs(y = 'Density') +
        theme_clean() +
        theme(axis.text.x = element_text(angle = 0, size = 15),
              axis.text.y = element_text(size = 15),
              axis.title = element_text(size = 15)) 
      return(g)
    }
    
  })
  
  ################
  # Output tab
  ################
  
  # OUTPUT 1
  output$annual_loss_plotly <- renderPlot({
    
    if(is.null(selected_damage_type())){
      NULL
    } else {
      # get best distirbution 
      dat_sim <- run_best_simulation()
      if(any(is.na(dat_sim))){
        NULL
      } else {
        # get country data
        data <- selected_damage_type()
        
        # remove obsevations with 0, if any
        data <- data[data$Loss > 0,]
        data <- data[order(data$Year, decreasing = FALSE),]
        
        # get country input for plot title
        plot_title <- input$country
        
        # get quaintles 
        output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99), na.rm = TRUE) 
        annual_avg <- mean(dat_sim)
        
        # create data frame dat to store output with chart labels 
        dat <- data_frame(`Annual average` = annual_avg, 
                          `1 in 5 Years` = output[1],
                          `1 in 10 Years` = output[2],
                          `1 in 25 Years` = output[3],
                          `1 in 50 Years` = output[4],
                          `1 in 100 Years` = output[5],
                          `Highest historical annual loss` = max(data$Loss),
                          `Most recent annual loss` = data$Loss[nrow(data)])
        
        # melt the data frame to get value and variable 
        dat <- melt(dat)
        
        f <- list(
          family = "Ubuntu",
          size = 20,
          color = "white"
        )
        
        dat$variable <- factor(dat$variable, levels = c('1 in 5 Years', '1 in 10 Years', '1 in 25 Years', '1 in 50 Years', '1 in 100 Years', 
                                                        'Annual average', 'Highest historical annual loss', 'Most recent annual loss'))
        dat$value <- round(dat$value, 2)
        
        # Plot
        g <- ggplot(dat, aes(x=variable, 
                             y=value,
                             text = value)) + 
          geom_bar(stat = 'identity',
                   fill = '#5B84B1FF',
                   col = '#FC766AFF', 
                   alpha = 0.6) + 
          geom_text(aes(label=round(value, 5)), position=position_dodge(width=0.9), vjust=-0.35) +
          theme_bw(base_size = 14, 
                   base_family = 'Ubuntu')  +
          theme(axis.text.x = element_text(angle = 45, 
                                           hjust = 1)) +
          labs(title='', x = '', y = ' ') 
        return(g)
      }
    }
   
  })
  

  ############ OUTPUT 2
  
  output$loss_exceedance_plotly <- renderPlotly({
    
    if(is.null(selected_damage_type())){
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
        
        # get country input for plot title
        plot_title <- input$country
        
        
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
        
        p <- plot_ly(peril_exceedance_curve, 
                     x = ~Probability, 
                     y = ~`Total Loss`, 
                     text = ~`Total Loss`, 
                     type = 'scatter', 
                     mode = 'lines') %>%
          layout(shapes=list(type='line', x0= 0.5, x1= 1, y0=largest_loss_num, y1=largest_loss_num, line=list(dash='dot', width=1)),
                 title = '',
                 xaxis = list(showgrid = FALSE),
                 yaxis = list(showgrid = FALSE)) 
        return(p)
      }
    }
    
    
  })
  
  
  ############ OUTPUT 3
  output$annual_loss_gap_plotly <- renderPlot({
    
    if(is.null(selected_damage_type())){
      NULL
    } else {
      dat_sim <- run_best_simulation()
      
      if(any(is.na(dat_sim))){
        NULL 
      } else {
        # get country data
        data <- selected_damage_type()
        
        # remove obsevations with 0, if any
        data <- data[data$Loss > 0,]
        
        data <- data[order(data$Year, decreasing = FALSE),]
        # get country input for plot title
        plot_title <- input$country
        
        # get quaintles 
        output <- quantile(dat_sim,c(0.8,0.9, 0.96,0.98,0.99)) 
        annual_avg <- mean(dat_sim)
        
        # create data frame dat to store output with chart labels 
        dat <- data_frame(`Average` = annual_avg, 
                          `Severe` = output[2],
                          `Extreme` = output[5])
        
        # melt the data frame to get value and variable 
        dat <- melt(dat)
        
        # divide valueb by 100k
        # dat$value <- dat$value/scale_by
        
        f <- list(
          family = "Ubuntu",
          size = 20,
          color = "white"
        )
        
        # plot
        g <- ggplot(dat, aes(x=variable, 
                             y=value,
                             text = value)) + 
          geom_bar(stat = 'identity',
                   fill = '#5B84B1FF',
                   col = '#FC766AFF', 
                   alpha = 0.6) + 
          geom_text(aes(label=round(value, 5)), position=position_dodge(width=0.9), vjust=-0.35) +
          theme_bw(base_size = 14, 
                   base_family = 'Ubuntu')  +
          theme(axis.text.x = element_text(angle = 45, 
                                           hjust = 1)) +
          labs(title='', x = '', y = ' ') 
        
        return(g)
        
      }
    }
    
    
    
  })
  
  # OUTPUT 4
  output$loss_exceedance_gap_plotly <- renderPlotly({
    
    if(is.null(selected_damage_type())) {
      NULL
    } else {
      dat_sim <- run_best_simulation()
      
      if(any(is.na(dat_sim))){
        NULL
      } else {
        data <- selected_damage_type()
        
        data <- data[order(data$Year, decreasing = FALSE),]
        largest_loss_num <- max(data$Loss)
        largest_loss_year <- data$Year[data$Loss == max(data$Loss)]
        budget <- input$budget
        
        # get country input for plot title
        plot_title <- input$country
        
        # get best distirbution 
        dat_sim <- run_best_simulation()
        funding_gap_curve <- as.data.frame(quantile(dat_sim,seq(0.5,0.98,by=0.002)))
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
        funding_gap_curve$`Funding gap` <- funding_gap_curve$`Funding gap` - budget
        
        p <- plot_ly(funding_gap_curve, 
                     x = ~`Probability of exceeding loss`, 
                     y = ~`Funding gap`, 
                     text = ~`Funding gap`, 
                     type = 'scatter', 
                     mode = 'lines') %>%
          layout(
            title = '',
            xaxis = list(title = 'Probability of exceeding loss', autorange = 'reversed'),
            yaxis = list(title = 'Funding gap', autorange = 'reversed')
            # annotations = list(yref='paper',xref="paper", text = 'dndfs lknsdfs ', showarrow = F, x = 1.3, y = 1, legendtitle = TRUE),
           ) 
        return(p)
      }
    }
    
    
  })
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
