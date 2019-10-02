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
                       # start new roww for boarder color
                       fluidRow(# border color
                         # begin tags for 'notes' in top right of this tab 
                         tags$hr(style="border-color: black;border-top: 3px solid #F511BC;",
                                        tags$p(class = "intro-divider", tags$b("Notes:"),
                                               tags$p("First choose a country to examine and the type of data you're using (loss values in coster per person or total damage)"),
                                               tags$p("Hover over inputs for further directions."),
                                               tags$p("Once you have selected your inputs (you can leave them as default), proceed to the simulations and output"))),
                                tags$p(class = "intro-divider",
                                       tags$b("Sources:"),
                                       htmlOutput("Put sources here")
                                )), #end row
                       
                       #  start new row that encompasses inputs for country, download buttons, damage type, and currency
                       fluidRow(
                         column(4, # one third of page
                                div(class = "well",
                                    # input for country
                                    selectInput("country", 
                                                "Choose a country",
                                                choices = c("Afghanistan")), # only one choice currently, but building in the others
                                    tags$p(tags$b("Click on buttons below download preloaded datasets or upload user perild data")),
                                    # download buttons
                                    fluidRow(column(12,
                                                    downloadButton("download_data",
                                                                   "Download Peril Data"),
                                                    br(), br(),
                                                    actionButton("upload_data",
                                                                 "Upload Peril Data"))
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
                                          placement = "middle", trigger = "hover", options = list(container ='body'))
                         ),
                         # start a column that takes almost half the page. This column contains inputs for damage_type, currency, and the probability distribution.
                         column(5, 
                                div(class = 'well',
                                    radioButtons('damage_type', # If cost per person, must specify the amount. Otherwise it's monetary loss data
                                                 'Data type',
                                                 choices = c('Total damage', 'Cost per person'),
                                                 selected = 'Total damage',
                                                 inline = FALSE),
                                    radioButtons('currency',
                                                 'Choose a currency',
                                                 choices = currencies,
                                                 selected = 'USD',
                                                 inline = TRUE),
                                    fluidRow( 
                                      column(6,
                                             # advanced user only - the uiOutput function creates this input on the server side because it only appears for advanced users and thus 
                                             # dependent on whether someone checks the box.
                                             uiOutput('prob_dis'))
                                    ) 
                                ),
                                # popups for the inputs damage_type, currency.Need to add one on the server side for prob_dis
                                bsPopover(id = "damage_type", title = '', 
                                          content = "Select whether you would like to view the loss as cost per person or as total damage. If you choose cost per person, please enter the cost. In this mode you must use USD", 
                                          placement = "middle", trigger = "hover", options = list(container ='body')),
                                bsPopover(id = "currency", title = '', 
                                          content = "If other is chosen, please select a currency code and exchange rate.", 
                                          placement = "middle", trigger = "hover", options = list(container ='body'))
                         )),# end row
                       # begin row that contains inputs for scaling, uploading data, removing trends,
                       fluidRow(
                         column(5,
                                div(class = 'well',
                                    selectInput('scale_by',
                                                'Scale by',
                                                choices = c('Population', 
                                                            'GDP',
                                                            'Inflation'),
                                                selected = 'Population'))
                         ),
                         column(3,
                                # button for scaling data
                                actionButton('upload', 'Upload Scaling Data'),
                                br(),
                                br(),
                                actionButton('further_detrend', 'Remove Trends From Data'),
                                # popus for the upload the further detrend inputs
                                bsPopover(id = "upload", title = '', 
                                          content = "Upload user data and scale by either Population, GDP, or Inflation", 
                                          placement = "middle", trigger = "hover", options = list(container ='body')),
                                bsPopover(id = "further_detrend", title = '', 
                                          content = "If there is no available data to scale by, select this to detrend the data in a linear fashion.", 
                                          placement = "middle", trigger = "hover", options = list(container ='body'))
                         )
                         
                         
                       ),# end row
                       # this columns are floating outside any row - won't make a difference though.
                       column(3,
                              uiOutput('cost_per_person')),
                       column(2, 
                              uiOutput('rate')),
                       column(2, 
                              uiOutput('code')),
                       
                       fluidRow(# begin row for the table of data at the bottom of the user inputs page
                         column(12,
                                DT::dataTableOutput('data_table'))
                       )# end row
                       
                       
              ), # end user inputs tab panel
              
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
                       br(),
                       
                       # 
                       bsPopover(id = "annual_loss_plotly", title = 'Exhibit 1', 
                                 content = "This graph shows the estimated annual loss across all selected perils. A return period of 1 in 5 years is the estimated annual loss expected to happend every five years (ie 20% probability). Similarly, a period of 1 in 10 years is the estimated annual loss expectedto happen every 10 years (ie 10% probability.", 
                                 placement = "middle", trigger = "hover", options = list(container ='body')),
                       
                       bsPopover(id = "loss_exceedance_plotly", title = 'Exhibit 2', content = "This graph shows the probability of a year taking place that exceeds the aggregate annual loss amount on the y-axis. The probability of exceeding the available budget is represented by the probability where the available budget line and the loss exceedance curve cross.",
                                 placement = "left", trigger = "hover", options = list(container ='body')),
                       # 
                       bsPopover(id = 'annual_loss_gap_plotly', title = 'Exhibit 3', content = "The funding gap is the difference between the available federal budget and the estimated annual loss at the return period. A loss value below the red budget line represents an estimated surplus (if above, it would be a deficit)",
                                 placement = "middle", trigger = "hover", options = list(container ='body')),
                       # 
                       # 
                       bsPopover(id = 'loss_exceedance_gap_plotly', title = 'Exhibit 4', content = "The graph shows the probability of experiencing different sized funding gaps/surpluses. When the line is above the x-axis, it indicates a funding surplus - if below, it indicates a funding deficit.",
                                 placement = "left", trigger = "hover", options = list(container ='body')),
                       # 
                       column(6,div(class = "well",tags$h5(tags$b("Estimated Average Annual Loss by Time Period")),
                                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                            tags$ol(class = "intro-divider")
                                    ), plotOutput('annual_loss_plotly'))),
                       
                       column(6,div(class = "well",tags$h5(tags$b("Loss Exceedance Curve")),
                                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                            tags$ol(class = "intro-divider")
                                    ),  plotlyOutput('loss_exceedance_plotly'))),
                       
                       column(6,div(class = "well",tags$h5(tags$b("Estimated Average Annual Loss by Severity")),
                                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                            tags$ol(class = "intro-divider")
                                    ), plotOutput('annual_loss_gap_plotly'))),
                       
                       column(6,div(class = "well",tags$h5(tags$b("Funding Gap")),
                                    tags$hr(style="border-color: red;border-top: 3px solid #F511BC;"),
                                    plotlyOutput('loss_exceedance_gap_plotly')))
                       # fluidRow(
                       #   box(title = 'Estimated Average Annual Loss', 
                       #       width = 6,
                       #       status = 'danger', # success= green, info = lighterblue, warning = orange, 
                       #       solidHeader = TRUE,
                       #       column(12,
                       #              plotOutput('annual_loss'))),
                       #   box(title = 'Loss Exceedance Curve', 
                       #       width = 6,
                       #       status = 'danger', # success= green, info = lighterblue, warning = orange, 
                       #       solidHeader = TRUE,
                       #       column(12,
                       #              plotOutput('loss_exceedance')))
                       # ),
                       # fluidRow(
                       #   box(title = 'Estimated Avg Annual Loss', 
                       #       width = 6,
                       #       status = 'danger', # success= green, info = lighterblue, warning = orange, 
                       #       solidHeader = TRUE,
                       #       column(12,
                       #              plotOutput('annual_loss_gap'))),
                       #   box(title = 'Loss Exceedance Curve of funding gap', 
                       #       width = 6,
                       #       status = 'danger', # success= green, info = lighterblue, warning = orange, 
                       #       solidHeader = TRUE,
                       #       column(12,
                       #              plotOutput('loss_exceedance_gap')))
                       # )
                       # 
                       
              )
              
              
              
            ) #end navbar
            
            
  )
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  
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
  
  counter <- reactiveVal(value = 0)
  
  observeEvent(input$advanced,{
    cc <- counter()
    message('current counter is ', cc)
    new_cc <- cc + 1
    counter(new_cc)
    message('new counter is ', new_cc)
    if(cc %% 2 == 0){
      hideTab(inputId = 'Navbar', target = 'simulations')
      
    } else {
      showTab(inputId = 'Navbar', target = 'simulations')
      
    }
  })
  
  
  # create a uioutput for when data type == cost per person
  output$cost_per_person <- renderUI({
    if(input$damage_type != 'Cost per person'){
      NULL
    } else {
      numericInput('cost_per_person',
                   'Enter coster per person USD', 
                   min = 0,
                   max = 1000,
                   step = 10,
                   value = 50)
    }
  })
  
  # create uioutput code for cases where the user choses a currency other than USD
  output$code <- renderUI({
    if(input$currency == 'USD' | input$damage_type == 'Cost per person'){
      NULL
    } else {
      selectInput('code',
                  'Choose a currency code', 
                  choices = other_currencies, 
                  selected = NULL)
    }
  })
  
  # create uioutput rate for cases where the user choses a currency other than USD
  output$rate <- renderUI({
    if(input$currency == 'USD' | input$damage_type == 'Cost per person'){
      return(NULL) 
    } else {
      numericInput('rate',
                   'Enter conversion rate',
                   min = 0,
                   max = 100,
                   step = 1,
                   value = 1)
    }
  })
  
  # create a uiouput for if an invalid code is selected once "other" is chosen for currency
  output$display_rate <- renderText({
    if(is.null(input$code) | input$damage_type == 'Cost per person'){
      NULL
    } else {
      "Conversion rate only required when data type is 'Total damage'"
    }
    
  })
  ###  OUTPUT page
  
  # get a reactive object that selects country data (list) based on imput
  selected_country <- reactive({
    
    country_name <- input$country
    
    if(country_name == 'Afghanistan'){
      country_data <- raw_data_af
    } else if (country_name == 'Malaysia'){
      country_data <- raw_data_malay
    } else if (country_name == 'Senegal'){
      country_data <- raw_data_sen
    } else {
      country_data <- raw_data_som
    }
    return(country_data)
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
  
  # HERE: MAKE SURE YOU HAVE THE RIGHT MLES BEFORE SIMULATIONS, GAMMA NEEDS AN INVERSE AND ONE OTHER NEEDS A SQUARED, BOTH FROM SECOND MLE
  # 
  # CREATE A REACTIVE OBJECT THAT GETS AIC SCORES FOR EACH PARAMETRIC LOSS DISTRIBUTION
  get_aic_mle <- reactive({
    
    # get country data
    data  <- selected_country()
    
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
    
    
    
    # fit beta (only one not replicating)
    # normalize data to 0, 1
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
  })
  
  # create table for aic
  output$aic_table <- renderDataTable({
    
    aic_mle_data <- get_aic_mle()
    DT::datatable(aic_mle_data, options = list(dom = 't'))
    
    
  }) 
  
  # make a reactive object that grabs the name of the best distribution
  get_best_dis <- reactive({
    # get aic_mle_data
    dat <- get_aic_mle()
    # for now remove beta
    # dat <- dat[dat$Distribution != 'Beta',]
    # get index for minimum aic
    aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
    # # subset my index
    dat <- dat[aic_min_ind,]    
    best_dis <- dat$Distribution
    
    return(best_dis)
  })
  
  
  # ui for prob_dis
  output$prob_dis <- renderUI({
    
    # get aic_mle_data
    dat <- get_aic_mle()
    # for now remove beta
    # dat <- dat[dat$Distribution != 'Beta',]
    # get index for minimum aic
    aic_min_ind <- which(dat$AIC == min(dat$AIC, na.rm = T))
    # # subset my index
    dat <- dat[aic_min_ind,]    
    best_dis <- dat$Distribution
    selectInput('prob_dis', 'Choose distribution (default is best fit)', 
                choices = advanced_parametric,
                selected = best_dis)
  })
  
  
  # # create a reactive function to get rag ratings
  # get_rag_ratings <- reactive({
  #   aic_mle_data <- get_aic_mle()
  #   # assign red, amber, or green to the distributions 
  #  
  # })
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
    data <- selected_country()
    
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
  
  ##############################################################################################
  # radioButtons('bubble_line', 'Choose chart type', choices = c('Bubble and Pie', 'Line and Candle')))
  # 
  
  
  
  # annual loss exhibit 1
  output$annual_loss_plotly <- renderPlot({
    
    # get best distirbution 
    dat_sim <- run_best_simulation()
    if(any(is.na(dat_sim))){
      NULL
    } else {
      
      # get country data
      data <- selected_country()
      data <- data[order(data$Year, decreasing = FALSE),]
      # get country input for plot title
      plot_title <- input$country
      if(plot_title == 'Afghanistan'){
        scale_by = 1
      } else {
        scale_by = 1000000
      }
      
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
      
      # g_plot <- plotly::ggplotly(g, tooltip = 'text')  %>% plotly::config(displayModeBar = F) 
      
      
      return(g)
      
      
      
    }
    
  })
  
  
  ########################################################################################
  
  # exhibit 2
  
  
  
  output$loss_exceedance_plotly <- renderPlotly({
    dat_sim <- run_best_simulation()
    if(any(is.na(dat_sim))){
      return(NULL)
    } else {
      data <- selected_country()
      country_name <- unique(data$Country)
      
      data <- data[order(data$Year, decreasing = FALSE),]
      largest_loss_num <- max(data$Loss)
      largest_loss_year <- data$Year[data$Loss == max(data$Loss)]
      
      # get country input for plot title
      plot_title <- input$country
      if(plot_title == 'Afghanistan'){
        scale_by = 1
      } else {
        scale_by = 1000000
      }
      # get best distirbution 
      
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
      
      
      p <- plot_ly(peril_exceedance_curve, x = ~Probability, y = ~`Total Loss`, text = ~`Total Loss`, type = 'scatter', mode = 'markers', color = ~`Total Loss`, colors = 'Reds',
                   marker = list(size = ~(`Total Loss`/30)^1.2, opacity = 0.6)) %>%
        layout(shapes=list(type='line', x0= 0.5, x1= 1, y0=largest_loss_num, y1=largest_loss_num, line=list(dash='dot', width=1)),
               title = '',
               xaxis = list(showgrid = FALSE),
               yaxis = list(showgrid = FALSE)) 
      
      p
      
      return(p)
      
      
    }
    
    
    
  })
  
  
  ##############################################################################
  
  
  output$annual_loss_gap_plotly <- renderPlot({
    
    dat_sim <- run_best_simulation()
    
    if(any(is.na(dat_sim))){
      NULL 
    } else {
      
      # get country data
      
      data <- selected_country()
      data <- data[order(data$Year, decreasing = FALSE),]
      # get country input for plot title
      plot_title <- input$country
      if(plot_title == 'Afghanistan'){
        scale_by = 1
      } else {
        scale_by = 1000000
      }
      # get best distirbution 
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
      dat$value <- dat$value/scale_by
      
      f <- list(
        family = "Ubuntu",
        size = 20,
        color = "white"
      )
      
      
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
      # 
      # g_plot <- plotly::ggplotly(g, tooltip = 'text')  %>% plotly::config(displayModeBar = F) 
      # 
      
      return(g)
      
      
    }
    
    
    
    
    
  })
  
  
  ############################################################################
  # exhibit 4
  
  
  
  output$loss_exceedance_gap_plotly <- renderPlotly({
    
    dat_sim <- run_best_simulation()
    
    if(any(is.na(dat_sim))){
      NULL
    } else {
      data <- selected_country()
      data <- data[order(data$Year, decreasing = FALSE),]
      largest_loss_num <- max(data$Loss)
      largest_loss_year <- data$Year[data$Loss == max(data$Loss)]
      
      # get country input for plot title
      plot_title <- input$country
      if(plot_title == 'Afghanistan'){
        scale_by = 1
      } else {
        scale_by = 1000000
      }
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
      
      
      p <- plot_ly(funding_gap_curve, 
                   x = ~`Probability of exceeding loss`, 
                   y = ~`Funding gap`, 
                   text = ~`Funding gap`, 
                   type = 'scatter', 
                   mode = 'markers', 
                   color = ~`Funding gap`, 
                   colors = 'Reds',
                   marker = list(size = ~(`Funding gap`/30)^1.2, opacity = 0.6)) %>%
        layout(
          title = '',
          xaxis = list(title = 'Probability of exceeding loss'),
          yaxis = list(title = 'Funding gap'),
          # annotations = list(yref='paper',xref="paper", text = 'dndfs lknsdfs ', showarrow = F, x = 1.3, y = 1, legendtitle = TRUE),
          xaxis = list(showgrid = FALSE),
          yaxis = list(showgrid = FALSE)) 
      
      p
      
      return(p)
      
      
    }
    
  })
  
  
  
  
  # # create table for aic
  # output$mle_table <- renderDataTable({
  #   
  #     country_data <- selected_country()
  #     mle_data <- country_data[[2]]
  #     mle_data <- apply(mle_data, 2, function(x) as.numeric(x))
  #     mle_data <- apply(mle_data, 2, function(x) round(x, 4))
  #     DT::datatable(mle_data, options = list(dom = 't'))
  #     
  #   
  # }) 
  # 
  # create an amendable table for table_1 if input$
  output$data_table_peril <- renderDataTable({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
      country_data <- selected_country()
      DT::datatable(country_data)
    } else {
      NULL
    }
  })  
  
  # uioutput to give action button 
  output$upload_data <- renderUI({
    amend_upload <- input$amend_upload
    if(amend_upload == 'Use preloaded data' | amend_upload == 'Amend preloaded data'){
      NULL
    } else {
      actionButton('uplod_data',
                   'Upload data')
    }
  })
  
  # create a data table  
  output$data_table <- DT::renderDataTable({
    country_data <- selected_country()
    datatable(country_data, options = list(dom='t',ordering=F))
  })
  
  # create a data plot
  output$data_plot <- renderPlot({
    if(is.null(input$advanced)){
      NULL
    } 
    else {
      country_data <- selected_country()
      # get data based on country input
      ggplot(country_data, aes(Year, Loss)) +
        geom_bar(stat = 'identity', fill = 'darkblue',
                 color = 'blue', 
                 alpha = 0.7) +
        labs(title = 'Peril Data')+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 12)) 
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
