library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# Source the data set-up
source('global.R')

# # Create a dictionary of tab names / numbers
tab_dict <- data_frame(number = 1:5,
                       name = toupper(c('Tool settings',
                                'Input',
                                'Data',
                                'Simulations',
                                'Output')))
n_tabs <- nrow(tab_dict)


header <- dashboardHeader(title="Benchmarking tool")
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
      fluidPage(
        tabsetPanel(
          id = 'tabs',
          tabPanel('TOOL SETTINGS', p('A!!!')),
          tabPanel('INPUT', p('B!!!')),
          tabPanel('DATA', p('C!!!')),
          tabPanel('SIMULATIONS', p('D!!!')),
          tabPanel('OUTPUT', p('E!!!'))
      ),
      br(),
      actionButton("prevBtn", "< Previous"),
      actionButton("nextBtn", "Next >")
    )),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Built in partnership with ',
             a(href = 'http://databrew.cc',
               target='_blank', 'Databrew'),
             align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output, session) {
  
  ######### Log-in
  set.seed(122)
  histdata <- rnorm(500)
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "Log-in", easyClose = FALSE, footer = modalButton('Accept'),
      p('Some text')))

  })
  
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
    ))
  })
  
  # Observe the "Get started" button on the log-in page and remove the modal
  observeEvent(input$get_started, {
    removeModal()
    
    showModal(modalDialog(
      title = "Disaster Risk Financing Tool 1", easyClose = FALSE, footer = modalButton('Accept'),
      welcome_modal
    ))
  })
  
  # Define a reactive value which is the currently selected tab number
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < n_tabs)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  
  # Define function for changing the tab number in one direction or the 
  # other as a function of forward/back clicks
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  # Observe the forward/back clicks, and update rv$page accordingly
  observeEvent(input$prevBtn, {
    # Update rv$page
    navPage(-1)
  })
  observeEvent(input$nextBtn, {
    # Update rv$page
    navPage(1)
  })
  
  # Observe any changes to rv$page, and update the selected tab accordingly
  observeEvent(rv$page, {
    tab_number <- rv$page
    tab_name <- tab_dict %>% filter(number == tab_number) %>% .$name
    updateTabsetPanel(session, inputId="tabs", selected=tab_name)
  })
  
  # Observe any click on the left tab menu, and update accordingly the rv$page object
  observeEvent(input$tabs, {
    tab_name <- input$tabs
    tab_number <- tab_dict %>% filter(name == tab_name) %>% .$number
    message(paste0('Selected tab is ', tab_name, '. Number: ', tab_number))
    rv$page <- tab_number
  })
}

shinyApp(ui, server)