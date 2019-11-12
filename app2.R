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
  
  ##########
  # Beginning observeEvent functions for Landing page and showing the 'simulations' tab if advanced user is selected
  # creates a pop up page that the user must accept to acces the app
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
    message('tab_name is: ', tab_name)
    tab_number <- tab_dict %>% filter(name == tab_name) %>% .$number
    message('tab_number is: ', tab_number)
    message(paste0('Selected tab is ', tab_name, '. Number: ', tab_number))
    rv$page <- tab_number
  })
}

shinyApp(ui, server)