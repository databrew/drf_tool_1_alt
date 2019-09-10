library(shiny)
library(DT)
library(plotly)
library(htmltools)
library(shiny)
source('global.R')

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(theme = "bootstrap.css",
            
            tags$head( tags$style( " .title { background:url('banner.jpg'); background-repeat: no-repeat; background-size: 50% 200%; color: white;
                               font-family: Optima, Segoe, 'Segoe UI', Candara, Calibri, Arial, sans-serif;
	                                font-size: 24px;
                                 font-style: normal;
                                 font-variant: normal;
                                 font-weight: 2000;
                                 line-height: 26.4px} " ) ), 
            tags$head(tags$style(
              type="text/css",
              "#image img {max-width: 100%; width: auto; height: auto}"
            )),
            
            tags$head(tags$style(HTML('
      .modal.in .modal-dialog{
        width:100%;
        height:100%;
        margin:0px;
      }

      .modal-content{
        width:100%;
        height:100%;
      }
    '))),
            
            titlePanel(windowTitle = "Disaster Risk Financin Tool 1)",
                       title =
                         div(
                           img(
                             src = "",
                             height = 0,
                             width = 0,
                             style = "margin:100px 100px"
                           ),
                           "DCMS Tourism Dashboard (alpha v1.0)",
                           class = 'title'
                         )                  
            ),
            
            navbarPage(id = "Navbar",
                       fluid = TRUE,
                       theme = shinythemes::shinytheme("spacelab"),
                       footer = helpText(
                         "Please send feedback to",
                         a(href="blah", target="_blank", "here")
                       ),
                       tags$style(type="text/css", "body {padding-top: 0px;}"),
                       
                       # Show a plot of the generated distribution
                       
                       tabPanel("About",
                                
                                fluidRow(
                                  
                                  #column(3,div(tags$style(
                                  #type="text/css",
                                  # "#image img {max-width: auto; max-height: auto}"
                                  # ),div(img(src = "Sidepanel.png"), id = "image")),tags$br()),
                                  
                                  column(12,tags$p(class = "intro",
                                                   "The development of this Tool was led by the Disaster Risk Financing and Insurance 
                                  Program (DRFIP), a partnership of the World Bank Group's Finance Competitiveness and 
                                  Innovation Global Practice and the Global Facility for Disaster Reduction and Recovery 
                                  (GFDRR)."),
                                         tags$br("The World Bank invests substantial resources in the development of its models, 
                                  modelling methodologies and databases. This Tool contains proprietary and confidential 
                                  information and is intended for the exclusive use of World Bank partners with whom this 
                                  Tool has been shared. Any user is subject to the restrictions of the confidentiality 
                                  provisions set forth in license and other nondisclosure agreements."),
                                         tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                 tags$p("Use the button below to search for data online based on the country you selected")
                                         ),
                                         fluidRow(column(1),column(12,
                                                                   actionButton('Fetch', 'Check for new data'),
                                                                   tags$br(tags$p(" "))
                                         ))))),
                       navbarMenu("User inputs",
                                  tabPanel("Basic mode",
                                           fluidRow(column(4,
                                                           div(class = "well",selectInput("country","Choose a country",choices = c("Afghanistan","Malaysia")),
                                                               uiOutput("overall_rng"),
                                                               tags$p(tags$b("Click on the below buttons to download elements of this page:")),
                                                               fluidRow(column(12,downloadButton("downloadEconomicPlot","Download data"),
                                                                               downloadButton("downloadEconomicTable","Upload data"))
                                                               )),
                                                           fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                                            
                                                                            tags$p(class = "intro-divider",tags$b("Notes:"),
                                                                                   tags$p("1. help text"),
                                                                                   tags$p("2. help text"))),
                                                                    tags$p(class = "intro-divider",tags$b("Sources:"),
                                                                           htmlOutput("EconomicSources")
                                                                    ))
                                           ),
                                           column(8,
                                                  plotly::plotlyOutput("EconomicPlot",height = "auto"),
                                                  DT::dataTableOutput("EconomicTable"),
                                                  tags$br(tags$p(paste0("Contains National Statistics data ","\u00A9"," Crown copyright and database right ",format(Sys.Date(), "%Y"))))
                                           ) 
                                           ), fluidRow(tags$br())),
                                  
                                  tabPanel("Advanced mode",id = "VSTab",
                                           fluidRow(column(4,
                                                           div(class = "well",selectInput("VisitSpendDT","Choose a country",choices = c("Afghanistan", 'Malaysia')),
                                                               numericInput("VisitSpendPeriod","Enter conversion rate", value = 1),
                                                               
                                                               ##This is style tag just to hide red shiny output errors
                                                               
                                                               tags$style(type="text/css",
                                                                          ".shiny-output-error { visibility: hidden; }",
                                                                          ".shiny-output-error:before { visibility: hidden; }"),uiOutput("visitspend_rng"),
                                                               uiOutput("visitspend_tpq"),
                                                               uiOutput("visitspend_tpm"),
                                                               tags$p(tags$b("Click on the below buttons to download elements of this page:")),
                                                               fluidRow(column(12,downloadButton("downloadVisitSpendPlot","Download Data "),
                                                                               downloadButton("downloadVisitSpendTable","Upload data"))
                                                               )),
                                                           fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                                            
                                                                            tags$p(class = "intro-divider",tags$b("Notes:"),
                                                                                   tags$p("1. Help text"),
                                                                                   tags$p("2. Help text"),
                                                                                   
                                                                                   htmlOutput("VisitSpendNotes"))),
                                                                    tags$p(class = "intro-divider",tags$b("Sources:"),
                                                                           htmlOutput("VisitSpendSources")
                                                                    ))
                                           ),
                                           column(8,
                                                  htmlOutput("GBDVNote"),
                                                  plotly::plotlyOutput("VisitSpendPlot"),
                                                  tags$br(),
                                                  DT::dataTableOutput("VisitSpendTable"),
                                                  tags$br(tags$p(paste0(" ","\u00A9"," Crown copyright ",format(Sys.Date(), "%Y"))))
                                           ) 
                                           ), fluidRow(tags$br()))
                       ),
                       
                       tabPanel("Simulations"
                                # column(4,div(class = "well",radioButtons("MapInputData","Please choose a metric:",c("GVA" = "GVA","Employment" = "Employment")),
                                #              tags$p(tags$b("Click on a region for more information and then click on either of the buttons below to download the map and/or table:")),
                                #              fluidRow(column(12,downloadButton("downloadRegionalPlot","Download Map "),
                                #                              downloadButton("downloadRegionalTable","Download Table"))
                                #              ),tags$br(tags$b("Please note that the map may take a few seconds to download"))),
                                #        fluidRow(tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                #                         
                                #                         tags$p(class = "intro-divider",tags$b("Notes:"),
                                #                                tags$p("1. GVA figures and spend figures are expressed in current prices (i.e. not accounting for inflation)"),
                                #                                tags$p("2. A definition of Tourism is provided on the 'More Information' panel"))),
                                #                 tags$p(class = "intro-divider",tags$b("Sources:"),
                                #                        htmlOutput("RegionalSources")
                                #                 ))
                                #        
                                # ),
                                
                       ),
                       tabPanel("Output",
                                column(6,div(class = "well",tags$h5(tags$b("Output 1")),
                                             tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                     tags$ol(class = "intro-divider")
                                             ), plotlyOutput('example_1'))),
                                
                                column(6,div(class = "well",tags$h5(tags$b("Output 2")),
                                             tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                     tags$ol(class = "intro-divider")
                                             ),  plotlyOutput('example_2'))),
                                
                                column(6,div(class = "well",tags$h5(tags$b("Output 3")),
                                             tags$hr(style="border-color: red;border-top: 3px solid #F511BC;",
                                                     tags$ol(class = "intro-divider")
                                             ), plotlyOutput('example_3'))),
                                
                                column(6,div(class = "well",tags$h5(tags$b("Output 4")),
                                             tags$hr(style="border-color: red;border-top: 3px solid #F511BC;"),
                                             plotlyOutput('example_4'))))
                       
                       
                       
            )
            
            
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
  
  
  output$example_1 <- renderPlotly({
   
  })
  
  output$example_2 <- renderPlotly({
   
  })
  
  output$example_3 <- renderPlotly({
    

    
  })
  
  
  output$example_4 <- renderPlotly({
  
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
  