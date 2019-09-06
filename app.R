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
  
  output$example_1 <- renderPlotly({
    p1 <- dat %>%
      plot_ly(
        x = ~gdpPercap,
        y = ~lifeExp,
        size = ~pop,
        color = ~continent,
        frame = ~year,
        text = ~country,
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      ) %>%
      layout(
        xaxis = list(
          type = "log"
        )
      )
    return(p1)
    # Download
  })
  
  output$example_2 <- renderPlotly({
    p <- plot_ly(ds, x = ~Date) %>%
      add_lines(y = ~AAPL.Adjusted, name = "Apple") %>%
      add_lines(y = ~MSFT.Adjusted, name = "Microsoft") %>%
      layout(
        title = "Stock Prices",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "Price"))
    p
    
  })
  
  output$example_3 <- renderPlotly({
    
    p <- plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~revenue, marker = list(color = 'rgba(55, 128, 191, 0.7)',
                                            line = list(color = 'rgba(55, 128, 191, 0.7)',
                                                        width = 2))) %>%
      add_trace(y = ~costs, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                          line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                      width = 2))) %>%
      add_trace(y = ~profit, marker = list(color = 'rgba(50, 171, 96, 0.7)',
                                           line = list(color = 'rgba(50, 171, 96, 1.0)',
                                                       width = 2))) %>%
      layout(title = 'Annual Profit - 2015',
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             barmode = 'stack',
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)',
             showlegend = FALSE) %>%
      add_annotations(text = text,
                      x = x,
                      y = y,
                      xref = "x",
                      yref = "y",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(245, 246, 249, 1)'),
                      showarrow = FALSE)
    
  })
  
  
  output$example_4 <- renderPlotly({
    p <- plot_ly(data_2, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
                 marker = list(color = 'rgba(38, 24, 74, 0.8)',
                               line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
      add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
      add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
      add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = 'stack',
             paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
             margin = list(l = 120, r = 10, t = 140, b = 80),
             showlegend = FALSE) %>%
      # labeling the y-axis
      add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                      xanchor = 'right',
                      text = y,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE, align = 'right') %>%
      # labeling the percentages of each bar (x_axis)
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 / 2, y = y,
                      text = paste(data_2[,"x1"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 / 2, y = y,
                      text = paste(data_2[,"x2"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 / 2, y = y,
                      text = paste(data_2[,"x3"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 + x4 / 2, y = y,
                      text = paste(data_2[,"x4"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                      text = paste(data_2[,"x5"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      # labeling the first Likert scale (on the top)
      add_annotations(xref = 'x', yref = 'paper',
                      x = c(21 / 2, 21 + 30 / 2, 21 + 30 + 21 / 2, 21 + 30 + 21 + 16 / 2,
                            21 + 30 + 21 + 16 + 12 / 2),
                      y = 1.15,
                      text = top_labels,
                      font = list(family = 'Arial', size = 12,
                                  color = 'rgb(67, 67, 67)'),
                      showarrow = FALSE)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
  