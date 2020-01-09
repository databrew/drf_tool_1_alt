
## WAS PREVIOUSLY USED TO NAVIGATE TABS, BUT ITS DISABLED NOW, SO THE USER HAS TO USE THE "NEXT" BUTTON
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




## WAS PREVIOULSY USED TO UPLOAD DATA. WILL KEEP IF WE NEED IT AGAIN
# # Reactive dataset
# input_data <- reactive({
#   user_supplied <- input$upload_or_auto
#   if(user_supplied != 'User-supplied data'){
#     NULL
#   } else {
#     message('Going to try to read user-supplied data!')
#     inFile <- input$upload_csv
#     if (is.null(inFile)){
#       NULL
#     } else {
#       out <- read.csv(inFile$datapath)
#       if(is.null(out)){
#         NULL
#       } else {
#         out
#       }
#     }
#   }
# })

# WAS PREVIOUSLY USED TO UPLOAD DATA. NOW WE CAN MANUALLY INPUT. 
# # Upload menu
# observeEvent(input$upload_or_auto, {
#   iu <- input$upload_or_auto
#   if(iu == 'User-supplied data'){
#     showModal(modalDialog(
#       title = "Upload your own data", easyClose = TRUE,
#       fluidPage(uiOutput('ui_upload_type'),
#                 uiOutput('ui_upload_type_text'),
#                 uiOutput('ui_upload_data'))
#     ))
#   }
#   
# })

# WAS PREVIOUSLY USED FOR UPLOADING DATA 


# # Define the upload type options if upload is user-supplied
# output$ui_upload_type <- renderUI({
#   if(input$upload_or_auto == 'User-supplied data'){
#     radioButtons('upload_type',
#                  'Type of data',
#                  choices = c('Loss', 'Cost', 'Population'),
#                  inline = TRUE,
#                  selected = 'Loss')
#   } else {
#     NULL
#   }
# })

# output$ui_upload_type_text <- renderUI({
#   if(input$upload_or_auto == 'User-supplied data'){
#     typey <- input$upload_type
#     if(is.null(typey)){
#       return(NULL)
#     } else {
#       if(typey == 'Loss'){
#         helpText('Upload a csv with the following 4 column headers: "Country", "Year", "Peril", "Loss')
#       } else if(typey == 'Cost'){
#         helpText('Upload a csv with the following 4 column headers: "Country", "Year", "Peril", "Affected')
#       } else if(typey == 'Population'){
#         helpText('Upload a csv with the following 3 column headers: "Country", "Year", "Population"')
#       }
#     }
#   } else {
#     NULL
#   }
# })

# output$ui_upload_data <- renderUI({
#   if(input$upload_or_auto == 'User-supplied data'){
#     fileInput(inputId = 'upload_csv', label = 'Choose a CSV file',accept = c(
#       "text/csv",
#       "text/comma-separated-values,text/plain",
#       ".csv")
#     )
#   } else {
#     NULL
#   }
#   
# })

# observeEvent(input$tabs,{
#   disable('simulations_ui')
# })
# 
# selected_country <- reactive({
#   if(is.null(input$country)){
#     NULL
#   } else {
#     # store input country
#     country_name <- input$country
#     
#     # get a list called country data
#     country_data <- country_data[country_data$country ==country_name,]
#     
#     # If user-supplied, overwrite some data
#     user_supplied <- input$upload_or_auto
#     if(user_supplied == 'User-supplied data'){
#       the_data <- input_data()
#       if(!is.null(the_data)){
#         country_data <- list()
# 
#         # Define which index to overwrite
#         typey <- input$upload_type
#         if(typey == 'Loss'){
#           the_index <- 1
#         } else if(typey == 'Cost'){
#           the_index <- 2
#         } else if(typey == 'Population'){
#           the_index <- 3
#         }
# 
#         # Overwrite the auto data with user-supplied data
#         country_data[[the_index]] <- the_data
#       }
#     }
#     return(country_data)
# 
#   }
# })
