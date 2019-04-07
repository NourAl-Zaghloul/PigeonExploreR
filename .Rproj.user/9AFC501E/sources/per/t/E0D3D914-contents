#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("ggPigeon",
             tabPanel("Start",
                      sidebarLayout(
                        sidebarPanel(                      
                          fileInput(inputId = "file",
                                    label = "Choose CSV File",
                                    accept = c(
                                      "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                          checkboxInput(inputId = "header",
                                        label = "Header",
                                        value = TRUE),
                          checkboxInput(inputId = "head",
                                        label = "Head",
                                        value = FALSE),
                          uiOutput("xchoice"),
                          uiOutput("ychoice")),
                        mainPanel(tableOutput("table")))),
             tabPanel("Layer",
                      sidebarLayout(
                        sidebarPanel(
                          #### EXAMPLE INPUT
                          sliderInput("bins",
                                      "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30)),
                        mainPanel(
                          plotOutput("distPlot"))))
             )
)

server <- function(input, output) {
  
  # Getting Data from the csv file uploaded
  Data_df <- reactive({
    req(input$file)
    as.data.frame(read.csv(input$file$datapath,
                           header = input$header))
    })
  Data_names <- reactive({
    names(Data_df())
  })

  # Setting up variables
  output$xchoice <- renderUI({
    req(input$file)

    selectInput(inputId = "xchoice",
                label = "Choose x variable",
                choices = Data_names())
  })

  output$ychoice <- renderUI({
    req(input$file)

    selectInput(inputId = "ychoice",
                label = "Choose y variable",
                choices = Data_names())
  })
  
  # Main Table Outputs
  output$table <- renderTable({
    req(input$file)
    if(input$head){
      return(head(Data_df()))
    }else{
      return(Data_df())
    }})
  
  
 #### EXAMPLE STRUCTURE  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

