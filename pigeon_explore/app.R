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
                          sliderInput("input_quantity",
                                      "How many variables?",
                                      1,3,2),
                          uiOutput("xchoice"),
                          conditionalPanel("input.input_quantity > 1",
                                           uiOutput("ychoice"))),
                        mainPanel(tableOutput("table")))),
             tabPanel("Layer",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("layerPlot_1"),
                          uiOutput("layerx_1"),
                          conditionalPanel("input.inputquantity > 1",
                                           uiOutput("layery_1"))
                        ),
                        mainPanel(
                          plotOutput("density")))),
             tabPanel("Themes"),
             tabPanel("R code"),
             tabPanel("Extras")
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
  
  # Makes layer1_ui
  output$layerPlot_1 <- renderUI({
    req(input$file)
    selectInput(inputId = "layerPlot_1",
                label = "Select your plot",
                choices = c("Area","Density","Histogram","Dotplot","FreqPoly","qq","histogram_discrete"))    
  })
  output$layerx_1 <- renderUI({
    req(input$file)
    selectInput(inputId = "layerx_1",
                label = "Choose x variable",
                choices = Data_names(),
                selected = input$xchoice)
  })
  output$layery_1 <- renderUI({
    req(input$file)
    selectInput(inputId = "layery_1",
                label = "Choose y variable",
                choices = Data_names(),
                selected = input$ychoice)
  })
  
  # Main Table Outputs
  output$table <- renderTable({
    req(input$file)
    if(input$head){
      return(head(Data_df()))
    }else{
      return(Data_df())
    }})
  
  # Practice Plot
  output$density <- renderPlot({
    req(input$layerx_1)
      plot_base <- ggplot2::ggplot(Data_df(), ggplot2::aes_string(x = {input$layerx_1}))
      layer1_input <- reactive({
        switch(input$layerPlot_1,
               "Area" = ggplot2::geom_area(stat = "bin"),
               "Density" = ggplot2::geom_density(),
               "Histogram" = ggplot2::geom_histogram(),
               "Dotplot" = ggplot2::geom_dotplot(),
               "FreqPoly" = ggplot2::geom_freqpoly(),
               "qq" =  ggplot2::geom_qq(),
               "histogram_discrete" = ggplot2::geom_bar())
      })
      return(plot_base + layer1_input())
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

