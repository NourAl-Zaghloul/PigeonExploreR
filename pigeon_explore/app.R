library(shiny)
library(tidyverse)

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
                          conditionalPanel("input.input_quantity > 1",
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
  # Setting up reused input options
  inputNames_1d <- c("Area","Density","Histogram","Dotplot","FreqPoly","qq","histogram_discrete")
  inputNames_2d <- c("Label", "Point", "Jitter", "Quantile", "Rug", "Smooth", "Text", #Continuous XY
                     "Columns", "Boxplot", "Dotplot_2d", "Violin", #Discrete X, Continuous Y
                     "Count", # Discrete XY
                     "Bin 2d", "Density 2d", "Hex", #Continuous Bivariate Dist.
                     "Area_2d", "Line", "Step", #Continuous Function
                     "Map", #Maps
                     "Crossbar", "Errorbar", "Linerange", "Pointrange") #Error
  inputNames_3d <- c("Contour", "Raster", "Tile")
  inputNames_0d <- c("Blank", "Curve", "Path", "Polygon", "Rectangle", "Ribbon", #primitives
                     "abline", "hline", "vline", "segment", "spoke") #line segments
  
  
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
                choices = if(input$input_quantity == 1){inputNames_1d} else
                  if(input$input_quantity == 2){inputNames_2d} else {inputNames_3d}
                  )    
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
      plot_base <- ggplot2::ggplot(Data_df(), ggplot2::aes_string(x = {input$layerx_1},
                                                                  y = {ifelse(input$input_quantity > 1,
                                                                             {input$layery_1}, NULL)}
                                                                  z = {ifelse(input$input_quantity > 2,
                                                                              {input$layerz_1}, NULL)}))
      layer1_input <- reactive({
        input$layerPlot_1 %>% 
        switch(
               "Area" = ggplot2::geom_area(stat = "bin"), #Continuous X
               "Density" = ggplot2::geom_density(),
               "Histogram" = ggplot2::geom_histogram(),
               "Dotplot" = ggplot2::geom_dotplot(),
               "FreqPoly" = ggplot2::geom_freqpoly(),
               "qq" =  ggplot2::geom_qq(),
               "histogram_discrete" = ggplot2::geom_bar(), #Discrete X
               "Label" = ggplot2::geom_label(), #Continuous XY
               "Point" = ggplot2::geom_point(), 
               "Jitter" = ggplot2::geom_jitter(), 
               "Quantile" = ggplot2::geom_quantile(), 
               "Rug" = ggplot2::geom_rug(), 
               "Smooth" = ggplot2::geom_smooth(), 
               "Text" = ggplot2::geom_text(),
               "Columns" = ggplot2::geom_col(),  #Discrete X, Continuous Y
               "Boxplot" = ggplot2::geom_boxplot(), 
               "Dotplot_2d" = ggplot2::geom_dotplot(), 
               "Violin" = ggplot2::geom_violin(),
               "Count" = ggplot2::geom_count(), # Discrete XY
               "Bin 2d" = ggplot2::geom_bin2d(), #Continuous Bivariate Dist.
               "Density 2d" = ggplot2::geom_density2d(), 
               "Hex" = ggplot2::geom_hex(), 
               "Area_2d" = ggplot2::geom_area(), #Continuous Function
               "Line" = ggplot2::geom_line(), 
               "Step" = ggplot2::geom_step(), 
               "Map" = ggplot2::geom_map(), #Maps
               "Crossbar" = ggplot2::geom_crossbar(), #Error
               "Errorbar" = ggplot2::geom_errorbar(), 
               "Linerange" = ggplot2::geom_linerange(), 
               "Pointrange" = ggplot2::geom_pointrange(),
               "Contour" = ggplot2::geom_contour(), #XYZ
               "Raster" = ggplot2::geom_raster(), 
               "Tile" = ggplot2::geom_tile())
      })
      return(plot_base + layer1_input())
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

