#### Libraries ----
library(shiny)
library(shinyjs)
library(tidyverse)
library(quantreg)

#### Always variables ----
plotTypes <- c("Histogram", "Density", "Histogram_discrete")
data <- iris

#### UI ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("PlotType", "What Type of Plot?",
                  plotTypes),
      selectInput("PlotX", "What X variable?",
                  names(data)),
      conditionalPanel("input.PlotType == 'Histogram'",
                       uiOutput("ChoiceA"))
    ),
    mainPanel(
      plotOutput("PLOT")
    )
  )
)


#### Server ----
server <- function(input, output) {

  plot_layer <- reactive({
    input$PlotType %>% switch(
      "Histogram" = geom_histogram(binwidth = {input$ChoiceA}),
      "Density" = geom_density(binwidth = {input$ChoiceA}),
      "Histogram_discrete" = geom_bar()
    )
  })

  output$ChoiceA <- renderUI({
    sliderInput("ChoiceA", "Choose binwidth",
                min = sd(data[, c(input$PlotX)])/mean(data[, c(input$PlotX)]) * .5, max = IQR(data[, c(input$PlotX)]), value = sd(data[, c(input$PlotX)])/mean(data[, c(input$PlotX)]) * 2)
  })

  output$PLOT <- renderPlot({ggplot(data, aes_string(x = {input$PlotX})) + plot_layer()})
}

# Run the application
shinyApp(ui = ui, server = server)



#### Reorganize Pigeon ----

# Current: Simple Version (Only Base Tab)
pigeon_shinyggplot <- function(){



  #### UI ----
  ui <- fluidPage(
    navbarPage("ggPigeon",
               tabPanel("Base",
                        sidebarLayout(sidebarPanel(
                          fileInput(inputId = "file",
                                    label = "Choose CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                          checkboxInput(inputId = "header",
                                        label = "Header",
                                        value = TRUE),
                          tags$hr,
                          sliderInput("input_quantity",
                                      "How many variables?",
                                      1,3,1),
                          uiOutput("xchoice"),
                          conditionalPanel("input.input_quantity > 1",
                                           uiOutput("ychoice"),
                                           conditionalPanel("input.input_quantity == 3",
                                                            uiOutput("zchoice"))),
                          # tags$hr,
                          # uiOutput("Layer1_choice"),
                          # tags$hr,
                          uiOutput("CompleteThemes")),
                          mainPanel(plotOutput("BasePlot")))),
               navbarMenu("Layers"),
               navbarMenu("Themes"),
               tabPanel("End Results")
               )
  )

  #### Server ----
  server <- function(input, output) {
    PlotNames_1d <- c("Area","Density","Histogram","Dotplot","FreqPoly","qq","histogram_discrete")
    PlotNames_2d <- list(
      "Continuous XY" = c("Point", "Jitter", "Quantile", "Rug", "Smooth", "Label", "Text"),
      "Discrete X, Continuous Y" = c("Columns", "Boxplot", "Dotplot_2d", "Violin"),
      "Discrete XY" = c("Count", ""),
      "Continuous Bivariate Dist." = c("Bin 2d", "Density 2d", "Hex"),
      "Continuous Function"= c("Area_2d", "Line", "Step"),
      "Maps" = c("Map", ""),
      "Error" = c("Crossbar", "Errorbar", "Linerange", "Pointrange"))
    PlotNames_3d <- c("Contour", "Raster", "Tile")
    PlotNames_0d <- list(
      "Primitives" = c("Blank", "Curve", "Path", "Polygon", "Rectangle", "Ribbon"),
      "Line Segments" = c("abline", "hline", "vline", "segment", "spoke"))

    lal <- reactive({
      req(input$file)
      as.data.frame(read.csv(input$file$datapath, header = input$header))})
    lal_names <- reactive({
      req(input$file)
      colnames(lal())})

    output$xchoice <- renderUI({
      selectInput(inputId = "xchoice",
                  label = "Choose x variable",
                  choices = lal_names())
    })
    output$ychoice <- renderUI({
      selectInput(inputId = "ychoice",
                  label = "Choose y variable",
                  choices = lal_names())
    })
    output$zchoice <- renderUI({
      selectInput(inputId = "zchoice",
                  label = "Choose z variable",
                  choices = lal_names())
    })

    output$Layer1_choice <- renderUI({
      req(input$file)
      selectInput(inputId = "Layer1_choice",
                  label = "Choose Plot",
                  choices = if(input$input_quantity == 1){PlotNames_1d} else
                    if(input$input_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })


  }

  shinyApp(ui, server)

}

pigeon_shinyggplot()
