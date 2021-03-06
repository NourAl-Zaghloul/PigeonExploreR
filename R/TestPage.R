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
      "Density" = geom_density(),
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
                          tabsetPanel(
                            tabPanel("Data",
                                     fileInput(inputId = "file",
                                               label = "Choose CSV File",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                                     checkboxInput(inputId = "header",
                                                   label = "Header",
                                                   value = TRUE),
                                     tags$hr(),
                                     sliderInput("variable_quantity",
                                                 "How many variables?",
                                                 1,3,1),
                                     uiOutput("xchoice"),
                                     conditionalPanel("input.variable_quantity > 1",
                                                      uiOutput("ychoice"),
                                                      conditionalPanel("input.variable_quantity == 3",
                                                                       uiOutput("zchoice")))),
                            tabPanel("Plotting",
                                     sliderInput("layer_quantity",
                                                 "How many layers?",
                                                 1,5,1),
                                     uiOutput("Layer1_choice"),
                                     conditionalPanel("input.layer_quantity > 1",
                                                      uiOutput("Layer2_choice"),
                                                      conditionalPanel("input.layer_quantity > 2",
                                                                       uiOutput("Layer3_choice"),
                                                                       conditionalPanel("input.layer_quantity > 3",
                                                                                        uiOutput("Layer4_choice"),
                                                                                        conditionalPanel("input.layer_quantity > 4",
                                                                                                         uiOutput("Layer5_choice"))))),
                                     tags$hr(),
                                     uiOutput("CompleteThemes_base")))),
                          mainPanel(plotOutput("BasePlot")))
               ),
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

    Themes_default <- c("Grey", "Black & White", "Linedraw", "Light", "Dark",
                        "Minimal", "Classic", "Void", "Test")

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
                  label = "Choose First Layer",
                  choices = if(input$variable_quantity == 1){PlotNames_1d} else
                    if(input$variable_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })
    output$Layer2_choice <- renderUI({
      req(input$file)
      selectInput(inputId = "Layer2_choice",
                  label = "Choose Second Layer",
                  choices = if(input$variable_quantity == 1){PlotNames_1d} else
                    if(input$variable_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })
    output$Layer3_choice <- renderUI({
      req(input$file)
      selectInput(inputId = "Layer3_choice",
                  label = "Choose Third Layer",
                  choices = if(input$variable_quantity == 1){PlotNames_1d} else
                    if(input$variable_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })
    output$Layer4_choice <- renderUI({
      req(input$file)
      selectInput(inputId = "Layer4_choice",
                  label = "Choose Fourth Layer",
                  choices = if(input$variable_quantity == 1){PlotNames_1d} else
                    if(input$variable_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })
    output$Layer5_choice <- renderUI({
      req(input$file)
      selectInput(inputId = "Layer5_choice",
                  label = "Choose Fifth Layer",
                  choices = if(input$variable_quantity == 1){PlotNames_1d} else
                    if(input$variable_quantity == 2){PlotNames_2d} else {PlotNames_3d})
    })

    output$CompleteThemes_base <- renderUI({
      req(input$file)
      selectInput(inputId = "CompleteThemes_base",
                  label = "Choose Base Theme",
                  choices = Themes_default)
    })

    output$BasePlot <- renderPlot({
      req(input$Layer1_choice)
      base_plot <- input$variable_quantity %>% switch(
        "1" = ggplot(lal(), aes_string(x = {input$xchoice})),
        "2" = ggplot(lal(), aes_string(x = {input$xchoice}, y = {input$ychoice})),
        "3" = ggplot(lal(), aes_string(x = {input$xchoice}, y = {input$ychoice}, z = {input$zchoice})))
      base_layers <- input$Layer1_choice %>% switch(
        "Area" = ggplot2::geom_area(stat = "bin", alpha = .8), #Continuous X
        "Density" = ggplot2::geom_density(alpha = .8),
        "Histogram" = ggplot2::geom_histogram(alpha = .8),
        "Dotplot" = ggplot2::geom_dotplot(alpha = .8),
        "FreqPoly" = ggplot2::geom_freqpoly(alpha = .8),
        "qq" =  ggplot2::geom_qq(alpha = .8),
        "histogram_discrete" = ggplot2::geom_bar(alpha = .8), #Discrete X
        "Label" = ggplot2::geom_label(alpha = .8), #Continuous XY
        "Point" = ggplot2::geom_point(alpha = .8),
        "Jitter" = ggplot2::geom_jitter(alpha = .8),
        "Quantile" = ggplot2::geom_quantile(alpha = .8),
        "Rug" = ggplot2::geom_rug(alpha = .8),
        "Smooth" = ggplot2::geom_smooth(alpha = .8),
        "Text" = ggplot2::geom_text(alpha = .8),
        "Columns" = ggplot2::geom_col(alpha = .8),  #Discrete X, Continuous Y
        "Boxplot" = ggplot2::geom_boxplot(alpha = .8),
        "Dotplot_2d" = ggplot2::geom_dotplot(alpha = .8),
        "Violin" = ggplot2::geom_violin(alpha = .8),
        "Count" = ggplot2::geom_count(alpha = .8), # Discrete XY
        "Bin 2d" = ggplot2::geom_bin2d(alpha = .8), #Continuous Bivariate Dist.
        "Density 2d" = ggplot2::geom_density2d(alpha = .8),
        "Hex" = ggplot2::geom_hex(alpha = .8),
        "Area_2d" = ggplot2::geom_area(alpha = .8), #Continuous Function
        "Line" = ggplot2::geom_line(alpha = .8),
        "Step" = ggplot2::geom_step(alpha = .8),
        "Map" = ggplot2::geom_map(alpha = .8), #Maps
        "Crossbar" = ggplot2::geom_crossbar(alpha = .8), #Error
        "Errorbar" = ggplot2::geom_errorbar(alpha = .8),
        "Linerange" = ggplot2::geom_linerange(alpha = .8),
        "Pointrange" = ggplot2::geom_pointrange(alpha = .8),
        "Contour" = ggplot2::geom_contour(alpha = .8), #XYZ
        "Raster" = ggplot2::geom_raster(aes_string(fill = {input$zchoice}), alpha = .8),
        "Tile" = ggplot2::geom_tile(aes_string(fill = {input$zchoice}), alpha = .8))
      base_theme <- input$CompleteThemes_base %>% switch(
        "Grey" = theme_grey(),
        "Black & White" = theme_bw(),
        "Linedraw" = theme_linedraw(),
        "Light" = theme_light(),
        "Dark" = theme_dark(),
        "Minimal" = theme_minimal(),
        "Classic" = theme_classic(),
        "Void" = theme_void(),
        "Test" = theme_test()
      )
      return(base_plot + base_layers + base_theme)
    })


  }

  shinyApp(ui, server)

}

pigeon_shinyggplot()
