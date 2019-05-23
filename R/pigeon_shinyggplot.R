

pigeon_shinyggplot <- function(){

  #### Libraries ----
  # library(shiny)
  # library(shinyjs)
  # library(tidyverse)
  # library(quantreg)

  #### UI ----
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    navbarPage("Pigeon ExploreR",
               #### .. ui Data ----
               tabPanel("Data",
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
                                             uiOutput("ychoice"),
                                             conditionalPanel("input.input_quantity == 3",
                                                              uiOutput("zchoice"))),
                            uiOutput("CompleteThemes")),
                          mainPanel(tableOutput("table")))),
               #### .. ui Plot ----
               navbarMenu("Plot",
                          #### .... Layer1 ----
                          tabPanel("Layer1",
                                   sidebarLayout(
                                     sidebarPanel(
                                       tabsetPanel(
                                         #### ...... Plot ----
                                         tabPanel("Plot 1",
                                                  uiOutput("layerPlot_1"),
                                                  uiOutput("layerx_1"),
                                                  conditionalPanel("input.input_quantity > 1",
                                                                   uiOutput("layery_1"),
                                                                   conditionalPanel("input.input_quantity == 3",
                                                                                    uiOutput("layerz_1"))),
                                                  # Uses shinyjs to use a reactive on the server side as a boolean
                                                  conditionalPanel("output.A1",
                                                                   uiOutput("layerChoiceA_1")),
                                                  conditionalPanel("output.B1",
                                                                   uiOutput("layerChoiceB_1"))
                                         ),
                                         #### ...... Aes ----
                                         tabPanel("Aes")
                                       )
                                     ),
                                     mainPanel(
                                       plotOutput("ThePlot")))),
                          #### .... Theme ----
                          tabPanel("Themes",
                                   sidebarLayout(
                                     sidebarPanel(
                                         tabPanel("base",
                                                  uiOutput("Theme_basetheme")),
                                         tabPanel("axis"),
                                         tabPanel("legend"),
                                         tabPanel("panel"),
                                         tabPanel("plot"),
                                         tabPanel("strip")),
                                       mainPanel(
                                         plotOutput("ThePlot_Theme")
                                       )))
               ),
               #### .. R code ----
               tabPanel("R code"),
               #### .. About Page ----
               tabPanel("About")
    )
  )

  server <- function(input, output) {
    #### Reused Inputs ----
    # Setting up reused input options
    PlotNames_1d <- c("Area","Density","Histogram","Dotplot","FreqPoly","qq","histogram_discrete")
    PlotNames_2d <- list(
      "Continuous XY" = c("Point", "Jitter", "Quantile", "Rug", "Smooth", "Label", "Text"),
      "Discrete X, Continuous Y" = c("Columns", "Boxplot", "Dotplot_2d", "Violin"),
      "Discrete XY" = c("Count", ""),
      "Continuous Bivariate Dist." = c("Bin 2d", "Density 2d", "Hex"),
      "Continuous Function"= c("Area_2d", "Line", "Step"),
      "Maps" = c("Map", ""),
      "Error" = c("Crossbar", "Errorbar", "Linerange", "Pointrange")
    )
    PlotNames_3d <- c("Contour", "Raster", "Tile")
    PlotNames_0d <- list(
      "Primitives" = c("Blank", "Curve", "Path", "Polygon", "Rectangle", "Ribbon"),
      "Line Segments" = c("abline", "hline", "vline", "segment", "spoke")
    )

    #TODO: plotChoiceA-C primitives
    PlotAdditions_A <-c("qq", #1d
                     "Label","Text","Dotplot_2d","Map","Crossbar","Errorbar","Linerange","Pointrange") #2d
    PlotAdditions_B <- c("Crossbar","Errorbar","Linerange","Pointrange") #2d
    PlotAdditions_C <- c()
    PlotAdditions_Primitives <- c()
    #TODO: plot aes

    # Complete Themes
    ThemesComplete_Default <- c("Grey", "Black & White", "Linedraw", "Light", "Dark",
                                "Minimal", "Classic", "Void", "Test")


    #### Uploaded Data ----
    # Getting Data from the csv file uploaded
    Data_df <- reactive({
      req(input$file)
      as.data.frame(read.csv(input$file$datapath,
                             header = input$header))
    })
    Data_names <- reactive({
      names(Data_df())
    })

    #### User Variables ----
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
    output$zchoice <- renderUI({
      req(input$file)
      selectInput(inputId = "zchoice",
                  label = "Choose z variable",
                  choices = Data_names())
    })


    # Makes layer1_ui
    output$layerPlot_1 <- renderUI({
      req(input$file)
      selectInput(inputId = "layerPlot_1",
                  label = "Select your plot",
                  choices = if(input$input_quantity == 1){PlotNames_1d} else
                    if(input$input_quantity == 2){PlotNames_2d} else {PlotNames_3d}
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
    output$layerz_1 <- renderUI({
      req(input$file)
      selectInput(inputId = "layerz_1",
                  label = "Choose z variable",
                  choices = Data_names(),
                  selected = input$zchoice)
    })

    # Makes Theme UI
    output$CompleteThemes <- renderUI({
      req(input$file)
      selectInput(inputId = "CompleteThemes",
                  label = "Choose Complete Theme",
                  choices = ThemesComplete_Default)
    })
    output$Theme_basetheme <- renderUI({
      req(input$file)
      selectInput(inputId = "Theme_basetheme",
                  label = "Choose Base Theme",
                  choices = ThemesComplete_Default,
                  selected = input$CompleteThemes)
    })


    #### Reactive Booleans for UI ----
    # Returns a boolean to conditional panel to see if we need aditional options
    output$A1_text <- reactive({
      input$layerPlot_1 %>%
        switch()
    })

    output$A1 <- reactive({
      input$layerPlot_1 %in% PlotAdditions_A
    })
    outputOptions(output, 'A1', suspendWhenHidden = FALSE)
    output$layerChoiceA_1 <- renderUI({
      req(input$file)
      selectInput(inputId = "layerChoiceA_1",
                  label = "Choose setting",
                  choices = Data_names())
    })

    output$B1 <- reactive({
      input$layerPlot_1 %in% PlotAdditions_B
    })
    outputOptions(output, 'B1', suspendWhenHidden = FALSE)
    output$layerChoiceB_1 <- renderUI({
      req(input$file)
      selectInput(inputId = "layerChoiceB_1",
                  label = "Choose setting",
                  choices = Data_names())
    })

    #### Aes Options ----

    #### Theme Options ----

    #### Grouping Options ----

    #### Main Outputs ----
    # Main Table Outputs
    output$table <- renderTable({
      req(input$file)
      if(input$head){
        return(head(Data_df()))
      }else{
        return(Data_df())
      }})

    # Main Plot
    PLOTPLOT <- reactive({
      req(input$layerx_1)

      if(input$input_quantity == 1){
        plot_base <- ggplot2::ggplot(Data_df(), ggplot2::aes_string(x = {input$layerx_1}))
      }else if(input$input_quantity == 2){
        plot_base <- ggplot2::ggplot(Data_df(), ggplot2::aes_string(x = {input$layerx_1},
                                                                    y = {input$layery_1}))
      }else if(input$input_quantity == 3){
        plot_base <- ggplot2::ggplot(Data_df(), ggplot2::aes_string(x = {input$layerx_1},
                                                                    y = {input$layery_1},
                                                                    z = {input$layerz_1}))
      }

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
            "Raster" = ggplot2::geom_raster(aes_string(fill = {input$layerz_1})),
            "Tile" = ggplot2::geom_tile(aes_string(fill = {input$layerz_1})))
      })

      return(plot_base + layer1_input())
    })

    THEMETHEME <- reactive({
      req(input$CompleteThemes)
      if(is_empty(input$Theme_basetheme)){
        input$CompleteThemes %>% switch(
          "Grey" = theme_grey(),
          "Black & White" = theme_bw(),
          "Linedraw" = theme_linedraw(),
          "Light" = theme_light(),
          "Dark" = theme_dark(),
          "Minimal" = theme_minimal(),
          "Classic" = theme_classic(),
          "Void" = theme_void(),
          "Test" = theme_test())
      } else {
        input$Theme_basetheme %>% switch(
          "Grey" = theme_grey(),
          "Black & White" = theme_bw(),
          "Linedraw" = theme_linedraw(),
          "Light" = theme_light(),
          "Dark" = theme_dark(),
          "Minimal" = theme_minimal(),
          "Classic" = theme_classic(),
          "Void" = theme_void(),
          "Test" = theme_test())
      }
    })

    output$ThePlot <- renderPlot({plot(PLOTPLOT() + THEMETHEME())})

    output$ThePlot_Theme <- renderPlot({plot(PLOTPLOT() + THEMETHEME())})
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

pigeon_shinyggplot()
