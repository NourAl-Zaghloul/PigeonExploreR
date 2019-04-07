library(ggplot2)
library(tidyverse)
library(shiny)
library(devtools)

### UI ####
ui <- fluidPage(

  titlePanel(
    "Welcome to pigeon_explore"
  ),

  sidebarLayout(
    sidebarPanel(

      tabsetPanel(
        type = 'tabs',
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
                 checkboxInput(inputId = "head",
                               label = "Head",
                               value = FALSE),
                 tags$hr(),
                 selectInput(inputId = "var_quant",
                             label = "How many variables?",
                             choices = 1:3),
                 uiOutput("xchoice"),
                 conditionalPanel(
                   condition = "input.var_quant >= 2",
                   uiOutput("ychoice")),
                 conditionalPanel(
                   condition = "input.var_quant == 3",
                   uiOutput("zchoice")),
                 radioButtons(inputId = "aes_include",
                              label = "Grouping?",
                              choices = c("Yes", "No"),
                              selected = "No"),
                 conditionalPanel(
                   condition = "input.aes_include == 'Yes'",
                   selectInput(inputId = "aestype",
                               label = "What type of grouping?",
                               choices = c("Color", "Fill", "Size", "Shape", "Linetype", "Facet")),
                   uiOutput("aeschoice")
                   #TODO include number and types of graphs on this menu

                 ),

        #TODO dynamic plots instead of predetermined panels
        # tabPanel("Plots",
        #          uiOutput("mytabs")),

        tabPanel("P1",
                 conditionalPanel(
                   condition = "input.var_quant == 1",
                   selectInput(inputId = "q1p1",
                               label = "Choose your plot",
                               choices = c("Density", "Histogram", "Rug"),
                               selected = NULL)),
                 conditionalPanel(
                   condition = "input.var_quant == 2",
                   selectInput(inputId = "q2p1",
                               label = "Choose your plot",
                               choices = c("Box", "Scatter", "Bar", "Violin"),
                               selected = NULL))
                 ),

        tabPanel("P2",
                 conditionalPanel(
                   condition = "input.var_quant == 1",
                   selectInput(inputId = "q1p2",
                               label = "Choose your plot",
                               choices = c("Density", "Histogram", "Rug"),
                               selected = NULL)),
                 conditionalPanel(
                   condition = "input.var_quant == 2",
                   selectInput(inputId = "q2p2",
                               label = "Choose your plot",
                               choices = c("Box", "Scatter", "Bar", "Violin"),
                               selected = NULL))
                 ),

        tabPanel("P3",
                 conditionalPanel(
                   condition = "input.var_quant == 1",
                   selectInput(inputId = "q1p3",
                               label = "Choose your plot",
                               choices = c("Density", "Histogram", "Rug"),
                               selected = NULL)),
                 conditionalPanel(
                   condition = "input.var_quant == 2",
                   selectInput(inputId = "q2p3",
                               label = "Choose your plot",
                               choices = c("Box", "Scatter", "Bar", "Violin"),
                               selected = NULL))
                 ),

        tabPanel("Theme",
                textInput(inputId = "titletext",
                          label = "titletext",
                          value = "TitleText"),
                 #TODO change axis text
                 #TODO change axis labels
                 #TODO add legend
                selectInput(inputId = "linedraw",
                            label = "Add lines?",
                            choices = c("None", "Horizontal", "Vertical", "Both"),
                            selected = NULL),
                 conditionalPanel(
                   condition = "input.linedraw == 'Horizontal' || input.linedraw == 'Both'",
                   numericInput(inputId = "hline",
                                label = "hline",
                                value = 0)),
                 conditionalPanel(
                   condition = "input.linedraw == 'Vertical' || input.linedraw == 'Both'",
                   numericInput(inputId = "vline",
                                label = "vline",
                                value = 0))

                )
        )
      )

    ),
    ### Main Panel ####
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Table", tableOutput("table")),
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("R code", verbatimTextOutput("ggcode"))
      )
    )
  )
)

### Server ####
server <- function(input, output) {

  ### Reactive functions ####
  rawdf <- reactive({
    req(input$file)
    as.data.frame(read.csv(input$file$datapath,
                           header = input$header))
  })

  var_names <- reactive({
    names(rawdf())
  })

  lbl_names <- reactive({

  })

  ### Var UI Outputs ####

  output$xchoice <- renderUI({
    req(input$file)

    selectInput(inputId = "xchoice",
                label = "Choose x variable",
                choices = var_names())
  })

  output$ychoice <- renderUI({
    req(input$file)

    selectInput(inputId = "ychoice",
                label = "Choose y variable",
                choices = var_names())
  })

  output$zchoice <- renderUI({
    req(input$file)

    selectInput(inputId = "zchoice",
                label = "Choose z variable",
                choices = var_names())
  })

  output$aeschoice <- renderUI({
    req(input$file)

    selectInput(inputId = "aeschoice",
                label = "Choose grouping variable",
                choices = var_names())
  })

  #### Plot UI Outputs ####

  #TODO

  output$densityUI <- renderUI({})

  output$histogramUI <- renderUI({})

  output$rugUI <- renderUI({})


  ### Main Table Outputs ####

  output$table <- renderTable({
    req(input$file)

    if(input$head){
      return(head(rawdf()))
    }else{
      return(rawdf())
    }

  })

  output$plot <- renderPlot({

    if (input$var_quant == 1){

      if (input$q1p1 == "Density"){
        plot1 <- geom_density()
      } else if (input$q1p1 == "Histogram"){
        plot1 <- geom_histogram()
      } else if (input$q1p1 == "Rug"){
        plot1 <- geom_rug()
      }

      if (input$aes_include == "Yes"){

        if (input$aestype == "Color"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], color = rawdf()[,input$aeschoice])) +
            plot1
        } else if (input$aestype == "Fill"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], fill = rawdf()[,input$aeschoice])) +
            plot1
        } else if (input$aestype == "Size"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], size = rawdf()[,input$aeschoice])) +
            plot1
        } else if (input$aestype == "Shape"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], shape = rawdf()[,input$aeschoice])) +
            plot1
        } else if (input$aestype == "Linetype"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], linetype = rawdf()[,input$aeschoice])) +
            plot1
        } else if (input$aestype == "Facet"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice])) +
            plot1 +
            facet_grid(row = input$aeschoice)
        }
      } else {
        dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice])) +
          plot1
      }


    } else if (input$var_quant == 2){
      if(input$q2p1 == "Box"){
        plot1 <- geom_boxplot()
      } else if(input$q2p1 == "Scatter"){
        plot1 <- geom_jitter()
      } else if (input$q2p1 == "Bar"){
        plot1 <- geom_bar()
      } else if (input$q2p1 == "Violin"){
        plot1 <- geom_violin()
      }

      # if(input$q2p2 == "Box"){
      #   plot2 <- geom_boxplot()
      # } else if(input$q2p2 == "Scatter"){
      #   plot2 <- geom_jitter()
      # } else if (input$q2p2 == "Bar"){
      #   plot2 <- geom_bar()
      # } else if (input$q2p2 == "Violin"){
      #   plot2 <- geom_violin()
      # }

      if(input$aes_include == "Yes"){

        if (input$aestype == "Color"){
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice], color = rawdf()[,input$aeschoice]))
        } else if (input$aestype == "Fill") {
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice], fill = rawdf()[,input$aeschoice]))
        } else if (input$aestype == "Size") {
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice], size = rawdf()[,input$aeschoice]))
        } else if (input$aestype == "Shape") {
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice], shape = rawdf()[,input$aeschoice]))
        } else if (input$aestype == "Linetype") {
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice], linetype = rawdf()[,input$aeschoice]))
        } else if (input$aestype == "Facet") {
          dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice])) +
            facet_grid(row = input$aeschoice)
        }
      } else {
        dfplot <- ggplot(rawdf(), aes(x = rawdf()[,input$xchoice], y = rawdf()[,input$ychoice]))
      }
    }

    dfplot <- dfplot + plot1

    return(dfplot)

  })

  output$ggcode <- renderText({
    "Code goes here"
  })
}

### Shiny App####
shinyApp(ui = ui, server = server)
