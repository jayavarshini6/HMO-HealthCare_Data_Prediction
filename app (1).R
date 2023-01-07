#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(tidyverse)
library(lattice)
library(shiny)
library(shinythemes)
ui <- fluidPage(theme = shinytheme("superhero"),
                tabsetPanel(
                  tabPanel(titlePanel("HealthCare Prediction"),
                           sidebarLayout(
                             sidebarPanel(width=2,
                                          h4("Dataset and title"),
                                          fileInput("file", "File input:", accept = c(".csv")),
                                          fileInput("upload_Solution", label="HMO solution file", accept = c(".csv")),
                                          textInput("title", label="Choose a title for your plot", value="Plot"), ## Plot title ##
                                          h4("Choose the type of plot"), ## Text in sidebarPanel ##
                                          selectInput(inputId="Plot_type", label="Which plot do you like to create?", list('',
                                                                                                                           'Histogram', 'Scatterplot', 'Boxplot'), multiple=FALSE), ## Which plot type to create? ##
                                          h4("Choose corresponding variables"), ## Text in sidebarPanel ##
                                          uiOutput("x"), ## Choose variable for the x-axis ##
                                          uiOutput("y"), ## Choose variable for the y-axis ##
                                          uiOutput("hist_type2"), ## Percent, count or density for histogram? ##
                                          uiOutput("group2"), ## Choose grouping variable ##
                                          uiOutput("box_ratio"), ## Choose ratio of the boxes ##
                                          uiOutput("split"), ## Choose (optional) splitting variable(s) ##
                                          h4("Size of the plot"), ## Text in sidebarPanel ##
                                          numericInput(inputId="hei", label="Height", value=800, min=500, max=2000), ## Height ##
                                          numericInput(inputId="wid", label="Width", value=1000, min=500, max=2000), ## Weight ##
                                          submitButton("Update!")
                             ),
                             mainPanel(width=10,
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Plot", plotOutput(outputId = "plot", height="auto", width="auto")),
                                                   tabPanel("Dataset", tableOutput("dataset")),
                                                   tabPanel("Prediction",verbatimTextOutput("txt_results", placeholder = TRUE))
                                       )
                             )
                           )
                  )
                )
)
server <- function(input, output,session) {
  
  ####### Read the dataset -> csv-format required #######
  data1 <- reactive({
    if (is.null(input$file)){
      return(NULL)
    }
    read.csv(input$file$datapath, header=T, sep=",")
  })
  
  ######### Choose variables which should be displayed within the plot ##################
  ## Which variable should be displayed on the x-axis? --> necessary for each plot ##
  output$x <- renderUI({
    if (is.null(data1())) return()
    tagList(
      selectInput(inputId="x", label="Choose a variable for the x-axis", names(data1()),
                  multiple=FALSE)
    )
  })
  ## Which variable should be displayed on the y-axis? --> not necessary for Histogram ##
  output$y <- renderUI({
    if (is.null(data1()) || input$Plot_type == "Histogram") return()
    tagList(
      selectInput(inputId="y", label="Choose a variable for the y-axis", names(data1()),
                  multiple=FALSE)
    )
  })
  ## Percent, count or density for histogram? --> Just necessary if Plot_type=Histogram ##
  output$hist_type2 <- renderUI({
    if (is.null(data1()) || input$Plot_type != "Histogram") return()
    tagList(
      radioButtons(inputId="hist_type", label="Display of percent, count or density?",
                   choices=list('Percent', 'Count', 'Density'))
    )
  })
  ## Group variable? --> Just necessary for Plot_type=Scatterplot ##
  output$group2 <- renderUI({
    if (is.null(data1()) || input$Plot_type != "Scatterplot") return()
    tagList(
      selectInput(inputId="group", label="Choose the grouping variable", multiple=FALSE,
                  names(data1()))
    )
  })
  ## Ratio of the boxes? --> Just necessary for Plot_type=Boxplot ##
  output$group <- renderUI({
    if (is.null(data1()) || input$Plot_type != "Boxplot") return()
    tagList(
      sliderInput(inputId="box_ratio", label="Choose the ratio of the boxes", min=0, max=1,
                  value=0.5, step=0.1)
    )
  })
  ## Additional split variable? --> 1 or 2 splitting variable(s) can be chosen for each graph ##
  output$split <- renderUI({
    if (is.null(data1())) return()
    tagList(
      selectInput(inputId="split", label="Choose one or two variables as splitting variable
 (optional)", multiple=TRUE, names(data1()))
    )
  })
  
  ####### tabPanel 1: Create the plot #######
  
  output$plot <- renderPlot({
    
    if (is.null(data1()) || input$Plot_type == "") return()
    
    # Histogram #
    else if (input$Plot_type == "Histogram") {
      x <- input$x
      split <- input$split
      if (is.null(split)) {
        if (input$hist_type == "Percent") {histogram( ~ data1()[,which(names(data1())==x)],
                                                      data=data1(), xlab=x, main=input$title, type="percent")}
        else if (input$hist_type == "Count") {histogram( ~ data1()[,which(names(data1())==x)],
                                                         data=data1(), xlab=x, main=input$title, type="count")}
        else if (input$hist_type == "Density") {histogram( ~ data1()[,which(names(data1())==x)],
                                                           data=data1(), xlab=x, main=input$title, type="density")}
      }
      else {
        if (length(split)==1) {
          if (input$hist_type == "Percent") {histogram( ~ data1()[,which(names(data1())==x)] |
                                                          get(split), data=data1(), xlab=x, main=input$title, type="percent")}
          else if (input$hist_type == "Count") {histogram( ~ data1()[,which(names(data1())==x)] |
                                                             get(split), data=data1(), xlab=x, main=input$title, type="count")}
          else if (input$hist_type == "Density") {histogram( ~ data1()[,which(names(data1())==x)]
                                                             | get(split), data=data1(), xlab=x, main=input$title, type="density")}
        }
        else if (length(split)==2) {
          if (input$hist_type == "Percent") {histogram( ~ data1()[,which(names(data1())==x)] |
                                                          get(split[1])*get(split[2]), data=data1(), xlab=x, main=input$title, type="percent")}
          else if (input$hist_type == "Count") {histogram( ~ data1()[,which(names(data1())==x)] |
                                                             get(split[1])*get(split[2]), data=data1(), xlab=x, main=input$title, type="count")}
          else if (input$hist_type == "Density") {histogram( ~ data1()[,which(names(data1())==x)]
                                                             | get(split[1])*get(split[2]), data=data1(), xlab=x, main=input$title, type="density")}
        }
      }
    }
    
    # Scatterplot #
    else if (input$Plot_type == "Scatterplot") {
      x <- input$x
      y <- input$y
      group <- input$group
      split <- input$split
      if (is.null(split)) {
        if (group == "No groups") {
          xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)],
                 data=data1(), xlab=x, ylab=y, main=input$title)
        }
        else {
          xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)],
                 groups=data1()[,which(names(data1())==group)], data=data1(), xlab=x, ylab=y,
                 main=input$title, auto.key=TRUE)
        }
      }
      else {
        if (length(split)==1) {
          if (group == "No groups") {
            xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                     get(split), data=data1(), xlab=x, ylab=y, main=input$title)
          }
          else {
            xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                     get(split), groups=data1()[,which(names(data1())==group)], data=data1(),
                   xlab=x, ylab=y, main=input$title, auto.key=TRUE)
          }
        }
        else if (length(split)==2) {
          if (group == "No groups") {
            xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                     get(split[1])*get(split[2]), data=data1(), xlab=x, ylab=y, main=input$title)
          }
          else{
            xyplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                     get(split[1])*get(split[2]), groups=data1()[,which(names(data1())==group)],
                   data=data1(), xlab=x, ylab=y, main=input$title, auto.key=TRUE)
          }
        }
      }
    }
    
    # Boxplot #
    else if (input$Plot_type == "Boxplot") {
      x <- input$x
      y <- input$y
      split <- input$split
      if (is.null(split)) {
        bwplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)],
               data=data1(), box.ratio=input$box_ratio, xlab=x, ylab=y, main=input$title)
      }
      else {
        if (length(split)==1) {
          bwplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                   get(split), data=data1(), box.ratio=input$box_ratio, xlab=x, ylab=y,
                 main=input$title)
        }
        else if (length(split)==2) {
          bwplot(data1()[,which(names(data1())==y)] ~ data1()[,which(names(data1())==x)] |
                   get(split[1])*get(split[2]), data=data1(), box.ratio=input$box_ratio, xlab=x,
                 ylab=y, main=input$title)
        }
      }
    }
  }, height=function(x) input$hei, width=function(x) input$wid) # Height and weight of the plot
  
  
  ####### tabPanel 2: Show the dataset #######
  output$dataset <- renderTable({
    data1()
  })
  
  ###tab3: Prediction###
  getTestData <- reactive({
    req(input$file)
    read_csv(input$file$name)
  })
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({
    req(input$upload_Solution)
    read_csv(input$upload_Solution$name)
  })
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  library(caret); library(kernlab); library(e1071)
  #load a model, do prediction and compute the confusion matrix
  use_model_to_predict <- function(df, df_solution){
    #load the pre-built model, we named it ‘out_model.rda’)
    load(file="our_model.rda")
    #use the model with new data
    svmPred <- predict(our_model, df, type = "raw")
    #show how the model performed
    df_solution$expensive <- as.factor(df_solution$expensive)
    confusionMatrix(svmPred, df_solution$expensive)
  }
}
shinyApp(ui = ui, server = server)
