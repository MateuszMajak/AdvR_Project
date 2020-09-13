library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(bestNormalize)
library(ggplot2)
library(corrplot)
library(caret)


files <- list.files(pattern="*.csv")
files_list <- lapply(files, read.csv, sep=",", dec=".", header=TRUE, 
                     stringsAsFactors=TRUE)
atp <- do.call(rbind, files_list)

atp <- 
  atp %>%   
  dplyr::select(surface, tourney_level, winner_hand, winner_ht, winner_age, winner_name, winner_rank, winner_rank_points, loser_hand, loser_ht, loser_age, 
                loser_name, loser_rank, loser_rank_points, minutes, w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, w_bpSaved, w_bpFaced, l_ace,
                l_df, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_SvGms, l_bpSaved, l_bpFaced) %>%
  drop_na() %>%
  dplyr::filter(minutes >= 18, #shortest atp tennis match lasted 18 minutes so any time shorter than that is not possible and must be a mistake
                minutes <= 665, #longest documented tennis match lasted 11:05h, so this is an upper bound
                loser_hand != "U") %>% #because of only 1 obs left in this category
  dplyr::filter_if(is.factor, all_vars(.!="")) %>% #this is the way NA's were coded for factors
  dplyr::mutate(., surface = droplevels(surface), winner_hand = droplevels(winner_hand), loser_hand = droplevels(loser_hand))


source("var_selection.R")




ui <- fluidPage(headerPanel("Atp project"),
                
                sidebarLayout(
                  sidebarPanel(tabsetPanel(
                    
                    tabPanel(
                      title = "Data selection",
                      
                      br(),
                      
                      selectInput(
                        inputId = "vary",
                        label = "Select dependent variable",
                        choices = names(atp %>% select_if(is.numeric)),
                        selected = "minutes",
                        multiple = F,
                        selectize = T
                      ),
                      
                      checkboxInput(
                        inputId = "boxcox",
                        label = "Box Cox transformation of dependent variable",
                        value = F,
                        width = "400px"
                      ),
                      
                      checkboxGroupInput(
                        inputId = "varx",
                        label = "Select independent variables",
                        choices = names(atp %>% select(-winner_name,-loser_name)),
                        selected = "w_svpt"
                      ),

                    ),
                    tabPanel(
                      title = "Model selection",
                      
                      h4("Linear model:"),
                      
                      actionButton("showlin", "Yes"),
                      actionButton("hidelin", "No"),
                      
                      h4("KNN:"),
                      
                      actionButton("showknn", "Yes"),
                      actionButton("hideknn", "No"),
                      
                      
                      sliderInput(
                        inputId = "knntune",
                        label = "KNN - range of neighbours:",
                        min = 1,
                        max = 205,
                        value = c(2,6),
                        step = 1,
                        round = TRUE,
                        ticks = FALSE,
                        animate = F
                      ),
                      
                      
                      numericInput(
                        inputId = "cvv",
                        label = "KNN - number of folds in cross-validation",
                        value = 5,
                        min = 0,
                        max = 5
                      )
                    )
                  ),
                      
                      actionButton(
                        inputId = "action1",
                        label = "Apply",
                        icon = icon("calculator"),
                        width = "200px"
                      )
                    
                    
                  ),
                  
                  mainPanel(
                    htmlOutput("txt"),
                    htmlOutput("txt2"),
                    htmlOutput("txtlambda"),
                    htmlOutput("txt3"),
                    
                    tabsetPanel(
                      id = "main",
                      
                      tabPanel(
                        title = "Data",
                        DT::dataTableOutput("tab"),
                        br(),
                        h3("Dependent Variable"),
                        div(htmlOutput("message"),style="color: red;"),
                        plotOutput("plot1"),
                        plotOutput("plot2")
                      ),
                      
                      
                      tabPanel(
                        title = "Linear",
                        verbatimTextOutput("linsum"),
                        verbatimTextOutput("selectknn")
                      ),
                      
                      
                      tabPanel(
                        title = "KNN",
                        verbatimTextOutput("knnsum"),
                        plotOutput("plotknn"),
                      
                      )
                      
                    )
                    
                  )
                ))


server <- function(input, output, session){
  
#  selectedData <- reactive({
#    input$action1
#    data <- isolate(as.data.frame(atp[, c(input$vary, input$varx)]))
#    isolate(if(input$boxcox == T){
#      data[, input$vary] <- boxcox(data[, input$vary])$x.t})
#    data
#    })
 
  

  selectedData <- reactive({
    input$action1
    data <- isolate(var_selection(atp, input$vary, input$varx, input$boxcox))
    data
  })
 
  dataPrepared <- reactive({
    as.data.frame(selectedData()[1])
  })
  
#  selectedData <- reactive({
 #     data_prepared <- atp[, c(input$vary, input$varx)]
#      data_prepared <- as.data.frame(data_prepared)
#      message <- ""
#      if(input$boxcox == T){
#        if(min(data_prepared[, input$vary] == 0)){
#          message <- "The minimum of the dependent variable is equal to 0. All values were changed +1 to execute the Box Cox transformation."
#        }
#        tryCatch(
#          bc <- boxcox(data_prepared[, input$vary]),
#          error = function(c) {
#            bc <- boxcox(data_prepared[, input$vary] + 1)
#          })
#        data_prepared[, input$vary] <- bc$x.t
#      }
#        list(atp, message, bc$lambda)
#        
#      
#    }
#  })  
  
  dep <- reactive({
    input$action1
    str_c(strong("Dependent variable:"), isolate(input$vary), sep = " ")
  })

  box <- reactive({
    input$action1
    str_c(strong("Used Box Cox transformation:"), isolate(input$boxcox), sep = " ")
  })
  
  lambda <- reactive({
    if(!is.null(selectedData()[3])){
      l <- unlist(selectedData()[3]) 
      str_c(strong("Value of lambda:"), l, sep = " ")
    }
  })
  
  ind <- reactive({
    input$action1
    str_c(strong("Independent variables:"), str_c(isolate(input$varx), collapse = ", "), sep = " ")
  })
  

  output$txt <- renderText({dep()})
  
  output$txt2 <- renderText({box()})
  
  output$txtlambda <- renderText({lambda()})
  
  output$txt3 <- renderText({ind()})
 
   output$tab <- DT::renderDataTable({
    dataPrepared()
  })

   
  notification <- reactive({
    if(!is.null(selectedData()[2])){
      unlist(selectedData()[2]) 
    }
  })
  
  
  output$message <- renderText({
    notification()
  })
  
  
   
  output$plot1 <- renderPlot({
    ggplot(dataPrepared(),
           aes_string(x = isolate(input$vary))) +
      geom_histogram(fill = "red",
                     bins = 100) +
      theme_bw()
  
  })
  

  
  cor <- reactive({
    input$action1
    data_numeric <- dataPrepared() %>% select_if(is.numeric)
    stats::cor(data_numeric, use = "pairwise.complete.obs")
  })
  
  output$plot2 <- renderPlot({
    corrplot(cor())
  })
  
  
  linear <- reactive({
    input$action1
    isolate(lm(reformulate(input$varx, input$vary),  
               data = dataPrepared()))
  })
  
  output$linsum <- renderPrint({
    summary(linear())
  })
  
  
  knn <- reactive({
    input$action1
    isolate(train(reformulate(input$varx, input$vary),
                  data = dataPrepared(),
                  method = "knn",
                  trControl = trainControl(method = "cv",
                                           number = input$cvv),
                  tuneGrid = data.frame(k = seq(input$knntune[1], input$knntune[2], 1))
                  ))
  })
  
  output$knnsum <- renderPrint({
    knn()
  })
  
  output$plotknn <- renderPlot({
    plot(knn())
  })
  

  
  observeEvent(
    input$hidelin, {
      hideTab(inputId = "main", target = "Linear")}
    )

  observeEvent(
    input$showlin, {
      showTab(inputId = "main", target = "Linear")}
  )

  observeEvent(
    input$hideknn, {
      hideTab(inputId = "main", target = "KNN")}
  )
  
  observeEvent(
    input$showknn, {
      showTab(inputId = "main", target = "KNN")}
  )
  
  
}



shiny::shinyApp(ui, server)