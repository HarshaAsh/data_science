#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse, attach.required = TRUE)
library(caret, attach.required = TRUE)
library(shiny)
library(ggplot2, attach.required = TRUE)
library(rattle, attach.required = TRUE)
library(RANN, attach.required = TRUE)
library(e1071, attach.required = TRUE)

ui <- fluidPage(
    
    title = "Machine learning",
    tags$head(tags$style(".rightAlign{float:right;} .image{width : 50px;height : 50px;}")),
    fluidRow(
        column(6,{
            h1('Machine learning')
        }),
        column(6,{
            a(img(src='https://www.harshaash.website/wp-content/uploads/2018/05/15965520_1432357420169094_8309024494448822162_n-1.jpg', class = 'image'), href = 'https://www.harshaash.website/log-of-posts/', class = 'rightAlign')
        }),
        column(12,
               p('"A computer program is said to learn from experience E with respect to some class of tasks T and performance measure P, if its performance at tasks in T, as measured by P, improves with experience E." - (Tom M. Mitchell, 1997). A general machine learning process is built of three core variables. Let us understand them using the famous titanic problem.')
        )
    ),
    fluidRow(
        column(4,
               h3("Experience"),
               p('Experience is the data that is used to understand the problem in question. Its usually a sample of past observations'),
               p('In this example, a sample is taken at random to change the experience for different runs.'),
               actionButton('goButton', 'Change experience', icon("paper-plane"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               br(),
               plotOutput('distPlot')
        ),
        column(4,
               h3("Task"),
               p('Task is the model used or the action performed on the experience(data) to understand the problem. For classifying survival in titanic, Logistic regression and CART are two models that can be used'),
               selectInput("changeTask", 'Change the task', c('Logistic regression' = 'glm', 'Decision trees' = 'rpart')),
               verbatimTextOutput('task'),
               plotOutput('taskPlot')
        ),
        column(4,
               h3("Performance"),
               p('Performance are the metrics to maximise or minimise that provide the best understanding. This can be used to find optimal tuning parameters in various models. Sensitivity, specificity, accuracy, AUC etc are performance metrics for classification while RMSE, SMAPE etc are regression performance metrics.'),
               verbatimTextOutput('performance')
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    raw_df <- read_csv("titanic.csv")
    colnames(raw_df) <- make.names(colnames(raw_df))
    
    preProcValues <- preProcess(raw_df %>% dplyr::select(-one_of('PassengerId', 'Ticket', 'Cabin', 'Name', 'Survived')),
                                method = c("knnImpute"),
                                k = 20,
                                knnSummary = mean)
    impute_df <- predict(preProcValues, raw_df, na.action = na.pass)
    
    procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
    
    for(i in procNames$col){
        impute_df[i] <- impute_df[i]*preProcValues$std[i]+preProcValues$mean[i] 
    }
    raw_df <- na.omit(impute_df %>% mutate(Survived = as.factor(if_else(Survived==1, 'I', 'O')), Pclass = as.factor(Pclass)) %>% 
                          mutate_if(is.character, as.factor) %>% dplyr::select(-one_of('PassengerId', 'Ticket', 'Cabin', 'Name'))) %>% 
        filter(Fare < 300)
    raw_df$class <- 'Selected'
    rm(list = c('impute_df', 'procNames', 'preProcValues'))
    reactive_values <- reactiveValues(
        raw_df = raw_df,
        model = NULL
    )
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(reactive_values$raw_df, aes(x = Age, y = Fare, color = class)) +
            geom_point()+
            labs(x = 'Age', y='Fare', color = 'Experience',  title = 'Titanic data') +
            theme_minimal()+theme(legend.position="bottom")
    })
    observeEvent(input$goButton,{
        trainList_bi <- createDataPartition(y = unlist(reactive_values$raw_df['Survived']), times = 1,p = 0.5, list = FALSE)
        reactive_values$raw_df$class[-trainList_bi] <- 'Selected'
        reactive_values$raw_df$class[trainList_bi] <- 'Not.selected'
    })
    output$task <- renderPrint({
        modified_df <- reactive_values$raw_df %>% 
            dplyr::filter(class == 'Selected') %>% 
            dplyr::select(-one_of('class'))
        form_2 = as.formula(paste0('Survived ~ .'))
        set.seed(1234)
        objControl <- trainControl(method = "none",
                                   summaryFunction = twoClassSummary,
                                   classProbs = TRUE,
                                   # sampling = 'smote',
                                   savePredictions = TRUE,
                                   verboseIter = FALSE)
        
        reactive_values$model <- train(form_2, data = modified_df,
                                       method = input$changeTask,
                                       trControl = trainControl(method = "cv"),#objControl,
                                       metric = "accuracy"
        )
        if(input$changeTask == 'glm'){
            summary(reactive_values$model$finalModel)
        }
        else{
            "Classification tree"
        }
        
    })
    output$taskPlot <- renderPlot({
        if(input$changeTask == 'glm'){
            NULL
        }
        else{
            fancyRpartPlot(reactive_values$model$finalModel)
        }
    })
    output$performance <- renderPrint({
        caretPredictedClass <- predict(object = reactive_values$model, 
                                       reactive_values$raw_df)
        confusionMatrix(caretPredictedClass,reactive_values$raw_df$Survived)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)