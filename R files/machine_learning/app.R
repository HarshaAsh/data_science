#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(caret)
library(shiny)
library(ggplot2)

setwd('C:\\Users\\Achyuthuni\\Desktop\\attendance\\EDA\\data')
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

ui <- fluidPage(
    
    title = "Machine learning",
    
    fluidRow(
        column(4,
               h2("Experience"),
               actionButton('goButton', 'Change experience'),
               br(),
               plotOutput('distPlot')
        ),
        column(4,
               h2("Task")
               # selectInput('x', 'X', names(dataset)),
               # selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
               # selectInput('color', 'Color', c('None', names(dataset)))
        ),
        column(4,
               h2("Performance")
               # selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
               # selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    reactive_values <- reactiveValues(
        raw_df = raw_df
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
}

# Run the application 
shinyApp(ui = ui, server = server)
