#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Anova simulation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Difference between means:",
                        min = 0,
                        max = 2,
                        value = 0.2, step = 0.01)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput('anovaTest')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        range <- seq(-input$bins-4,input$bins+4, by = 0.1)
        norm.dist <- data.frame(range = range, dist = dnorm(x = range, mean = 0, sd = 1))
        # Plotting sampling distribution and x_bar value with cutoff
        ggplot(data = norm.dist, aes(x = range,y = dist)) +
            stat_function(fun = dnorm, color='grey40',  size = 1, args = list(mean = 0, sd = 1 )) +
            stat_function(fun = dnorm, color='red',  size = 1, args = list(mean = -input$bins, sd = 1 )) +
            stat_function(fun = dnorm, color='green', size = 1, args = list(mean = input$bins, sd = 1 )) +
            labs(x = 'x', y='Density',  title = 'Comparing three means') +
            theme_minimal()+theme(legend.position="bottom")
    })
    output$anovaTest <- renderPrint({
        cat('Anova summary\n')
        set.seed(1)
        norm.dist1 <- data.frame(dist = rnorm(n = 50, mean = 0, sd = 1), class = 'class1')
        norm.dist2 <- data.frame(dist = rnorm(n = 50, mean = -input$bins, sd = 1), class = 'class2')
        norm.dist3 <- data.frame(dist = rnorm(n = 50, mean = input$bins, sd = 1), class = 'class3')
        aov.dist <- rbind(norm.dist1, norm.dist2, norm.dist3)
        summary(aov(dist~class,aov.dist))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
