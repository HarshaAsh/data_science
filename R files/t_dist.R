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
    titlePanel("Student t distribution for 'n'"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "n = ",
                        min = 2,
                        max = 29,
                        value = 5, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           p('This simulation is created by Achyuthuni Sri Harsha'),
           p('Comparing t distribution with a standard normal distribution.'),
           plotOutput("distPlot"),
           verbatimTextOutput('kurtosis')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        library(ggplot2)
        cols <- c("Normal distribution"="darkred","Student t distribution"="darkBlue","BAR"="#62c76b")
        ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
            stat_function(fun = dnorm, aes(color="Normal distribution"),  size = 1, args = list(mean = 0, sd = 1 )) +
            stat_function(fun = dt, aes(color="Student t distribution"),  size = 1, args = list(df = input$n - 1)) +
            labs(x = 'Student t distribution', y='Density') +
            scale_colour_manual(name="Distribution",values=cols) +
            theme_minimal() + theme(legend.position="bottom")
    })
    output$kurtosis <- renderPrint({
        library(moments)
        paste('Kurtosis is ', kurtosis(rt(n = 10000, df = input$n - 1)))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
