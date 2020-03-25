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
    titlePanel("Corona - why social distancing is required"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Change the R0 value to see the number of infected people across time."),
            sliderInput("R0",
                        "Reproduction number",
                        min = 0.5,
                        max = 3,
                        value = 2.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("selected_text"),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
       covid19 <- data.frame(days = 1:15) %>% mutate(new.cases = input$R0^days) %>% mutate(total_cases = cumsum(new.cases))
       ggplot(data = covid19, aes(x=days, y = total_cases)) + geom_line() + xlab('No of days since first case') + ylab('No of cases')+
           theme_minimal()
    })
    output$selected_text <- renderUI({
        a <- "Scientists use R0 (the reproduction number) to describe the intensity of an infectious disease outbreak. R0 is the number of people on an average a person will spread the disease."
        b <- "For example, for corona, on average, a person spreads the disease to two and half other people if no precautions are taken. With active social distancing measures, the R0 of corona can be brought down to 1.6. In certain areas in Wuhan, China has now brought down the R0 to less than 0.75."
        c <- "Change the slider from 2.5 to 1.6 and observe the difference in the number of people that are affected in 15 days. You will understand the importance of social distancing."
        d <- "Dont take this as medical advice. Please consult experts and doctors."
        HTML(paste(a, b, c, d, sep = '<br/>'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
