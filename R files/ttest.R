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
    titlePanel("t test simulation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Difference between means:",
                        min = 0,
                        max = 2,
                        value = 0.37, step = 0.01),
            p('This simulation is created by Achyuthuni Sri Harsha'),
            a("Visit my blog for more", href="http://www.harshaash.website/hypothesis-test-for-population-parameters/")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput('t.test'),
            plotOutput("testPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    reactve <- reactiveValues(anova.summary = c())
    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        range <- seq(-input$bins-4,input$bins+4, by = 0.1)
        norm.dist <- data.frame(range = range, dist = dnorm(x = range, mean = 0, sd = 1))
        # Plotting sampling distribution and x_bar value with cutoff
        ggplot(data = norm.dist, aes(x = range,y = dist)) +
            stat_function(fun = dnorm, color='red',  size = 1, args = list(mean = -input$bins/2, sd = 1 )) +
            stat_function(fun = dnorm, color='green', size = 1, args = list(mean = input$bins/2, sd = 1 )) +
            labs(x = 'x', y='Density',  title = 'Comparing sample means') +
            theme_minimal()+theme(legend.position="bottom")
    })
    output$t.test <- renderPrint({
        cat('T test summary\n')
        set.seed(1)
        norm.dist2 <- data.frame(dist = rnorm(n = 50, mean = input$bins/2, sd = 1), class = 'class2')
        norm.dist3 <- data.frame(dist = rnorm(n = 50, mean = -input$bins/2, sd = 1), class = 'class3')
        reactve$t.test.summary <<- t.test(norm.dist2$dist, norm.dist3$dist)
        print(reactve$t.test.summary)                         
    })
    output$testPlot <- renderPlot({
        t.plot <- function(pop.mean=0, alternative = 'two.sided', alpha = 0.05, t.score, df,
            label = 'Student t distribution',title = 't-test'){
        # Creating a sample normal distribution
        range <- seq(pop.mean - 4, pop.mean + 4, by = 0.001)
        t.dist <- data.frame(range = range, dist = dt(x = range, ncp = pop.mean, df = df)) %>% 
            dplyr::mutate(H0 = case_when(alternative == 'two.sided' ~ if_else((range <= qt(p = 1-alpha/2,
                                                                                           ncp = pop.mean,
                                                                                           df = df,
                                                                                           lower.tail = TRUE)) & 
                                                                                  (range >= qt(p = 1-alpha/2, 
                                                                                               ncp = pop.mean, 
                                                                                               df = df,
                                                                                               lower.tail = FALSE)),
                                                                              'Retain', 'Reject'),
                                         alternative == 'greater' ~ if_else(range <= qt(p = 1-alpha, 
                                                                                        ncp = pop.mean, 
                                                                                        df = df,
                                                                                        lower.tail = TRUE),
                                                                            'Retain', 'Reject'),
                                         alternative == 'less' ~ if_else(range >= qt(p = 1-alpha, 
                                                                                     ncp = pop.mean, 
                                                                                     df = df, 
                                                                                     lower.tail = FALSE),
                                                                         'Retain', 'Reject')))
            # Plotting sampling distribution and x_bar value with cutoff
            ggplot(data = t.dist, aes(x = range,y = dist)) +
                geom_area(aes(fill = H0)) +
                scale_color_manual(drop = TRUE, values = c('Retain' = "#00BFC4", 'Reject' = "#F8766D"), aesthetics = 'fill') +
                geom_vline(xintercept = t.score, size = 2) +
                geom_text(aes(x = t.score, label = paste0('t statistic = ', round(t.score,3)), y = mean(dist)), colour="blue", vjust = 1.2) +
                labs(x = label, y='Density',  title = title) +
                theme_minimal()+theme(legend.position="bottom")
        }
        t.plot(t.score = reactve$t.test.summary$statistic, df = reactve$t.test.summary$parameter)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
