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
           verbatimTextOutput('anovaTest'),
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
        reactve$anova.summary <<- summary(aov(dist~class,aov.dist))
        print(reactve$anova.summary)                         
    })
    output$testPlot <- renderPlot({
        f.plot <- function(pop.mean=0, alpha = 0.05, f, df1, df2,
                           label = 'F distribution',title = 'Anova test'){
            # Creating a sample F distribution
            range <- seq(qf(0.0001, df1, df2), qf(0.9999, df1, df2), by = (qf(0.9999, df1, df2)-qf(0.0001, df1, df2))*0.001)
            f.dist <- data.frame(range = range, dist = df(x = range, ncp = pop.mean, df1 = df1, df2 = df2)) %>% 
                dplyr::mutate(H0 = if_else(range <= qf(p = 1-alpha, ncp = pop.mean, df1 = df1,df2 = df2),'Retain', 'Reject'))
            # Plotting sampling distribution and F value with cutoff
            plot.test <- ggplot(data = f.dist, aes(x = range,y = dist)) +
                geom_area(aes(fill = H0)) +
                scale_color_manual(drop = TRUE, values = c('Retain' = "#00BFC4", 'Reject' = "#F8766D"), aesthetics = 'fill') +
                geom_vline(xintercept = f, size = 2) +
                geom_text(aes(x = f, label = paste0('F = ', round(f,3)), y = mean(dist)), colour="blue", vjust = 1.2) +
                labs(x = label, y='Density',  title = title) +
                theme_minimal()+theme(legend.position="bottom")
            plot(plot.test)
        }
        f.plot(f = reactve$anova.summary[[1]]$F[1], df1 = reactve$anova.summary[[1]]$Df[1], df2 = reactve$anova.summary[[1]]$Df[2])
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
