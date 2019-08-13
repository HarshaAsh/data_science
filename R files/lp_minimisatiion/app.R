#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 800,
                        max = 1200,
                        value = 1100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        lpData <- data.frame(A = (0:30)*25, B = (0:30)*25)
        lpData <- lpData %>% mutate(c1 = 125, c2 = 350 - A, c3 = (600-2*A), c4 = (input$bins-2*A)/3)
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4)) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= 250),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
