library(shiny)
library(tidyverse)
library(lpSolve)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Plot", 
            # Application title
            titlePanel("Finding optimal solution"),
        
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    sliderInput("bins",
                                "2A + 3B = ",
                                min = 800,
                                max = 1200,
                                value = 1200)
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                    plotOutput("distPlot")
                )
            )
        ),
        tabPanel("Changing A",
                 # Application title
                 titlePanel("Sensitivity analysis"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("A",
                                     "Changing A keeping B constant",
                                     min = 0.0,
                                     max = 3.0,
                                     step = 0.1,
                                     value = 2.0)
                     ),
                     mainPanel(
                         plotOutput('ChangingA')
                     )
                 )
        ),
        tabPanel("Changing B",
                 # Application title
                 titlePanel("Sensitivity analysis"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("B",
                                     "Changing B keeping A constant",
                                     min = 2.0,
                                     max = 5.0,
                                     step = 0.1,
                                     value = 3.0)
                     ),
                     mainPanel(
                         plotOutput('ChangingB')
                     )
                 )
        ),
        tabPanel("Max Processing time constraint",
                 # Application title
                 titlePanel("Increasing the maximum processing time constraint"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("proc",
                                     "2A + B = 600 + ",
                                     min = -125,
                                     max = 100,
                                     step = 10,
                                     value = 0)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("Changing_proc"),
                         uiOutput("description_proc")
                     )
                 )
        ),
        tabPanel("Min production constraint",
                 # Application title
                 titlePanel("Increasing the minimum production constraint"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("prod",
                                     "A + B = 350 + ",
                                     min = -50,
                                     max = 125,
                                     step = 10,
                                     value = 0)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot_prod"),
                         uiOutput("description_prod")
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        lpData <- data.frame(A = (0:30)*25, B = (0:30)*25)
        lpData <- lpData %>% mutate(c1 = 125, c2 = 350 - A, c3 = (600-2*A), c4 = (input$bins-2*A)/3)
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4, color = 'red'),size = 1) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= 250),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400)) +
            labs(x = 'Gallons of A', y='Gallons of B') +
            theme_minimal() + theme(legend.position="none")
    })
    output$ChangingA <- renderPlot({
        lpData <- data.frame(A = (0:30)*25, B = (0:30)*25)
        lpData <- lpData %>% mutate(c1 = 125, c2 = 350 - A, c3 = (600-2*A), c4 = 100 -(input$A/3)*(A - 250))
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4, color = 'red'),size = 1) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= 250),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400)) +
            labs(x = 'Gallons of A', y='Gallons of B') +
            theme_minimal() + theme(legend.position="none")
    })
    output$ChangingB <- renderPlot({
        lpData <- data.frame(A = (0:30)*25, B = (0:30)*25)
        lpData <- lpData %>% mutate(c1 = 125, c2 = 350 - A, c3 = (600-2*A), c4 = 100 -(2/input$B)*(A - 250))
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4, color = 'red'),size = 1) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= 250),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400)) +
            labs(x = 'Gallons of A', y='Gallons of B') +
            theme_minimal() + theme(legend.position="none")
    })
    output$Changing_proc <- renderPlot({
        # A matrix of LHS of constraints (except the non negative)
        constraints.LHS <- matrix(c(1,0,
                                    1,1,
                                    2,1,
                                    0,1), nrow = 4, byrow = TRUE)
        
        # A list of RHS of constraints (except the non negative)  
        RHS <- c(125, 350, 600+input$proc,400)
        
        # A list of the constraints directions (except the non negative)  
        constranints_direction  <- c(">=", ">=", "<=", '<=')
        
        # A list of objective function coefficients
        objective.fxn <- c(2,3)
        
        # Find the optimal solution
        optimum <-  lp(direction="min",
                       objective.in = objective.fxn,
                       const.mat = constraints.LHS,
                       const.dir = constranints_direction,
                       const.rhs = RHS,
                       all.int = T,
                       compute.sens = TRUE)
        lpData <- data.frame(A = 100:350, B = 100:350)
        lpData <- lpData %>% mutate(c1 = 125, c2 = (350 - A), c3 = (600-2*A+input$proc), c4 = (optimum$objval-2*A)/3)
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4, color = 'red'),size = 1) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= optimum$solution[1]),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400)) +
            labs(x = 'Gallons of A', y='Gallons of B') +
            theme_minimal() + theme(legend.position="none")
    })
    output$description_proc <- renderPrint({
        # A matrix of LHS of constraints (except the non negative)
        constraints.LHS <- matrix(c(1,0,
                                    1,1,
                                    2,1,
                                    0,1), nrow = 4, byrow = TRUE)
        
        # A list of RHS of constraints (except the non negative)  
        RHS <- c(125, 350, 600+input$proc,400)
        
        # A list of the constraints directions (except the non negative)  
        constranints_direction  <- c(">=", ">=", "<=", '<=')
        
        # A list of objective function coefficients
        objective.fxn <- c(2,3)
        
        # Find the optimal solution
        optimum <-  lp(direction="min",
                       objective.in = objective.fxn,
                       const.mat = constraints.LHS,
                       const.dir = constranints_direction,
                       const.rhs = RHS,
                       all.int = T,
                       compute.sens = TRUE)
        optimum
    })
    output$distPlot_prod <- renderPlot({
        library(lpSolve)
        
        # A matrix of LHS of constraints (except the non negative)
        constraints.LHS <- matrix(c(1,0,
                                    1,1,
                                    2,1,
                                    0,1), nrow = 4, byrow = TRUE)
        
        # A list of RHS of constraints (except the non negative)  
        RHS <- c(125, 350+input$prod, 600,400)
        
        # A list of the constraints directions (except the non negative)  
        constranints_direction  <- c(">=", ">=", "<=", '<=')
        
        # A list of objective function coefficients
        objective.fxn <- c(2,3)
        
        # Find the optimal solution
        optimum <-  lp(direction="min",
                       objective.in = objective.fxn,
                       const.mat = constraints.LHS,
                       const.dir = constranints_direction,
                       const.rhs = RHS,
                       all.int = T,
                       compute.sens = TRUE)
        lpData <- data.frame(A = 100:300, B = 100:300)
        lpData <- lpData %>% mutate(c1 = 125, c2 = (350 +input$prod - A), c3 = (600-2*A), c4 = (optimum$objval-2*A)/3)
        ggplot(lpData, aes(x=A, y=B)) + 
            geom_line(aes(y = c4, color = 'red'),size = 1) +
            geom_ribbon(data=subset(lpData, 125 <= A & A <= optimum$solution[1]),
                        aes(ymin=c2,ymax=c3), fill="blue", alpha="0.5") +
            scale_y_continuous(expand = c(0, 0), limits=c(0,650)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,400)) +
            labs(x = 'Gallons of A', y='Gallons of B') +
            theme_minimal() + theme(legend.position="none")
    })
    output$description <- renderPrint({
        library(lpSolve)
        
        # A matrix of LHS of constraints (except the non negative)
        constraints.LHS <- matrix(c(1,0,
                                    1,1,
                                    2,1,
                                    0,1), nrow = 4, byrow = TRUE)
        
        # A list of RHS of constraints (except the non negative)  
        RHS <- c(125, 350+input$prod, 600,400)
        
        # A list of the constraints directions (except the non negative)  
        constranints_direction  <- c(">=", ">=", "<=", '<=')
        
        # A list of objective function coefficients
        objective.fxn <- c(2,3)
        
        # Find the optimal solution
        optimum <-  lp(direction="min",
                       objective.in = objective.fxn,
                       const.mat = constraints.LHS,
                       const.dir = constranints_direction,
                       const.rhs = RHS,
                       all.int = T,
                       compute.sens = TRUE)
        optimum
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
