#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(dplyr)
library(ggplot2)
source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Mortgage Calc"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Number of years:",
                  min = 1,
                  max = 30,
                  value = 30),
      numericInput("initialprinciple",
                   "Initial principle",
                   min = 0,
                   value = 100000
      ),
      numericInput("interest",
                   "Interest rate (%)",
                   min = 0,
                   value = 5
      )

    ),


    # Show a plot of the generated distribution
    mainPanel(
      textOutput("initialrepayment"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$initialrepayment <-renderText(
    monthly_payment(principle = input$initialprinciple, annual_interest = input$interest/100, years = input$years)
  )


  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$years + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
