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
library(mortgageCalc)


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
      ),
      textOutput("repayment"),
      textOutput("totals")

    ),


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Cumulative",
                 plotOutput("cumulativePlot")

        ),
        tabPanel(title = "table", dataTableOutput("amort_tab"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$repayment <-renderText(
    paste("Monthly repayment amount:",monthly_payment(principle = input$initialprinciple,
                    annual_interest = input$interest/100, years = input$years) %>% round(.,2))
  )

  amort <- reactive({amort_table(principle = input$initialprinciple,
                       annual_interest = input$interest/100,
                       years = input$years) %>% mutate_at(-1, funs(round(., 2)))
  })

  output$cumulativePlot <- renderPlot({


    amort()%>% select(payment, contains("cumulative")) %>%
      gather(key = "category", value = "value", -1 ) %>%
      ggplot(aes(x = payment, y = value, by = category, colour = category)) + geom_line()
  })

  output$amort_tab <- renderDataTable({
    amort()
  })

  output$totals <- renderText({
    paste(amort() %>%
      summarise_at(vars(contains("portion"), month_payment), sum), collapse = " ")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
