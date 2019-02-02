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
library(scales)
library(mortgageCalc)
library(plotly)

theme_set(theme_minimal())

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
      textOutput("total_interest"),
      textOutput("total")
    ),


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Plots",
                 tabsetPanel(
                   tabPanel(title = "Cumlative",
                            plotOutput("cumulativePlot",
                                       click = "cumulative_click",
                                       hover = hoverOpts(id = "cumulative_hover",
                                                                           delay = 500,
                                                                           clip = TRUE,
                                                                           delayType = c('debounce'))),
                            verbatimTextOutput("hover_info"),
                            verbatimTextOutput("click_info")
                   ),
                   tabPanel(title = "Payment portions",
                            plotOutput("principleInterestPortionPlot")

                   )
                 )
        ),
        tabPanel(title = "Table", dataTableOutput("amort_tab"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  amort <- reactive({amort_table(principle = input$initialprinciple,
                                 annual_interest = input$interest/100,
                                 years = input$years) %>% mutate_at(-1, funs(round(., 2)))
  })

  output$repayment <-renderText(
    paste("Monthly repayment amount:",monthly_payment(principle = input$initialprinciple,
                                                      annual_interest = input$interest/100, years = input$years) %>% dollar())
  )

  output$principleInterestPortionPlot <- renderPlot({
    amort() %>% select(payment, contains("portion")) %>%
      gather( key = "portion", value = "Dollars", contains("portion")) %>%
      ggplot(aes(payment, Dollars, colour = portion,fill = portion,  group = payment)) +
      geom_bar(stat = 'identity', position = 'stack') +
      scale_y_continuous(labels= dollar_format())
  })

  output$cumulativePlot <- renderPlot({
    p<- amort()%>% select(payment, contains("cumulative"), balance) %>%
      gather(key = "category", value = "value", -1 ) %>%
      ggplot(aes(x = payment, y = value, by = category, colour = category)) +
      geom_line() +
      scale_y_continuous(labels= dollar_format())
    if(!is.null(input$cumulative_hover)){
      p <- p + geom_vline(xintercept =input$cumulative_hover$x)
    }
    return(p)
  })

  output$amort_tab <- renderDataTable({
    amort()
  })

  output$total_interest <- renderText({
    paste("Total interest:", amort() %>%
            summarise(dollar(sum(interest_portion))) )
  })

  output$total <- renderText({
    paste("Total:", amort() %>%
            summarise(dollar(sum(month_payment))) )
  })

  output$click_info <- renderPrint({
    cat("input$cumulative_click:\n")
    str(input$cumulative_click)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
