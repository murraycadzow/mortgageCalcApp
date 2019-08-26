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

e <- NULL

# some examples for loading events
#inc <- seq(12, 360, by = 12)
#e <- tibble::tibble(payment = c(2,inc),
#                    annual_interest = c(NA,rep(c(0.055,0.057,0.059,0.058, 0.056, 0.054),length(inc)/6)),
#                    month_payment = 1800*52/12, principle = c(NA,rep(1000, 30)))
#e <- tibble::tibble(payment = c(2,15,36,72, 120),
#                    annual_interest = c(NA,0.05,0.06,0.065 ,NA),
#                    month_payment = c(5800,7300,7800, NA,3100),
#                    principle = c(NA,-500000,NA,NA, 540000))


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
                   value = 400000
      ),
      numericInput("interest",
                   "Interest rate (%)",
                   min = 0,
                   value = 5,
                  step = 0.05
      ),
      textOutput("weekly_repayment"),
      textOutput("fortnightly_repayment"),
      textOutput("monthly_repayment"),
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


  amort <- reactive({amort_event_table(principle = input$initialprinciple,
                                 annual_interest = input$interest/100,
                                 years = input$years, events = e) %>% mutate(interest_rate = interest_rate *100) %>% mutate_at(-1, funs(round(., 2)))
  })

  monthly_payment_amount <- reactive({monthly_payment(principle = input$initialprinciple,
                                             annual_interest = input$interest/100, years = input$years) })

  output$monthly_repayment <-renderText(
    paste("Monthly repayment amount:",monthly_payment_amount() %>% dollar())
  )

  output$fortnightly_repayment <-renderText(
    paste("Fortnightly repayment amount:",{as.numeric(monthly_payment_amount()) * 12 / 52 * 2} %>% dollar())
  )


  output$weekly_repayment <-renderText(
    paste("Weekly repayment amount:",{monthly_payment_amount() * 12 / 52} %>% dollar())
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
    amort() %>% mutate_at(vars(principle:cumulative_total), funs(sprintf("%.2f", .)))
  }, options = list(
    columnDefs = list(list(className = 'text-right', targets = "_all"))))

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
