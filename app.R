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
   titlePanel("Future Value of Investment on Different Modalities"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
         sliderInput("inital",
                     "Inital Amount:",
                     min = 0,
                     max = 100000,
                     step = 500,
                     value = 1000),
         sliderInput("contrib",
                     "Annual Contribution:",
                     min = 0,
                     max = 50000,
                     step = 500,
                     value = 2000)
     ),
     column(4,
         sliderInput("return",
                     "Return Rate:",
                     min = 0,
                     max = 0.2,
                     step = 0.001,
                     value = 0.05),
         sliderInput("growth",
                     "Growth Rate:",
                     min = 0,
                     max = 0.2,
                     step = 0.001,
                     value = 0.02)
     ),
     column(4, 
      sliderInput("years",
                  "Years:",
                  min = 0,
                  max = 50,
                  step = 1,
                  value = 20),
      selectInput("facet",
                  "Facet?:",
                  choices = c("No", "Yes"))
     )
     ),
   hr(),
   h4("Timlines"),
   br(),
   
      # Show a plot of the generated distribution
      mainPanel(
        column(12, offset = 2,
         plotOutput("distPlot")
        ),
    br(),
    hr(),
    h4("Balances"),
    br(),
        column(12, offset = 2,
        verbatimTextOutput("table")
      )
   )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     library(ggplot2)
     #' @title Future Value
     #' @description calculates the future value of inital investment
     #' @param amount inital investment
     #' @param rate rate of growth
     #' @param years number of years invested
     #' @return computed future value
     future_value <- function(amount = 0, rate = 0, years = 0) {
       return(amount*(1 + rate)^years)
     }
     
     future_value(100,0.05,1)
     future_value(500,0.05,5)
     future_value(1000,0.05,10)
     
     #' @title Future Value of Annuity
     #' @description calculates the future value of annuity from the inital investment
     #' @param contrib amount deposited at the end of each year
     #' @param rate annual rate of return
     #' @param years number of years invested
     #' @return computed future value
     annuity <- function(contrib = 0, rate = 0, years = 0) {
       return(contrib*(((1 + rate)^years - 1)/rate))
     }
     
     annuity(200, 0.05, 1)
     annuity(200, 0.05, 2)
     annuity(200, 0.05, 10)
     
     #' @title Future Value of Growing Annuity
     #' @description calculates the future value of growing annuity from the inital investment
     #' @param contrib inital amount deposited
     #' @param rate annual rate of return
     #' @param growth annual growth rate 
     #' @param years number of years invested
     #' @return computed future value
     growing_annuity <- function(contrib = 0, rate = 0, growth = 0, years = 0) {
       return(contrib*((1 + rate)^years - (1 + growth)^years)/(rate - growth))
     }
     growing_annuity(200, 0.05, 0.03, 1)
     growing_annuity(200, 0.05, 0.03, 2)
     growing_annuity(200, 0.05, 0.03, 10)
     
     no_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       no_contrib[year + 1] <- future_value(input$inital, input$return, year)
     }
     
     fixed_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       fixed_contrib[year + 1] <- future_value(input$inital, input$return, year) + annuity(input$contrib, input$return, year)
     }
     
     growing_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       growing_contrib[year + 1] <- future_value(input$inital, input$return,year) + growing_annuity(input$contrib, input$return, input$growth, year)
     }
     
     modalities <- data.frame("year" = 0:input$years, no_contrib, fixed_contrib, growing_contrib)
     
     modal <- append(append(rep("No Contribution", input$years + 1),rep("Fixed Contribution", input$years + 1)), rep("Growing Contribution", input$years + 1))
     facetted <- data.frame("year" = 0:input$years, append(append(no_contrib, fixed_contrib), growing_contrib), modal)
     names(facetted)[2] <- "balance"
      
      # draw the histogram with the specified number of bins
     if(input$facet == "No") {
      ggplot(data = modalities) + geom_line(aes(x = year, y = no_contrib, color = "No Contribution")) + geom_line(aes(x = year, y = fixed_contrib, color = "Fixed Contribution")) + geom_line(aes(x = year, y = growing_contrib, color = "Growing Contribution")) + labs(x = "Years After Inital Investment", y = "Current Value of Investment", title = "Future Value of Investment for Different Investment Modes" ) 
     }
     else {
       ggplot(data = facetted) + geom_area(aes(x = year, y = balance, color = modal, fill = modal)) + facet_grid(.~modal)
     }
    })
   output$table <- renderPrint({
     
     library(ggplot2)
     #' @title Future Value
     #' @description calculates the future value of inital investment
     #' @param amount inital investment
     #' @param rate rate of growth
     #' @param years number of years invested
     #' @return computed future value
     future_value <- function(amount = 0, rate = 0, years = 0) {
       return(amount*(1 + rate)^years)
     }
     
     future_value(100,0.05,1)
     future_value(500,0.05,5)
     future_value(1000,0.05,10)
     
     #' @title Future Value of Annuity
     #' @description calculates the future value of annuity from the inital investment
     #' @param contrib amount deposited at the end of each year
     #' @param rate annual rate of return
     #' @param years number of years invested
     #' @return computed future value
     annuity <- function(contrib = 0, rate = 0, years = 0) {
       return(contrib*(((1 + rate)^years - 1)/rate))
     }
     
     annuity(200, 0.05, 1)
     annuity(200, 0.05, 2)
     annuity(200, 0.05, 10)
     
     #' @title Future Value of Growing Annuity
     #' @description calculates the future value of growing annuity from the inital investment
     #' @param contrib inital amount deposited
     #' @param rate annual rate of return
     #' @param growth annual growth rate 
     #' @param years number of years invested
     #' @return computed future value
     growing_annuity <- function(contrib = 0, rate = 0, growth = 0, years = 0) {
       return(contrib*((1 + rate)^years - (1 + growth)^years)/(rate - growth))
     }
     growing_annuity(200, 0.05, 0.03, 1)
     growing_annuity(200, 0.05, 0.03, 2)
     growing_annuity(200, 0.05, 0.03, 10)
     
     no_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       no_contrib[year + 1] <- future_value(input$inital, input$return, year)
     }
     
     fixed_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       fixed_contrib[year + 1] <- future_value(input$inital, input$return, year) + annuity(input$contrib, input$return, year)
     }
     
     growing_contrib <- rep(0,input$years)
     for (year in 0:input$years) {
       growing_contrib[year + 1] <- future_value(input$inital, input$return,year) + growing_annuity(input$contrib, input$return, input$growth, year)
     }
     
     modalities <- data.frame("year" = 0:input$years, no_contrib, fixed_contrib, growing_contrib)
     
     modalities
     
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

