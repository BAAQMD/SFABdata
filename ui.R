# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("San Francisco Air Basin Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date range:"),
      selectInput("parameter", "Parameter:", choices = PARAMETERS),
      selectInput("units", "Units:", choices = UNITS, selected = "007"),
      selectizeInput("sites", label = "Sites:", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Filter ... '))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Trends", plotOutput("tsPlot")),
        tabPanel("Data", dataTableOutput("tsData"))
      )
    )
  )

))
