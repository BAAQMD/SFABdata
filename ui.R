# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(

  # Application title
  titlePanel("San Francisco Air Basin Data"),

  # Progress bar (placeholder)
  progressInit(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date range:", start = TODAY - ddays(7), end = TODAY, max = TODAY),
      selectInput("parameter", "Parameter:", choices = PARAMETERS, selected = "BC"),
      selectizeInput("sites", label = "Sites:", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Click to select one or more ... ')),
      p("Download data:"),
      downloadButton("downloadCSV", "CSV")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Stripchart", plotOutput("tsPlot")),
        tabPanel("Data", dataTableOutput("tsData"))
      )
    )
  )

))
