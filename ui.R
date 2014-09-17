# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(

  titlePanel("San Francisco Air Basin Data"),
  ggvisOutput("ggvis_plot"),

  hr(),

  # Progress bar (placeholder)
  progressInit(),

  fluidRow(

    column(4, offset = 1,
      h3("Query"),
      selectInput("parameter", "Parameter:", choices = PARAMETERS, selected = "BC"),
      dateRangeInput("dateRange", "Date range:", start = TODAY - ddays(7), end = TODAY, max = TODAY)),

    column(4,
      h3("Filter"),
      selectizeInput("sites", label = "Sites:", choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Click to select one or more ... '))),

    column(3,
      h3("Download"),
      downloadButton("downloadCSV", "CSV")
    ))

))
