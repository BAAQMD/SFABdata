# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer( function(input, output, session) {

  .selected_param <- reactive({ input$parameter[1] })  # e.g. "OZONE"
  .selected_units <- reactive({ input$parameter[2] })     # e.g. "008" (means "ppb")
  .selected_dates <- reactive({ seq(from = input$dateRange[1], to = input$dateRange[2], by = "1 day") })              # POSIXct object
  .selected_sites <- reactive({ input$sites })

  get_hourly <- function (date, param, units) {

    response <- GET(
      "http://www.arb.ca.gov/aqmis2/display.php",
      query = list(
        download = "y",
        param = param, units = units, statistic = "DAVG",
        year = year(date), mon = month(date), day = mday(date), hours = "all",
        county_name = "--COUNTY--", basin = "SFB-San Francisco Bay", latitude = "--PART OF STATE--",
        report = "HVAL", order = "basin,county_name,s.name",
        submit = "Retrieve Data", ptype = "aqd"))

    stop_for_status(response)

    content(response) %>%
      textConnection() %>%
      read.csv(colClasses = "character") %>%
      as.tbl() %>%
      filter(str_detect(value, "[0-9]")) %>%
      mutate_each(funs(str_trim))
  }

  rbindapply <- function (X, FUN, ...) {
    do.call(rbind, lapply(X, FUN, ...))
  }

  .get_range <- reactive({
    rbindapply(.selected_dates(), get_hourly, param = .selected_param(), units = .selected_units()) %>%
      mutate(start = ymd(date, tz = "America/Los_Angeles") + 3600 * extract_numeric(start_hour),
             value = extract_numeric(value),
             variable = factor(variable),
             units = factor(units, levels = UNITS, labels = names(UNITS)),
             quality = factor(quality, levels = QA_FLAGS, labels = names(QA_FLAGS)))
  })

  .site_tbl <- reactive({
    site_tbl <- .get_range() %>% select(site_id = site, site_name = name) %>% unique()
    SITES <<- with(site_tbl, setNames(site_id, site_name))
    updateSelectizeInput(session, 'sites', choices = SITES)
    site_tbl
  })

  .data_tbl <- reactive({
    .get_range() %>%
      select(site, start, value, variable, units, quality) %>%
      mutate(site = factor(site, levels = .site_tbl()$site_id, labels = .site_tbl()$site_name))
  })

  .filtered_data_tbl <- reactive({
    if (length(.selected_sites()) == 0) {
      .data_tbl()
    } else {
      site_tbl_subset <- site_tbl %>% filter(site_id %in% .selected_sites())
      .data_tbl() %>% semi_join(site_tbl_subset %>% select(site = site_name, site_id), by = "site")
    }
  })

  output$tsPlot <- renderPlot({
    .filtered_data_tbl() %>%
      qplot(start, value, geom = "line", color = site, data = .) +
      labs(x = "", y = "") +
      facet_grid(variable + units ~ .) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(ncol = 3))
  }, height = 400)

  output$tsData <- renderDataTable(
    .filtered_data_tbl(),
    options = list(iDisplayLength = 10)
  )

})
