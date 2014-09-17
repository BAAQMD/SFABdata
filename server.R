# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer( function (input, output, session) {

  .selected_parameter <- reactive({
    factor(input$parameter, levels = PARAMETERS, labels = names(PARAMETERS))
  })

  .units <- reactive({
    factor(PARAM_UNITS[.selected_parameter()], levels = UNITS, labels = names(UNITS))
  })

  .selected_dates <- reactive({
    seq(from = input$dateRange[1], to = input$dateRange[2], by = "1 day")
  })

  .selected_sites <- reactive({
    input$sites
  })

  get_date <- function (date, parameter, units) {

    query <- list(
      download = "y",
      param = PARAMETERS[parameter], units = UNITS[units], statistic = "DAVG",
      year = year(date), mon = month(date), day = mday(date), hours = "all",
      county_name = "--COUNTY--", basin = "SFB-San Francisco Bay", latitude = "--PART OF STATE--",
      report = "HVAL", order = "basin,county_name,s.name",
      submit = "Retrieve Data", ptype = "aqd")

    cache_key <- digest(query, "sha1", raw = FALSE)

    response <- cached(
      file.path(substr(cache_key, 1, 3), substr(cache_key, 4, 6), cache_key), # hash in ASCII format
      GET("http://www.arb.ca.gov/aqmis2/display.php", query = query))

    stop_for_status(response)

    content(response) %>%
      textConnection() %>%
      read.csv(colClasses = "character") %>%
      as.tbl() %>%
      filter(str_detect(value, "[0-9]")) %>%
      mutate_each(funs(str_trim)) %>%
      mutate(start = ymd(date, tz = LST) + 3600 * extract_numeric(start_hour),
             value = extract_numeric(value),
             variable = factor(variable, levels = PARAMETERS, labels = names(PARAMETERS)),
             units = factor(units, levels = UNITS, labels = names(UNITS)),
             quality = factor(quality, levels = QA_FLAGS, labels = names(QA_FLAGS))) %>%
      select(site_id = site, site_name = name, start, value, parameter = variable, units, quality)
  }

  .parsed_data <- reactive({
    process <- lapply_with_progress(session, message = "Updating, please wait", detail = "Fetching data ...")
    chunks <- process(.selected_dates(), get_date, .selected_parameter(), .units())
    do.call(rbind, chunks)
  })

  .site_tbl <- reactive({
    site_tbl <- .parsed_data() %>% select(site_id, site_name) %>% unique()
    SITES <<- with(site_tbl, setNames(site_id, site_name))
    updateSelectizeInput(session, 'sites', choices = SITES)
    site_tbl
  })

  .data_tbl <- reactive({
    .parsed_data() %>%
      mutate(site = factor(site_id, levels = .site_tbl()$site_id, labels = .site_tbl()$site_name)) %>%
      select(-site_id, -site_name)
  })

  .filtered_data_tbl <- reactive({
    if (length(.selected_sites()) == 0) {
      .data_tbl()
    } else {
      selected_site_tbl <- .site_tbl() %>% filter(site_id %in% .selected_sites())
      .data_tbl() %>% semi_join(selected_site_tbl %>% select(site = site_name, site_id), by = "site")
    }
  })

  output$tsPlot <- renderPlot({
    .filtered_data_tbl() %>%
      qplot(start - dhours(0.5), value, geom = "line", color = site, data = .) +
      scale_x_datetime(LST, expand = c(0, 0)) +
      scale_y_continuous(sprintf("%s, %s", .selected_parameter(), .units())) +
      expand_limits(y = 0) +
      expand_limits(x = as.POSIXct(TODAY + ddays(1) - dminutes(1))) +
      theme(legend.position = "bottom", axis.title.x = element_blank(),
            axis.title.y = element_text(vjust = 1.0)) +
      guides(color = guide_legend("", nrow = 7, title.position = "top"))
  }, height = 400)

  output$tsData <- renderDataTable(
    .filtered_data_tbl(),
    options = list(iDisplayLength = 10)
  )

  .reactive_filename <- reactive({
    sprintf("SFAB-%s-%s-%s.csv", str_replace_all(.selected_parameter(), "[^A-Za-z0-9]", ""),
            format(min(.selected_dates()), "%Y%m%d"), format(max(.selected_dates()), "%Y%m%d"))
  })

  output$downloadCSV <- downloadHandler(
    filename = function () {
      .reactive_filename()
    },
    content = function (file) {
      write.csv(.filtered_data_tbl(), file = file, row.names = FALSE)
    }
  )

})
