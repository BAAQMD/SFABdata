# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer( function (input, output, session) {

  .selected_parameter <- reactive({
    categorical(input$parameter, choices = PARAMETERS)
  })

  .units <- reactive({
    categorical(PARAM_UNITS[.selected_parameter()], choices = UNITS)
  })

  .selected_dates <- reactive({
    seq(from = input$dateRange[1], to = input$dateRange[2], by = "1 day")
  })

  .selected_sites <- reactive({
    categorical(input$sites, choices = .SITES())
  })

  get_date <- function (date, parameter, units) {

    query <- list(
      download = "y",
      param = PARAMETERS[parameter], units = UNITS[units], statistic = "DAVG",
      year = year(date), mon = month(date), day = mday(date), hours = "all",
      county_name = "--COUNTY--", basin = "SFB-San Francisco Bay",
      latitude = "--PART OF STATE--", report = "HVAL",
      order = "basin,county_name,s.name",
      submit = "Retrieve Data", ptype = "aqd")

    cache_key <- digest(query, "sha1", raw = FALSE)

    response <- cached(
      file.path(substr(cache_key, 1, 3), substr(cache_key, 4, 6), cache_key),
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
             parameter = categorical(variable, choices = PARAMETERS),
             #units = categorical(units, choices = UNITS),
             quality = categorical(quality, choices = QA_FLAGS)) %>%
      select(site_id = site, site_name = name, start, value, parameter, quality)
  }

  .parsed_data <- reactive({
    msg <- "Updating, please wait"
    detail <- "Fetching data ..."
    process <- lapply_with_progress(session, msg, detail)
    chunks <- process(.selected_dates(), get_date, .selected_parameter(), .units())
    do.call(rbind, chunks)
  })

  .site_tbl <- reactive({

  })

  .SITES <- reactive({
    SITES <<- .parsed_data() %>%
      select(site_id, site_name) %>%
      unique() %>%
      with(setNames(site_id, site_name))
    updateSelectizeInput(session, 'sites', choices = SITES)
    SITES
  })

  .data_tbl <- reactive({
    .parsed_data() %>%
      mutate(site = categorical(site_id, choices = .SITES())) %>%
      select(-site_id, -site_name)
  })

  .filtered_data_tbl <- reactive({
    if (length(.selected_sites()) == 0) {
      .data_tbl()
    } else {
      .data_tbl() %>% filter(site %in% .selected_sites())
    }
  })

  .y_axis_title <- reactive({
    str_c(.selected_parameter(), ", ", .units())
  })

  .filtered_data_tbl %>%
    ggvis(x = ~start, y = ~value, stroke = ~site) %>%
    set_options(height = 300) %>%
    layer_lines() %>%
    add_axis("x", title = "Timestamp (hour beginning)") %>%
    add_axis("y", title = "Reported 1h value") %>%
    bind_shiny("ggvis_plot")

  .reactive_filename <- reactive({
    sprintf("SFAB-%s-%s-%s.csv", str_replace_all(.selected_parameter(), "[^A-Za-z0-9]", ""),
            format(min(.selected_dates()), "%Y%m%d"), format(max(.selected_dates()), "%Y%m%d"))
  })

  output$downloadCSV <- downloadHandler(
    filename = function () {
      .reactive_filename()
    },
    content = function (file) {
      write.csv(.data_tbl(), file = file, row.names = FALSE)
    }
  )

})
