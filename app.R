library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(leaflet)
library(sf)
library(glue)
library(DT)

#https://github.com/rstudio/leaflet/issues/615
css_fix <- paste0("div.info.legend.leaflet-control br {clear: both;}\n",
                  ".shiny-output-error-notice {font-weight: bold; font-size: 18px; color: #000000};")
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
                    dashboardHeader(title = "2020 Manitoba Temperature with weathercan",
                                    titleWidth = 450),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(HTML(html_fix), fluidRow(
                      column(width = 5, box(leafletOutput("map", height = 650, width = "100%"),
                                            width = NULL)),
                      column(width = 7,
                             box(plotOutput("year", height = 300), width = NULL),
                             box(DTOutput("station"), height = 300, width = NULL)))))

server <- function(input, output) {
  # Initial Data ------------------------------------------------------------
  region <- readRDS("mb_regions.rds") %>%
    st_set_crs(3347) #shinyapps.io problem: https://github.com/r-spatial/mapview/issues/300

  mb <- readRDS("mb_2020.rds")

  stn <- mb %>%
    group_by(station_name, lat, lon) %>%
    summarize(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  region <- stn %>%
    st_transform(crs = st_crs(region)) %>%
    st_join(region, .)

  stn <- select(stn, -mean_temp) %>%
    unique()

  mb <- left_join(mb, select(region, region_nam, station_name), by = "station_name")

  region <- region %>%
    group_by(region_nam) %>%
    summarize(mean_temp = mean(mean_temp, na.rm = TRUE),
              n = n_distinct(station_name, na.rm = TRUE), .groups = "drop")

  # Map ---------------------------------------------------------------------

  output$map <- renderLeaflet({

    pal <- colorNumeric(palette = "Blues",
                        domain = region$mean_temp,
                        na.color = "grey", reverse = TRUE)
    region %>%
      st_transform(crs = 4326) %>%
      leaflet() %>%
      setView(lat = 54, lng = -97, zoom = 6) %>%
      addPolygons(color = "black", weight = 1,
                  opacity = 1.0,
                  fillOpacity = 1,
                  fillColor = ~pal(mean_temp),
                  highlightOptions = highlightOptions(weight = 2),
                  label = ~region_nam, layerId = ~region_nam) %>%
      addCircleMarkers(data = stn,
                 label = ~glue("Station: {station_name}"), layerId = ~station_name,
                 color = NA, fillColor = "black", fillOpacity = 0.5,
                 radius = 10) %>%
      addLegend("bottomright", pal = pal, values = ~mean_temp,
                title = "Average daily<br>mean temp (C°)",
                opacity = 1, labFormat = labelFormat(suffix = "C"))
  })


  # Map Clicks --------------------------------------------------------------
  clicks <- reactiveValues(type = NULL, id = NULL, row = NULL)
  observe({
    if(!is.null(input$map_shape_click)) {
      clicks$type <- "area"
      clicks$id <- input$map_shape_click[["id"]]
      clicks$row <- NULL
    }
  })
  observe({
    if(!is.null(input$map_marker_click)) {
      clicks$type <- "stn"
      clicks$id <- input$map_marker_click[["id"]]
      clicks$row <- NULL
    }
  })
  observe({
    if(!is.null(input$station_rows_selected)) {
      clicks$row <- input$station_rows_selected
    }
  })

  # Data --------------------------------------------------------------------
  mb_data <- reactive({
    req(clicks$type, clicks$id)

    isolate({
      d <- NULL
      if(clicks$type == "area") {
        d <- filter(mb, region_nam == clicks$id)
        dd <- group_by(d, station_name) %>%
          summarize(n = sum(!is.na(mean_temp)))
        if(any(dd$n == 0)) d <- filter(d, !station_name %in% dd$station_name[dd$n == 0])
      } else if(clicks$type == "stn") {
        d <- filter(mb, station_name == clicks$id) %>%
          select(-region_nam)
      }
      d
    })
  })


  # Plots -------------------------------------------------------------------
  output$year <- renderPlot({
    validate(need(!is.null(clicks$id),
                  "Click on a Region or Station to display yearly data"),
             errorClass = "notice")
    req(mb_data())
    validate(need(nrow(mb_data()) > 0, "No data for this selection"),
             errorClass = "notice")

    if("region_nam" %in% names(mb_data())) {
      g <- ggplot(data = mb_data(),
                  aes(x = date, y = mean_temp,colour = station_name, fill = station_name))
    } else {
      g <- ggplot(data = mb_data(), aes(x = date, y = mean_temp))
    }
    g <- g +
      theme_bw(base_size = 15) +
      geom_line(size = 1, na.rm = TRUE) +
      geom_ribbon(aes(ymin = min_temp, ymax = max_temp), colour = NA, alpha = 0.3) +
      scale_colour_viridis_d(name = "Station Name", end = 0.8, aesthetics = c("colour", "fill")) +
      labs(y = "Daily Temperature (C°)", x = "Date",
           title = clicks$id, subtitle = "Mean (Min - Max)",
           caption = "Only showing stations with data")

    if(!is.null(clicks$row)) {
      g <- g + geom_point(data = mb_data()[clicks$row, ],
                          colour = "black", size = 5, show.legend = FALSE)
    }
    g
  })


  # Data Table --------------------------------------------------------------
  output$station <- renderDT({
    req(mb_data())
    if(nrow(mb_data()) > 0) {
      d <- datatable(select(mb_data(),
                            station_name,
                            date,
                            mean_temp,
                            min_temp, max_temp),
                     options = list(pageLength = 6, dom = "tp"),
                     caption = htmltools::tags$caption(style = 'caption-side: top; color:black; font-size:125%; font-weight: bold','Click on rows to highlight dates in above figure') )
    }

  })
}


shinyApp(ui = ui, server = server)
