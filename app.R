library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(leaflet)
library(sf)
library(glue)
library(DT)
library(ggiraph)

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
                             box(radioButtons("smoother", "Add loess smoothing function?",
                                              choices = c("Yes" = TRUE, "No" = FALSE),
                                              selected = FALSE,
                                              inline = TRUE, width = "100%"),
                                 width = NULL, height = 70),
                             box(girafeOutput("year", width = "100%", height = "100%"),
                                 width = NULL, height = 280),
                             box(DTOutput("station"), height = 280, width = NULL)))))

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
  output$year <- renderGirafe({
    validate(need(!is.null(clicks$id),
                  "Click on a Region or Station to display yearly data"),
             errorClass = "notice")
    req(mb_data())
    validate(need(nrow(mb_data()) > 0, "No data for this selection"),
             errorClass = "notice")

    if("region_nam" %in% names(mb_data())) {
      g <- ggplot(data = mb_data(),
                  aes(x = date, y = mean_temp, colour = station_name, group = station_name))
      if(input$smoother == TRUE) {
        g <- g + stat_smooth(method = "loess", formula = "y ~ x", se = FALSE, na.rm = TRUE)
      }
    } else {
      g <- ggplot(data = mb_data(), aes(x = date, y = mean_temp, group = station_name))

      if(input$smoother == TRUE) {
        g <- g + stat_smooth(method = "loess", formula = "y ~ x",
                             se = TRUE, na.rm = TRUE, colour = "black")
      }
    }
    g <- g +
      theme_bw(base_size = 13) +
      geom_point_interactive(aes(tooltip = station_name, data_id = station_name),
                             size = 2, na.rm = TRUE) +
      scale_colour_viridis_d(name = "Station Name", end = 0.8) +
      labs(y = "Daily Temperature (C°)", x = "Date",
           title = glue("{clicks$id} - Hover to highlight a station"),
           caption = "Only showing stations with data")

    if(!is.null(clicks$row)) {
      g <- g + geom_point(data = mb_data()[clicks$row, ], na.rm = TRUE,
                          colour = "black", size = 5, show.legend = FALSE)
    }
    girafe(ggobj = g, width_svg = 12, height_svg = 4, options = list(
      opts_hover_inv(css = "opacity:0.1;"),
      opts_hover(css = "stroke-width:2;")
    ))
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
                     options = list(pageLength = 4, dom = "tp"),
                     caption = htmltools::tags$caption(style = 'caption-side: top; color:black; font-size:125%; font-weight: bold','Click on rows to highlight dates in above figure') )
    }

  })
}


shinyApp(ui = ui, server = server)
