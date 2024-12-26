library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(bslib)
library(httr)
library(jsonlite)

# -- 1. Prepare the custom SVG icon in a base64-encoded format ----
#    Fill color = #d9534f (red from Simplex theme), width & height = 20 (50% smaller than 40)
svg_icon <- '
<svg fill="#d9534f" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 576 512">
<path d="M192 32c0-17.7 14.3-32 32-32L352 0c17.7 0 32 14.3 32 32l0 32 48 0c26.5 0 48 21.5 48 48l0 128 44.4 14.8c23.1 7.7 29.5 37.5 11.5 53.9l-101 92.6c-16.2 9.4-34.7 15.1-50.9 15.1c-19.6 0-40.8-7.7-59.2-20.3c-22.1-15.5-51.6-15.5-73.7 0c-17.1 11.8-38 20.3-59.2 20.3c-16.2 0-34.7-5.7-50.9-15.1l-101-92.6c-18-16.5-11.6-46.2 11.5-53.9L96 240l0-128c0-26.5 21.5-48 48-48l48 0 0-32zM160 218.7l107.8-35.9c13.1-4.4 27.3-4.4 40.5 0L416 218.7l0-90.7-256 0 0 90.7zM306.5 421.9C329 437.4 356.5 448 384 448c26.9 0 55.4-10.8 77.4-26.1c0 0 0 0 0 0c11.9-8.5 28.1-7.8 39.2 1.7c14.4 11.9 32.5 21 50.6 25.2c17.2 4 27.9 21.2 23.9 38.4s-21.2 27.9-38.4 23.9c-24.5-5.7-44.9-16.5-58.2-25C449.5 501.7 417 512 384 512c-31.9 0-60.6-9.9-80.4-18.9c-5.8-2.7-11.1-5.3-15.6-7.7c-4.5 2.4-9.7 5.1-15.6 7.7c-19.8 9-48.5 18.9-80.4 18.9c-33 0-65.5-10.3-94.5-25.8c-13.4 8.4-33.7 19.3-58.2 25c-17.2 4-34.4-6.7-38.4-23.9s6.7-34.4 23.9-38.4c18.1-4.2 36.2-13.3 50.6-25.2c11.1-9.4 27.3-10.1 39.2-1.7c0 0 0 0 0 0C136.7 437.2 165.1 448 192 448c27.5 0 55-10.6 77.5-26.1c11.1-7.9 25.9-7.9 37 0z"/>
</svg>
'
svg_icon_b64 <- jsonlite::base64_enc(svg_icon)
svg_icon_url <- paste0("data:image/svg+xml;base64,", svg_icon_b64)

custom_icon <- leaflet::makeIcon(
  iconUrl = svg_icon_url,
  iconWidth = 20,  # 50% size
  iconHeight = 20
)

# -- 2. Fetch data from API --------------------------------------------------
fetch_data <- function() {
  url <- "https://api.scu.co.id/vtms/oses/poi"
  token <- "73ob73y64nt3n63MP4tk4l1"
  
  response <- GET(
    url,
    add_headers(
      Authorization = paste("Bearer", token),
      'Content-Type' = 'application/json'
    )
  )
  
  if (status_code(response) == 200) {
    raw_content <- rawToChar(response$content)
    parsed_data <- fromJSON(raw_content)
    
    if ("data" %in% names(parsed_data)) {
      df <- as.data.frame(parsed_data$data, stringsAsFactors = FALSE) %>%
        mutate(
          id = as.numeric(id),
          lat = as.numeric(lat),
          lon = as.numeric(lon),
          rad = as.numeric(rad)
        )
      return(df)
    } else {
      stop("Unexpected data format: 'data' key not found in API response")
    }
  } else {
    stop("Failed to fetch data from API: HTTP status ", status_code(response))
  }
}

# -- 3. UI -------------------------------------------------------------------
ui <- page_navbar(
  title = "Tower Location Dashboard",
  theme = bs_theme(
    version = 5, 
    bootswatch = "simplex",
    font_scale = NULL,
    font_base = "Calibri"
  ),
  
  nav_panel(
    "Main",
    div(
      style = "min-height: 1300px; font-family: Calibri;",
      
      # Info cards
      layout_columns(
        col_widths = c(6, 6),
        card(
          h4("Statistics", style = "font-size: 18px; font-weight: bold;"),
          div(
            style = "font-size: 16px;",
            verbatimTextOutput("stats")
          )
        ),
        card(
          h4("Summary", style = "font-size: 18px; font-weight: bold;"),
          div(
            style = "font-size: 16px;",
            verbatimTextOutput("summary")
          )
        )
      ),
      
      # Map section
      card(
        style = "position: relative;",
        h4("Map View", style = "font-size: 18px; font-weight: bold;"),
        
        # Map Type Selection
        div(
          style = "position: absolute; z-index: 1000; background-color: rgba(255, 255, 255, 0.8);
                   padding: 10px; border-radius: 5px; width: auto; top: 10px; right: 10px;",
          selectInput(
            "map_type", "Map Type",
            choices = c(
              "Default"     = "OpenStreetMap",
              "Satellite"   = "Esri.WorldImagery",
              "Dark Mode"   = "CartoDB.DarkMatter",
              "Terrain"     = "Esri.WorldTerrain",
              "Topo"        = "OpenTopoMap",
              "Streets"     = "CartoDB.Positron"
            ),
            selected = "OpenStreetMap",
            width = "150px"
          )
        ),
        
        # Enlarged map icon
        div(
          style = "
            position: absolute;
            z-index: 1000;
            right: 10px;
            bottom: 10px;
            opacity: 0.3;
            color: #d9534f;
            font-size: 24px;
            cursor: pointer;
          ",
          tags$i(class="fa fa-expand")
        ),
        
        # Leaflet map
        leafletOutput("map", height = "400px")
      ),
      
      # Table
      card(
        h4("Data Table", style = "font-size: 18px; font-weight: bold;"),
        div(
          style = "width: auto; font-family: Calibri;",  # Ensure table adapts dynamically
          DTOutput("table")
        )
      )
    )
  )
)

# -- 4. Server ---------------------------------------------------------------
server <- function(input, output, session) {
  # Hold the data in a reactiveVal
  data_rv <- reactiveVal(fetch_data())
  
  # Statistics
  output$stats <- renderText({
    data <- data_rv()
    req(data)
    paste0(
      "Total Points: ", nrow(data), "\n",
      "Average Radius: ", round(mean(data$rad, na.rm = TRUE), 2)
    )
  })
  
  # Summary
  output$summary <- renderText({
    data <- data_rv()
    req(data)
    paste0(
      "Latitude Range: ", round(min(data$lat, na.rm = TRUE), 2), 
      " to ", round(max(data$lat, na.rm = TRUE), 2), "\n",
      "Longitude Range: ", round(min(data$lon, na.rm = TRUE), 2),
      " to ", round(max(data$lon, na.rm = TRUE), 2)
    )
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    data <- data_rv()
    req(data)
    
    leaflet(data) %>%
      setView(lng = mean(data$lon, na.rm = TRUE),
              lat = mean(data$lat, na.rm = TRUE),
              zoom = 8) %>%
      addProviderTiles(input$map_type) %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~paste0(
          "<strong>", name, "</strong><br>",
          "<b>ID:</b> ", id, "<br>",
          "<b>Type:</b> ", type, "<br>",
          "<b>Type Name:</b> ", type_name, "<br>",
          "<b>Latitude:</b> ", round(lat, 5), "<br>",
          "<b>Longitude:</b> ", round(lon, 5), "<br>",
          "<b>Radius:</b> ", rad, " meters<br>",
          "<b>Status:</b> ", ifelse(active_status, "Active", "Inactive"), "<br>",
          "<hr style='margin: 5px 0;'>",
          "<small></small>"
        ),
        label = ~name,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "padding" = "5px"),
          textsize = "14px",
          direction = "auto"
        ),
        icon = custom_icon
      )
  })
  
  # Render data table
  output$table <- renderDT({
    data <- data_rv()
    req(data)
    
    datatable(
      data,
      filter = "top",
      options = list(
        pageLength = 10,   # Show 10 rows by default
        scrollX = TRUE,    # Horizontal scroll only
        autoWidth = TRUE
      ),
      class = "display compact stripe hover nowrap"  # Improve visual spacing
    )
  })
}

shinyApp(ui, server)
