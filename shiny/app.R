# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
# library(readr)
# library(raster)
# library(rgdal)
# library(broom)
# library(leaflet)
# library(rasterly)
# library(RColorBrewer)


# Data --------------------------------------------------------------------

# For testing
# setwd("shiny/")

# Svalbard bbox
sval_bbox <- c(9, 30, 76, 81)

# ERA5 annual anomalies
ERA5_anom <- readRDS("data/ERA5_anom_smol.Rds") %>% 
  pivot_longer(T2m:`snow melt`) %>% 
  mutate(cat = "Anomaly", pvalue = NA, rvalue = NA) %>% 
  filter(year != 2021)

# ERA5 annual means
ERA5_mean <- readRDS("data/ERA5_mean_smol.Rds") %>% 
  pivot_longer(T2m:`snow melt`) %>%
  mutate(cat = "Mean", pvalue = NA, rvalue = NA) %>% 
  filter(year != 2021)

# ERA5 decadal trends
ERA5_trend <- readRDS("data/ERA5_trend_smol.Rds") %>% 
  mutate(cat = "Trend", year = NA)

# Combine for ease of filtering
ERA5_ALL <- rbind(ERA5_anom, ERA5_mean, ERA5_trend)

# Glacier
glacier_fortified <- readRDS("data/glacier_fortified.Rds")

# Glacier frontlines
glacier_frontlines <- readRDS("data/glacier_frontlines.Rds")

# Base maps
map_base_low <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  # filter(subregion == "Svalbard")
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
       lat >= sval_bbox[3], lat <= sval_bbox[4])
map_base_hi <- readRDS("data/map_base.Rds")

# Popup coordinates
pop_coords <- data.frame(site = c("Airport", "Ny Alesund"),
                         lon = c(15.4633742, 11.8695311),
                         lat = c(78.2460841, 78.9237485))

## Station data
# Airport data
station_airport <- read.csv("data/Svalbard_Airport.csv") %>% 
  dplyr::select(YEAR:DEC) %>% 
  pivot_longer(JAN:DEC, names_to = "month", values_to = "T2m") %>% 
  filter(T2m < 30) %>% 
  mutate(site = "Airport",
         date = paste0(YEAR,"-",month,"-01"),
         date = as.Date(date, format = "%Y-%B-%d"))

# Ny Alesund data
station_ny <- read.csv("data/ny_alesund.csv") %>% 
  dplyr::select(YEAR:DEC) %>% 
  pivot_longer(JAN:DEC, names_to = "month", values_to = "T2m") %>% 
  filter(T2m < 30) %>% 
  mutate(site = "Ny Alesund",
         date = paste0(YEAR,"-",month,"-01"),
         date = as.Date(date, format = "%Y-%B-%d"))

# Test bit
df2 <- data.frame(x = c(rep('a', 10), rep('b', 10)),
                  y = c(rnorm(10), rnorm(10, 3, 1)))

# Picture URLs
src_airport <- "https://upload.wikimedia.org/wikipedia/commons/8/8d/Svalbard_Airport%2C_Longyear_1.jpg"
src_ny <- "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Ny-%C3%85lesund_2013_06_07_3603.jpg/1200px-Ny-%C3%85lesund_2013_06_07_3603.jpg"


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "blue",
                    
                    # The app title
                    dashboardHeader(disable = FALSE, title = "Map the Science"),
                    
                    # The primary options
                    dashboardSidebar(
                      sidebarMenu(id = "comparisonMenu",
                                  menuItem("Map", tabName = "map", icon = icon("map"), selected = TRUE),
                                  # menuItem("Tables", tabname = "tables", icon = icon("table")),
                                  menuItem("About", tabName = "about", icon = icon("question")),
                                  # The reactive controls based on the primary option chosen
                                  uiOutput(outputId = "sidebar_controls_cat"),
                                  uiOutput(outputId = "sidebar_controls_other"))
                    ),
                    
                    # The dashboard
                    dashboardBody(
                      tabItems(
                        

                        # Map tab -----------------------------------------------------------------
                        
                        tabItem(tabName = "map",
                                fluidRow(box(plotlyOutput("map", width = '100%', height = '580px'), width = 12, height = '600px', type = 6, color = "#b0b7be"),
                                         width = 12, status = "danger", solidHeader = TRUE, collapsible = FALSE),
                                # fluidRow(textOutput("maply_click")),
                                fluidRow(box(plotlyOutput("ts", width = '100%', height = '180px'), width = 12, height = '200px', type = 6, color = "#b0b7be"),
                                         width = 12, status = "danger", solidHeader = TRUE, collapsible = FALSE)),
                        
                        
                        # Tables ------------------------------------------------------------------
                        
                        # tabItem(tabname = "tables",
                        #         # tableOutput("resultsKable"),
                        #         h2("test")),
                        
                        
                        # App explanation ---------------------------------------------------------
                        
                        tabItem(tabName = "about", 
                                fluidPage(
                                  column(12,
                                         h2(tags$b("Documentation")),
                                         h3(tags$b("Contributors")),
                                         p(tags$div(
                                           tags$ul(
                                             tags$li("Robert Schlegel (@FACEITArctic, @ChocTeamLov @IMEV_mer): GUI"),
                                             tags$li("Adrien Wehrlé (University of Zurich): Data analysis")
                                           )
                                           )
                                         ),
                                         h3(tags$b("Graphical user interface (GUI)")),
                                         p("There are many options in the top-right corner of the map, such as : resetting the axes, 
                                         zooming in and out or scaling automatically. The current map can also be downloaded as a .png file.
                                         Hovering over the map with the mouse will display the values in the pixels. By left-clicking 
                                         a given pixel the time series of annual means will be plotted and analysed through a linear regression.
                                         The slope, correlation value, and p-value will appear by hovering over the blue line."),
                                         p("For the individual controls in the panel on the left:",
                                         tags$div(
                                             tags$ul(
                                               tags$li("Category: choose one of the three statistics presented in the Data Analysis section."),
                                               tags$li("Data layer: choose one of the variables presented in the Datasets section. "),
                                               tags$li("Glacier layer: activate the mask for permanent snow and ice."),
                                               tags$li("Station layer: activate the visualisation of time series at weather station locations."),
                                               tags$li("Time evolution: move the scroll button to visualise the evolution of the selected variable and analysis.")
                                             )
                                           )
                                         ),
                                         h3(tags$b("Data Analysis")),
                                         p("The variables presented below are visualised through three main statistics:",
                                           tags$div(
                                             tags$ul(
                                               tags$li("Annual means "),
                                               tags$li("Anomaly from 1979-2000 mean"),
                                               tags$li("1979-2021 trend")
                                             )
                                           )
                                         ),
                                         h3(tags$b("Datasets")),
                                         h4(tags$b("Raster data")),
                                         p("ECMWF Re-Analysis (ERA5; 1979-2020, 25km resolution, annual means):",
                                           tags$div(
                                             tags$ul(
                                               tags$li("2-meter air temperature (°C)"),
                                               tags$li("Sea surface temperature(°C)"),
                                               tags$li("Sea ice cover (%) "),
                                               tags$li("Mean sea level pressure (hPa)"),
                                               tags$li("Snow melt (m of water equivalent)")
                                               )
                                             ),
                                           " CryoSat-2 and Envisat products (2002-2021, 25km resolution, monthly means)",
                                           tags$div(
                                             tags$ul(
                                               tags$li("Sea ice thickness (m)")
                                             )
                                           )
                                         ),
                                         h4(tags$b("Vector data")),
                                         p("European Space Agency (ESA) Climate Change Initiative (CCI) land cover products (2018, 250m resolution):",
                                           tags$div(
                                             tags$ul(
                                               tags$li("Permanent snow and ice mask"),
                                               tags$li("Coastline ")
                                             )
                                           ),
                                           "Sentinel-2 and Landsat-8 products (2008-2020, 30m resolution):",
                                           tags$div(
                                             tags$ul(
                                               tags$li("Annual frontlines of marine-terminating glaciers")
                                             )
                                           )
                                         ),
                                         h4(tags$b("Point data")),
                                         p("Global Historical Climatology Network (GHCN; 1932-2021; monthly means):",
                                           tags$div(
                                             tags$ul(
                                               tags$li("2-meter air temperature (°C)")
                                             )
                                           ),
                                           "High Arctic climatological dataset of the Polish Polar Station Hornsund (1979-2018, daily means):",
                                           tags$div(
                                             tags$ul(
                                               tags$li("2-meter air temperature (°C)"),
                                               tags$li("Relative humidity (%)"),
                                               tags$li("Precipitation (mm)"),
                                             )
                                           )
                                         ),
                                         h3(tags$b("References")),
                                         p("Copernicus Climate Change Service (C3S) (2017): ERA5: Fifth generation of ECMWF atmospheric reanalyses 
                                           of the global climate . Copernicus Climate Change Service Climate Data Store (CDS), March 12 2021. 
                                           https://cds.climate.copernicus.eu/cdsapp#!/home"),
                                         p("ESA. Land Cover CCI Product User Guide Version 2. Tech. Rep. (2017). 
                                           Available at: maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf"),
                                         p("ESA Sea Ice CCI project team; Sandven, S. (2016): ESA Sea Ice Climate Change Initiative (Sea Ice CCI) Dataset Collection. 
                                           Centre for Environmental Data Analysis, date of citation. http://catalogue.ceda.ac.uk/uuid/5e789087d4e847308a39b3fe5b26e281"),
                                         p("Menne, M.J., C.N. Williams, B.E. Gleason, J.J. Rennie, and J.H. Lawrimore, 2018: The Global Historical 
                                           Climatology Network Monthly Temperature Dataset, Version 4. J. Climate, 31, 98359854, doi:doi.org/10.1175/JCLI-D-18-0094.1."),
                                         p("Moholdt, G., Maton, J., Majerska, M., & Kohler, J. (2021). Annual frontlines of marine-terminating 
                                           glaciers on Svalbard [Data set]. Norwegian Polar Institute. https://doi.org/10.21334/npolar.2021.d60a919a"),
                                         p("Wawrzyniak, Tomasz; Osuch, Marzena (2019): A consistent High Arctic climatological dataset (1979-2018) of the Polish Polar 
                                           Station Hornsund (SW Spitsbergen, Svalbard). PANGAEA, https://doi.org/10.1594/PANGAEA.909042")
                                  )
                                )
                        )
                      )
                    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # Render UI ---------------------------------------------------------------
  
  # Select broad category
  picker_category <- pickerInput(inputId = "category", label = "Category:",
                                 choices = unique(ERA5_ALL$cat), 
                                 multiple = FALSE, selected = "Trend")
  
  # Specific layer for selection
  picker_layer <- pickerInput(inputId = "layer", label = "Data layer:",
                              choices = unique(ERA5_ALL$name),
                              multiple = FALSE,
                              options = list(size = 6),
                              selected = unique(ERA5_ALL$name)[1])
  
  # Select years from a dropdown
  # picker_year <- pickerInput(inputId = "year", label = "Year:",
  #                            choices = seq(min(ERA5_ALL$year, na.rm = T), max(ERA5_ALL$year, na.rm = T)),
  #                            # choices = yearChoices(),
  #                            multiple = FALSE, selected = max(ERA5_ALL$year,na.rm = T), options = list(size = 5))
  picker_year <- sliderInput("year", "Year", min(ERA5_ALL$year, na.rm = T), max(ERA5_ALL$year, na.rm = T),
                             value = max(ERA5_ALL$year, na.rm = T), step = 1, sep = "",
                             animate = animationOptions(interval = 2000))
  
  # Glacier layer
  switch_glacier <- materialSwitch(inputId = "glacier", label = "Glaciers:", status = "info")
  
  # Glacier frontline layer
  # switch_glacierfront <- materialSwitch(inputId = "glacierfront", label = "Glacier fronts:", status = "info")
  
  # Station layer
  switch_station <- materialSwitch(inputId = "station", label = "Stations:", status = "primary", value = TRUE)
  
  # Coastline layer
  switch_coast <- materialSwitch(inputId = "coast", label = "Hi-res coast:", status = "info", value = TRUE)
  
  # The high level UI controls
  output$sidebar_controls_cat <- renderUI({
    if(input$comparisonMenu == "map"){
      sidebarMenu(switch_coast, switch_station, switch_glacier, picker_category, picker_layer)
    } else if(input$comparisonMenu == "about"){
      # Intentionally empty
    } else{
      # Intentionally empty
    }
  })
  
  # The low level UI controls
  output$sidebar_controls_other <- renderUI({
    req(input$category)
    if(input$category == "Trend"){
    } else if(input$category %in% c("Mean", "Anomaly")){
      sidebarMenu(picker_year)
    } else { 
    }
  })
  
  

  # Images ------------------------------------------------------------------

  output$picture_airport <- renderText({c('<img src="',src_airport,'" width=600px>')})
  output$picture_ny <- renderText({c('<img src="',src_ny,'" width=600px>')})
  

  # Render Modal ------------------------------------------------------------
  
  # Observe to open modal
  observeEvent(event_data("plotly_click", source = "map_ly"), {
    
    # Collect click info
    event.data <- event_data("plotly_click", source = "map_ly")
    
    # Begin modal process
    if(event.data$curveNumber[1] == 2){
      
      # Time series data
      modalData <- modalData()
      
      # Wide for table
      modalDataWide <- modalData %>% 
        dplyr::select(YEAR, month, T2m) %>%
        pivot_wider(names_from = month, values_from = T2m) %>% 
        dplyr::select(YEAR, toupper(month.abb))
      
      # Prep photo link
      if(modalData$site[1] == "Airport"){
        modal_photo <- "picture_airport"
      } else if(modalData$site[1] == "Ny Alesund"){
        modal_photo <- "picture_ny"
      }
      
      # The modal
      showModal(
        modalDialog(size = "l", title = modalData$site[1],
                    fluidPage(
                      tabsetPanel(
                        tabPanel(title = "Time series",
                                 br(),
                                 renderPlotly({
                                   ggplotly(
                                     modalData %>% 
                                       ggplot(aes(x = date, y = T2m)) +
                                       geom_line() +
                                       geom_point() +
                                       geom_smooth(method = "lm", se = F) +
                                       scale_x_date(expand = c(0, 0)) +
                                       labs(x = NULL, y = "T2m (°C)") +
                                       theme(panel.border = element_rect(fill = NA, colour = "black"))
                                     )
                                   })
                        ),
                        tabPanel(title = "Photos",
                                 br(),
                                 htmlOutput(modal_photo)
                                 ),
                        tabPanel(title = "Table",
                                 br(),
                                 renderDataTable({
                                   datatable(modalDataWide,
                                             options = list(pageLength = 10))
                                 })
                                 )
                        )
                    )
        )
        )
      }
  })
  
  
  # Reactive values ---------------------------------------------------------
  
  # Reactive expression for the data subsetted to what the user selected
  baseData <- reactive({
    req(input$layer); req(input$category)
      baseData <- ERA5_ALL %>% 
        # dplyr::filter(year == 1990, name == "SST", cat == "Anomaly") # For testing...
        dplyr::filter(name == input$layer,
                      cat == input$category) #%>% 
        # na.omit()
      if(input$category %in% c("Mean", "Anomaly")){
        req(input$year)
        baseData <- baseData %>% 
          dplyr::filter(year == input$year) %>% 
          dplyr::select(lon, lat, year, name, value) %>% 
          na.omit()
      }
    return(baseData)
  })
  
  # Reactive map_base for better detail or faster rendering
  mapBase <- reactive({
    req(!is.null(input$coast))
    if(input$coast){
      mapBase <- map_base_hi
    } else {
      mapBase <- map_base_low
    }
    return(mapBase)
  })
  
  # Reactive modal plot data
  modalData <- reactive({
    event.data <- event_data("plotly_click", source = "map_ly")
    if(event.data$curveNumber[1] == 2){
      site_idx <- pop_coords %>% 
        dplyr::filter(lon == event.data$x[1],
                      lat == event.data$y[1])
      if(site_idx$site[1] == "Airport"){
        modalData <- station_airport
      } else if(site_idx$site[1] == "Ny Alesund"){
        modalData <- station_ny
      } else{
      }
    }
  })
  
  
  # Map figure --------------------------------------------------------------
  
  # The map
  output$map <- renderPlotly({
    req(input$layer); req(input$category); req(!is.null(input$glacier)); req(!is.null(input$coast))
    # input <- data.frame(layer = "SST", category = "Anomaly", year = 2016) # tester...
    
    # Prep value labels
    if(input$layer %in% c("T2m", "SST")){
      unit_label <- " (°C)"
      dec_label <- " (°C/dec)"
    } else if(input$layer == "sea ice cover"){
      unit_label <- " (%)"
      dec_label <- " (%/dec)"
    } else if(input$layer == "MSLP"){
      unit_label <- " (hPa)"
      dec_label <- " (hPa/dec)"
    } else if(input$layer == "snow melt"){
      unit_label <- " (mm)"
      dec_label <- " (mm/dec)"
    }
    
    # Legend label
    if(input$category == "Trend"){
      legend_label <- dec_label
      legend_round <- 2
    } else {
      legend_label <- unit_label
      legend_round <- 0
      if(input$layer == "sea ice cover") legend_round <- 2
    }
    
    # Static legend units
    legendData <- ERA5_ALL %>% 
      dplyr::filter(name == input$layer,
                    cat == input$category)
    legendData <- seq(min(legendData$value, na.rm = T), max(legendData$value, na.rm = T), length.out = 6)
    legendData <- round(c(legendData), legend_round)
    
    # Prep data
    baseData <- baseData()
    mapBase <- mapBase()
    
    # Catch stragglers
    baseData <- baseData %>% 
      mutate(value = case_when(value > max(legendData) ~ max(legendData),
                               value < min(legendData) ~ min(legendData),
                               TRUE ~ value))
    
    # Create plot
    map_prep <- ggplot(data = baseData, aes(x = lon, y = lat)) +
      geom_polygon(data = mapBase, aes(group = group, text = NULL), colour = "black", fill = "grey90", alpha = 0.3) +
      geom_tile(aes(fill = value, text = value), alpha = 0.8) +
      coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F, ratio = 2) +
      labs(x = "Longitude (°E)", y = "Latitude (°N)") +
      theme(panel.border = element_rect(fill = NA, colour = "black"))
    
    # Add colour palette accordingly
    if(input$category %in% c("Trend", "Anomaly")) {
      map_plot <- map_prep +
        scale_fill_gradient2(paste0(input$layer,"\n",legend_label), low = "blue", high = "red", 
                             limits = c(min(legendData), max(legendData)), breaks = legendData)
    } else {
      map_plot <- map_prep +
        scale_fill_viridis_c(paste0(input$layer,"\n",legend_label), 
                             limits = c(min(legendData), max(legendData)), breaks = legendData)
    }
    
    # Add glacier layer
    if(input$glacier){
      map_plot <- map_plot + geom_polygon(data = glacier_fortified, fill = "lightblue",
                                          aes(group = group, text = "Glacier"))
    }
    
    # Add station layer
    if(input$station){
      map_plot <- map_plot + geom_point(data = pop_coords, aes(text = site),
                                        shape = 25, size = 8, colour = "black", fill = "grey50")
    }
    
    # Plotly output
    ggplotly(map_plot, tooltip = "text", dynamicTicks = F, source = "map_ly") %>% 
      style(hoverinfo = "skip", traces = 0)
  })
  

  # Time series figure ------------------------------------------------------
  
  # The TS plot
  output$ts <- renderPlotly({
    req(input$layer)
    
    # Prep value labels
    if(input$layer %in% c("T2m", "SST")){
      unit_label <- " (°C)"
      dec_label <- " (°C/dec)"
    } else if(input$layer == "sea ice cover"){
      unit_label <- " (%)"
      dec_label <- " (%/dec)"
    } else if(input$layer == "MSLP"){
      unit_label <- " (hPa)"
      dec_label <- " (hPa/dec)"
    } else if(input$layer == "snow melt"){
      unit_label <- " (mm)"
      dec_label <- " (mm/dec)"
    }
    
    # Click data
    event.data <- event_data(event = "plotly_click", source = "map_ly")

    # Create plot
    if (is.null(event.data)) {
      # Blank plot
      ts_plot <- ggplot() +
        geom_blank() +
        geom_text(aes(x = 0, y = 0, label = "Click on a map pixel to see the annual time series!")) +
        theme_void()
    } else if(event.data$curveNumber[1] == 2) {
      # Blank plot
      ts_plot <- ggplot() +
        geom_blank() +
        geom_text(aes(x = 0, y = 0, label = "Click on a map pixel to see the annual time series!")) +
        theme_void()
    } else if(event.data$curveNumber[1] == 1) {
      # input <- data.frame(layer = "T2m", category = "Mean") # tester...
      # event.data <- data.frame(x = 30, y = 75) # Tester...
      # Time series data
      
      pixelData <- ERA5_ALL %>%
        # data.frame() %>% 
        dplyr::filter(name == input$layer,
               cat == "Anomaly",
               lon == event.data$x[1],
               lat == event.data$y[1])
      
      # Linear model stats
      slopeData <- ERA5_ALL %>%
        # data.frame() %>% 
        dplyr::filter(name == input$layer[1],
                      cat == "Trend",
                      lon == event.data$x[1],
                      lat == event.data$y[1]) %>% 
        mutate(pvalue = case_when(pvalue == 0 ~ "p <0.01",
                                  TRUE ~ paste0("p = ",pvalue)),
               rvalue = paste0("R2 = ",rvalue),
               value = paste0("slope = ",value),
               text = paste0(value,dec_label,"\n",pvalue,"\n",rvalue))
      
      # Line plot
      ts_plot <- ggplot(data = pixelData, aes(x = year, y = value)) +
        geom_line(show.legend = F) +
        geom_point(aes(fill = value, text = value), shape = 21, show.legend = F) +
        geom_smooth(method = "lm", se = F, aes(text = slopeData$text[1])) +
        coord_cartesian(expand = F) +
        labs(x = NULL, y = paste0(input$layer,"\n",unit_label), 
             title = paste0("Longitude: ",slopeData$lon,"; Latitude: ",slopeData$lat)) +
        theme(panel.border = element_rect(fill = NA, colour = "black"))
    }

    # Plotly output
    ggplotly(ts_plot, tooltip = "text", dynamicTicks = F) #%>% 
      # style(hoverinfo = "skip", traces = 0)
  })
  

  # Test bits ---------------------------------------------------------------

  # output$maply_click <- renderText({
  #   event.data <- event_data(event = "plotly_click", source = "map_ly")
  #   if (is.null(event.data)) {
  #     print("Click to see the link of the point.")
  #   } else { 
  #     print(event.data)
  #   }
  # })
  
  # observe({
  #   event_click <- event_data("plotly_click", source = "map_ly")
  #   # hidden_labels <- relayout$hiddenlabels
  #   print(event_click)
  # })
  
  
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

