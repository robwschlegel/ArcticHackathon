# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
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

# MHW colour palette
# MHW_colours <- c(
#   "I Moderate" = "#ffc866",
#   "II Strong" = "#ff6900",
#   "III Severe" = "#9e0000",
#   "IV Extreme" = "#2d0000"
# )

# The empty dataframe for the legend
# MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

# Oliver 2018 MHW trend data
# oliver <- readRDS("data/Oliver_2018_sub.Rds") 

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

# MHW annual summary data
# MHW_summary <- readRDS("data/MHW_summary_sub.Rds") %>% 
#   mutate(year = lubridate::year(t))

# Glacier shapefile
# glacier_shp <- shapefile("../analyses/ESA_land_classes/Svalbard_glaciers_wgs84_postproc.shp")
# saveRDS(glacier_fortified, "data/glacier_fortified.Rds")
glacier_fortified <- readRDS("data/glacier_fortified.Rds") %>% 
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
         lat >= sval_bbox[3], lat <= sval_bbox[4])

# The two map projections
# inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

# Base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>% 
  # filter(subregion == "Svalbard")
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
       lat >= sval_bbox[3], lat <= sval_bbox[4])

# Popup coordinates
pop_coords <- data.frame(site = c("Airport"),
                         lon = c(15.4633742),
                         lat = c(78.2460841))

# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "blue",
                    
                    # The app title
                    dashboardHeader(disable = FALSE, title = "Troubled waters: environmental change around Svalbard"),
                    
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
                                fluidRow(textOutput("maply_click")),
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
                                         h2(tags$b("About")),
                                         p("This shiny app is designed to allow one to quickly and easily see what the trends in environmental change
                                           are around Svalbard. Clicking on a pixel when bring up an annual time series for further investigation.
                                           In the control panel on the left one may select what category of data one wants, the variable, and the year.
                                           If one is visualising trend data there will be no year to select."),
                                         # h2(tags$b("Map")),
                                         # p("The 'Map' tab shows Svalbard with.")
                                         h2(tags$b("Acknowledgements")),
                                         p("This app was built in part to serve as a proof of concept for how to easily and interactively visualise
                                           environmental change in the Arctic (Svalbard) as part of the #HackTheArctic event. Contributions to this 
                                           app represent work that is of interest towards the deliverables for...")
                                  )
                                )
                        )
                      )
                    )
)



#     bootstrapPage(
#     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#     leafletOutput("map", width = "100%", height = "100%"),
#     absolutePanel(top = 10, right = 10,
#                   sliderInput("year", "Year", min(MHW_summary$year), max(MHW_summary$year),
#                               value = 2020, step = 1, animate = T, width = '400px', sep = ""
#                   ),
#     #               selectInput("colors", "Color Scheme",
#     #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#     #               ),
#                   checkboxInput("legend", "Show legend", TRUE)
#     )
# )


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
                             animate = T, animationOptions(interval = 3000))
  
  # Glacier layer
  switch_glacier <- materialSwitch(inputId = "glacier", label = "Glacier layer:", status = "info")
  
  # Station layer
  switch_station <- materialSwitch(inputId = "station", label = "Station layer:", status = "primary")
  
  # The high level UI controls
  output$sidebar_controls_cat <- renderUI({
    if(input$comparisonMenu == "map"){
      sidebarMenu(switch_station, switch_glacier, picker_category, picker_layer)
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
  
  
  # Map figure --------------------------------------------------------------
  
  # The map
  output$map <- renderPlotly({
    req(input$layer); req(input$category); req(!is.null(input$glacier))
    # input <- data.frame(layer = "T2m", category = "Trend") # tester...
    
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
    
    # Create plot
    map_prep <- ggplot(data = baseData, aes(x = lon, y = lat)) +
      geom_polygon(data = map_base, aes(group = group, text = NULL), colour = "black", fill = "grey90", alpha = 0.3) +
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
                                        shape = 25, size = 10, colour = "black", fill = "hotpink")
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
    } else {
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
        labs(x = NULL, y = paste0(input$layer,"\n",unit_label)) +
        theme(panel.border = element_rect(fill = NA, colour = "black"))
    }

    # Plotly output
    ggplotly(ts_plot, tooltip = "text", dynamicTicks = F) #%>% 
      # style(hoverinfo = "skip", traces = 0)
  })
  

  # Test bits ---------------------------------------------------------------

  output$maply_click <- renderText({
    event.data <- event_data(event = "plotly_click", source = "map_ly")
    if (is.null(event.data)) {
      print("Click to see the link of the point.")
    } else { 
      print(event.data)
    }
  })
  
  observe({
    event_click <- event_data("plotly_click", source = "map_ly")
    # hidden_labels <- relayout$hiddenlabels
    print(event_click)
  })
  
  
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

