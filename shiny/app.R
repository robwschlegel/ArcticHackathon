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
library(readr)
library(lubridate)
library(ggplot2)
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
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

# Oliver 2018 MHW trend data
oliver <- readRDS("data/Oliver_2018_sub.Rds") 

# ERA5 annual anomalies
ERA5_anom <- readRDS("data/ERA5_anom_smol.Rds") %>% 
  pivot_longer(T2m:`snow melt`) %>% 
  mutate(cat = "Anomaly")

# ERA5 annual means
ERA5_mean <- readRDS("data/ERA5_mean_smol.Rds") %>% 
  pivot_longer(T2m:`snow melt`) %>% 
  mutate(cat = "Mean")

# Combine for ease of filtering
ERA5_ALL <- rbind(ERA5_anom, ERA5_mean)

# MHW annual summary data
MHW_summary <- readRDS("data/MHW_summary_sub.Rds") %>% 
  mutate(year = lubridate::year(t))

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


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "blue",
                    
                    # The app title
                    dashboardHeader(disable = TRUE),
                    
                    # The primary options
                    dashboardSidebar(
                      sidebarMenu(id = "comparisonMenu",
                                  # menuItem("Annual", tabName = "annual", icon = icon("chart-line")),
                                  # menuItem("Daily", tabName = "daily", icon = icon("chart-line")),
                                  menuItem("Map", tabName = "map", icon = icon("map"), selected = TRUE),
                                  # menuItem("Tables", tabname = "tables", icon = icon("table")),
                                  menuItem("About", tabName = "about", icon = icon("question")),
                                  # The reactive controls based on the primary option chosen
                                  uiOutput(outputId = "sidebar_controls"))
                    ),
                    
                    # The dashboard
                    dashboardBody(
                      tabItems(
                        
                        
                        # Total figures -----------------------------------------------------------
                        
                        # tabItem(tabName = "annual",
                        #         fluidRow(box(plotlyOutput(ns("totalCount")), width = 12, 
                        #                      title = "Average daily MHW occurrence per year (% of ocean)",
                        #                      status = "primary", solidHeader = TRUE, collapsible = TRUE),
                        #                  box(plotlyOutput(ns("totalFirst")), width = 12, 
                        #                      title = "Total coverage per year (% of ocean)",
                        #                      status = "warning", solidHeader = TRUE, collapsible = TRUE),
                        #                  box(plotlyOutput(ns("totalCum")), width = 12, 
                        #                      title = "Total MHW days per pixel per year",
                        #                      status = "success", solidHeader = TRUE, collapsible = TRUE))),
                        
                        
                        # Daily figures -----------------------------------------------------------
                        
                        # tabItem(tabName = "daily",
                        #         fluidRow(box(plotlyOutput(ns("dailyCount")), width = 12, title = "Daily count (% of ocean)",
                        #                      status = "primary", solidHeader = TRUE, collapsible = TRUE),
                        #                  box(plotlyOutput(ns("dailyFirst")), width = 12, title = "Total coverage (% of ocean)",
                        #                      status = "warning", solidHeader = TRUE, collapsible = TRUE),
                        #                  box(plotlyOutput(ns("dailyCum")), width = 12, title = "MHW days per pixel",
                        #                      status = "success", solidHeader = TRUE, collapsible = TRUE))),
                        
                        # Map figures -------------------------------------------------------------
                        
                        tabItem(tabName = "map",
                                # fluidRow(box(shinycssloaders::withSpinner(plotOutput(ns("compOISST")), type = 6, color = "#b0b7be"),
                                #              width = 4, title = "OISST", status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                #          box(shinycssloaders::withSpinner(plotOutput(ns("compCCI")), type = 6, color = "#b0b7be"),
                                #              width = 4, title = "CCI", status = "success", solidHeader = TRUE, collapsible = TRUE),
                                #          box(shinycssloaders::withSpinner(plotOutput(ns("compCMC")), type = 6, color = "#b0b7be"),
                                #              width = 4, title = "CMC", status = "warning", solidHeader = TRUE, collapsible = TRUE)),
                                # bootstrapPage(
                                #     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                #     leafletOutput("map", width = "100%", height = "100%"))),
                                fluidRow(box(plotlyOutput("map", width = '100%', height = '600px'), width = 12, height = '600px', type = 6, color = "#b0b7be"),
                                         width = 12, status = "danger", solidHeader = TRUE, collapsible = FALSE),
                                fluidRow()),
                        
                        
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
                                                 are around Svalbard."),
                                         h2(tags$b("Map")),
                                         p("The 'Map' tab shows Svalbard.")
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
                                 choices = c("Trend", "Mean", "Anomaly"), 
                                 multiple = FALSE, selected = "Anomaly")
  
  # Specific layer for selection
  # picker_layer <- pickerInput(inputId = "layer", label = "Data layers:",
  #                            choices = list(
  #                              Trend = c("Trend: SST", "Trend: sea ice cover", "Trend: T2M", "Trend: MSLP", "Trend: snow melt"),
  #                              Mean = c("Mean: SST", "Mean: sea ice cover", "Mean: T2M", "Mean: MSLP", "Mean: snow melt"),
  #                              Anomaly = c("Anomaly: SST", "Anomaly: sea ice cover", "Anomaly: T2M", "Anomaly: MSLP", "Anomaly: snow melt")
  #                            ),
  #                            multiple = FALSE,
  #                            options = list(size = 6),
  #                            selected = "Anomaly: SST")
  picker_layer <- pickerInput(inputId = "layer", label = "Data layers:",
                              choices = c("SST", "sea ice cover", "T2M", "MSLP", "snow melt"),
                              multiple = FALSE,
                              options = list(size = 6),
                              selected = "SST")
  
  # Glacier layer
  switch_glacier <- materialSwitch(inputId = "glacier", label = "Glacier layer:", status = "info")
  
  # Select years from a dropdown
  picker_year <- pickerInput(inputId = "year", label = "Year:",
                             choices = seq(1982, 2020), multiple = FALSE,
                             selected = 2019, options = list(size = 5))
  
  # The chosen controls per tab
  output$sidebar_controls <- renderUI({
    if(input$comparisonMenu == "map"){
      sidebarMenu(picker_category, picker_layer, switch_glacier, picker_year)
    } else if(input$comparisonMenu == "daily"){
      # sidebarMenu(picker_year)
    } else if(input$comparisonMenu == "annual"){
      # sidebarMenu()
    } else if(input$comparisonMenu == "about"){
      # Intentionally empty
    } else{
      # sidebarMenu()
    }
  })
  
  
  # Reactive values ---------------------------------------------------------
  
  # Values that appear in the list of selectable data layers
  
  # Pre base data
  baseDataPre <- reactive({
    red(input$category)
  })
  
  # Reactive expression for the data subsetted to what the user selected
  baseData <- reactive({
    req(input$layer); req(input$layer); req(input$year)
    # if(input$layer == "Anomaly: SST"){
      baseData <- ERA5_ALL %>% 
        # dplyr::filter(year == 1990,  # For testing...
        #               name == "SST",
        #               cat == "Mean")
        dplyr::filter(year == input$year,
                      name == input$layer,
                      cat == input$category)
    # } else {
      # baseData <- MHW_summary %>% 
        # dplyr::filter(year == 1990) %>% # For testing... 
        # dplyr::filter(year == input$year[1]) #%>% 
        # dplyr::select(lon, lat, category) %>% 
        # mutate(text = paste0("Lon: ",lon,"\n",
        #                      "Lat: ",lat,"\n",
        #                      "Category: ",category))
    # }
    return(baseData)
  })
  
  
  # Map figures -------------------------------------------------------------
  
  # The map
  output$map <- renderPlotly({
    req(input$year); req(input$layer); req(input$category)
    
    # Prep data
    baseData <- baseData()
    # baseData <- MHW_summary %>%
    #     filter(year == 1990)
    
    # Don't want tooltip for map of Svalbard
    trace_skip <- 1
    
    # Create plot
    map_prep <- ggplot(data = baseData, aes(x = lon, y = lat)) +
      geom_polygon(data = map_base, aes(group = group, text = NULL), colour = "black", fill = "grey30") +
      coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F, ratio = 2) +
      labs(x = NULL, y = NULL)
    if(input$category %in% c("Trend", "Anomaly")) {
      map_plot <- map_prep +
        geom_tile(aes(fill = value, text = "")) +
        # scale_fill_gradient2(low = "blue", high = "red") # For testing...
        scale_fill_gradient2(input$layer, low = "blue", high = "red")
    } else {
      map_plot <- map_prep +
        geom_tile(aes(fill = value, text = "")) +
        # scale_fill_gradient2(low = "blue", high = "red") # For testing...
        scale_fill_viridis_c(input$layer)
    }
    # } else {
    #   map_plot <- ggplot(data = baseData, aes(x = lon, y = lat)) +
    #     geom_polygon(data = map_base, aes(group = group, text = NULL), colour = "black", fill = "grey30") +
    #     # geom_polygon(data = glacier_fortified, aes(group = group, text = NULL), fill = "lightblue") +
    #     geom_tile(aes(fill = category, text = "")) +
    #     scale_fill_manual(values = MHW_colours) +
    #     coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F, ratio = 2) +
    #     labs(x = NULL, y = NULL)
    # }
    
    # Add glacier layer
    if(input$glacier){
      map_plot <- map_plot + geom_polygon(data = glacier_fortified, aes(group = group, text = NULL), fill = "lightblue")
      trace_skip <- c(1, 5)
    }

    # Plotly output
    ggplotly(map_plot, tooltip = "text", dynamicTicks = F) %>% 
      style(hoverinfo = "skip", traces = trace_skip)
  })
  
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

