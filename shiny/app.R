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
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

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


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "blue",
                    
                    # The app title
                    dashboardHeader(disable = TRUE),
                    
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
                                fluidRow(box(plotlyOutput("ts", width = '100%', height = '200px'), width = 12, height = '200px', type = 6, color = "#b0b7be"),
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
                              choices = unique(ERA5_ALL$name),
                              multiple = FALSE,
                              options = list(size = 6),
                              selected = unique(ERA5_ALL$name)[1])
  
  # Select years from a dropdown
  picker_year <- pickerInput(inputId = "year", label = "Year:",
                             choices = seq(min(ERA5_ALL$year, na.rm = T), max(ERA5_ALL$year, na.rm = T)),
                             # choices = yearChoices(),
                             multiple = FALSE, selected = max(ERA5_ALL$year,na.rm = T), options = list(size = 5))
  
  # Glacier layer
  switch_glacier <- materialSwitch(inputId = "glacier", label = "Glacier layer:", status = "info")
  
  # The high level UI controls
  output$sidebar_controls_cat <- renderUI({
    if(input$comparisonMenu == "map"){
      sidebarMenu(picker_category, picker_layer)
    } else if(input$comparisonMenu == "about"){
      # Intentionally empty
    } else{
      # sidebarMenu()
    }
  })
  
  # The low level UI controls
  output$sidebar_controls_other <- renderUI({
    req(input$category)
    if(input$category == "Trend"){
      sidebarMenu(switch_glacier)
    } else {
      sidebarMenu(picker_year, switch_glacier)
    }
  })
  
  
  # Reactive values ---------------------------------------------------------
  
  # Years for selection
  # yearChoices <- reactiveValues({
  #   req(input$category)
  #   if(input$category == "Trend") {
  #     yearChoices <- NA
  #   } else {
  #     yearChoices <- seq(min(ERA5_ALL$year, na.rm = T), max(ERA5_ALL$year, na.rm = T))
  #   }
  #   return(yearChoices)
  # })
  
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
          filter(year == input$year)
      }
    return(baseData)
  })
  
  
  # Map figure --------------------------------------------------------------
  
  # The map
  output$map <- renderPlotly({
    req(input$layer); req(input$category); req(!is.null(input$glacier))
    
    # Prep data
    baseData <- baseData()
    
    # Create plot
    map_prep <- ggplot(data = baseData, aes(x = lon, y = lat)) +
      geom_polygon(data = map_base, aes(group = group, text = NULL), colour = "black", fill = "grey90", alpha = 0.3) +
      geom_tile(aes(fill = value, text = value), alpha = 0.8) +
      coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F, ratio = 2) +
      labs(x = NULL, y = NULL)
    if(input$category %in% c("Trend", "Anomaly")) {
      map_plot <- map_prep +
        # scale_fill_gradient2(low = "blue", high = "red") # For testing...
        scale_fill_gradient2(input$layer, low = "blue", high = "red")
    } else {
      map_plot <- map_prep +
        scale_fill_viridis_c(input$layer)
    }
    # if(input$layer %in% c("T2m", "MSLP")){
    #   map_plot <- map_plot + geom_tile(data = baseData, aes(fill = value, text = value), alpha = 0.8)
    # }
    
    # Add glacier layer
    if(input$glacier){
      map_plot <- map_plot + geom_polygon(data = glacier_fortified, aes(group = group, text = "Glacier"), fill = "lightblue")
    }

    # Plotly output
    ggplotly(map_plot, tooltip = "text", dynamicTicks = F) %>% 
      style(hoverinfo = "skip", traces = 0)
  })
  

  # Time series figure ------------------------------------------------------
  
  # The TS plot
  output$ts <- renderPlotly({
    req(input$year); req(input$layer); req(input$category)
    
    # Prep data
    pixelData <- ERA5_ALL %>%
      # filter(name == "SST", cat == "Mean") %>%  # For testing...
      filter(name == input$layer,
             cat == input$category) %>%
      group_by(year) %>% 
      summarise(value = mean(value, na.rm = T), .groups = "drop")
    
    # Create plot
    ts_plot <- ggplot(data = pixelData, aes(x = year, y = value)) +
      geom_line() +
      geom_point(aes(fill = value, text = value), shape = 21) +
      # labs(x = NULL, y = "Name") # For testing...
      labs(x = NULL, y = input$layer)
    
    # Plotly output
    ggplotly(ts_plot, tooltip = "text", dynamicTicks = F)# %>% 
      # style(hoverinfo = "skip", traces = 0)
  })
  
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

