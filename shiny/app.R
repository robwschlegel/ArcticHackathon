# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(broom)
library(ggplot2)
# library(RColorBrewer)


# Data --------------------------------------------------------------------

# For testing
# setwd("shiny/")

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

# MHW annual summary data
MHW_summary <- readRDS("data/MHW_summary_sub.Rds") %>% 
    mutate(year = lubridate::year(t))

# Glacier shapefile
glacier_shp <- shapefile("../analyses/ESA_land_classes/Svalbard_glaciers_wgs84_postproc.shp")
# plot(glacier_shp)
# glacier_fortified <- broom::tidy(glacier_shp, region = "DN")
# ggplot() +
#     geom_polygon(data = glacier_fortified, aes( x = long, y = lat, group = group), fill = "#69b3a2", color = "white") +
#     theme_void()
# saveRDS(glacier_fortified, "data/glacier_fortified.Rds")
glacier_fortified <- readRDS("data/glacier_fortified.Rds")

# The two map projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "black",
                    
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
                                    fluidPage(box(shinycssloaders::withSpinner(plotlyOutput("map")), type = 6, color = "#b0b7be"),
                                              width = 12, title = "Svalbard", status = "danger", solidHeader = TRUE, collapsible = FALSE)),
                            
                            
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

    # Select years from a dropdown
    picker_year <- pickerInput(inputId = "year", label = "Year:",
                               choices = seq(1982, 2020), multiple = FALSE,
                               selected = 2019, options = list(size = 5))
    
    # The chosen controls per tab
    output$sidebar_controls <- renderUI({
        if(input$comparisonMenu == "map"){
            sidebarMenu(picker_year)
        } else if(input$comparisonMenu == "daily"){
            sidebarMenu(picker_year)
        } else if(input$comparisonMenu == "annual"){
            # sidebarMenu()
        } else if(input$comparisonMenu == "about"){
            # Intentionally empty
        } else{
            # sidebarMenu()
        }
    })
    
    
    # Filter data -------------------------------------------------------------
    
    # Reactive expression for the data subsetted to what the user selected
    baseData <- reactive({
        req(input$year)
        MHW_summary %>% 
            filter(year == input$year[1]) %>% 
            dplyr::select(lon, lat, category)
    })
    
    # Reactive colour palette
    pal_react <- reactive({
        baseData <- baseData()
        # if(input$layer == "Anomaly: OISST"){
        #     domain_low <- min(baseDataPre$anom, na.rm = T)
        #     domain_high <- max(baseDataPre$anom, na.rm = T)
        # }
        # if(input$layer %in% trend_layers){
        #     domain_low <- min(baseDataPre$val, na.rm = T)
        #     domain_high <- max(baseDataPre$val, na.rm = T)
        # }
        # if(input$layer %in% cat_layers){
        #     domain_low <- 1; domain_high <- 1
        # }
        # if(abs(domain_high) > abs(domain_low)){
        #     domain_low <- -domain_high
        # } else{
        #     domain_high <- -domain_low
        # }
        # if(input$layer %in% cat_layers){
            colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
        # } else {
        #     colorNumeric(palette = c( "blue", "white", "red"), na.color = NA, 
        #                  domain = c(domain_low, domain_high))
        # }
    })

    
    # Project data ------------------------------------------------------------
    
    ### Non-shiny-projected raster data
    rasterNonProj <- reactive({
        baseData <- baseData()
        # if(input$layer %in% c("Category: OISST", "Summary: OISST")){
        #     MHW_raster <- baseData %>%
        #         dplyr::select(lon, lat, category) 
        # } else if(input$layer == "Anomaly: OISST"){
        #     MHW_raster <- baseData %>%
        #         dplyr::select(lon, lat, anom) 
        # } else if(input$layer %in% trend_layers){
        #     MHW_raster <- baseData %>%
        #         dplyr::select(lon, lat, val)
        # } else {
            MHW_raster <- baseData
        # }
        colnames(MHW_raster) <- c("X", "Y", "Z")
        MHW_raster$Z <- as.numeric(MHW_raster$Z)
        suppressWarnings(
            rasterNonProj <- raster::rasterFromXYZ(MHW_raster, res = c(0.25, 0.25),
                                                   digits = 3, crs = inputProj)
        )
        return(rasterNonProj)
    })
    
    ### Shiny-projected raster data
    rasterProj <- reactive({
        rasterNonProj <- rasterNonProj()
        # NB: Smoothing the pixels is easy, but causes massive artifacts
        # if(input$pixels == "Smooth"){
        # rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "bilinear")
        # } else {
        suppressWarnings(
            rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
        )
        # }
        return(rasterProj)
    })
    
    ### The reactive map
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(data = MHW_cat_clim_sub, options = leafletOptions(zoomControl = FALSE)) %>% 
            addTiles() %>%
            # leaflet::addPolygons(glacier_shp) %>% 
            fitBounds(9, 76, 30, 81) %>% 
            addScaleBar(position = "bottomright")
            # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        # pal <- colorpal()

        leafletProxy("map") %>% #, data = rasterProj()) %>%
            # clearShapes() %>%
            addRasterImage(rasterProj(), colors = pal_react(), layerId = "map_raster",
                           project = FALSE, opacity = 0.8)
    })
    
    # Use a separate observer to recreate the legend as needed.
    # observe({
    #     proxy <- leafletProxy("map", data = MHW_cat_clim_sub)
    # 
    #     # Remove any existing legend, and only if the legend is
    #     # enabled, create a new one.
    #     proxy %>% clearControls()
    #     if (input$legend) {
    #         pal <- pal_react()
    #         proxy %>% addLegend(position = "bottomright",
    #                             bins = 4,
    #                             pal = pal, values = ~category
    #         )
    #     }
    # })
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

