# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(raster)
library(shinydashboard)
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

# The two map projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"


# UI ----------------------------------------------------------------------

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("year", "Year", min(MHW_summary$year), max(MHW_summary$year),
                              value = 2020, step = 1, animate = T, width = '400px', sep = ""
                  ),
    #               selectInput("colors", "Color Scheme",
    #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    #               ),
                  checkboxInput("legend", "Show legend", TRUE)
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    baseData <- reactive({
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

