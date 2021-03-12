# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(raster)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
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
                                    menuItem("How To", tabName = "about", icon = icon("question")),
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
                                    fluidRow(box(shinycssloaders::withSpinner(leafletOutput("map")), type = 6, color = "#b0b7be"), 
                                             width = 16, title = "Svalbard", status = "danger", solidHeader = TRUE, collapsible = TRUE)),
                            
                            
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
                                       #         h2(tags$b("Annual")),
                                       #         p("Clicking on the 'Annual' tab at the top of the menu bar to the left will open a new tab screen that
                                       # shows three line graphs. The x-axis for each line graph is in years. The first line graph shows the average daily 
                                       # % of the ocean that experienced MHWs during a given year. The second line graph shows the total % of the ocean that
                                       # experienced at least one MHW in a given year. The third line graph shows the average number of MHW days experienced
                                       # by the entire ocean. On each graph there are separate lines for each category of MHW and each product. One may turn
                                       # lines on and off by clicking on the corresponding category/product name in the legend. One may also zoom in on any
                                       # area of a plot by clicking and dragging like a selection tool. Hovering over any area of a plot will bring up 
                                       # additional information. Hovering over any graph will also bring up a menu bar on the top right with additional
                                       # controls to explore. In the menu bar on the left one may also choose which climatology period to use for the 
                                       # comparison of the annual results. The climatology period that most closely matches the current WMO standard would
                                       # be '1982-2011' however; the first full year of the CMC product is 1992, meaning that to compare all three the
                                       # climatology period '1992-2018' must be used."),
                                       #         h2(tags$b("Daily")),
                                       #         p("This tab contains a similar set of three line graphs as to the 'Annual' tab above it. The primary difference being
                                       # that the x-axis of each figure is now in days, rather than years. One may now choose in the menu bar which 
                                       # year to display. Note that the range of data available for the CMC product is 1992 - 2019, and for CCI it is
                                       # 1982 - 2018. All years are available for the OISST product."),
                                               h2(tags$b("Map")),
                                               p("The 'Map' tab shows Svalbard.")#,
                                       #         p(tags$b("NB:")," The maps may take up to one minute to render due to their size."),
                                       #         h2(tags$b("Bugs")),
                                       #         p("To report any bugs or to provide any other feedback on the app please contact 
                                       # the developer at: robert.schlegel@dal.ca"),
                                       #         h2(tags$b("References")),
                                       #         h4(tags$b("MHW")),
                                       #         p("The marine heatwave data displayed in this app were calculated with the heatwaveR R package. To cite
                                       # heatwaveR in publications please use:"),
                                       #         p("Schlegel, R. W., and Smit, A. J. (2018). heatwaveR: a central algorithm for the detection of heatwaves 
                                       # and cold-spells. J. Open Sour. Softw. 3:821. doi: 10.21105/joss.00821"),
                                       #         p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
                                       #         p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C. J., et al. (2016). 
                                       # A hierarchical approach to defining marine heatwaves. Progr. Oceanogr. 141, 227–238. 
                                       # doi: 10.1016/j.pocean.2015.12.014"),
                                       #         p("Hobday, A. J., Oliver, E. C. J., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., et al. (2018).
                                       # Categorizing and naming marine heatwaves. Oceanography 31, 162–173. doi: 10.5670/oceanog.2018.5205"),
                                       #         h4(tags$b("NOAA OISST")),
                                       #         p("Any use of the NOAA OISST data should be accompanied by the following reference:"),
                                       #         p("Reynolds, R. W., Smith, T. M., Liu, C., Chelton, D. B., Casey, K. S., and Schlax, M. G. (2007). 
                                       # Daily high-resolution-blended analyses for sea surface temperature. J. Clim. 20, 5473–5496. 
                                       # doi: 10.1175/2007JCLI1824.1"),
                                       #         p("The use of v2.0 data should additionally be accompanied with this reference:"),
                                       #         p("Banzon, V., Smith, T. M., Chin, T. M., Liu, C., and Hankins, W. (2016). A long-term record of blended 
                                       # satellite and in situ sea-surface temperature for climate monitoring, modeling and environmental studies. 
                                       # Earth Syst. Sci. Data 8, 165–176. doi: 10.5194/essd-8-165-2016"),
                                       #         p("The use of v2.1 data should refer to this publication:"),
                                       #         p("Banzon, V., Smith, T. M., Steele, M., Huang, B., & Zhang, H. M. (2020). Improved Estimation of Proxy 
                                       # Sea Surface Temperature in the Arctic. Journal of Atmospheric and Oceanic Technology, 37(2), 341-349. 
                                       # doi: 10.1175/JTECH-D-19-0177.1"),
                                       #         h4(tags$b("ESA CCI SST")),
                                       #         p("Any use of the ESA CCI SST data should be accompanied by the following reference:"),
                                       #         p("Merchant, C. J., Embury, O., Bulgin, C. E., Block, T., Corlett, G. K., Fiedler, E., ... & Eastwood, S. 
                                       # (2019). Satellite-based time-series of sea-surface temperature since 1981 for climate applications. 
                                       # Scientific data, 6(1), 1-18. doi: 10.1038/s41597-019-0236-x"),
                                       #         h4(tags$b("CMC SST")),
                                       #         p("Any use of the CMC SST data should be accompanied by the following reference:"),
                                       #         p("Meissner, T., Wentz, F. J., Scott, J., & Vazquez-Cuervo, J. (2016). Sensitivity of ocean surface 
                                       # salinity measurements from spaceborne L-band radiometers to ancillary sea surface temperature. 
                                       # IEEE Transactions on Geoscience and Remote Sensing, 54(12), 7105-7111. doi: 10.1109/TGRS.2016.2596100")
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

