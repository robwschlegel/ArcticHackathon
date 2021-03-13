# shiny/app.R
# This script contains all of the code needed to run the Svalbard shiny app


# Libraries ---------------------------------------------------------------

library(shiny)
# library(leaflet)
library(dplyr)
library(lubridate)
# library(raster)
# library(rgdal)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
# library(broom)
library(ggplot2)
library(plotly)
library(rasterly)
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

# MHW annual summary data
MHW_summary <- readRDS("data/MHW_summary_sub.Rds") %>% 
    mutate(year = lubridate::year(t))

# Glacier shapefile
# glacier_shp <- shapefile("../analyses/ESA_land_classes/Svalbard_glaciers_wgs84_postproc.shp")
# plot(glacier_shp)

# ggplot() +
#     geom_polygon(data = glacier_fortified, aes( x = long, y = lat, group = group), fill = "#69b3a2", color = "white") +
#     theme_void()
# saveRDS(glacier_fortified, "data/glacier_fortified.Rds")
glacier_fortified <- readRDS("data/glacier_fortified.Rds")

# The two map projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

# Base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>% 
    filter(subregion == "Svalbard")
    # filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
    #        lat >= sval_bbox[3], lat <= sval_bbox[4])


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
                                    fluidRow(box(shinycssloaders::withSpinner(plotlyOutput("map")), width = 12, type = 6, color = "#b0b7be"),
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
        dplyr::select(lon, lat, category) %>% 
        mutate(text = paste0("Lon: ",lon,"\n",
                             "Lat: ",lat,"\n",
                             "Category: ",category))
    })
    
    
    # Map figures -------------------------------------------------------------
    
    # The map
    output$map <- renderPlotly({
        req(input$year)#; req(input$clim_period)
        # baseData <- MHW_summary %>%
        #     filter(year == 1990)
        baseData <- baseData()
        map_plot <- ggplot(data = baseData, aes(x = lon, y = lat)) +
            # borders(colour = "black", fill = "grey30") +
            geom_polygon(data = map_base, aes(group = group, text = NULL), colour = "black", fill = "grey30") +
            # geom_polygon(data = glacier_fortified, aes(group = group, text = NULL), fill = "lightblue") +
            geom_tile(aes(fill = category, text = "")) +
            scale_fill_manual(values = MHW_colours) +
            coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F) +
            labs(x = NULL, y = NULL)
        # map_plot <- plotRasterly(data = baseData, aes(x = lon, y = lat, colour = category),
        #                          alpha = 1, point_size = 5, shape = 15, show_raster = T, color = MHW_colours,
        #                          as_image = F, sizing = "contain", plot_width = 300, plot_height = 150) #+
            # scale_fill_manual(values = MHW_colours) +
            # scale_colour_manual(values = MHW_colours) +
            # coord_equal(xlim = sval_bbox[1:2], ylim = sval_bbox[3:4], expand = F)
        # map_plot <- plot_ly(data = baseData) %>% add_rasterly_image(x = ~lon, y = ~lat)
        # map_plot
        ggplotly(map_plot, tooltip = "text", dynamicTicks = F) %>% 
          style(hoverinfo = "skip", traces = 1)
        # plot_ly(baseData) %>% 
        #     add_rasterly_image(x = ~lon, y = ~lat, color = ~category,
        #                        # even `color_map` is deprecated,
        #                        # it is still a good way to specify the color mapping
        #                        color = MHW_colours, 
        #                        plot_width = 400, plot_height = 400)
        # baseDataxyz <- baseData %>% 
        #     dplyr::rename(x = lon, y = lat, z = category)
        # plot_geo() %>% 
        #     add_rasterly_image(raster(baseDataxyz))
    })
    
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

