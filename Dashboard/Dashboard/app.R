#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# List of packages used
#packages <- c("shiny", "leaflet", "dplyr", "leaflet.minicharts", "ggplot2", "rgdal", "reshape2", "htmltools", "plotly")

# Check if each package is installed (if not, install them) and load them
#for (lib in packages){
#    if (!require(lib, character.only=TRUE)){
#        install.packages(lib, character.only=TRUE, dependencies=TRUE)
#        library(lib, character.only = TRUE)
#    }
#}

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(rgdal)
library(reshape2)
library(htmltools)
library(plotly)

# Read the data from the dataset
data <- read.csv("datasetCODA.csv")
data[is.na(data)] = 0
data <- data %>% mutate(DATE = as.Date(DATE, format="%Y-%m-%d"))

# Complementary file for functions
source("functions.R")

# Prepare data for map
spdf = readOGR(dsn=getwd(), layer="World_Countries")
spdf@data$COUNTRY <- toupper(spdf@data$COUNTRY)
spdf@data <- recodeCountries(spdf@data)
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
mapdf <- data %>% 
    mutate(CONFIRMED = CUMULATIVE_CONFIRMED,
           DEATHS = CUMULATIVE_DEATHS,
           RECOVERED = CUMULATIVE_RECOVERED)

# Prepare variables for select boxes and radio buttons
countries <- unique(data$COUNTRY)
variablesGraph <- names(data)[-c(1:3)]
variablesMap <- c("CONFIRMED", "DEATHS", "RECOVERED", "ACTIVE", "DANGER_INDEX", "R0", "INC")

# Filter parameters and choices
modes = c("Multiple Countries" = "mc", "Multiple Variables" = "mv")
graphTypes = c("Line Graph" = "line", "Box Plot" = "box")

# Starting date and last update of the dataset
startDate <- as.Date("2020-03-08")
endDate <- as.Date("2021-05-12")

# Dashboard UI

header <- dashboardHeader(disable = TRUE)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height;

        $("#map").height(boxHeight - 50);
        $("#mapTable").css("top", boxHeight - 320);
        $("#plot").height(boxHeight - 305);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
    tabsetPanel(
        tabPanel(title = "Graph",
            fluidRow(
                box(width = 2, height = 200, status="primary",
                    h3("Global cases"),
                    h4(textOutput("totalCases")),
                    h3("Global deaths"),
                    h4(textOutput("totalDeaths"))
                ),
                box(width = 3, height = 200, status="primary",
                    tableOutput(outputId = "graphTable")
                ),
                box(width = 7, height = 200, status="primary",
                    fluidRow(
                        column(id = "filterBox", width = 6,
                            column(width=7, radioButtons(inputId = "mode", label = "Mode:", choices = modes)),
                            conditionalPanel("input.mode == 'mc'",
                                column(width=5, radioButtons(inputId = "graph", label = "Graph Type:", choices = graphTypes))
                            ),
                            column(width=12,
                                sliderInput(inputId = "date", label = "Date:", min = startDate, max = endDate,
                                            value=c(endDate - 6 * 30, Sys.Date()), timeFormat="%b %d %Y")
                            )
                        ),
                        column(width = 6,
                            conditionalPanel("input.mode == 'mc'",
                                selectizeInput(inputId = "country_mc", label = "Choose countries:", choices = countries, multiple = TRUE, selected = countries[1], options = list(maxItems = 5)),
                                selectInput(inputId = "variable_mc", label = "Choose a variable:", choices = variablesGraph)
                            ),
                            conditionalPanel("input.mode == 'mv'",
                                selectInput(inputId = "country_mv", label = "Choose a country:", choices = countries),
                                selectizeInput(inputId = "variable_mv", label = "Choose variables:", choices = variablesGraph, multiple = TRUE, selected = variablesGraph[1], options = list(maxItems = 5))
                            )                   
                        )
                    )
                
                )
        
            ),
            box(id = "content", width = 12, status="primary",
                plotlyOutput(outputId = "plot")
            )
        ),
        tabPanel("Map",
             leafletOutput(outputId = "map", width = "100%", height = 610),
             absolutePanel(top = 150, left = 30,
                 sliderInput(inputId = "mapSlider", "Date", startDate, endDate, endDate, timeFormat="%b %d %Y", width="200px"),
                 selectizeInput(inputId = "variableMap", label = "Choose a variable:", choices = list(Cumulative = variablesMap[c(1:4)], Daily = variablesMap[c(5:7)]), selected = variablesMap[1], width = "150px"),
             ),
             absolutePanel(top = 70, right = 20, width = 250,
                 box(width = 12, status="primary",
                     h2(textOutput("selectedCountryMap")),
                     h5("(Cumulative values)"),
                     tableOutput(outputId = "countryTable")
                 )
             ),
             absolutePanel(id = "mapTable", right = 20, width = 250,
                 box(width = 12, status="primary",
                     h2("Top Countries"),
                     h5("(Cumulative values)"),
                     tableOutput(outputId = "mapTable")
                 )
             )
         )
    )
)

ui <- dashboardPage(header, sidebar, body)

# Dashboard server function
server <- function(input, output) {
    # Graph functions
    
    # Graph colors
    colors <- c("darkslateblue", "indianred3", "gold", "forestgreen", "gray45")
    
    # Global data table
    output$totalCases <- renderText({
        sum(data$DAILY_CONFIRMED, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    output$totalDeaths <- renderText({
        sum(data$DAILY_DEATHS, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    
    # Radio buttons observer
    observeEvent({
        input$graph
        input$mode
        }, {
            #Graph Plot
            output$plot <- renderPlotly({
                if(input$mode == "mc"){
                    dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
                    if(is.null(input$country_mc) || dim(dfReactive())[1] == 0)
                        ggplot()
                    else if(input$graph == "line"){
                        p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y=input$variable_mc, color="COUNTRY")) + geom_line() + geom_point(size=0.9)
                        drawLinePlot(p, colors)
                    }
                    else if(input$graph == "box"){
                        p <- ggplot(dfReactive(), mapping = aes_string(x="COUNTRY", y=input$variable_mc, color="COUNTRY")) + geom_boxplot()
                        drawBoxPlot(p, colors)
                    }
                }
                else if(input$mode == "mv"){
                    dfReactive <- reactive({getVariables(data, input$country_mv, input$variable_mv) %>% filter(between(DATE, input$date[1], input$date[2]))})
                    if(is.null(input$variable_mv) || dim(dfReactive())[1] == 0)
                        ggplot()
                    else {
                        p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y="value", color="variable")) + geom_line() + geom_point(size=0.9)
                        drawLinePlot(p, colors)
                    }
                }
            })
            
            # Graph Table
            output$graphTable <-renderTable({ 
                
                if(input$mode == "mc"){
                    #output$selectedVarGraph <- renderText({paste0("Countries by number of ", tolower(input$variable_mv[1]))})
                    showTable(data, input$variable_mc, input$date[2])
                }
                else if(!is.null(input$variable_mv[1])){
                    #output$selectedVarGraph <- renderText({paste0("Countries by number of ", tolower(input$variable_mv[1]))})
                    showTable(data, input$variable_mv[1], input$date[2])
                }
            }, digits = 0) 
    })

    # Map Functions 
    
    # Map Data Slider and Select Box observer
    observeEvent({
        input$mapSlider
        input$variableMap
        }, { 
            # Set variables for map data
            varString <- input$variableMap
            column <- varString
            if(varString %in% c("CONFIRMED", "DEATHS", "RECOVERED"))
                column <- paste0("CUMULATIVE_", input$variableMap)
            mapdf <- mapdf %>% filter(DATE == input$mapSlider) 
            spdf@data <- left_join(spdf@data, mapdf, by = "COUNTRY")
            mypalette <- setPalette(spdf@data, column)
            df <- spdf@data
            labs <- lapply(seq(nrow(df)), function(i) {
                num <- format(df[i, column], big.mark = ',', scientific=FALSE)
                paste0(df[i, "COUNTRY"], '<br>', num)
            })
            
            # Global cases information table
            output$mapTable <-renderTable({ 
                showTable(mapdf, column, input$mapSlider)
            })
            
            # Map rendering
            output$map <- renderLeaflet({
                leaflet(spdf) %>%
                addTiles(tilesURL) %>%
                setView(lng = 40.2085, lat = -3.713, zoom = 2)  %>% 
                addLegend(pal = mypalette, values = ~spdf@data[[column]],
                          opacity = 0.5, title = "Legend",position = "bottomleft") %>%
                addPolygons(data = spdf,
                    fillColor = ~mypalette(as.numeric(spdf@data[[column]])),
                    weight = 0.05,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(weight=3,
                        color="gray",
                        fillOpacity=0.7,
                        bringToFront=TRUE),
                    layerId = sapply(slot(spdf, "polygons"), function(x) slot(x, "ID")),
                    label=lapply(labs, HTML),
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal"),
                        textsize = "15px",
                        direction = "auto"))
            })
    })
    
    # Initialize country specific table
    output$selectedCountryMap <- renderText({"Spain"})
    output$countryTable <- renderTable({
        showCountryTable(mapdf, "SPAIN", input$mapSlider)
    })
    
    # Map click observer
    observeEvent(input$map_shape_click, {
        country <- input$map_shape_click
        countryString <- getCountryName(spdf@data$COUNTRY, country)
        
        output$selectedCountryMap <- renderText({countryString})
        output$countryTable <- renderTable({
            showCountryTable(mapdf, countryString, input$mapSlider)
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
