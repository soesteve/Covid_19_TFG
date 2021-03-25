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
library(leaflet)
library(dplyr)
library(leaflet.minicharts)
library(ggplot2)
library(rgdal)
library(reshape2)
library(htmltools)
library(plotly)

# Read the data from the dataset
data <- read.csv("worldWithoutUSA.csv")
#data <- bind_rows(read.csv("DataUSA.csv"), data)
data[is.na(data)] = 0

# Complementary file for functions
source("functions.R")

# Rename the columns of the dataset
data <- renameColumns(data)

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
variablesGraph <- names(data)[c(3:9)]
variablesMap <- c("CONFIRMED", "DEATHS", "RECOVERED", "ACTIVE")

# Filter parameters and choices
modes = c("Multiple Countries" = "mc", "Multiple Variables" = "mv")
graphTypes = c("Line Graph" = "line", "Box Plot" = "box")

# Starting date and last update of the dataset
startDate <- as.Date("2020-03-08")
endDate <- as.Date("2021-03-01")

# Dashboard UI
ui <- fluidPage(
    tabsetPanel(
        tabPanel(title = "Graph",
            fluidRow(
                column(width = 3, 
                    wellPanel(
                        h3("Global cases"),
                        h4(textOutput("totalCases")),
                        br(),
                        h3("Global deaths"),
                        h4(textOutput("totalDeaths"))
                    )
                ),
                column(width = 3,
                    wellPanel(
                        tableOutput(outputId = "graphTable")
                    )
                ),
                column(width = 6,
                    wellPanel(
                        fluidRow(
                            column(width = 6,
                                column(width=6, radioButtons(inputId = "mode", label = "Mode:", choices = modes)),
                                conditionalPanel("input.mode == 'mc'",
                                    column(width=6, radioButtons(inputId = "graph", label = "Graph Type:", choices = graphTypes))
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
                                    selectInput(inputId = "variable_mv", label = "Choose variables:", choices = variablesGraph, multiple = TRUE, selected = variablesGraph[1])
                                )                   
                            )
                        )
                    )
                )
            ),
            mainPanel(width = 12,
                plotlyOutput(outputId = "plot", height = 400)
            ),
        ),
        tabPanel("Map",
            leafletOutput(outputId = "map", width = "100%", height = 600),
            absolutePanel(top = 150, left = 25,
                sliderInput(inputId = "mapSlider", "Date", startDate, endDate, endDate, timeFormat="%b %d %Y"),
                radioButtons(inputId = "variableMap", label = "Choose a variable", choices =  variablesMap),
            ),
            absolutePanel(top = 50, right = 20, width = 250,
                wellPanel(
                    h3(textOutput("selectedCountryMap")),
                    tableOutput(outputId = "countryTable")
                )
            ),
            absolutePanel(bottom = 380, right = 20, width = 250,
                wellPanel(
                    h3(textOutput("selectedVarMap")),
                    tableOutput(outputId = "mapTable")
                )
            )
        )
    )
)

# Dashboard server function
server <- function(input, output) {
    # Graph colors
    colors <- c("darkslateblue", "gold", "indianred3", "forestgreen", "gray45")
    
    # Global data table
    output$totalCases <- renderText({
        sum(data$DAILY_CONFIRMED, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    output$totalDeaths <- renderText({
        sum(data$DAILY_DEATHS, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    
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
                        drawPlot(p, colors)
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
    observeEvent({
        input$mapSlider
        input$variableMap
        }, { 
            # Set variables for map data
            varString <- input$variableMap
            if(varString != "ACTIVE")
                column <- paste0("CUMULATIVE_", input$variableMap)
            mapdf <- mapdf %>% filter(DATE == input$mapSlider) 
            spdf@data <- left_join(spdf@data, mapdf, by = "COUNTRY")
            mypalette <- setPalette(spdf@data, column)
            df <- spdf@data
            labs <- lapply(seq(nrow(df)), function(i) {
                num <- format(df[i, column], big.mark = ',', scientific=FALSE)
                paste0(df[i, "COUNTRY"], '<br>', num)
            })
            
            output$mapTable <-renderTable({ 
                showTable(mapdf, column, input$mapSlider)
            })
            
            output$selectedVarMap <- renderText({paste0("Global ", tolower(varString), " cases")})
            
            output$map <- renderLeaflet({
                leaflet(spdf) %>%
                addTiles(tilesURL) %>%
                setView(lng = 40.2085, lat = -3.713, zoom = 2)  %>% 
                addLegend(pal = mypalette, values = ~spdf@data[[column]],
                          opacity = 0.7, title = "Legend",position = "bottomleft") %>%
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
    
    # Initial table set text
    output$selectedCountryMap <- renderText({"Spain"})
    output$countryTable <- renderTable({
        showCountryTable(mapdf, "SPAIN", isolate(input$mapSlider))
    })
    
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
