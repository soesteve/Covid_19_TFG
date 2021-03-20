#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# List of packages used
packages <- c("shiny", "leaflet", "dplyr", "leaflet.minicharts", "ggplot2", "rgdal", "reshape2", "htmltools", "plotly")

# Check if each package is installed (if not, install them) and load them
for (lib in packages){
    if (!require(lib, character.only=TRUE)){
        install.packages(lib, character.only=TRUE, dependencies=TRUE)
        library(lib, character.only = TRUE)
    }
}


# Read the data from the dataset
#data <- read.csv("dataset.csv")
data <- read.csv("worldWithoutUSA.csv")
data[is.na(data)] = 0

# Rename the columns of the dataset
#data <- data %>% rename(COUNTRY = COUNTRY, DATE = FECHA, INFECTED = CONTAGIADOS, DEATHS = FALLECIDOS, RECOVERIES = RECUPERADOS,
#   HOSPITALIZATIONS = HOSPITALIZADOS,ICUs = UCIs) %>% mutate(DATE = as.Date(DATE, format="%d/%m/%Y"))
    #%>% mutate(COUNTRY = toupper(COUNTRY))
data <- data %>% 
    rename(COUNTRY = Country_Region, DATE = Last_Update1, 
           INFECTED = Daily_Confirmed, DEATHS = Daily_Deaths, RECOVERIES = Daily_Recovered) %>% 
    mutate(DATE = as.Date(DATE, format="%Y-%m-%d"), COUNTRY = toupper(COUNTRY))

# Complementary file for functions
source("variables.R")

# Prepare data for map
spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
spdf@data$NAME <- toupper(spdf@data$NAME)

# Prepare variables for select box
countries <- unique(data$COUNTRY)
#variablesGraph <- names(data)[-c(1, 2)]
#variablesMap <- c(names(data)[-c(1,2)])
variablesGraph <- names(data)[-c(1,2,3,4,5,6,7,8,9,10,14,15)]
variablesMap <- c(names(data)[-c(1,2,3,4,5,6,7,8,9,10,14,15)])


# Filter parameters and choices
modes = c("Multiple Countries" = "mc", "Multiple Variables" = "mv")
graphTypes = c("Line Graph" = "line", "Box Plot" = "box")

# Dashboard UI
ui <- fluidPage(
    tabsetPanel(
        tabPanel(title = "Graph",
            fluidRow(
                column(width = 3, 
                    wellPanel(
                        h2("Global number of cases"),
                        hr(),
                        h3(textOutput("totalCases")),
                        br(),
                        h2("Global number of deaths"),
                        hr(),
                        h3(textOutput("totalDeaths"))
                    )
                ),
                column(width = 3,
                    wellPanel(
                        h2(textOutput("selectedVarGraph")),
                        tableOutput(outputId = "graphTable")
                    )
                ),
                column(width = 6,
                    wellPanel(
                        h2("Filter"),
                        hr(),
                        fluidRow(
                            column(width = 6,
                                column(width=6, radioButtons(inputId = "mode", label = "Choose", choices = modes)),
                                column(width=6, radioButtons(inputId = "graph", label = "Graph Type", choices = graphTypes)),
                                sliderInput(inputId = "date", label = "Date", min = as.Date("2020-03-08","%Y-%m-%d"), max = Sys.Date(),
                                            value=c(as.Date("2020-03-08", "%Y-%m-%d"), Sys.Date()), timeFormat="%b %d %Y")
                            ),
                            column(width = 6,
                                conditionalPanel("input.mode == 'mc'",
                                    selectizeInput(inputId = "country_mc", label = "Choose a country", choices = countries, multiple = TRUE, selected = countries[1], options = list(maxItems = 5)),
                                    selectInput(inputId = "variable_mc", label = "Choose a variable", choices = variablesGraph)
                                ),
                                conditionalPanel("input.mode == 'mv'",
                                    selectInput(inputId = "country_mv", label = "Choose a country", choices = countries),
                                    selectInput(inputId = "variable_mv", label = "Choose a variable", choices = variablesGraph, multiple = TRUE, selected = variablesGraph[1])
                                )                   
                            )
                        )
                    )
                )
            ),
            mainPanel(width = 12,
                plotlyOutput(outputId = "plot", height = 600)
            ),
        ),
        tabPanel("Map",
            leafletOutput(outputId = "map", width = "100%", height = 950),
            absolutePanel(top = 150, left = 25,
                sliderInput(inputId = "mapSlider", "Date", min=as.Date("2020-03-09","%Y-%m-%d"), max=Sys.Date(), value=Sys.Date(), timeFormat="%b %d %Y"),
                radioButtons(inputId = "variableMap", label = "Choose a variable", choices =  variablesMap),
            ),
            absolutePanel(top = 50, right = 100, width = 250,
                wellPanel(
                    h3(textOutput("selectedVarMap")),
                    hr(),
                    tableOutput(outputId = "mapTable")
                )
            )
        )
    )
)

# Dashboard server function
server <- function(input, output) {

    # Graph Functions
    colors <- c("darkslateblue", "gold", "indianred3", "forestgreen", "gray45")
    
    # Global data table
    output$totalCases <- renderText({
        sum(data$INFECTED, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    output$totalDeaths <- renderText({
        sum(data$DEATHS, nar.rm=TRUE) %>% format(big.mark=",",scientific=FALSE)
    })
    
    observeEvent({
        input$graph
        input$mode
        }, {
            
            #Graph Plot
            output$plot <- renderPlotly({
                if(input$mode == "mc"){
                    dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
                    if(is.null(input$country_mc))
                        ggplot()
                    else if(input$graph == "line"){
                        p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y=input$variable_mc, color="COUNTRY")) + geom_line()
                        drawLinePlot(p, colors)
                    }
                    else if(input$graph == "box"){
                        p <- ggplot(dfReactive(), mapping = aes_string(x="COUNTRY", y=input$variable_mc, color="COUNTRY")) + geom_boxplot()
                        drawPlot(p, colors)
                    }
                }
                else if(input$mode == "mv"){
                    dfReactive <- reactive({getVariables(data, input$country_mv, input$variable_mv) %>% filter(between(DATE, input$date[1], input$date[2]))})
                        if(is.null(input$variable_mv))
                            ggplot()
                        else {
                            p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y="value", color="variable")) + geom_line()
                            drawLinePlot(p, colors)
                        }
                }
            })
            
            # Graph Table
            output$graphTable <-renderTable({ 
                
                if(input$mode == "mc"){
                    output$selectedVarGraph <- renderText({paste0("Countries by number of ", tolower(input$variable_mv[1]))})
                    showTable(data, input$variable_mc)
                }
                else {
                    output$selectedVarGraph <- renderText({paste0("Countries by number of ", tolower(input$variable_mv[1]))})
                    showTable(data, input$variable_mv[1])
                }
            }, digits = 0) 
    })

    # Map Functions 
    observeEvent({
        input$mapSlider
        input$variableMap
        }, {
            output$mapTable <-renderTable({ 
                showTable(data, input$variableMap)
            })
        
            output$selectedVarMap <- renderText({paste0("Global ", tolower(input$variableMap), " cases")})
            
            mapdf <- data %>% filter(between(DATE, as.Date("2020-03-08", "%Y-%m-%d"), input$mapSlider)) %>% 
                group_by(COUNTRY) %>%
                summarize(INFECTED=sum(INFECTED,na.rm=TRUE),
                          DEATHS=sum(DEATHS,na.rm=TRUE),
                          RECOVERIES=sum(RECOVERIES,na.rm=TRUE)) %>%
                          #HOSPITALIZATIONS=sum(HOSPITALIZATIONS,na.rm=TRUE),
                          #ICUs=sum(ICUs,na.rm=TRUE)) %>%
                rename(NAME = COUNTRY)
            spdf@data <- left_join(spdf@data, mapdf, by = "NAME")
            column <- input$variableMap
            maxNum <- (max(spdf@data[[column]], na.rm = TRUE))
            df <- spdf@data
            labs <- lapply(seq(nrow(df)), function(i) {
                num <- format(df[i, column], big.mark = ',', scientific=FALSE)
                paste0(df[i, "NAME"], '<br>', num)
            })
            mybins <- c(0, maxNum %/% 25, maxNum %/% 20, maxNum %/% 10, maxNum %/% 7 , maxNum %/% 5, maxNum %/% 2, (maxNum * 3) %/% 4, maxNum)
            mypalette <- colorBin( palette ="YlOrRd", domain=as.numeric(spdf@data[[column]]), na.color="transparent", bins=mybins)
            tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
            output$map <- renderLeaflet({
                leaflet(spdf) %>%
                    addTiles(tilesURL) %>%
                    setView(lng = 40.2085, lat = -3.713, zoom = 2.5) %>%
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
                        label=lapply(labs, HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal"),
                            textsize = "15px",
                            direction = "auto")) %>% 
                    addLegend(pal = mypalette, values = ~spdf@data[[column]], opacity = 0.7, title = "Legend",position = "bottomleft")
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
