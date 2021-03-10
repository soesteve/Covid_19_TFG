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
data <- read.csv("dataset.csv")
data[is.na(data)] = 0

# Rename the columns of the dataset
data <- data %>% rename(COUNTRY = COUNTRY, DATE = FECHA, INFECTED = CONTAGIADOS, DEATHS = FALLECIDOS, RECOVERIES = RECUPERADOS, 
	HOSPITALIZATIONS = HOSPITALIZADOS,ICUs = UCIs) %>% mutate(DATE = as.Date(DATE, format="%d/%m/%Y"))

countries <- unique(data$COUNTRY)
source("variables.R")
variables <- names(data)[-c(1, 2)]
variablesMap <- c(names(data)[-c(1,2)])

# Prepare data for map
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
myspdf@data$NAME <- toupper(myspdf@data$NAME)

# Filter parameters and choices
choices = c("Multiple Countries" = "mc", "Multiple Variables" = "mv")
graphTypes = c("Line Graph" = "line", "Box Plot" = "box")

# Dashboard UI
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Graph",
            fluidRow(
                column(width = 3, 
                       wellPanel(
                           h2("Total cases"),
                           hr(),
                           h3(textOutput("totalCases")),
                           br(),
                           h2("Total deaths"),
                           hr(),
                           h3(textOutput("totalDeaths"))
                       )
                ),
                column(width = 3,
                    wellPanel(
                        h2(textOutput("selected_var")),
                        hr(),
                        tableOutput("table")
                    )
                ),
                column(width = 6,
                    wellPanel(
                        h2("Filter"),
                        hr(),
                        fluidRow(
                            column(width = 6,
                                   column(width=6,
                                           radioButtons(inputId = "choices", label = "Choose", choices = choices)
                                           ),
                                   column(width=6, 
                                          radioButtons(inputId = "graph", label = "Graph Type", choices = graphTypes)
                                          ),
                                   sliderInput("date", "Date", min=as.Date("2020-03-08","%Y-%m-%d"), max=Sys.Date(), value=c(as.Date("2020-03-08", "%Y-%m-%d"), Sys.Date()), timeFormat="%b %d %Y")
                                   #dateRangeInput(inputId = "date", label = "Date", start = "2020-03-08", end = Sys.Date(), format = "dd-mm-yyyy")
                            ),
                            column(width = 6,
                                   conditionalPanel("input.choices == 'mc'",
                                                    selectizeInput(inputId = "country_mc", label = "Choose a country", choices = countries, multiple = TRUE, selected = countries[1], options = list(maxItems = 5)),
                                                    selectInput(inputId = "variable_mc", label = "Choose a variable", choices = variables)),
                                   conditionalPanel("input.choices == 'mv'",
                                                    selectInput(inputId = "country_mv", label = "Choose a country", choices = countries),
                                                    selectInput(inputId = "variable_mv", label = "Choose a variable", choices = variables, multiple = TRUE, selected = variables[1])
                                                    )                   
                                    )
                            )
                        )
                    )
            ),
            mainPanel(width=12,
                      plotlyOutput("plot1", height = 600)
            ),
        ),
        tabPanel("Map",
            
            leafletOutput("mymap", width = "100%", height = 950),
            absolutePanel(top = 150, left = 25,
                    sliderInput("mapSlider", "Date", min=as.Date("2020-03-09","%Y-%m-%d"), max=Sys.Date(), value=Sys.Date(), timeFormat="%b %d %Y"),
                    radioButtons("variableMap", "Choose a variable", choices =  variablesMap),
            ),
            absolutePanel(top = 50, right = 100, width = 250,
                          wellPanel(
                             h3(textOutput("selected_varMap")),
                             hr(),
                             tableOutput("mapTable"))
            )
        )
    )
)

# Dashboard server function
server <- function(input, output) {
    colors <- c("darkslateblue", "gold", "indianred3", "forestgreen", "gray45")
    
    drawPlot <- function(plot, graphType){
        plot <- plot + graphType() +
            scale_color_manual(values = colors) +
            theme_minimal() +
            theme(
                text = element_text(size = 15),
                axis.line = element_blank(),
                axis.text.x = element_text(color = "black", angle=45),
                axis.text.y = element_text(color = "black"),
                plot.background = element_rect(fill = "gray96", color = NA),
                panel.border = element_rect(linetype = "dashed", fill = NA),
                panel.background = element_rect(fill = "gray96", color = NA),
                panel.grid = element_line(color = "black"),
                legend.background = element_rect(fill = "gray96", color = NA),
                plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
            scale_x_date(date_labels = "%b %d", date_minor_breaks = "1 day", date_breaks="1 week") 
        ggplotly(plot) %>% layout(legend = list(orientation = 'h', y= 105))
    }
    
    drawBoxPlot <- function(plot, graphType){
        plot <- plot + graphType() +
            scale_color_manual(values = colors) +
            theme_minimal() +
            theme(
                text = element_text(size = 15),
                axis.line = element_blank(),
                axis.text.x = element_text(color = "black", angle=45),
                axis.text.y = element_text(color = "black"),
                plot.background = element_rect(fill = "gray96", color = NA),
                panel.border = element_rect(linetype = "dashed", fill = NA),
                panel.background = element_rect(fill = "gray96", color = NA),
                panel.grid = element_line(color = "black"),
                legend.background = element_rect(fill = "gray96", color = NA),
                plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))
        ggplotly(plot) %>% layout(legend = list(orientation = 'h', y= 105))
    }
    
    
    showTable <- function(variable){
        data %>% select(COUNTRY, !!variable) %>% 
        group_by(COUNTRY) %>% summarize(!!variable := sum(!!as.name(variable), na.rm=TRUE)) %>%
        arrange(desc(!!as.name(variable))) %>% slice(0:5)
    }
    
    output$totalCases <- renderText({
        sum(data$INFECTED, nar.rm=TRUE)
    })
    output$totalDeaths <- renderText({
        sum(data$DEATHS, nar.rm=TRUE)
    })
    
    observeEvent(input$graph,{
        if(input$graph == "line"){
            dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
            output$plot1 <- renderPlotly({
                p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y=input$variable_mc, color="COUNTRY")) 
                drawPlot(p, geom_line)
            })
            output$table <-renderTable({ 
                showTable(input$variable_mc)
            })
        }
        else if(input$graph == "box"){
            dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
            output$plot1 <- renderPlotly({
                p <- ggplot(dfReactive(), mapping = aes_string(x="COUNTRY", y=input$variable_mc, color="COUNTRY")) 
                drawBoxPlot(p, geom_boxplot)
            })
            output$table <-renderTable({ 
                showTable(input$variable_mc)
            })
        }
    })
    
    observeEvent(input$choices, {
        
        if(input$choices == "mc"){
            
            output$selected_var <- renderText({paste0("Global ", tolower(input$variable_mc), " cases")})
            
            dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
            if(input$graph == "line"){
                dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
                output$plot1 <- renderPlotly({
                    p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y=input$variable_mc, color="COUNTRY"))
                    drawPlot(p, geom_line)
                })
                output$table <-renderTable({ 
                    showTable(input$variable_mc)
                })
            }
            else if(input$graph == "box"){
                dfReactive <- reactive({getCases(data, input$country_mc, input$variable_mc) %>% filter(between(DATE, input$date[1], input$date[2]))})
                output$plot1 <- renderPlotly({
                    p <- ggplot(dfReactive(), mapping = aes_string(x="COUNTRY", y=input$variable_mc, color="COUNTRY")) 
                    drawBoxPlot(p, geom_boxplot)
                })
                output$table <-renderTable({ 
                    showTable(input$variable_mc)
                })
            }
        }
        else if(input$choices == "mv"){
            
            output$selected_var <- renderText({paste0("Top 10 countries by number of ", tolower(input$variable_mv[1]))})
            
            dfReactive <- reactive({getVariables(data, input$country_mv, input$variable_mv) %>% filter(between(DATE, input$date[1], input$date[2]))})
            output$plot1 <- renderPlotly({
                if(is.null(input$variable_mv)){
                    ggplot()
                } else {
                    p <- ggplot(dfReactive(), mapping = aes_string(x="DATE", y="value", color="variable"))
                    drawPlot(p, geom_line)
                }
            })
            output$table <-renderTable({ 
                showTable(input$variable_mv[1])
            })
        }
    })
    
     
    observeEvent({
        input$mapSlider
        input$variableMap
        }, 
        {
        output$mapTable <-renderTable({ 
            showTable(input$variableMap)
        })
        output$selected_varMap <- renderText({paste0("Global ", tolower(input$variableMap), " cases")})
        
            
        mapdf <- data %>% filter(between(DATE, as.Date("2020-03-08", "%Y-%m-%d"), input$mapSlider)) %>% 
            group_by(COUNTRY) %>%
            summarize(INFECTED=sum(INFECTED,na.rm=TRUE),
                      DEATHS=sum(DEATHS,na.rm=TRUE),
                      RECOVERIES=sum(RECOVERIES,na.rm=TRUE),
                      HOSPITALIZATIONS=sum(HOSPITALIZATIONS,na.rm=TRUE),
                      ICUs=sum(ICUs,na.rm=TRUE)) %>%
            rename(NAME = COUNTRY)
        myspdf@data <- left_join(myspdf@data, mapdf, by = "NAME")
        column <- input$variableMap
        maxNum <- (max(myspdf@data[[column]], na.rm = TRUE))
        df <- myspdf@data
        labs <- lapply(seq(nrow(df)), function(i) {
            paste0(df[i, "NAME"], '<br>', df[i, column])
        })
        mybins <- c(0, maxNum %/% 25, maxNum %/% 20, maxNum %/% 10, maxNum %/% 7 , maxNum %/% 5, maxNum %/% 2, (maxNum * 3) %/% 4, maxNum)
        mypalette <- colorBin( palette ="PuBu", domain=as.numeric(myspdf@data[[column]]), na.color="transparent", bins=mybins)
        tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
        output$mymap <- renderLeaflet({
            leaflet(myspdf) %>%
                addTiles(tilesURL) %>%
                setView(lng = 40.2085, lat = -3.713, zoom = 2.5) %>%
                addPolygons(data = myspdf,
                            fillColor = ~mypalette(as.numeric(myspdf@data[[column]])),
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
                              direction = "auto")) %>% addLegend(pal = mypalette, values = ~myspdf@data[[column]], opacity = 0.7, title = "Legend",
                      position = "bottomleft")
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
