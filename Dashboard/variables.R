#Functions to get data about a given country:

getCases <- function(data, country, variable){
  aux <- data %>% filter(COUNTRY %in% country)#data[data$COUNTRY==country,]
  #if (variable == "CONTAGIADOS_CUM.") {
  #  result <- aux %>% mutate(CONTAGIADOS_CUM. = cumsum(CONTAGIADOS))
  #} else if (variable == "FALLECIDOS_CUM.") {
  #  result <- aux %>% mutate(FALLECIDOS_CUM. = cumsum(FALLECIDOS))
  #} else if (variable == "RECUPERADOS_CUM.") {
  #  result <- aux %>% mutate(RECUPERADOS_CUM. = cumsum(RECUPERADOS))
  #} else if (variable == "HOSPITALIZADOS_CUM.") {
  #  result <- aux %>% mutate(HOSPITALIZADOS_CUM. = cumsum(HOSPITALIZADOS))
  #} else if (variable == "UCIs_CUM.") {
  #  result <- aux %>% mutate(UCIs_CUM. = cumsum(UCIs))
  #} else {
    result <- aux %>% select(COUNTRY, DATE, variable)
  #}
  
  #result <- data %>% filter(COUNTRY == country) %>% mutate(CONTAGIADOS_CUM.=cumsum((CONTAGIADOS)),
  #                         FALLECIDOS_CUM.=cumsum(FALLECIDOS),
  #                         RECUPERADOS_CUM.=cumsum(RECUPERADOS),
  #                         HOSPITALIZADOS_CUM.=cumsum(HOSPITALIZADOS),
  #                         UCIs_CUM.=cumsum(UCIs))
  result
}

#getDeaths <- function(country){
#  result <- data[data$COUNTRY==country,] %>% select(FECHA, FALLECIDOS)
#  result
#}

#getRecoveries <- function(country){
#  result <- data[data$COUNTRY==country,] %>% select(FECHA, RECUPERADOS)
#  result
#}

getVariables <- function(data, country, variable){
  melt(getCases(data, country, variable), id.vars=c("COUNTRY", "DATE"))
}


getAccumData <- function(data){
  accumulated <- data %>% group_by(COUNTRY) %>%
    summarize(CONTAGIADOS=sum(CONTAGIADOS,na.rm=TRUE),
              FALLECIDOS=sum(FALLECIDOS,na.rm=TRUE),
              RECUPERADOS=sum(RECUPERADOS,na.rm=TRUE))
              #HOSPITALIZADOS=sum(HOSPITALIZADOS,na.rm=TRUE),
              #UCIs=sum(UCIs,na.rm=TRUE))
  accumulated
}


getR0 <- function(country){
  R0_table <- data %>% filter(COUNTRY==country) %>%
    mutate(FECHA=as.Date(FECHA, "%d/%m/%Y")) %>% arrange(FECHA) %>%
    summarize(FECHA=FECHA, CONTAGIADOS=CONTAGIADOS)
  
  R0_values <- list()
  n <- length(R0_table$CONTAGIADOS)
  for(i in 1:n){
    R0_values[i] = R0_table$CONTAGIADOS[i] / R0_table$CONTAGIADOS[1]
  }
  
  R0_table$R0 <- R0_values
  R0_table
}

drawPlot <- function(plot, colors){
  plot <- plot +
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

drawLinePlot <- function(plot, colors){
  plot <-  plot +
    scale_x_date(date_labels = "%b %d", date_minor_breaks = "1 day", date_breaks="1 week") +
    scale_y_continuous(labels=scales::comma) 
  drawPlot(plot, colors) 
}

showTable <- function(data, variable){
  if(substr(variable, 1, 11) == "CUMULATIVE_"){
    variable <- sub("CUMULATIVE_", "", variable)
  }
  df <- data %>% select(COUNTRY, !!variable) %>% 
    group_by(COUNTRY) %>% summarize(!!variable := sum(!!as.name(variable), na.rm=TRUE)) %>%
    arrange(desc(!!as.name(variable))) %>% slice(0:5)
  df[[2]] <- format(df[[2]], big.mark = ',', scientific = FALSE)
  df
}

showCountryTable <- function(data, countryString, date){
  df <- data %>% filter(COUNTRY == toupper(countryString), between(DATE, as.Date("2020-03-08", "%Y-%m-%d"), date)) %>% 
    summarize(INFECTED=sum(INFECTED,na.rm=TRUE),
              DEATHS=sum(DEATHS,na.rm=TRUE),
              RECOVERED=sum(RECOVERED,na.rm=TRUE))
  variables <- colnames(df)[2:4]
  values <- c(df[[2]], df[[3]], df[[4]])
  df <- data.frame(variables, values)
  df <- unname(df)
  df[[2]] <- format(df[[2]], big.mark = ',', scientific = FALSE)
  df
}
