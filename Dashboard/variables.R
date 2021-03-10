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
              RECUPERADOS=sum(RECUPERADOS,na.rm=TRUE),
              HOSPITALIZADOS=sum(HOSPITALIZADOS,na.rm=TRUE),
              UCIs=sum(UCIs,na.rm=TRUE))
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