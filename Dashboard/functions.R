#Functions to get data about a given country:

renameColumns <- function(data){
  data <- data %>% 
    rename(COUNTRY = Country_Region, DATE = Last_Update1, 
           DAILY_CONFIRMED = Daily_Confirmed,
           DAILY_DEATHS = Daily_Deaths,
           DAILY_RECOVERED = Daily_Recovered,
           CUMULATIVE_CONFIRMED = Confirmed,
           CUMULATIVE_DEATHS = Deaths,
           CUMULATIVE_RECOVERED = Recovered,
           ACTIVE = Active) %>% 
    mutate(DATE = as.Date(DATE, format="%Y-%m-%d"), COUNTRY = toupper(COUNTRY))
  data <- data  %>% filter(!COUNTRY  %in% c("SOUTH KOREA", "TAIWAN", "CZECH REPUBLIC", "EAST TIMOR")) %>% 
                      mutate(COUNTRY = recode(COUNTRY, 
                                           "TIMOR-LESTE" = "EAST TIMOR",
                                           "CZECHIA" = "CZECH REPUBLIC",
                                           "US" = "UNITED STATES",
                                           "NORTH MACEDONIA" = "MACEDONIA",
                                           "CONGO (KINSHASA)" = "DEMOCRATIC REPUBLIC OF THE CONGO",
                                           "CONGO (BRAZZAVILLE)" = "CONGO",
                                           "KOREA, SOUTH" = "SOUTH KOREA",
                                           "TAIWAN*" = "TAIWAN"))
  data <- data %>% group_by(COUNTRY, DATE) %>%
    summarize(DAILY_CONFIRMED = sum(DAILY_CONFIRMED),
              DAILY_DEATHS = sum(DAILY_DEATHS),
              DAILY_RECOVERED = sum(DAILY_RECOVERED),
              ACTIVE = sum(ACTIVE),
              CUMULATIVE_CONFIRMED = sum(CUMULATIVE_CONFIRMED),
              CUMULATIVE_DEATHS = sum(CUMULATIVE_DEATHS),
              CUMULATIVE_RECOVERED = sum(CUMULATIVE_RECOVERED),
              DANGER_INDEX = sum(IP))
  data <- data %>% group_by(COUNTRY) %>% mutate(R0 = getR0(DAILY_CONFIRMED))
  data
}

recodeCountries <- function(data){
  data <- data %>% mutate(COUNTRY = recode(COUNTRY, 
                                           "SOUTH GEORGIA AND THE SOUTH SANDWICH IS (UK)" = "SOUTH GEORGIA AND THE SOUTH SANDWICH (UK)",
                                           "REUNION (FRANCE)" = "REUNION",
                                           "SWAZILAND" = "ESWATINI",
                                           "FAROE ISLANDS (DENMARK)" = "FAROE ISLANDS",
                                           "GREENLAND (DENMARK)" = "DENMARK",
                                           "SVALBARD (NORWAY)" = "NORWAY",
                                           "JAN MAYEN (NORWAY)" = "NORWAY",
                                           "PUERTO RICO (US)" = "PUERTO RICO",
                                           "ARUBA (NETHERLANDS)" = "ARUBA",
                                           "CURACAO (NETHERLANDS)" = "CURACAO",
                                           "GUADELOUPE (FRANCE)" = "GUADELOUPE",
                                           "MARTINIQUE (FRANCE)" = "MARTINIQUE",
                                           "ST. LUCIA" = "SAINT LUCIA",
                                           "ST. VINCENT AND THE GRENADINES" = "SAINT VINCENT AND THE GRENADINES",
                                           "MYANMAR" = "VIETNAM",
                                           "IVORY COAST" = "COTE D'IVOIRE",
                                           "FRENCH GUIANA (FRANCE)" = "FRENCH GUIANA"))
  data
}

getCases <- function(data, country, variable){
  aux <- data %>% filter(COUNTRY %in% country)
  result <- aux %>% select(COUNTRY, DATE, variable)
  result
}

getVariables <- function(data, country, variable){
  melt(getCases(data, country, variable), id.vars=c("COUNTRY", "DATE"))
}

getR0 <- function(R0_data){
  
  n <- length(R0_data)
  R0 <- rep(0,n)
  if(n >= 15){
    for(i in 15:n){
      if(R0_data[i] != 0 && R0_data[i-14] != 0){
        R0[i] <- R0_data[i] / R0_data[i-14]
      } else {
        R0[i] <- R0[i-1]
      }
    }
    
    for(i in 1:14) {
      R0[i] <- R0[15]
    }
  }
  R0
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

showTable <- function(data, variable, date){
  if (variable == "R0" || variable == "DANGER_INDEX"){
    variable <- "CUMULATIVE_CONFIRMED"
  } else if(substr(variable, 1, 5) == "DAILY"){
    variable <- sub("DAILY", "CUMULATIVE", variable)
  }
  
  df <- data %>% filter(DATE == date) %>% select(COUNTRY, !!variable) %>% 
    arrange(desc(!!as.name(variable)))
  if(variable != "ACTIVE"){
    str <- substring(variable, 12)
    df <- df %>% rename(!!str := variable)
  }
  
  df[[2]] <- format(df[[2]], big.mark = ',', scientific = FALSE)
  df <- df[1:4,]
  df
}

showCountryTable <- function(data, countryString, date){
  df <- data %>% filter(COUNTRY == toupper(countryString), DATE == date)
  variables <- c("CONFIRMED", "DEATHS", "RECOVERED", "ACTIVE")
  values <- c(NA, NA, NA, NA)
  if(dim(df)[1] != 0) {
    values <- c(df[[7]], df[[8]], df[[9]], df[[6]])
  }
  df <- data.frame(variables, values)
  df <- unname(df)
  df[[2]] <- format(df[[2]], big.mark = ',', scientific = FALSE)
  df
}

getCountryName <- function(data, country){
  id <- as.numeric(country["id"]) + 1
  countryString <- tolower(data[id])
  char <- substr(countryString, 0, 1)
  countryString <- sub(char, toupper(char), countryString)
  countryString
}

setPalette <- function(data, variable){
  maxNum <- (max(data[[variable]], na.rm = TRUE))
  mybins <- c(0, maxNum %/% 25, maxNum %/% 20, maxNum %/% 10, maxNum %/% 7 , maxNum %/% 5, maxNum %/% 2, (maxNum * 3) %/% 4, maxNum)
  mypalette <- colorBin( palette ="YlOrRd", domain=as.numeric(data[[variable]]), na.color="transparent", bins=mybins)
  mypalette
}