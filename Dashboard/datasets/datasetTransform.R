library(dplyr)

# Calculate and get the R0 vector given the R0_data vector.
getR0 <- function(R0_data){
  
  n <- length(R0_data)
  R0 <- rep(0,n)
  if(n >= 15){
    for(i in 15:n){
      R0_data[is.na(R0_data)] = 0
      if(R0_data[i] != 0 && R0_data[i-14] != 0){
        R0[i] <- R0_data[i] / R0_data[i-14]
      } else {
        R0[i] <- R0[i-1]
      }
    }
  }
  R0
}

# Calculate and get the INC vector given the INC_data vector.
getRawINC <- function(INC_data) {
  n <- length(INC_data)
  INC <- rep(0,n)
  if(n >= 15){
    INC_data[is.na(INC_data)] = 0
    for(i in 15:n){
      INC[i] = sum(INC_data[c((i-14):i-1)])
    }
  }
  INC
}

# Calculate smoothed variable vector using 7-day moving 
# average given the data vector.
getSmoothVariables <- function(data){
  n <- length(data)
  dataAux <- c(data, c(rep(0,4)))
  smoothVar <- rep(0,n)
  if(n >= 4){
    dataAux[is.na(data)] = 0
    for(i in 4:n)
      smoothVar[i] = sum(dataAux[c((i-3):(i+3))])/7
  }  
  smoothVar %>% round(digits = 2)
}

# Read and combine datasets.
data <- read.csv("WorldWitoutUSA_NA_NoRep_Wed_May_12_14_13_19_2021_comma_SinRepPon.csv")
data <- bind_rows(read.csv("US_NaN_NoRep_Wed_May_12_08_26_16_2021_comma_SinRepPon.csv"), data)
populationDF <- read.csv("WPP2019_TotalPopulationBySex.csv")
populationDF <- populationDF %>% filter(Time == 2020, Variant == "High") %>% rename(COUNTRY = Location, POP = PopTotal) %>%
  mutate(COUNTRY = toupper(COUNTRY)) %>% select(COUNTRY, POP)

# Rename some columns of the dataframe 'data'. 
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

# Rename some rows of the 'COUNTRY' column of the dataframe 'data'.
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

# Combine rows by grouping regions of the same country.
data <- data %>% group_by(COUNTRY, DATE) %>%
  summarize(DAILY_CONFIRMED = sum(DAILY_CONFIRMED),
            DAILY_DEATHS = sum(DAILY_DEATHS),
            DAILY_RECOVERED = sum(DAILY_RECOVERED),
            ACTIVE = sum(ACTIVE),
            CUMULATIVE_CONFIRMED = sum(CUMULATIVE_CONFIRMED),
            CUMULATIVE_DEATHS = sum(CUMULATIVE_DEATHS),
            CUMULATIVE_RECOVERED = sum(CUMULATIVE_RECOVERED),
            DANGER_INDEX = sum(IP))

# Combine data frame with the population dataframe and fetch the additional variables.
data <- left_join(data, populationDF, by = "COUNTRY")
data <- data %>% group_by(COUNTRY) %>% mutate(R0 = getR0(DAILY_CONFIRMED) %>% round(digits = 2))
data <- data %>% group_by(COUNTRY) %>% mutate(INC = (getRawINC(DAILY_CONFIRMED) * 100/POP) %>% round(digits = 2)) %>% select(-POP)
data <- data %>% group_by(COUNTRY) %>% mutate(SMOOTH_CONFIRMED = getSmoothVariables(DAILY_CONFIRMED),
                                              SMOOTH_DEATHS = getSmoothVariables(DAILY_DEATHS),
                                              SMOOTH_RECOVERD = getSmoothVariables(DAILY_RECOVERED),
                                              SMOOTH_R0 = getSmoothVariables(R0),
                                              SMOOTH_DI = getSmoothVariables(DANGER_INDEX),
                                              SMOOTH_INC = getSmoothVariables(INC))

# Save dataframe to a CSV file.
write.csv(data, "datasetCODA.csv")