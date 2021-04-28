library(dplyr)

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

getRawINC <- function(INC_data) {
  n <- length(INC_data)
  INC <- rep(0,n)
  if(n >= 15){
    INC_data[is.na(INC_data)] = 0
    for(i in 15:n){
      INC[i] = sum(INC_data[c((i-14):i)])
    }
  }
  INC
}


data <- read.csv("worldWithoutUSA.csv")
data <- bind_rows(read.csv("DataUSaV0_2.csv"), data)
populationDF <- read.csv("WPP2019_TotalPopulationBySex.csv")
populationDF <- populationDF %>% filter(Time == 2020, Variant == "High") %>% rename(COUNTRY = Location, POP = PopTotal)
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
data <- left_join(data, populationDF, by = "COUNTRY")
data <- data %>% group_by(COUNTRY) %>% mutate(R0 = getR0(DAILY_CONFIRMED) %>% round(digits = 2))
data <- data %>% group_by(COUNTRY) %>% mutate(INC = (getRawINC(DAILY_CONFIRMED) * 100000/POP) %>% round(digits = 2)) %>% select(-POP)

write.csv(data, "datasetCODA.csv")