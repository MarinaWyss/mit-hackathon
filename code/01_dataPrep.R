library(dplyr)
library(ggplot2)

# 01 load data -------------------------------------------------------
arkansas <- read.csv("internalData/Arkansas_20201021.csv")
newMexico <- read.csv("internalData/New_Mexico_20201021.csv")
pennsylvania <- read.csv("internalData/Pennsylvania_20201022.csv")
washington <- read.csv("internalData/Washington_20201021.csv")

hackathonData <- rbind(arkansas, newMexico, pennsylvania, washington)

demographics <- read.csv("externalData/censusData.csv")
politicalData <- read.csv("externalData/politicalData.csv")
prisonData <- read.csv("externalData/prisonData.csv")

externalData <- demographics %>% 
  left_join(politicalData) %>% 
  left_join(prisonData)

# 02 prep data for analysis ------------------------------------------
variables <- c("residents_confirmed", "staff_confirmed", "resident_deaths", "staff_deaths",        
               "residents_recovered", "staff_recovered", "residents_tested", "staff_tested",        
               "residents_pending", "staff_pending", "residents_negative", "staff_negative",      
               "residents_quarantine", "staff_quarantine" )

hackathonDataReported <- hackathonData %>% 
  group_by(state) %>% 
  summarise_at(vars(variables), funs(reported = ifelse(sum(!is.na(.)) >= 1, 1, 0)))

totalsNoPA <- hackathonData %>%
  filter(state %in% c("washington", "newMexico", "arkansas")) %>% 
  group_by(state, date) %>% 
  summarise_at(vars(variables), funs(sum(., na.rm = TRUE))) %>% 
  summarise_at(vars(variables), funs(max(.)))

totalsPA <- hackathonData %>% 
  filter(state == "pennsylvania" & date == "10/12/20") %>% 
  summarise_at(vars(variables), funs(sum(., na.rm = TRUE))) 
totalsPA <- cbind(state = "pennsylvania", totalsPA)

totals <- rbind(totalsNoPA, totalsPA)  

analysisData <- hackathonDataReported %>% 
  left_join(totals) %>% 
  left_join(externalData) %>% 
  mutate(state = factor(state, 
                        levels = c("arkansas", "newMexico", "pennsylvania", "washington")),
         residents_confirmed_percent = residents_confirmed/total_incarcerated_persons,
         resident_deaths_percent = resident_deaths/total_incarcerated_persons,
         residents_recovered_percent = residents_recovered/total_incarcerated_persons,
         tests_per_resident = residents_tested/total_incarcerated_persons,
         residents_quarantine_percent = residents_quarantine/total_incarcerated_persons)

# write.csv(analysisData, "externalData/analysisData.csv")
