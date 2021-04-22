library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(lattice)
library(ggplot2)
library(lubridate)
library(gridExtra)

#-----------------------------------------------------------------------------------------------

#-----------------#
# importing dataset
#-----------------#

#importing covid data
covid_data <- read_excel("Datasets/covid_data.xlsx")

#importing shrunk gdp data
Q2_gdp <- read_csv("Datasets/economic-decline-in-the-second-quarter-of-2020.csv")

#importing total test data set
test <- read_csv("Datasets/worldometer_data.csv")

#importing malariya
malariya <- read_csv("Datasets/malariya.csv")

#importing sars
sars <- read_excel("Datasets/sars.xlsx")

#importing H1V1
H1N1 <- read_csv("Datasets/Pandemic_H1N1_2009.csv")

#--------------------------------------------------------------------------------------

#---------------------#
#preparing the dataset
#---------------------#

#targeting the latest date
latestDate <- max(covid_data$date)
latests_date_sars <- max(sars$Date)
latest_date_malariya <- max(malariya$Date)

#freezing the data
View(freezed_data) <- covid_data %>% filter(date==latestDate)
sars <- sars %>% filter(Date==latests_date_sars)
malariya <- malariya %>% filter(Date==latest_date_malariya)

#Merging the datasets
#shrunk gdp and location
freezed_data$shrunk_gdp <- (Q2_gdp$`GDP growth from previous year, 2020 Q2`[match(freezed_data$location, Q2_gdp$Entity)])

#tests and location
freezed_data$total_tests <- (test$TotalTests[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_tests_per_million <- (test$`Tests/1M pop`[match(freezed_data$location, test$`Country/Region`)])
freezed_data$active_cases <- (test$ActiveCases[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_recovered <- (test$TotalRecovered[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_serious <- (test$`Serious,Critical`[match(freezed_data$location, test$`Country/Region`)])

#SARS Preparing Data
sars$overall_cases <- sum(sars$`Cumulative number of case(s)`)
sars$overall_deaths <- sum(sars$`Number of deaths`)
sars$overall_recoverd <- sum(sars$`Number recovered`)


#H1N1 Preparing Data
H1N1<-as.data.frame(tail(H1N1, 1))

#Malariya Preparing Data
malariya$overall_cases <- sum(malariya$`Cumulative no. of cases`)
malariya$overall_deaths <- sum(malariya$`Cumulative no. of deaths`)

#covid preparing data
malariya$overall_case_covid <- sum(freezed_data$total_cases)
malariya$overall_deaths_covid <- sum(freezed_data$total_deaths)

#Creating data frame Deaths to plot Barplot
deaths <- data.frame(Disease = c(
                                 "COVID-19",
                                 "Malaria",
                                 "SARS",
                                 "H1N1"
                                 ),
                    Cases = c(
                              unique(malariya$overall_case_covid),
                              unique(malariya$overall_cases),
                              unique(sars$overall_cases),
                              H1N1$Cases
                              ),
                    Deaths = c(
                               unique(malariya$overall_deaths_covid),
                               unique(malariya$overall_deaths),
                               unique(sars$overall_deaths),
                               H1N1$Deaths
                              ),
                    Fatality_Rate = c(
                                      ((unique(malariya$overall_deaths_covid)*100)/unique(malariya$overall_case_covid)) %>% round(5),
                                      ((unique(malariya$overall_deaths)*100)/unique(malariya$overall_cases)) %>% round(5),
                                      ((unique(sars$overall_deaths)*100)/unique(sars$overall_cases)) %>% round(5),
                                      ((H1N1$Deaths*100)/H1N1$Cases) %>% round(5)
                                      )
                    )


#Preparig the columns for total_tests_per_thousand
for(i in 1:dim(freezed_data)[1]){
  freezed_data$total_closed[i] <- ((freezed_data$total_cases[i]) - (freezed_data$active_cases[i]))
}


#Filtering the data continent wise
Africa <-freezed_data %>% filter(continent == 'Africa')
Asia <-freezed_data %>% filter(continent == 'Asia')
Europe <-freezed_data %>% filter(continent == 'Europe')
North_America <-freezed_data %>% filter(continent == 'North America')
Oceania <-freezed_data %>% filter(continent == 'Oceania')
South_America <-freezed_data %>% filter(continent == 'South America')

#combine the data of desired countries
comb <- freezed_data %>% filter(location == 'Australia' |
                                location == 'United States' |
                                location == 'Mexico' |
                                location == 'India'|
                                location == 'South Africa' |
                                location == 'France' |
                                location == 'Italy' |
                                location == 'United Kingdom' |
                                location == 'Brazil' |
                                location == 'Iran' |
                                location == 'Egypt' |
                                location == 'South Korea' |
                                location == 'Taiwan' |
                                location == 'Peru' |
                                location == 'Canada' |
                                location == 'Colombia' |
                                location == 'Germany' |
                                location == 'New Zealand')

comb_2 <- covid_data %>% filter(location == 'Australia' |
                                  location == 'United States' |
                                  location == 'Mexico' |
                                  location == 'India'|
                                  location == 'South Africa' |
                                  location == 'France' |
                                  location == 'Italy' |
                                  location == 'United Kingdom' |
                                  location == 'Brazil' |
                                  location == 'Iran' |
                                  location == 'Egypt' |
                                  location == 'South Korea' |
                                  location == 'Taiwan' |
                                  location == 'Peru' |
                                  location == 'Canada' |
                                  location == 'Colombia' |
                                  location == 'Germany' |
                                  location == 'New Zealand')



#-----------------------------------------------------------------------------------------------

#-------#
#Bar Plot
#-------#

a1 <- ggplot(data=deaths, aes(x=Disease, y=log(Cases), fill=Disease))+
  geom_bar(stat="identity")

a2 <- ggplot(data=deaths, aes(x=Disease, y=log(Deaths), fill=Disease))+
  geom_bar(stat="identity")

a3 <- ggplot(data=deaths, aes(x=Disease, y=Fatality_Rate, fill=Disease))+
  geom_bar(stat="identity")

gridExtra::grid.arrange(a1,a2,a3)


#--------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------#
#Scatterplot of GDP VS HOSPITAL BEDS, WITH TOTAL DEATHS
#-----------------------------------------------------#


#Africa
Africa %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Asia
Asia %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Europe
Europe %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#North_America
North_America %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Oceania
Oceania %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#South_America
South_America %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#--------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------#
#Scatterplot of GDP VS TOTAL TESTS, WITH TOTAL CASE
#-----------------------------------------------------#


#Africa
Africa %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Asia
Asia %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Europe
Europe %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#North_America
North_America %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Oceania
Oceania %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#South_America
South_America %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_tests_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")




#--------------------------------------------------------------------------------------------

#-----------------------------------------------------#
#Scatterplot of GDP VS TOTAL CASES WITH TOTAL DEATHS
#-----------------------------------------------------#

#Africa
Africa %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Asia
Asia %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Europe
Europe %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#North_America
North_America %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Oceania
Oceania %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#South_America
South_America %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Comb
comb %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location, fill=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=total_tests_per_million, y=total_cases_per_million, size=total_deaths_per_million, color=location, fill=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_deaths_per_million, color=location, fill=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=shrunk_gdp, y=total_deaths_per_million, size=total_cases_per_million, color=location, fill=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
















#--------------------------------------------------------------------------------------------

#-----------------------------------------------------#
#Scatterplot of GDP VS TOTAL TESTS WITH TOTAL DEATHS
#-----------------------------------------------------#

#Africa
Africa %>%
  ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Asia
Asia %>%
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Europe
Europe %>%
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#North_America
North_America %>%
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#Oceania
Oceania %>%
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")

#South_America
South_America %>%
  ggplot(aes(x=gdp_per_capita, y=total_cases_per_million, size=total_deaths_per_million, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=gdp_per_capita, y=total_deaths, size=total_deaths, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")


#Comb
comb %>%
  ggplot(aes(x=total_recovered, y=active_cases, size=total_serious, color=location)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Deaths")



#correlation Matrix of df 
corr<-cor(comb[,1:13]) %>% round(3) 
ggcorrplot(corr,
           method="square",
           outline.color = "black",
           hc.order = TRUE,
           type="lower",
           lab=TRUE)

View(head(comb))