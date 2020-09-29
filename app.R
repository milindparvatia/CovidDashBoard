#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(markdown)
library(plotly)
library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(lattice)
library(ggplot2)
library(lubridate)

#importing dataset
covid_data <- read_csv("covid-data.csv")

#importing shrunk gdp data
Q2_gdp <- read_csv("economic-decline-in-the-second-quarter-of-2020.csv")

#targeting the latest date
latestDate <- max(covid_data$date)

#freezing the data
freezed_data <- covid_data %>% filter(date==latestDate)

#Merging the datasets
freezed_data$shrunk_gdp <- (Q2_gdp$`GDP growth from previous year, 2020 Q2`[match(freezed_data$location, Q2_gdp$Entity)])

#Filtering the data continent wise
Africa <-freezed_data %>% filter(continent == 'Africa')
Asia <-freezed_data %>% filter(continent == 'Asia')
Europe <-freezed_data %>% filter(continent == 'Europe')
North_America <-freezed_data %>% filter(continent == 'North America')
Oceania <-freezed_data %>% filter(continent == 'Oceania')
South_America <-freezed_data %>% filter(continent == 'South America')

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
                                  location == 'Egypt')
ui <- shinyUI(
  fluidPage(
    navbarPage("Covid-19 Analysis",
               tabPanel("Plot 1",
                        titlePanel('GDP vs. Hospital beds per Deaths'),
                        plotlyOutput("plot1")
                        ),
               tabPanel("plot 2",
                        titlePanel("GDP per captcha vs. Human Index"),
                        plotlyOutput("plot2")
                        ),
               tabPanel("plot 3",
                        titlePanel("Effect of stringent measures on covid cases"),
                        plotlyOutput("plot3")
                        ),
               tabPanel("plot 4"),
               tabPanel("plot 5",
                        titlePanel("Total Death vs. shrunk GDP"),
                        plotlyOutput("plot5")
                        ),
               tabPanel("plot 6"),
               tabPanel("Summary")
    ),
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    comb %>%
      ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")
  })
  
  output$plot2 <- renderPlotly({
    freezed_data %>%
      ggplot(aes(x=human_development_index, y=gdp_per_capita, size=total_deaths_per_million, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")
  })
  
  output$plot3 <- renderPlotly({
    ggplot(c,aes(x=stringency_index,y=total_cases,colour=location,group=location,fill=location))+
      geom_line(size = 0.7)+
      scale_y_continuous(trans = 'log10') +
      labs(
           x = "stringency index",
           y = "The number of total cases") +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
  })
  
  output$plot5 <- renderPlotly({
    freezed_data %>%
      ggplot(aes(x=shrunk_gdp, y=total_deaths_per_million, size=total_deaths_per_million, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")
  })
  

}
# Run the application 
shinyApp(ui = ui, server = server)