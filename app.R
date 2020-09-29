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

#targeting the latest date
latestDate <- max(covid_data$date)

#freezing the data
freezed_data <- covid_data %>% filter(date==latestDate)

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
                        plotlyOutput("casesPlot")
                        ),
               tabPanel("plot 2",
                        titlePanel("plot 1"),
                        plotlyOutput("casesPlot2")
                        ),
               tabPanel("plot 3"),
               tabPanel("plot 4"),
               tabPanel("plot 5"),
               tabPanel("plot 6"),
               tabPanel("Summary")
    ),
  )
)

server <- function(input, output) {
  
  output$casesPlot <- renderPlotly({
      comb %>%
      ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)