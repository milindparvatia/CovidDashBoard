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
library(readxl)

#importing dataset
covid_data <- read_csv("Datasets/covid-data.csv")

#importing shrunk gdp data
Q2_gdp <- read_csv("Datasets/economic-decline-in-the-second-quarter-of-2020.csv")

#importing malariya
malariya <- read_csv("Datasets/malariya.csv")

#importing sars
sars <- read_excel("Datasets/sars.xlsx")

#importing H1V1
H1N1 <- read_csv("Datasets/Pandemic_H1N1_2009.csv")

test <- read_csv("Datasets/worldometer_data.csv")

data2_compare_diseases <- read_csv("Datasets/compareDisease.csv")

#targeting the latest date
latestDate <- max(covid_data$date)

#freezing the data
freezed_data <- covid_data %>% filter(date==latestDate)

data <- read_excel("Datasets/D2019-20.xlsx")

trade <- read_csv("Datasets/Trade.csv")

degree<-data%>% filter(`Degree of volatility`=="High Volatility Expected" & Date >= "2020/01/01")

#importing dataseta for tradings
bombay <- read_csv("Datasets/Stockmarket/BSE.csv")
hongkong <- read_csv("Datasets/Stockmarket/HKI.csv")
#japan <- read_csv("Datasets/Stockmarket/daily_adjusted_Japan_index.csv")
#london <- read_csv("Datasets/Stockmarket/daily_adjusted_London_stock_exchange.csv")
nasdaq <- read_csv("Datasets/Stockmarket/NASDAQ.csv")
new_york <- read_csv("Datasets/Stockmarket/NYSE.csv")
#torronto <- read_csv("Datasets/Stockmarket/daily_adjusted_Torronto_stock_exchange.csv")

#Creating Data Frames for Stock Exchanhe
bombay <- data.frame(bombay$timestamp,bombay$adjusted_close)
hongkong <- data.frame(hongkong$timestamp,hongkong$adjusted_close)
nasdaq <- data.frame(nasdaq$timestamp,nasdaq$adjusted_close)
new_york <- data.frame(new_york$timestamp,new_york$adjusted_close)

#creating Data Frame for Combined Stock Exchange
stock_exchange <- data.frame(bombay$bombay.timestamp,
                             bombay$bombay.adjusted_close)

colnames(stock_exchange) <- c("timestamp","bombay")

stock_exchange$hongkong <- (hongkong$hongkong.adjusted_close[match(stock_exchange$timestamp, hongkong$hongkong.timestamp)])
stock_exchange$nasdaq <- (nasdaq$nasdaq.adjusted_close[match(stock_exchange$timestamp, nasdaq$nasdaq.timestamp)])
stock_exchange$new_york <- (new_york$new_york.adjusted_close[match(stock_exchange$timestamp, new_york$new_york.timestamp)])


stock_exchange<- head(stock_exchange, 170)

#Merging the datasets
#shrunk gdp and location
freezed_data$shrunk_gdp <- (Q2_gdp$`GDP growth from previous year, 2020 Q2`[match(freezed_data$location, Q2_gdp$Entity)])

#tests and location
freezed_data$total_tests <- (test$TotalTests[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_tests_per_million <- (test$`Tests/1M pop`[match(freezed_data$location, test$`Country/Region`)])
freezed_data$active_cases <- (test$ActiveCases[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_recovered <- (test$TotalRecovered[match(freezed_data$location, test$`Country/Region`)])
freezed_data$total_serious <- (test$`Serious,Critical`[match(freezed_data$location, test$`Country/Region`)])

#Preparig the columns for total_tests_per_thousand
for(i in 1:dim(freezed_data)[1]){
  freezed_data$total_closed[i] <- ((freezed_data$total_cases[i]) - (freezed_data$active_cases[i]))
}

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

comb1 <- covid_data %>% filter(location == 'Australia' |
                                  location == 'United States' |
                                  location == 'India'|
                                  location == 'South Africa' |
                                  location == 'Italy' |
                                  location == 'Brazil' |
                                  location == 'Iran' |
                                  location == 'South Korea' |
                                  location == 'Taiwan' |
                                  location == 'New Zealand')

#targeting the latest date
latestDate <- max(covid_data$date)
latests_date_sars <- max(sars$Date)
latest_date_malariya <- max(malariya$Date)

#freezing the data
freezed_data <- covid_data %>% filter(date==latestDate)
sars <- sars %>% filter(Date==latests_date_sars)
malariya <- malariya %>% filter(Date==latest_date_malariya)

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

degree<-data%>% filter(`Degree of volatility`=="High Volatility Expected" & Date >= "2020/01/01")
d<-data%>% filter(`Degree of volatility`=="High Volatility Expected")
refine<-degree %>% filter(Even=="CPI" | Even=="Crude Oil Inventories" | Even=="GDP" | Even=="Initial Jobless Claims" | 
                            Even=="	Retail Sales" | Even=="Manufacturing PMI")

ui <- shinyUI(
  fluidPage(
    navbarPage("Covid-19 Analysis",
               tabPanel("Home",
                        titlePanel('WIL Project: Analysis of Covid-19'),
                        tags$div(
                          tags$h4("What do we know about Covid-19?"),
                          tags$p("So far, we don't have any vaccine for the epidemic of COVID-19 and in the early stage of this new virus to 
                        reduce it's spreading countries took different approaches."),

                          tags$h5("1) As a Doctor, I want to know what all facilities are available in the most cases areas so that we can suggest government to make more arrangements if required."),
                          tags$h5("2) As a Government, I want to know how GDP and human index are affecting total covid deaths and figure out some other measures to improve the conditions."),
                          tags$h5("3) As an International trader of New Zealand, I want better visibility of how my business is affecting over the years so that I can deal in some other way if needed."),
                          tags$h5("4) As an Economist, I want to get insight on which events had highest effect on volatility in 2020?"),
                          tags$h5("5) As a Doctor, I wants to know how harmfull Covid-19 in comparison to other major viruses ever, so I can understand which steps I take to prevent spreding"),
                          tags$h5("6) As a Government, I wants to know how Deaths are affecting GDP of countries?"),
                          tags$h5("7) As an Economist, I wants to know how much affect covid-19 has done to stock market?"),
                        ),
               ),
               tabPanel("Plot 1",
                        titlePanel('GDP vs. Hospital beds per Deaths'),
                        plotlyOutput("plot1"),
                        tags$div(
                          tags$h4("Why Hospital beds ratio plays big roll in pendamic?"),
                          tags$p("In the plot1, we can find out a rough pattern that the higher the GDP the country has, the more beds it has for the citizens and hence less are the deaths due to more facility. And vice versa."),
                          tags$p("Case 1: India, with GDP of 6426, only 0.53 beds are available for per thousand people and with 77427 deaths. Case 2: South Korea, with GDP of 35938, up to 12.27 beds are available for per thousand people and with only 355 deaths. "),
                          tags$p("Hence as GDP increases, the country can afford more medical services and thus it is resulting in saving people lives."),
                          tags$p("However, there is an exception that although the USA has GDP 54225, only 2.77 beds are available for per thousand people, which causes 193016 deaths. "),
                          tags$p("Therefore, I suggest to the government to put more financial resources on the medical services to help deal with the COVID-19."),
                        ),
                        ),
               tabPanel("plot 2",
                        titlePanel("GDP per capita vs. Human Index"),
                        plotlyOutput("plot2"),
                        tags$div(
                          #tags$h4("Why Hospital beds ratio plays big roll in pendamic?"),
                          tags$p("As we can see from the graph, there is a positive correlation between the GDP and HDI, i.e. the higher the GDP is, the higher the human development index is, which means a better standard of living. "),
                          tags$p("Case 1: Niger is with only 926 GDP and 0.354 HDI, so we can say Niger is a poor country and the number of COVID-19 deaths is only 2.85 per million. "),
                          tags$p("Case 2: USA is with extremely high 54225 GDP and 0.924 human development index. However, the total deaths number is up to 583 per million people. "),
                          tags$p("As a government, I can conclude that the higher the HDI is, the higher the death number will be. It is because the higher HDI means that citizens have the capability of traveling around the world, which can cause the prevalence of the COVID-19 whether internationally or domestically. "),
                          tags$p("To government, I want to suggest those high-developed countries to increase their lockdown level."),
                        ),
                        ),
               tabPanel("plot 3",
                        titlePanel("Effect on Import/Export Bussiness"),
                        plotOutput("plot3"),
                        tags$div(
                          tags$p("We can see from the graphs that from 2015-2016 the import-export business earned almost the same value. And it had an increasing trend in the money value the following years until the year 2020,  the pandemic year,
where export business was slightly down, but on the other hand import business had a downfall as the country had put on high lockdown restrictions, travel ban, and restrictions on business which impacted import business as well."),
                        ),
                        ),
               tabPanel("plot 4",
                        tabsetPanel(type = "tabs",
                                    tabPanel("Events Affecting Volatlity",
                                             plotlyOutput("plot4_1"),
                                             tags$div(
                                               #tags$h4("As an economist, I want to get insight on which events had the highest effect on volatility in 2020?"),
                                               tags$p("We can see from the graph that CPI, CRUDE OIL INVENTORIES, GDP, INITIAL JOBLESS CLAIMS, and MANUFACTURING PMI are the most affected areas which influence the volatility rate the most.
So because of these events, variation in volatility is highest which is affecting the global economy as well as has an effect on currency value in the international market. These are the area most severely hit during the pandemic."),
                                             ),
                                             ),
                                    tabPanel("Volatlity Affects in Different Countries", 
                                             plotlyOutput("plot4_2"),
                                             tags$div(
                                               tags$h4("GRAPH 2: Based on events how different countries are being affected "),
                                               tags$p("From the graph it is clear that, USA has been hit the most with highest volatility rate in crude oil inventories and jobless claims."),
                                               tags$p("UK comes on second number where variation in manufacturing PMI, GDP and CPI is highest."),
                                               tags$p("Australia has least amount of affect in CPI and GDP when compared to other countries."),
                                               tags$p("I suggest to economists that they make relevant recommendations from these insights to improve the efficiency and economy of countries during the pandemic.")
                                             ),
                                             )
                        ),
                        ),
               tabPanel("plot 5",
                        titlePanel("Comparison Covid-19 with other Major Viruses"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Comparison of Cases", 
                                             plotlyOutput("plot5"),
                                             tags$div(
                                               tags$h4("What can we say from this plot?"),
                                               tags$p("This plot shows the type of disease in logarithmic number of cases as ordinate. COVID-19 climbs the highest spot (at 17 cases) in terms of number of cases as compared to that of other deadly infections like H1N1, Malaria and SARS. Malaria comes second (at 12 cases) and H1N1 being third (at 9 cases). SARS has the least number (at 8 cases) of infectious cases falling just a little shy than H1N1."),
                                             ),
                                             ),
                                    tabPanel("Comparison of Deaths", 
                                             plotlyOutput("plot5_1"),
                                             tags$div(
                                               tags$h4("What can we say from this plot?"),
                                               tags$p("This plot shows the type of disease in logarithmic number of deaths as ordinate. COVID-19 here as well (at 14 deaths) tops the chart by a huge margin. SARS comes second (at 7 deaths) while Malaria grabs third position by a small margin (at 6 deaths). H1N1 is the least deadly diseases amongst all 4 (at 4 deaths)."),
                                             ),
                                             ),
                                    tabPanel("Comparison of Fatality Rates",
                                             plotlyOutput("plot5_2"),
                                             tags$div(
                                               tags$h4("What can we say from this plot?"),
                                               tags$p("This plot shows the type of disease in number of fatality rate as ordinate. Here, SARS gains top position at 9 fatality rates per 100 cases. COVID-19 comes second at 3 fatality rates per 100 cases. H1N1 and Malaria, respectively, have the least fatality rates of at 1 and 0.5 fatality rates per 100 cases respectively."),
                                             ),
                                             )
                        ),
                        ),
               tabPanel("plot 6",
                        titlePanel("GDP vs. Covid-19"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot1", plotlyOutput("plot8")),
                                    tabPanel("Plot2", plotlyOutput("plot8_1")),
                                    tabPanel("Plot3", plotlyOutput("plot8_2")),
                                    tabPanel("Plot4", plotlyOutput("plot8_3"))
                        ),
                        tags$div(
                          tags$p("Plot 1 shows effects of GDP on total number of tests per million with increasing GDP the total number of testes per million shows sharp increase. Size of circles shows total number of confirmed cases per million.
                        Considering Australia, their GDP is 44648.71 with testing per million is 181419. While India’s GDP per capita is 6426.674 with testing capabilities is 16035. Even though Australia has done testing, total number of confirmed cases are still low.  There is cluster between (10000 – 20000 GDP) and (0 – 100000) tests per million. (Peru, South Africa, Colombia and Brazil)."),
                          tags$p("This shows total number of tests per million and total cases per million. With increasing number of tests total number of cases have increased as well. 
                        This trend is also similar conserving total number of deaths. Some countries (New Zealand, Australia, Canada and Germany) show different trend because these countries have witnessed low number of cases and deaths due to COVID-19. Ergo, they eased the restriction of lockdown. But, for the safe side, they continued the practicing of tests, but noticed low number of confirmed COVID-19 cases."),
                          tags$p("Plot 3 shows total number of cased per million and total number of deaths per million. Total number of deaths per million shows a steep increase with increasing total number of cases per million.
                        This is obvious, as if confirmed cases are huge, then confirmed deaths are also huge."),
                          tags$p("Shrinking GDP and Total number of deaths per million are inversely proposal to each other, as shown in plot 4. As death per million have been increased, GDP of countries declined.
                        Comparing Peru with Deaths per million is 924, noticed highest drop in GDP of -30%. While Taiwan lost only 0.294, that’s why drop of GDP is only -0.58%."),
                          tags$p("Overall story is, 
                        Countries with higher GDP have afford the more testing, and because they conduct more testing, their total number of ‘Confirmed Cases’ are also big. As Confirmed Cases have been increases, all countries run towards health of their civilians and imposed the lockdown. As ‘Confirmed Cases’ were huge, ‘Confirmed deaths’ number is also whooping. Due to lockdown, whole nation or whole countries were freeze. After certain period, when some countries noticed slowdown of pandemic, they eased the restriction and their industries were back to track, like Taiwan. But, countries where death are continuously increasing, they stuck to the lockdown which cost then whopping amount of declination in GDP."),
                        ),
                      ),
               tabPanel("plot 7",
                        titlePanel("Stock Market"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("BSE", plotlyOutput("plot7")),
                                    tabPanel("HSI", plotlyOutput("plot7_1")),
                                    tabPanel("NASDAQ", plotlyOutput("plot7_2")),
                                    tabPanel("NYA", plotlyOutput("plot7_3"))
                        ),
                        tags$div(
                          tags$h4("Plot 1 shows performance of Bombay Stock Exchange during COVID-19 pandemic. The trading value dropped sharply in late March due to the imposed lockdown in the country. However, upon reducing lockdown, the trading increased sharply and shows a steady increase thereafter. Similar is the trend for NASDAQ in Plot 3."),
                          tags$h4("Plot 2 and 4 shows Hong Kong and New York Stock Exchange performance during COVID-19. During late March, the trading showed steep decrease in value from nearly $14000 to $2000 due to reduced business activities during lockdown. However, upon releasing the lockdown, both shows a near-steady increase in performance with some abrupt stops due to absence of trading."),
                        ),       
               )
    ),
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    comb %>%
      ggplot(aes(x=gdp_per_capita, y=hospital_beds_per_thousand, size=total_deaths, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("GDP. per Capita") + ylab("Beds in Hospital per Thousand People")
  })
  
  output$plot2 <- renderPlotly({
    freezed_data %>%
      ggplot(aes(x=human_development_index, y=gdp_per_capita, size=total_deaths_per_million, color=location)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("Human Development Index(HDI)") + ylab("GDP. per Capita")
  })
  
  output$plot3 <- renderPlot({
    trade %>%
    ggplot(aes(x=Year, y=Cumulative)) + geom_boxplot(aes(fill=Direction))+
      facet_wrap( ~ Year, scales="free") +
      xlab("Year") + ylab("$ Value") + ggtitle("Impact on export and import")
  })
  
  output$plot7 <- renderPlotly({
    stock_exchange %>%
      ggplot() + 
      geom_line(aes(x=timestamp, y=bombay), color = "red") +
      xlab("Year") + ylab("$ Value") 
  })
  
  output$plot7_1 <- renderPlotly({
    stock_exchange %>%
      ggplot() + 
      geom_line(aes(x=timestamp, y=hongkong), color = "blue") +
      xlab("Year") + ylab("$ Value")
  })
  output$plot7_2 <- renderPlotly({
    stock_exchange %>%
      ggplot() + 
      geom_line(aes(x=timestamp, y=nasdaq), color = "green") +
      xlab("Year") + ylab("$ Value")
  })
  output$plot7_3 <- renderPlotly({
    stock_exchange %>%
      ggplot() + 
      geom_line(aes(x=timestamp, y=new_york), color = "purple") +
      xlab("Year") + ylab("$ Value")
  })
  
  output$plot4_1 <- renderPlotly({
    refine %>%
    ggplot(aes(x = Even, 
               fill = `Degree of volatility`)) + 
      geom_bar(position = "dodge")+
      labs(title = "Events Affecting Volatlity",
           x = "Events",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
  })
  
  output$plot4_2 <- renderPlotly({
    refine %>%
      ggplot(aes(x = Even, 
                 fill = Country)) + 
      geom_bar(position = "dodge")+
      labs(title = "Events Affecting Volatlity in Different Countries",
           x = "Events",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 90,vjust = 0.5))
    
  })
  
  output$plot5 <- renderPlotly({
    ggplot(data=deaths, aes(x=Disease, y=log(Cases), fill=Disease))+
      geom_bar(stat="identity")+
      xlab("Disease") + ylab("Number of Cases in Logirithmic Value")
  })
  
  output$plot5_1 <- renderPlotly({
    ggplot(data=deaths, aes(x=Disease, y=log(Deaths), fill=Disease))+
      geom_bar(stat="identity")+
      xlab("Disease") + ylab("Number of Deaths in Logirithmic Value")
  })
  
  output$plot5_2 <- renderPlotly({
    ggplot(data=deaths, aes(x=Disease, y=Fatality_Rate, fill=Disease))+
      geom_bar(stat="identity")+
      xlab("Disease") + ylab("Number of Fatality Rates")
  })
  
  output$plot8 <- renderPlotly({
    #Comb
    comb %>%
      ggplot(aes(x=gdp_per_capita, y=total_tests_per_million, size=total_cases_per_million, color=location, fill=continent)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("GDP. per Capita") + ylab("Total Tests per Million")
    })
  
  output$plot8_1 <- renderPlotly({
    comb %>%
      ggplot(aes(x=total_tests_per_million, y=total_cases_per_million, size=total_cases_per_million, color=location, fill=continent)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("Total Tests per Million") + ylab("Total Cases per Million")
  })
  
  output$plot8_2 <- renderPlotly({
    #Comb
    comb %>%
      ggplot(aes(x=total_cases_per_million, y=total_deaths_per_million, size=total_cases_per_million, color=location, fill=continent)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("Total Cases per Million") + ylab("Total Deaths per Million")
  })
  
  output$plot8_3 <- renderPlotly({
    comb %>%
      ggplot(aes(x=shrunk_gdp, y=total_deaths_per_million, size=total_deaths_per_million, color=location, fill=continent)) +
      geom_point(alpha=0.5) +
      scale_size(range = c(.1, 24), name="Deaths")+
      xlab("GDP. Shrunk in %") + ylab("Total Deaths per Million")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)