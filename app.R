#required packages
library(shiny)
library(tidyverse)
library(readr)
library(ggridges)
library(gapminder)
library(stringr)
library(lubridate)
library(sf)
library(broom)

#load in all datasets used for project
drought = read_csv("https://github.com/jonathanhansen808/shipwrecks/blob/main/drought1.csv?raw=true")
city = read_csv("https://github.com/jonathanhansen808/shipwrecks/blob/main/cities.csv?raw=true")
natural_disasters <- read_csv("https://github.com/jonathanhansen808/shipwrecks/raw/main/data.csv")
ca <- read_sf("https://raw.githubusercontent.com/jonathanhansen808/shipwrecks/main/states.geojson")
counties = read_sf("https://raw.githubusercontent.com/jonathanhansen808/shipwrecks/main/counties.geojson")

#data preprocessing for the city dataset
city <- pivot_wider(names_from = Type, values_from = Value, data = city)

#city dataset used for aggregate functions
city_aggregate <- city %>%
  drop_na()%>%
  select(Area, Year, Precipitation, Temperature) %>%
  group_by(Area, Year) %>%
  summarize(Precipitation = mean(Precipitation), Temperature = mean(Temperature))

#grab all the years and areas in the data 
Year <- unique(city_aggregate$Year)
Area <- unique(city_aggregate$Area)

#data preprocessing for the drought dataset
drought = drought %>%
  select(c(drought_severity, drought_percent, Year, Date)) %>%
  distinct() %>%
  group_by(Year, drought_severity) %>%
  summarize(n = sum(drought_percent)) %>% 
  mutate(percentage = n/sum(n))

#data preprocessing for the natural disasters dataset
nat_disasters_pivoted <- natural_disasters %>%
  pivot_longer(
    cols = ends_with("Count"),
    names_to = "Type",
    values_to = "Count"
  ) %>% 
  select(Year, Type, Count)

#Take out "Count" from the disaster type and make it a factor 
nat_disasters_pivoted$Type <- gsub("Count","", as.character(nat_disasters_pivoted$Type))
nat_disasters_pivoted$Type = as.factor(nat_disasters_pivoted$Type)

natural_disasters <- natural_disasters %>% 
  select(-c("Drought Count", "Flooding Count", "Freeze Count","Severe Storm Count","Tropical Cyclone Count","Wildfire Count","Winter Storm Count"))

natural_diasters_data = merge(nat_disasters_pivoted, natural_disasters, by = "Year")

natural_disasters <- natural_disasters %>% 
  pivot_longer(
    cols = ends_with("Cost Range"),
    names_to = "Type",
    values_to  = "Cost Range"
  )
natural_disasters$Type <- gsub("Cost Range", "",as.character(natural_disasters$Type))

#final natural disasters datasest 
nat_disasters <- merge(nat_disasters_pivoted, natural_disasters, by= c("Year","Type"))

#pull out names of the drought severity levels 
drought_severities = unique(drought$drought_severity)


disasters <- nat_disasters %>% separate(`Cost Range`, c("low","high"))
disasters$low <- as.numeric(disasters$low)
disasters$high <- as.numeric(disasters$high)
disasters <- disasters %>% mutate(Avg  = (low + high)/2)

all_disasters <- disasters %>% filter(Type == "All Disasters ")
other_disasters <- disasters %>%filter(Type != "All Disasters ")

#precip/temp visual preprocessing 
ca <- ca %>% 
  filter(NAME== "California") 

#areas of interest and name correction
areas_of_interest  <- c("Modoc","Sacramento","Los Angeles","Humboldt", "Placer","San Diego","San Francisco","San Bernardino","Kings","Imperial")
city$Area[grepl("Alturas",city$Area)]<-"Modoc"
city$Area[grepl("Sacramento",city$Area)]<-"Sacramento"
city$Area[grepl("San Diego",city$Area)]<-"San Diego"
city$Area[grepl("San Francisco",city$Area)]<-"San Francisco"
city$Area[grepl("Los Angeles Downtown",city$Area)]<-"Los Angeles"
city$Area[grepl("Tahoe City",city$Area)]<-"Placer"
city$Area[grepl("Needles",city$Area)]<-"San Bernardino"
city$Area[grepl("Hanford",city$Area)]<-"Kings"
city$Area[grepl("El Centro",city$Area)]<-"Imperial"
city$Area[grepl("Eureka",city$Area)]<-"Humboldt"

counties <- counties %>% filter(NAME %in% areas_of_interest)
counties <- counties %>% dplyr::rename(Area = NAME)
counties_city_data <- merge(counties, city, all = T)

#data filtering/generation functions used in Shiny application 
generate_data = function(type, min, max) {
  drought %>%
    filter(between(Year, min, max)) %>% 
    filter(drought_severity %in% type)
}

generate_data_map <- function(year, month) {
  counties_city_data  %>% filter(Year == year) %>% dplyr::filter(Month == month)
}

#plotting functions used in Shiny application
bar_line = function() {
  ggplot(other_disasters, aes( x = Year)) + 
  geom_bar(aes(y = Count, fill = Type), stat = 'identity')  + 
  geom_line(data = subset(disasters, Type == "All Disasters "), aes(Year, Avg/3000), size = 2) + 
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis( ~ . * 3000, name = "Cost (Billions)")) +
  theme(
    axis.title.y = element_text(size=18),
    axis.title.y.right = element_text(size=18)
  )  + 
  ggtitle("California Natural Disasters by Count and Cost") + 
  theme(axis.title.x = element_blank()) +
    theme_bw()
}

scatter <- function(var1, var2, area_){
  ggplot(city_aggregate, aes_string(var1, var2)) +
    geom_point(data = city_aggregate %>% filter(Area != area_), col = "#d3d3d3") +
    geom_point(data = city_aggregate %>% filter(Area == area_), col = "orange")+
    geom_smooth(method = "lm", se = FALSE, col = "#d3d3d3") +
    geom_smooth(data = city_aggregate %>% filter(Area == area_), method="lm", se = FALSE, col = "orange") + 
    theme_bw()
}

bar <- function() {
  ggplot(nat_disasters, aes(Year, Count)) + 
    geom_bar(aes(Year, Count, fill = Type), stat = 'identity', alpha = 0.6) + 
    geom_line(data = . %>% filter(Type == "Wildfire")) + 
    geom_line(aes(Year, `Cost Range`)) + 
    theme_bw()
}

map_plot <- function(df, index, month, year) {
  if (month < 10) {
    month = paste0("0", month)
  } 
  if(index == "Precipitation") {
    ggplot(df) + 
      geom_sf(data = ca)+ 
      geom_sf(aes(fill = Precipitation)) + 
      scale_fill_gradient(low = "light green", high = "dark green", na.value = "transparent")  + 
      labs(
        title = paste0("Precipitation of Areas in ", month, "/", year)) +
      theme_bw()}
  else {
    ggplot(df) + 
      geom_sf(data = ca)+ 
      geom_sf(aes(fill = Temperature)) + 
      scale_fill_gradient(low = "yellow", high = "red",na.value = "transparent")  + 
      labs(
        title = paste0("Temperature of Areas in ", month, "/", year)) +
      theme_bw()}
}

#user interface for Shiny application
ui = navbarPage(title = "Weather Patterns in California", 
                tabPanel("Precipitation & Temperature",
                         tags$div(h4("Below are two scatterplots representing the daily average precipitation and temperature levels in different cities across California each year"),
                         h5("Instructions: In the dropdown below, feel free to select any city of interest, which will appear orange in the scatterplots below against the other cities, which appear in grey"), br()),
                         
                         
                         selectInput("Area", "Area", Area, multiple = FALSE),
                         fluidRow(
                           column(6, plotOutput("scatterplot1")),
                           column(6, plotOutput("scatterplot2"))
                         )),
                tabPanel("Precipitation & Temperature Maps",
                         tags$div(h4("On the left is an area map of the temperatures in nine California areas. On the right is an area map of precipitation in nine California areas."), 
                                  h5("Instructions: In the sliders below, feel free to select a year or month of interest to see the temperature and precipitation at that time."), br()),
                         sliderInput("year","Select a Year",2020,min = 1921, max = 2021),
                         sliderInput("month","Select a Month",1, min = 1, max = 12),
                         fluidRow(
                           column(6, plotOutput("Temp")),
                           column(6, plotOutput("Prec"))
                         )
                ),
                tabPanel("Drought Severity",  
                         tags$div(h4("Below is an area plot representing yearly drought severity levels across California"),
                                  h5("Instructions: In the dropdown below, feel free to select one or more drought severity levels of interest. D0 represents the least severe drought level, and D4 represents the most severe drought level. Also, there is a slider to adjust the date range of interest, which spans from 1895-2022." ), br()),
                         fluidRow(
                  column(checkboxGroupInput("type", "Drought Severity Type", drought_severities, drought_severities[1]), width = 2),
                  column(sliderInput("rng", "Year", value = c(1895, 2022), min = 1895, max = 2022), width = 4, offset = 1)),
                  plotOutput("areaplot")),
                tabPanel("Natural Disasters",
                         tags$div(h4("Below is a stacked bar plot representing natural disasters in California by their count each year (left-axis). Additionally, there is a line showing the cost (billions $USD) of the respective natural disasters (right-axis).")),
                         fluidRow(
                           plotOutput("barplot"))))

#server for Shiny application 
server <- function(input, output) {
  samples <- reactive({
    generate_data(input$type, input$rng[1], input$rng[2])})
  
  output$scatterplot1 <- renderPlot({
    scatter("Year", "Precipitation", input$Area)
  })
  
  output$scatterplot2 <- renderPlot({
    scatter("Year", "Temperature", input$Area)
  })
  
  output$areaplot <- renderPlot(
    ggplot(samples(), aes(x = Year, y = percentage, fill = drought_severity)) +
      geom_area() + 
      labs(fill = "Drought Severity Level", y = "Percentage") + 
      theme_bw())
  
  output$barplot <- renderPlot({
    bar_line()
  })
  
  map_samples<- reactive({
    generate_data_map(input$year,input$month)
  })
  
  output$Temp <- renderPlot(map_plot(map_samples(),"Temperature", input$month, input$year))
  output$Prec <- renderPlot(map_plot(map_samples(),"Precipitation", input$month, input$year))
}

#Shiny application 
app <- shinyApp(ui, server)
app

