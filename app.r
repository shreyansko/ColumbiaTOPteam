library(shiny)
library(urbnmapr)
library(leaflet)
library(RColorBrewer)
library (ggplot2)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(maps)
library(viridis)

# prepross data in overview
overview <- read_excel("overview.xlsx")

## preprocess data in residents tab

residents <- read_excel("residents.xlsx")
colnames(residents)[which(names(residents) == "State of Residence")] <- "state_name" #needs to match the column name in the urbnmapr datasets
residents<-residents %>%  mutate(across(where(is.character), ~na_if(., "D"))) #change the D values to NA
residents <- tibble::rowid_to_column(residents, "ID")
#Convert data from wide to long format
residents_long <-gather(residents, year, newresidents, '2019_Admissions':'2010_Admission', factor_key=TRUE)
residents_long$year<- recode(residents_long$year, '2019_Admissions' = 2019, '2018_Admission' = 2018, '2017_Admission'=2017,
                             '2016_Admission'=2016, '2015_Admission'=2015, '2014_Admission'=2014, '2013_Admission'=2013,
                             '2012_Admission'=2012, '2011_Admission'=2011, '2010_Admission'=2010)
residents_long=as.data.frame(residents_long)
residents_long$year_new <- substr(residents_long$year, 1, 4)
residents_long$ID <- as.character(residents_long$ID)
residents_long$newresidents <- as.numeric(residents_long$newresidents)
residents_long<-na.omit(residents_long)
residents_new <- aggregate(residents_long$newresidents, 
                           by=list(state_name=residents_long$state_name, 
                                   year = residents_long$year_new,
                                   origin_country = residents_long$`Country of Birth`), 
                           FUN=sum)
residents_new <- setNames(residents_new, replace(names(residents_new), names(residents_new) == 'x', 'residents_sum'))
residents_long <- residents_new

paletteBins <- c(0, 20, 50, 100, 500, 1000, 2000, 3000, 5000, 7500, 10000)
colorPalette <- colorBin(palette = "Reds", domain = residents_long$residents_sum, na.color = "transparent", bins = paletteBins)

# preprocess data in decennial data

df <- read_excel("decennial_df_new.xlsx")

# data in refugee tab

refugee_country <- REFUGEE_ARRIVALS_BY_COUNTRY_OF_NATIONALITY_FISCAL_YEARS_2010_TO_2019 <- read_excel("REFUGEE ARRIVALS BY COUNTRY OF NATIONALITY FISCAL YEARS 2010 TO 2019.xlsx",skip = 2)

refugee_country[refugee_country == "X"] <- "0"

colnames(refugee_country)[1] <- "Country_of_nationality"
colnames(refugee_country)[2] <- "refugee_2010"
colnames(refugee_country)[3] <- "refugee_2011"
colnames(refugee_country)[4] <- "refugee_2012"
colnames(refugee_country)[5] <- "refugee_2013"
colnames(refugee_country)[6] <- "refugee_2014"
colnames(refugee_country)[7] <- "refugee_2015"
colnames(refugee_country)[8] <- "refugee_2016"
colnames(refugee_country)[9] <- "refugee_2017"
colnames(refugee_country)[10] <- "refugee_2018"
colnames(refugee_country)[11] <- "refugee_2019"

refugee_country$refugee_2010 <- as.numeric(refugee_country$refugee_2010)
refugee_country$refugee_2011 <- as.numeric(refugee_country$refugee_2011)
refugee_country$refugee_2012 <- as.numeric(refugee_country$refugee_2012)
refugee_country$refugee_2013 <- as.numeric(refugee_country$refugee_2013)
refugee_country$refugee_2014 <- as.numeric(refugee_country$refugee_2014)
refugee_country$refugee_2015 <- as.numeric(refugee_country$refugee_2015)
refugee_country$refugee_2016 <- as.numeric(refugee_country$refugee_2016)
refugee_country$refugee_2017 <- as.numeric(refugee_country$refugee_2017)
refugee_country$refugee_2018 <- as.numeric(refugee_country$refugee_2018)
refugee_country$refugee_2019 <- as.numeric(refugee_country$refugee_2019)

refugee_country$compared_to_2010 <- refugee_country$refugee_2011 - refugee_country$refugee_2010
refugee_country$compared_to_2011 <- refugee_country$refugee_2012 - refugee_country$refugee_2011
refugee_country$compared_to_2012 <- refugee_country$refugee_2013 - refugee_country$refugee_2012
refugee_country$compared_to_2013 <- refugee_country$refugee_2014 - refugee_country$refugee_2013
refugee_country$compared_to_2014 <- refugee_country$refugee_2015 - refugee_country$refugee_2014
refugee_country$compared_to_2015 <- refugee_country$refugee_2016 - refugee_country$refugee_2015
refugee_country$compared_to_2016 <- refugee_country$refugee_2017 - refugee_country$refugee_2016
refugee_country$compared_to_2017 <- refugee_country$refugee_2018 - refugee_country$refugee_2017
refugee_country$compared_to_2018 <- refugee_country$refugee_2019 - refugee_country$refugee_2018

world <- map_data("world")

refugee_country$region <- refugee_country$Country_of_nationality
refugee_vis <- left_join(world, refugee_country)

re2010 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2010), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2010", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2011 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2011), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2011", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2012 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2012), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2012", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2013 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2013), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2013", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2014 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2014), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2014", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2015 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2015), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2015", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2016 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2016), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2016", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2017 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2017), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2017", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2018 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2018), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2018", fill = "refugee_num") +
  theme(axis.title = element_blank())

re2019 <-ggplot(refugee_vis, aes(long, lat, group = group)) +
  geom_polygon(aes(fill =  refugee_2019), color = "white") +
  scale_fill_viridis(option = "C") +
  labs(title = "Refugee Arrivals By Country of Nationality 2019", fill = "refugee_num") +
  theme(axis.title = element_blank())

yeartry <- tibble::tribble(
  ~year,
  "2019", 
  "2018", 
  "2017",
  "2016",
  "2015",
  "2014",
  "2013",
  "2012",
  "2011",
  "2010"
)
## design the ui
ui <- dashboardPage(
  dashboardHeader(title = "African Communities Together", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Africa American Overview", tabName = "overview", icon = icon("list")),
      menuItem("Africa Americans Trend Over the Decade", tabName = "decennial", icon = icon("chart-bar")),
      menuItem("African Residents in the US", tabName = "residents", icon = icon("dashboard")),
      menuItem("African Refugees", tabName = "refugees", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #first tab content
      tabItem(tabName = "overview",
              h1("Overview for African American by Year"),
              box(plotOutput("age"), width = 18),
              box(plotOutput("birthplace"), width = 18),
              box(plotOutput("education"), width = 18),
              box(plotOutput("employment"), width = 18),
              box(plotOutput("income"), width = 18),
              box(plotOutput("school"), width = 18)
              )
      ,
      # third tab content
      tabItem(tabName = "residents",
              h1("African Immigrants admitted by Year"),
              box(sliderInput("yeard", "Decennial Data Years:",
                                   min = 2010, max = 2019,
                                   value = 2010, step = 1, sep = "",
                                   animate = animationOptions(interval = 300, loop = TRUE)),
                  height = 100),
              box(pickerInput(inputId = "origin_country",
                                   label = "Country of Birth",
                                   choices= unique(residents_long$origin_country),
                                   options = list(`actions-box` = TRUE),
                                   multiple = T),
                  height = 100),
              box(leafletOutput("map"),
                  width = 12)
                )
      ,
      
      # Second tab content
  tabItem(tabName = "decennial",
          h1("Percentage Change of Africa Americans Over the Decade"),
          box(plotOutput("decennial"), width = 18)
      ),
  # Third tab content
  tabItem(tabName = "refugees",
          titlePanel("Refugee Arrivals By Country of Nationality"),
          sidebarLayout(
            sidebarPanel(
              selectInput(inputId = "year",
                          label = "Choose a year:",
                          choices = (yeartry$year)),
              tableOutput("view")),
            mainPanel(
              imageOutput("refugeemap")
              
            )
          )
          )
  )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "residents" = residents,
           "pressure" = pressure,
           "cars" = cars)
  })
  sliderValues <- reactive({
    data.frame(
      Name = c("Decennial Data Years"),
      Value = as.character(c(input$yeard)),
      stringsAsFactors = FALSE)
  })
  sliderValues <- reactive({
    data.frame(
      Name = c("Choose a country of birth:"),
      Value = as.character(c(input$origin_country)),
      stringsAsFactors = FALSE)
  })

  output$age <- renderPlot({
    ggplot(overview,aes(x=age_year,y=age_group_count,colour=age_group,group=age_group,fill=age_group)) +
      geom_line()+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Immigrant Population by Age')+
      scale_colour_discrete(na.translate = F)
  })
  output$birthplace <- renderPlot({
    ggplot(overview,aes(x=birthplace_year,y=birthplace_count,colour=birthplace,group=birthplace,fill=birthplace)) +
      geom_line()+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Immigrant Population by Birthplace')+
      scale_colour_discrete(na.translate = F)
  })
  output$education <- renderPlot({
    ggplot(overview,aes(x=education_year,y=education_count,colour=education,group=education,fill=education)) +
      geom_line()+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Immigrant Population by Education', 
           subtitle = 'For populations 25 years and over')+
      scale_colour_discrete(na.translate = F)
  })
  output$employment <- renderPlot({
    ggplot(overview,aes(x=employment_year,y=employment_count,colour=employment,group=employment,fill=employment)) +
      geom_line()+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Immigrant Population by Employment Status', 
           subtitle = 'For populations 16 years and over')+
      scale_colour_discrete(na.translate = F)
  })
  output$income <- renderPlot({
    ggplot(overview,aes(x=income_year,y=income_count,colour=income,group=income,fill=income)) +
      geom_line()+
      scale_y_continuous(name="Income", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Income for Immigrants in the Past 12 Months (In 2010 inflation-adjusted dollars)')+
      scale_colour_discrete(na.translate = F)
  })
  output$school <- renderPlot({
    ggplot(overview,aes(x=school_year,y=school_count,colour=school_enrollment,group=school_enrollment,fill=school_enrollment)) +
      geom_line()+
      scale_y_continuous(name="population", labels = scales::comma)+
      scale_x_continuous(breaks=seq(2010, 2019, 1))+
      labs(title = 'Immigrant Population by School Enrollment', 
           subtitle = 'For populations 3 years and over')+
      scale_colour_discrete(na.translate = F)
  })
  output$map <- renderLeaflet({
    selectedData_1<-residents_long %>% filter(
      residents_long$year==input$yeard, 
      residents_long$origin_country %in% c(input$origin_country)
    )
    validate(
      need(input$origin_country != "", "Please select at least one country of birth")
    )
    selectedData <- aggregate(selectedData_1$residents_sum, 
                                       by=list(state_name=selectedData_1$state_name, 
                                               year = selectedData_1$year),
                                       FUN=sum)
    
    selectedData_2 <- setNames(selectedData, replace(names(selectedData), names(selectedData) == 'x', 'residents_sum'))
    selectedData <- selectedData_2
    
    
    #Fun part: Match our residents data onto geolocations, by matching residents and urbmmapr datasets
    statesdata<-as.data.frame(get_urbn_labels()) #I'll be using this as a dataset from the urbnmapr package, but it has others too (e.g. 'counties')
    statesdata$Residents <-selectedData$residents_sum[match(statesdata$state_name, selectedData$state_name)]
    leaflet(statesdata) %>%
      addTiles()  %>%
      setView(lat = 40, lng = -97, zoom=3.5) %>%
      addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       radius = ~log(Residents) * 2.5,
                       weight = 1,
                       opacity = 1,
                       color = ~ifelse(Residents > 0, "black", "transparent"),
                       fillColor = ~ifelse(Residents > 0, colorPalette(Residents), "transparent"),
                       fillOpacity = 0.8)%>%
      addLegend(pal = colorPalette, values = selectedData$residents_sum, opacity=0.9, title = "Population", position = "bottomleft")
  })
  
  output$decennial <- renderPlot({
    ggplot(data = df, mapping = aes(x = State_Code, y = Proportion, fill = factor(Year))) + geom_bar(stat='identity', position="dodge")
  })
  
  output$refugeemap <- renderPlot({
    switch(input$year,
           "2019" = re2019,
           "2018" = re2018,
           "2017" = re2017,
           "2016" = re2016,
           "2015" = re2015,
           "2014" = re2014,
           "2013" = re2013,
           "2012" = re2012,
           "2011" = re2011,
           "2010" = re2010)
  })
  
  # Return the requested year's data ----
  datasetInput3 <- reactive({
    switch(input$year,
           "2019" = refugee_country[, c(1, 11, 20)],
           "2018" = refugee_country[, c(1, 10, 19)],
           "2017" = refugee_country[, c(1, 9, 18)],
           "2016" = refugee_country[, c(1, 8, 17)],
           "2015" = refugee_country[, c(1, 7, 16)],
           "2014" = refugee_country[, c(1, 6, 15)],
           "2013" = refugee_country[, c(1, 5, 14)],
           "2012" = refugee_country[, c(1, 4, 13)],
           "2011" = refugee_country[, c(1, 3, 12)],
           "2010" = refugee_country[, c(1, 2)])
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput3()
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)