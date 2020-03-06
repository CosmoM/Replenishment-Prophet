rm(list = ls())


library(tidyverse)
library(readxl)
library(forecast)
library(xts)
library(dygraphs)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(rhandsontable)
library(lubridate)
library(prophet)
library(plotly)
library(shinythemes)
library(purrr)
library(tidyr)


# reactive file reader

mydata1 <- read_excel("rpldatapo12.xlsx")
mydata2 <- read_excel('Sample Output.xlsx')

mydata1$Date = as.Date(mydata1$Date)
colnames(mydata1) <- gsub(" ", "_", colnames(mydata1))


header <- dashboardHeader(
  title = tags$h3("SUPERBALIST RPL AUTOMATION")
)


sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem("Time Series Plot", icon = icon("dashboard"), tabName = "TimeSeriesPlot"),
  hr(),
  hr(),
  
  selectizeGroupUI(
    id = "my-filters",
    params = list(
      dept = list(inputId = "Department", title = "DEPARTMENT:"),
      brand = list(inputId = "Brand", title = "BRAND:"),
      productId = list(inputId = "productId", title = "PRODUCT ID"),
      productId = list(inputId = "Product", title = "PRODUCT"),
      size = list(inputId = "Size", title = "Size")
    ),
    
    inline = FALSE
  ), 
  
  hr(),
  hr(),
  sliderInput(
    inputId = "lead_time",
    label = "LEAD TIME",
    min = 0,
    max = 20,
    value = 4,
    step = 1
  ),
  
  dateRangeInput(
    inputId = "forecast_horizon",
    label = "FORECAST PERIOD",
    start = Sys.Date()-1,
    end = Sys.Date()+365,
    startview = "month",
    weekstart = 1,
    language = "en"
  )
 )
)

body <- dashboardBody(
 
    tabItems(
      tabItem(tabName = "TimeSeriesPlot",
            h1("Initial Demo: Prophet Forecasting"),
            fluidRow(
            box(column(12,p(" "),dygraphOutput("plot1"))),
            box(column(12,p(" "),plotOutput("plot2"))),
            wellPanel(rHandsontableOutput("table1"))
            
            )
      )
  )
  
)



ui <- dashboardPage(header, sidebar, body)


# ===========================================================================

server <- function(input, output, session) {
  
  

  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = mydata1,
    vars = c("Department","Brand","productId","Product", "Size")
  )
  
    
   output$plot1<- renderDygraph({
    
   newdata <- res_mod()
   # newdata<-mydata1
   newdata$date1 = as.Date(newdata$Date)
   newdata$Year = lubridate::year(newdata$Date)
   newdata$Week = lubridate::floor_date(newdata$Date, "week")
   setnames(newdata, old=c("date1","actUnitSls"), new=c("ds", "y"))

   
    Series1 <-
      newdata %>%
      group_by(ds)%>%
      summarise(
      y = sum(y)
    )
   
     black_friday <- data_frame(
      holiday = 'Black Friday',
      ds = as.Date(c('2017-11-24', '2018-11-23', '2019-11-29',
                     '2020-11-27', '2021-11-26')),
      lower_window = 0,
      upper_window = 3
    )
    valentines  <- data_frame(
      holiday = 'Valentines Day',
      ds = as.Date(c('2017-02-14', '2018-02-14', '2019-02-14',
                     '2020-02-14', '2021-02-14')),
      lower_window = -7,
      upper_window = 1
    )
    fathers_day  <- data_frame(
      holiday = 'Fathers Day',
      ds = as.Date(c('2017-06-18', '2018-06-17', '2019-06-16',
                     '2020-06-21', '2021-11-26')),
      lower_window = -7,
      upper_window = 0
    )
    mothers_day  <- data_frame(
      holiday = 'Mothers Day',
      ds = as.Date(c('2017-05-14', '2018-05-13', '2019-05-12',
                     '2020-05-10', '2021-05-09')),
      lower_window = -7,
      upper_window = 0
    )
    new_year  <- data_frame(
      holiday = 'New Year',
      ds = as.Date(c('2017-01-01', '2018-01-01', '2019-01-01',
                     '2020-01-01', '2021-01-01')),
      lower_window = -1,
      upper_window = 0
    )
    human_rights_day  <- data_frame(
      holiday = 'Human Rights Day',
      ds = as.Date(c('2017-03-21', '2018-03-21', '2019-03-21',
                     '2020-03-21', '2021-03-21')),
      lower_window = 0,
      upper_window = 0
    )
    good_friday  <- data_frame(
      holiday = 'Good Friday',
      ds = as.Date(c('2017-04-14', '2018-03-30', '2019-04-19',
                     '2020-04-10', '2021-04-02')),
      lower_window = 0,
      upper_window = 3
    )
    freedom_day  <- data_frame(
      holiday = 'Freedom Day',
      ds = as.Date(c('2017-04-27', '2018-04-27', '2019-04-27',
                     '2020-04-27', '2021-04-27')),
      lower_window = 0,
      upper_window = 0
    )
    workers_day  <- data_frame(
      holiday = 'Int Workers Day',
      ds = as.Date(c('2017-05-01', '2018-05-01', '2019-05-01',
                     '2020-05-01', '2021-05-01')),
      lower_window = 0,
      upper_window = 0
    )
    youth_day  <- data_frame(
      holiday = 'Youth Day',
      ds = as.Date(c('2017-06-16', '2018-06-16', '2019-06-17',
                     '2020-06-16', '2021-06-16')),
      lower_window = 0,
      upper_window = 0
    )
    womens_day  <- data_frame(
      holiday = 'Womens Day',
      ds = as.Date(c('2017-08-09', '2018-08-09', '2019-08-19',
                     '2020-08-10', '2021-098-09')),
      lower_window = 0,
      upper_window = 0
    )
    heritage_day  <- data_frame(
      holiday = 'Heritage Day',
      ds = as.Date(c('2017-09-25', '2018-09-24', '2019-09-24',
                     '2020-09-24', '2021-09-24')),
      lower_window = 0,
      upper_window = 0
    )
    recon_day  <- data_frame(
      holiday = 'Day of Reconciliation',
      ds = as.Date(c('2017-12-16', '2018-12-17', '2019-12-16',
                     '2020-12-16', '2021-12-16')),
      lower_window = 0,
      upper_window = 0
    )
    xmas_day  <- data_frame(
      holiday = 'Xmas Day',
      ds = as.Date(c('2017-12-25', '2018-12-25', '2019-12-25',
                     '2020-12-25', '2021-12-25')),
      lower_window = 0,
      upper_window = 0
    )
    holidays <- bind_rows(xmas_day, recon_day, heritage_day, womens_day, youth_day, womens_day, freedom_day, youth_day, good_friday, human_rights_day, new_year, mothers_day, fathers_day, valentines,black_friday)
  # m <- add_country_holidays(m, country_name = 'ZA')
   m <- prophet(Series1,
                growth = "linear",
                yearly.seasonality = "auto",
                changepoint.prior.scale = 0.1,
                weekly.seasonality = "auto",
                daily.seasonality = "auto",
                seasonality.mode = "multiplicative",
                holidays = holidays,
                # holidays.prior.scale = 1,
                mcmc.samples = 0,
                interval.width = 0.001,
                uncertainty.samples = 1,
                fit = TRUE
                )
   
   output$table1 <- renderRHandsontable({rhandsontable(mydata2)})
   
   
   future <- make_future_dataframe(m, periods = 365)
   forecast <- predict(m, future)
   
   
   output$plot2 <- renderPlot({prophet_plot_components(m, forecast)})
   
    # plot_ly(m, forecast)
    
    dyplot.prophet(m, forecast)%>% 
      #dySeries("unitSls", label = "Unit Sales")%>%
      dyRoller(showRoller = TRUE, rollPeriod = 1) %>%
      dyAxis(name = "y", label = "Units Sold", valueRange = c(0, 2500)) %>%
      dyLegend(show = "always") %>%
      dyLegend(width = 500) %>%
      dyRangeSelector(height = 50)
  })

   # mydata5 <- mydata1 %>%
   #   mutate(group=paste(productId, Size))%>%
   #   nest(-group) %>% 
   #   mutate(m = map(data, prophet)) %>% 
   #   mutate(future = map(m, make_future_dataframe, period = 365)) %>% 
   #   mutate(forecast = map2(m, future, predict))%>%
   #   unnest(forecast) %>% 
   #   select(ds, group, yhat)
   
   
  
}

shiny::shinyApp(ui, server)
