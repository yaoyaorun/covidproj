library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(mice)
library(shinyWidgets)

#Initial data gathering and analytics period
#read files in and change the string value to data
dataNY <- read_csv("new-york-history.csv")
# NYcovid <- dataNY %>% mutate(date = dmy(date))
NYcovid$date <- as.POSIXct(dataNY$date, format = "%d/%m/%Y")
#imputate the missing value (be aware may only be useful in numerical data)
NYcovidstatstemp <- subset(NYcovid, select = c("death","hospitalizedCurrently"))
tempData <- mice(NYcovidstatstemp,meth="cart")
NYcovidata <- complete(tempData) 
NYcovidata <- transform (NYcovidata, date = NYcovid$date, state = NYcovid$state)

#Define UI for app that draws a histogram
ui <- fluidPage(
  #Layout
  #greeting users   
  theme = bslib::bs_theme(bootswatch = "darkly"),
  setBackgroundImage(
    src = "https://thumbor.forbes.com/thumbor/711x464/https://specials-images.forbesimg.com/imageserve/5f8889127f2508f99c5a784a/Saturn--Jupiter--Mars-and-the-Moon-Rise-Together-in-New-York-City/960x0.jpg?fit=scale"
    ),
  titlePanel(
    h1(strong("COVID-19 cases in New York City"), align = "center")
  ),
  br(),
  tags$hr(style="border-color: grey;"),
  br(),
      fluidRow(
        column(3, 
               textInput("name", "Hey, what's your name?"),
               br()),
        #select date range
        column(3,
               uiOutput("dateRange"),
               br()),
        # column(4, dateRangeInput("dateRange", "Which period of time do you want to look at?", min = "2020-03-02", max = "2021-03-07", format = "yyyy-mm-dd")),
        #select key factor
        column(3,
              selectInput("df", "What data do you want to look at?", choices = c("death", "hospitalizedCurrently"), multiple = TRUE)),
        column(3,
               br(),
              actionButton("see", "click me", width = "300px")
        ),
      #dynamically greeting users based on the user name
        column(12, 
               textOutput("greeting"),
               br()
               ),
               #show the result of the plot.
        column(4, 
               dataTableOutput("table")),
        column(8,
               plotOutput("plot", width="1200px", height = "600px"))
        #show the result of the table based on select attributes
      )
)
server <- function (input, output, session) {
    df1 <- subset(NYcovidata, select=c(date, state, death))
    df2 <- subset(NYcovidata, select=c(date, state, hospitalizedCurrently))
    df3 <- subset(NYcovidata, select=c(date, state, death, hospitalizedCurrently))
  NYcovidata1 <- ({
    NYcovidata %>%
    select (date, death) %>%
    gather (key = "variable", value = "covidresult", -date)
    })
  NYcovidata2 <- ({
    NYcovidata %>%
    select (date, hospitalizedCurrently) %>%
    gather (key = "variable", value = "covidresult", -date)
    })
  NYcovidata3 <- ({
    NYcovidata %>%
    select (date, death, hospitalizedCurrently) %>%
    gather (key = "variable", value = "covidresult", -date)
    })
  
  #Create reactions for greeting
  br()
  output$greeting <- renderText ({
    paste ("Hello!", input$name, ", below is the data you want to look for--", sep=" ")
  })%>%
    shiny::bindEvent(input$see)
  br()
  output$dateRange <- renderUI({
    dateRangeInput("date", "Select the date range:",
                   start = 
                     as.character(format(as.Date(min(NYcovidata$date))),"yyyy-mm-dd"), # Start 
                   end = 
                     as.character(format(as.Date(max(NYcovidata$date))),"yyyy-mm-dd"), # End 
                   min = 
                     as.character(format(as.Date(min(NYcovidata$date))),"yyyy-mm-dd"),
                   max = 
                     as.character(format(as.Date(max(NYcovidata$date))),"yyyy-mm-dd"),
                   format = "yyyy-mm-dd")
    
  })
  output$table <-renderDataTable ({
    value <- isolate(input$df)
    if (identical(input$df,NULL)) {
      print("please select at least an attribute!")
    }
    else if(identical(input$df,"death")) {
      df1 <- subset(df1, date >= as.Date(input$date[1]) & date <= 
                         as.Date(input$date[2]))
      df1$date <- as.character(df1$date)
      print(df1)
    }
    else if(identical(input$df,"hospitalizedCurrently")) {
      df2 <- subset(df2, date >= as.Date(input$date[1]) & date <= 
                      as.Date(input$date[2]))
      df2$date <- as.character(df2$date)
      print(df2)
    }
    else if (identical(input$df, c("death", "hospitalizedCurrently" )) | identical(input$df, c("hospitalizedCurrently","death" ))) {
      df3 <- subset(df3, date >= as.Date(input$date[1]) & date <= 
                      as.Date(input$date[2]))
      df3$date <- as.character(df3$date)
      print(df3)
    }
  }, options = list(pageLength = 10)
     )%>% shiny::bindEvent(input$see)
  output$plot <-renderPlot ({
    value <- isolate(input$df)
    if (identical(input$df,"death")) {
      NYcovidata1 <- subset(NYcovidata1, date >= as.Date(input$date[1]) & date <= 
                      as.Date(input$date[2]))
      gg1 <- ggplot(NYcovidata1, aes(x = date, y = covidresult)) + geom_line(aes(color = variable), size = 1) + scale_color_manual(values = "#181d2a") + theme_minimal() 
      print(gg1)}
    else if(identical(input$df,"hospitalizedCurrently")) {
      NYcovidata2 <- subset(NYcovidata2, date >= as.Date(input$date[1]) & date <= 
                              as.Date(input$date[2]))
      gg2 <- ggplot(NYcovidata2, aes(x = date, y = covidresult)) + geom_line(aes(color = variable), size = 1) + scale_color_manual(values = "#2e74b7") + theme_minimal() 
      print(gg2)}
    else if (identical(input$df, c("death", "hospitalizedCurrently" )) | identical(input$df, c("hospitalizedCurrently","death" ))) {
      NYcovidata3 <- subset(NYcovidata3, date >= as.Date(input$date[1]) & date <= 
                              as.Date(input$date[2]))
      gg3 <- ggplot(NYcovidata3, aes(x = date, y = covidresult)) + geom_line(aes(color = variable), size = 1) + scale_color_manual(values = c("#2e74b7", "#181d2a")) + theme_minimal()
      print(gg3)}
  })%>%
    shiny::bindEvent(input$see)
}

shinyApp(ui = ui, server = server)


#observeEvent(input$df, filter(input$df == "a")) ({
#data <- data.frame(NYcovidata$date, NYcovidata$state, NYcovidata$death)
#})
#observeEvent(input$df, filter(input$df == "b")) ({
#data <- data.frame(NYcovidata$date, NYcovidata$state, NYcovidata$hospitalizedCurrently)
#})
#else if (DF == "b"){
#select ("date" = NYcovidata$date, "state" = NYcovidata$state, "statistics" = NYcovidata$hospitalizedCurrently)}

#Create reactions for daterange
#output$dateRange <- renderUI({
#dateRangeInput("range", "Which period of time do you want to look at?", start = "2020-03-02", end = "2021-03-07", min = "2020-03-02", max = "2021-03-07", format = "yyyy-mm-dd")
#})
#Create reactions for tableoutput
#<- renderTable({

#subset(df, NYcovidata$date >= as.Date((input$range[1])) & NYcovidata$date <= as.Date((input$range[2])))
#})
#output$plot <- renderPlot(ggplot(NYcovidata1, aes(x = date, y = covidresult)) + geom_line(aes(color = variable), size = 1) + scale_color_manual(values = c("#FFAEBC","#A0E7E5")) + theme_minimal())

