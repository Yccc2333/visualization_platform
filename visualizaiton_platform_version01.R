library(shiny)
library(dplyr)
#install.packages('shinydashboard')
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)
library(flexdashboard)
library(shiny)
#install.packages("devtools")
#devtools::install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(plotly)
library(parsnip)
library(timetk)
library(xgboost)
library(ggthemes)

useShinyjs(rmd = TRUE)
setwd('D:/R-programing/project')
getwd()
# Load functions
source("function/TS_function.R")
source('function/xgboost_forcast_coustomer.R')

########################Data processing for page 5 and page 6 ##########################
# Load data
sales_data_raw <- read_csv('data/orders02.csv') 
#country_codes <- read_csv("https://datahub.io/core/country-list/r/data.csv")
#country_codes <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# Select relevant data
processed_data_tbl <- sales_data_raw %>% 
  select(ORDERDATE, ORDERNUMBER, ORDERLINENUMBER, COUNTRY, SALES, PRODUCTLINE, DEALSIZE, STATUS, CUSTOMERNAME)

# Preprocessing
processed_data_tbl <- processed_data_tbl %>%
  mutate(ORDERDATE = mdy_hm(ORDERDATE),
         ORDERDATE = as_datetime(ORDERDATE))

# Manual edits
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="UK"] <- "United Kingdom"
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="USA"] <- "United States"

########################Data processing for page 6 ##########################
vip_num=row_number(processed_data_tbl)[1:800]
lost_num=row_number(processed_data_tbl)[801:1200]
processed_data_tbl$classification='Retain_Customers'
processed_data_tbl$classification[vip_num]='Important_Vip'
processed_data_tbl$classification[lost_num]='Lost_Customers'

############################## ui #########################
ui <- dashboardPage(
  dashboardHeader(title = "Customer Dashboard"),
  dashboardSidebar(
    sidebarMenu( width = 160,
      id = "sidebar",
      menuItem("Customer Activity Analysis-HJ", tabName = "page1"),
      menuItem("Customer Order Analysis-HJ", tabName = "page2"),
      menuItem("Conversion Rate-YXC", tabName = "page3"),
      menuItem("Consumer Segmentation-YXC", tabName = "page4"),
      menuItem("Future Order Forecasting-WT", tabName = "page5"),
      menuItem("Customer Forecasting-WT", tabName = "page6")
    )
  ),
  dashboardBody(
    tabItems(
      # Page 1
      tabItem(
        tabName = "page1",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page1_filter", "Page 1 Filter", choices = c("Option 1", "Option 2"), selected = "Option 1")
          ),
          mainPanel(
            h2("Page 1"),
            # Add main panel content for page 1 here
            textOutput("page1_output")
          )
        )
      ),
      # Page 2
      tabItem(
        tabName = "page2",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 2 here
            selectInput("page2_filter", "Page 2 Filter", choices = c("Option A", "Option B"), selected = "Option A")
          ),
          mainPanel(
            h2("Page 2"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      ),
      # Page 3
      tabItem(
        tabName = "page3",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page3_filter", "Page 1 Filter", choices = c("Option 1", "Option 2"), selected = "Option 1")
          ),
          mainPanel(
            h2("Page 3"),
            # Add main panel content for page 1 here
            textOutput("page3_output")
          )
        )
      ),
      # Page 4
      tabItem(
        tabName = "page4",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 2 here
            selectInput(
              "page4_filter", "Predict Month", choices = c("2023-06", "2023-07","2023-08", "2023-09","2023-10", "2023-11"), selected = "Option A")
          ),
          mainPanel(
            h2("Page 4"),
            # Add main panel content for page 2 here
            textOutput("page4_output")
          )
        )
      ),
      # Page 5

      tabItem(
        tabName = "page5",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            shinyWidgets::airDatepickerInput(
              inputId = "date_range",
              label = h4("Date"),
              value = c(min(processed_data_tbl$ORDERDATE),
                        max(processed_data_tbl$ORDERDATE)),
              separator = " to ",
              range = TRUE,
              minDate  = min(processed_data_tbl$ORDERDATE),
              maxDate = max(processed_data_tbl$ORDERDATE),
              dateFormat = "mm-dd-yyyy",
              autoClose = TRUE,
              clearButton = TRUE,
              width = "100%",
              addon = "none"
          ),
          shinyWidgets::pickerInput(
            inputId  = "picker_country",
            label    = h4("Country"),
            choices  = sort(unique(processed_data_tbl$COUNTRY)),
            selected = unique(processed_data_tbl$COUNTRY),
            multiple = TRUE, # Allow multiple options
            options = list(
              `actions-box` = TRUE,  # Note back ticks
              size = 10,
              `selected-text-format` = "count > 3"
          )),
          shinyWidgets::radioGroupButtons(
            inputId   = "time_unit", # Create name 
            #label     = "Time Unit", # What is shown
            choices   = c("Day"="day","Week"="week","Month"="month","Qtr"="quarter","Year"="year"), # The options shown
            selected  = "month", # Default selection
            status    = "info", # Set color
            justified = TRUE
            ),
          h4("Forecast"),
          shinyWidgets::switchInput(inputId = "forecast_mode",
                                    handleWidth = 100,
                                    labelWidth = 100,
                                    inline = TRUE,
                                    value = FALSE,
                                    onStatus = "info",
                                    onLabel = "On",
                                    offLabel = "Off",
                                    width = "200px"),
          
          conditionalPanel(condition = "input.forecast_mode == 1",
                           numericInput(inputId = "n_future",
                                        label = "Forecast Horizon",
                                        value = 12,
                                        min = 1  # At least 1 period in the future
                           )),
          # APPLY BUTTONS -----
          actionButton(inputId = "apply", 
                       label   = "Apply Forcast", 
                       icon    = icon("play"),
                       width   = '50%')
          ),
          mainPanel(
            h2("Forcasting Future Order"),
  
            # Add main panel content for page 1 here
            plotlyOutput(outputId = "page5_output"),
            
            h2("Acutal and Forcast Order Info"),
            
            DT::dataTableOutput(outputId="page5_output02")
            
          )
        )
      ),
      # Page 6
      tabItem(
        tabName = "page6",
        sidebarLayout(
          sidebarPanel(
            shinyWidgets::airDatepickerInput(
              inputId = "date_range02",
              label = h4("Date"),
              value = c(min(processed_data_tbl$ORDERDATE),
                        max(processed_data_tbl$ORDERDATE)),
              separator = " to ",
              range = TRUE,
              minDate  = min(processed_data_tbl$ORDERDATE),
              maxDate = max(processed_data_tbl$ORDERDATE),
              dateFormat = "mm-dd-yyyy",
              autoClose = TRUE,
              clearButton = TRUE,
              width = "100%",
              addon = "none"
            ),
            shinyWidgets::pickerInput(
              inputId  = "picker_customer",
              label    = h4("Customer (at least 1 customers)"),
              choices  = unique(processed_data_tbl$CUSTOMERNAME) %>% sort(),
              selected = unique(processed_data_tbl$CUSTOMERNAME) %>% sort(),
              multiple = TRUE, # Allow multiple options
              options  = list(
                `actions-box` = TRUE,  # Note back ticks
                size = 10,
                `selected-text-format` = "count > 1"
              )),
            shinyWidgets::radioGroupButtons(
              inputId   = "time_unit02", # Create name 
              #label     = "Time Unit", # What is shown
              choices   = c("Day"="day","Week"="week","Month"="month","Qtr"="quarter","Year"="year"), # The options shown
              selected  = "month", # Default selection
              status    = "info", # Set color
              justified = TRUE
            ),
            h4("Forecast"),
            shinyWidgets::switchInput(inputId = "forecast_mode02",
                                      handleWidth = 100,
                                      labelWidth = 100,
                                      inline = TRUE,
                                      value = FALSE,
                                      onStatus = "info",
                                      onLabel = "On",
                                      offLabel = "Off",
                                      width = "200px"),
            
            conditionalPanel(condition = "input.forecast_mode02 == 1",
                             numericInput(inputId = "n_future02",
                                          label = "Forecast Horizon",
                                          value = 12,
                                          min = 1  # At least 1 period in the future
                             )),
            # APPLY BUTTONS -----
            actionButton(inputId = "apply02", 
                         label   = "Apply Forcast", 
                         icon    = icon("play"),
                         width   = '50%')
          ),
          mainPanel(
            h2("Forcasting Customer"),
            
            # Add main panel content for page 1 here
            plotlyOutput(outputId = "page6_output"),
            
            h2("Acutal and Forcast Customer Info"),
            
            DT::dataTableOutput(outputId="page6_output02")
            
          )
        )
      )
    )
  )
)

######################### server #########################

server <- function(input, output) {
  # Server logic goes here
  
  output$page1_output <- renderText({
    paste("Selected option in Page 1:", input$page1_filter)
  })
  
  output$page2_output <- renderText({
    paste("Selected option in Page 2:", input$page2_filter)
  })
  output$page3_output <- renderText({
    paste("Selected option in Page 3:", input$page1_filter)
  })
  
  output$page4_output <- renderText({
    paste("Selected option in Page 4:", input$page2_filter)
  })

  ######## Reactive event for page5 #########################
  #1. Reactive Event: waits until a button (Apply) is clicked to run reactive code 
  processed_data_filtered_tbl <- eventReactive(
    eventExpr = input$apply, 
    
    valueExpr = {
      
      processed_data_tbl %>%
        
        # Date Range filter
        filter(ORDERDATE %>% between(left = as_datetime(input$date_range[1]),
                                     right = as_datetime(input$date_range[2]))) %>%
        
        
        # Picker filter: Country
        filter(COUNTRY %in% input$picker_country)
    },
    ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
  )
  # 2.If time button is pressed, then act like Apply button is pressed
  observeEvent(eventExpr = input$time_unit, {
    
    if (input$forecast_mode) {
      
      delay(300, click(id = "apply")) # Add delay and simulate clicking actions in JavaScript
      
    } })
  
  # 3.If forecast switch is pressed, then act like Apply button is pressed...but only once
  observeEvent(eventExpr = input$forecast_mode, {
    
    delay(300, click(id = "apply")) # Add delay and simulate clicking actions in JavaScript
    
  }, once = TRUE) 

  # Reactive expression based on input
  time_plot_tbl <- reactive({
    
    processed_data_filtered_tbl() %>%
      aggregate_time_series(time_unit = input$time_unit)
    
  })
  
  # Reactive event for forecast switch
  time_plot_predictions_tbl <- eventReactive(eventExpr = input$apply, {
    
    if (input$forecast_mode) {
      time_plot_tbl() %>%
        generate_forecast(n_future = input$n_future, seed = 123)
    }
    
  })
  #############################################################

  
  output$page5_output <- plotly::renderPlotly({
    
    if (input$forecast_mode) {
      p <- time_plot_predictions_tbl() %>%
        plot_forecast()
    } else {
      p <- time_plot_tbl() %>%
        plot_time_series()
    }
    
    p %>%
      layout(margin = list(b = 150))
    
  })
  opts <- list(
 
    pageLength = 10,
    searchHighlight = TRUE
  )
  
  output$page5_output02 <- DT::renderDataTable({
    
    if (input$forecast_mode) {
      time_plot_predictions_tbl()  %>%dplyr::select(key,date,total_sales)
    }
    
    else {time_plot_tbl() %>%dplyr::select(date,total_sales)}
    
  },options = opts)

######## Reactive event for page6 #########################
#1. Reactive Event: waits until a button (Apply) is clicked to run reactive code 
### Tip: For performance reasons, find repetitive code and put here
  processed_customer_filtered <- eventReactive(
    eventExpr = input$apply02, 
    
    valueExpr = {
      
      processed_data_tbl %>%
        
        # Date Range filter
        filter(ORDERDATE %>% between(left = as_datetime(input$date_range02[1]),
                                     right = as_datetime(input$date_range02[2]))) %>%
        
        
        # Picker filter: Country
        filter(CUSTOMERNAME  %in% input$picker_customer)
    },
    ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
  )
  
  # 2.If time button is pressed, then act like Apply button is pressed
  observeEvent(eventExpr = input$time_unit02, {
    
    if (input$forecast_mode02) {
      
      delay(300, click(id = "apply02")) # Add delay and simulate clicking actions in JavaScript
      
    } })
  
  # 3.If forecast switch is pressed, then act like Apply button is pressed...but only once
  observeEvent(eventExpr = input$forecast_mode02, {
    
    delay(300, click(id = "apply02")) # Add delay and simulate clicking actions in JavaScript
    
  }, once = TRUE) 
  #######################################
  #######################################################
  # Reactive expression based on input
  customer_plot_tbl <- reactive({
    
    processed_customer_filtered() %>%
      classification_time_series(time_unit = input$time_unit02)
    
  })
  
  # Reactive event for forecast switch
  customer_plot_predictions_tbl <- eventReactive(eventExpr = input$apply02, {
    
    if (input$forecast_mode02) {
      customer_plot_tbl() %>%
        customer_forecast(n_future = input$n_future02, seed = 123)
    }
    
  })
  #############################################################
  
  
  output$page6_output <- plotly::renderPlotly({
    
    if (input$forecast_mode02) {
      p02 <- customer_plot_predictions_tbl() %>%
        plot_customer_forecast ()
    } else {
      p02 <- customer_plot_tbl() %>%
        plot_classification_series()
    }
    
    p02 %>%
      layout(margin = list(b = 150))
    
  })
  
  output$page6_output02 <- DT::renderDataTable({
    
    if (input$forecast_mode02) {
      customer_plot_predictions_tbl ()  %>%dplyr::select(key,date,classification ,total_num)
    }
    
    else {customer_plot_tbl() %>%dplyr::select(date,classification,total_num)}
    
  },options = opts)
}

shinyApp(ui, server)





