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
library(DT)
library(ggplot2)
library(forecast)

#import the data

arima <- readRDS("fit_arima.rds")
mean <- readRDS("fit_m2.rds")
naive <- readRDS("fit_n.rds")
seasonal_naive <- readRDS("fit_sn.rds")
drift_method <- readRDS("fit_drift.rds")
holts_method <- readRDS("fit_holt.rds")
simple_exp_smooth <- readRDS("fit_ses.rds")
ess <- readRDS("fit_ess.rds")
eleven_ym_ts <- readRDS("eleven_ym_ts.rds")
linear_model <- readRDS("fit_spl.rds")
accuracy_table <- readRDS("accuracy_table3.rds")

# Define UI for application that draws the forecast plots

ui <- dashboardPage(
  dashboardHeader(title = "Foreasting Crime for the 11th Police District, Chicago",
                  titleWidth = 900),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    fluidRow(
      column(width = 7,
             box(plotOutput("model"),
                 width = NULL),
             box(plotOutput("model2"),
                 width = NULL)),
      column(width = 5,
             box(selectInput("model", "Select Forecast Method:",
                             c("Arima" = "arima",
                               "Mean" = "mean",
                               "Naive" = "naive",
                               "Seasonal Naive" = "seasonal_naive",
                               "Drift Method" = "drift_method",
                               "Holt's Method" = "holts_method",
                               "Simple Exponential Smoothing" = "simple_exp_smooth",
                               "Exponential Smoothing State Space" = "exp_smooth_state",
                               "Linear Model - Cubic Smoothing Spines" = "linear_model")),
                 width=NULL),
             box(DT::dataTableOutput("accuracy_table"),
                 width=NULL),
             box(verbatimTextOutput('print_summary'),
                 width=NULL))
    )
    )
    )


# Define server logic required to draw the plots/datatables/text outputs

server <- function(input, output) {
  output$model <- renderPlot({
    if (input$model == "arima") {
      plot(forecast(arima,h = 48))
    } else if (input$model == "mean") {
      autoplot(mean)  
    } else if (input$model == "naive") {
      autoplot(naive)
    } else if (input$model == "seasonal_naive") {
      autoplot(seasonal_naive)
    } else if (input$model == "drift_method") {
      autoplot(drift_method)
    } else if (input$model == "holts_method") {
      autoplot(holts_method)
    } else if (input$model == "simple_exp_smooth") {
      autoplot(simple_exp_smooth)
    } else if (input$model == "exp_smooth_state") {
      ess <- eleven_ym_ts[,3] %>% ets %>% forecast(h=48)  
      autoplot(eleven_ym_ts[,3], series="Data") + 
        autolayer(ess, series="Forecast") + 
        autolayer(fitted(ess), series="Fitted")
    } else if (input$model == "linear_model") {
      autoplot(linear_model)
    }
  })
  
  output$model2 <- renderPlot({
    if (input$model == "arima") {
      checkresiduals(arima)
    } else if (input$model == "mean") {
      checkresiduals(mean)
    } else if (input$model == "naive") {
      checkresiduals(naive)
    } else if (input$model == "seasonal_naive") {
      checkresiduals(seasonal_naive)
    } else if (input$model == "drift_method") {
      checkresiduals(drift_method)
    } else if (input$model == "holts_method") {
      checkresiduals(holts_method)
    } else if (input$model == "simple_exp_smooth") {
      checkresiduals(simple_exp_smooth)
    } else if (input$model == "exp_smooth_state") {
      checkresiduals(ess)
    } else if (input$model == "linear_model") {
      checkresiduals(linear_model)
    }
  })
  output$accuracy_table = DT::renderDataTable({
    if (input$model == "arima") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('arima'), c('yellow')))
    }
    else if (input$model == "mean") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('mean'), c('yellow')))
    }
    else if (input$model == "naive") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('naive'), c('yellow')))
    }
    else if (input$model == "seasonal_naive") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('snaive'), c('yellow')))
    }
    else if (input$model == "drift_method") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('drift'), c('yellow')))
    }
    else if (input$model == "holts_method") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('holt'), c('yellow')))
    }
    else if (input$model == "simple_exp_smooth") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('ses'), c('yellow')))
    }
    else if (input$model == "exp_smooth_state") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('ess'), c('yellow')))
    }
    else if (input$model == "linear_model") {
      datatable(accuracy_table) %>% formatStyle(
        'Method',
        target = 'row',
        backgroundColor = styleEqual(c('spline'), c('yellow')))
    }
  })
  output$print_summary = renderPrint({
    if (input$model == "arima") {
      summary(arima)
    }
    else if (input$model == "mean") {
      mean$model
    }
    else if (input$model == "naive") {
      naive$model
    }
    else if (input$model == "seasonal_naive") {
      seasonal_naive$model
    }
    else if (input$model == "drift_method") {
      drift_method$model
    }
    else if (input$model == "holts_method") {
      holts_method$model
    }
    else if (input$model == "simple_exp_smooth") {
      simple_exp_smooth$model
    }
    else if (input$model == "exp_smooth_state") {
      ess$model
    }
    else if (input$model == "linear_model") {
      linear_model$model
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

