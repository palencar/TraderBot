library("shiny")
library("TraderBot")

shinyUI(navbarPage("TraderBot",
                   tabPanel("Backtest",
                            sidebarPanel(
                              headerPanel("Filters"),
                              selectizeInput("filterSymbol", "Symbols", choices = getSymbolNames(), multiple = TRUE),
                              sliderInput("smaPeriod",  "Sma Period:",  min =100, max =1000, value = c(300,500), step = 5),
                              sliderInput("upperBand",  "Upper Band:",  min = -2, max =   4, value = c(0.5,2.5), step= 0.1),
                              sliderInput("lowerBand",  "Lower Band:",  min = -4, max =   2, value = c(-2.5,-0.5), step= 0.1),
                              sliderInput("downChange", "Down Change:", min = -8, max =   0, value = c(-8,-4), step= 0.1),
                              sliderInput("upChange",   "Up Change:",   min =  0, max =   8, value = c(4,8), step= 0.1),
                              sliderInput("lowLimit",   "Low Limit:",   min =  0, max =   1, value = c(0,1), step= 0.1),
                              sliderInput("stopGain",   "Stop Gain:",   min =  1, max =   5, value = c(1,5), step= 0.1),
                              sliderInput("stopLoss",   "Stop Loss:",   min =  0, max =   1, value = c(0,1), step= 0.1),
                              sliderInput("bullish",    "Bullish:",     min =  0, max =   1, value = c(0.3,0.8), step= 0.1),
                              sliderInput("bearish",    "Bearish:",     min =  0, max =   1, value = c(0.2,0.7), step= 0.1),
                              sliderInput("proffit",    "Proffit:",     min = -1, max =   5, value = c(-1,5), step= 0.1)
                              ),
                            mainPanel(
                              tableOutput("values"),

                              plotOutput("parameters", height = "1600px"),
                              dataTableOutput("dataTable"))
                            ),

                   tabPanel("Alerts",
                            sidebarPanel(
                              headerPanel("Options"),

                              numericInput("numAlerts", "Number of alerts to view:", 5, min = 0, max = 30),

                              selectInput(inputId = "alertsTimeFrame",
                                          label = "Time frame",
                                          choices = c("Daily" = "daily",
                                                      "Weekly" = "weekly")),

                              dateRangeInput(inputId = "alertsDateRange", label = "Date range",
                                             start = Sys.Date() - 730, end = Sys.Date())
                              ),

                            mainPanel(
                              uiOutput("alerts")
                              )
                            ),

                   tabPanel("Charts",
                            sidebarPanel(
                              headerPanel("Options"),

                              selectizeInput(
                                'symbolNames', 'Symbols', choices = getSymbolNames(), multiple = TRUE
                              ),

                              selectInput(inputId = "chartsTimeFrame",
                                          label = "Time frame",
                                          choices = c("Daily" = "daily",
                                                      "Weekly" = "weekly")),

                              dateRangeInput(inputId = "chartsDateRange", label = "Date range",
                                             start = Sys.Date() - 730, end = Sys.Date())
                              ),

                            mainPanel(
                              uiOutput("charts")
                            )
                            )
))
