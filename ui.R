library("shiny")
library("TraderBot")

shinyUI(navbarPage("TraderBot",
                   tabPanel("Backtest",
                            sidebarPanel(
                              headerPanel("Filters"),
                              #textInput("filterSymbol", label = h3("Symbol"), value = ""),
                              selectizeInput("filterSymbol", "Symbols", choices = getSymbolNames(), multiple = TRUE),
                              sliderInput("V1", "Sma Period:",  min = 50, max = 500, value = c(50,500)),
                              sliderInput("V2", "Upper Band:",  min =  0, max =   4, value = c(0,4), step= 0.1),
                              sliderInput("V3", "Lower Band:",  min = -4, max =   0, value = c(-4,0), step= 0.1),
                              sliderInput("V4", "Down Change:", min = -2, max =   0, value = c(-2,0), step= 0.1),
                              sliderInput("V5", "Up Change:",   min =  0, max =   2, value = c(0,2), step= 0.1),
                              sliderInput("V6", "Low Limit:",   min =  0, max =   1, value = c(0,1), step= 0.1),
                              sliderInput("V7", "Stop Gain:",   min =  1, max =   5, value = c(1,5), step= 0.1),
                              sliderInput("V8", "Stop Loss:",   min =  0, max =   1, value = c(0,1), step= 0.1)
                              ),
                            mainPanel(
                              tableOutput("values"),

                              plotOutput("parameters", height = "800px"),
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
