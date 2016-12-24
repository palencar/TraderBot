library("shiny")
library("TraderBot")

shinyUI(navbarPage("TraderBot",
                   tabPanel("Backtest",
                            plotOutput("parameters", height = "800px"),
                            dataTableOutput("dataTable")),

                   tabPanel("Alerts",
                            sidebarPanel(
                              headerPanel("Options"),

                              numericInput("numAlerts", "Number of alerts to view:", 5, min = 0, max = 30),

                              selectInput(inputId = "alertsTimeFrame",
                                          label = "Time frame",
                                          choices = c("Daily" = "daily",
                                                      "Weekly" = "weekly")),

                              radioButtons("alertsPeriod", "Period",
                                           c("6m"=180, "1y"=365, "2y"=730, "5y"=1825, "10y"=3650), selected = 730),

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

                              radioButtons("chartsPeriod", "Period",
                                           c("6m"=180, "1y"=365, "2y"=730, "5y"=1825, "10y"=3650), selected = 730),

                              dateRangeInput(inputId = "chartsDateRange", label = "Date range",
                                             start = Sys.Date() - 730, end = Sys.Date())
                              ),

                            mainPanel(
                              uiOutput("charts")
                            )
                            )
))
