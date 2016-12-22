library("shiny")

shinyUI(navbarPage("TraderBot",
                    tabPanel("Backtest",
                               plotOutput("parameters", height = "800px"),
                               dataTableOutput("dataTable")
                             ),

                    tabPanel("Alerts",
                             sidebarPanel(
                               headerPanel("Parameters"),

                               numericInput("numAlerts", "Number of alerts to view:", 5, min = 0, max = 30),

                               selectInput(inputId = "time_frame",
                                           label = "Time frame",
                                           choices = c("Daily" = "daily",
                                                       "Weekly" = "weekly")),

                               dateRangeInput(inputId = "daterange", label = "Date range",
                                              start = Sys.Date() - 730, end = Sys.Date())
                               ),

                             mainPanel(
                               uiOutput("charts")
                             )
                   )
))
