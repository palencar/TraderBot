source("R/report.R")
source("R/chart.R")
source("R/dbInterface.R")


mMergeBacktest <- memoise(mergeBacktest, ~timeout(600))

timeFrameChoices <- c("1D", "1H", "30M", "15M", "10M", "5M")

ui <- shinyUI(navbarPage("TraderBot",

                   tabPanel("Wallet",
                            sidebarPanel(
                              headerPanel("Options"),

                              selectInput(inputId = "walletTimeFrame",
                                          label = "Time frame",
                                          choices = timeFrameChoices,
                                          selected = "1D"),

                              dateRangeInput(inputId = "walletDateRange", label = "Date range",
                                             start = (Sys.Date() - 730), end = NULL)
                            ),

                            mainPanel(
                              uiOutput("wallet")
                            )
                   ),

                   tabPanel("Operations",
                            sidebarPanel(
                              titlePanel("Insert operation"),
                              div(
                                id = "form",
                                selectizeInput('opSymbol', 'Symbols', choices = NULL, selected = NULL, multiple = FALSE),
                                dateInput("opDate", "Date"),
                                selectInput("opType", "Operation type", c("",  "buy", "sell")),
                                textInput("opSize", "Size", value = "100"),
                                textInput("opPrice", "Price", ""),
                                textInput("opCost", "Cost", ""),
                                actionButton("opSubmit", "Submit", class = "btn-primary")
                                )),

                            mainPanel(
                              uiOutput("operations")
                            )
                   ),

                   tabPanel("Charts",
                            sidebarPanel(
                              headerPanel("Options"),

                              selectizeInput(
                                'symbolNames', 'Symbols', choices = NULL, multiple = TRUE
                              ),

                              selectInput(inputId = "chartsTimeFrame",
                                          label = "Time frame",
                                          choices = timeFrameChoices,
                                          selected = "1D"),

                              dateRangeInput(inputId = "chartsDateRange", label = "Date range",
                                             start = (Sys.Date() - 730), end = NULL)
                            ),

                            mainPanel(
                              uiOutput("charts")
                            )
                   ),

                   tabPanel("Alerts",
                            sidebarPanel(
                              headerPanel("Options"),

                              selectizeInput(
                                'symbolAlerts', 'Symbols', choices = NULL, multiple = TRUE
                              ),

                              numericInput("numAlerts", "Alerts:", 5, min = 0, max = 30),

                              selectizeInput('typeAlerts', 'Type', choices = c("buy", "sell"), selected = c("buy", "sell"), multiple = TRUE),

                              selectizeInput("numIntervals", "Period:", choices = c(30, 90, 180, 360, 720, 3600), selected = 720)
                            ),

                            mainPanel(
                              uiOutput("alerts")
                            )
                   ),

                   tabPanel("Backtest",
                            sidebarPanel(
                              headerPanel("Filters"),
                              checkboxInput('open', 'Open', TRUE),
                              checkboxInput('closed', 'Closed', TRUE),
                              checkboxInput('long', 'Long', TRUE),
                              checkboxInput('short', 'Short', TRUE),
                              selectInput("group", "Group by:", choices = c("State"="state", "Type"="type", "Time Frame"="timeframe", "State and Time Frame"="state_timeframe", "None" = "none")),
                              selectizeInput("filterSymbol", "Symbols",   choices = NULL, multiple = TRUE),
                              selectizeInput("timeFrames", "Time Frames", choices = timeFrameChoices, selected = timeFrameChoices, multiple = TRUE),
                              sliderInput("smaPeriod",     "Sma Period:",  min =100, max =1000, value = c(0,1000), step = 5),
                              sliderInput("upperBand",     "Upper Band:",  min = 1,  max = 2.5, value = c(1,2.5), step= 0.1),
                              sliderInput("lowerBand",     "Lower Band:",  min = -2.5, max =-1, value = c(-2.5,-1), step= 0.01),
                              sliderInput("lowLimit",      "Low Limit:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("stopGainLong",  "Stop Gain Long:", min = 1, max = 5, value = c(1,5), step= 0.01),
                              sliderInput("stopGainShort", "Stop Gain Short:", min = 1, max = 5, value = c(1,5), step= 0.01),
                              sliderInput("stopLoss",      "Stop Loss:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("grade",         "Grade:",       min = -10, max = 10, value = c(-10,10), step= 0.01)
                            ),
                            mainPanel(
                              tableOutput("values"),

                              plotOutput("parameters", height = "1200px"),
                              dataTableOutput("dataTable"))
                   )
))

server <- shinyServer(function(input, output, session)
{
  values <- reactiveValues(operations = getOperations(decreasing = TRUE))

  make_chart <- function(symbol, intervals = 730, startDate = NULL, endDate = Sys.time(), timeFrame, mode = "operation")
  {
    if(timeFrame == "1D")
      symbol <- getSymbolsDaily(symbol, adjust = c("split", "dividend"))
    else
      symbol <- getSymbolsIntraday(symbol, timeFrame, adjust = c("split", "dividend"))

    if(!is.null(symbol))
      chartSymbols(symbol, period = intervals, startDate = startDate, endDate = endDate, timeFrame = timeFrame, mode = mode)
  }

  observeEvent(input$opSubmit, {

    size   <- as.integer(input$opSize)
    price  <- as.numeric(input$opPrice)
    cost   <- as.numeric(input$opCost)

    if(is.character(input$opSymbol) && !is.na(size) && !is.na(price) && !is.na(cost) && nchar(input$opType) > 0)
      insertOperations(input$opSymbol, as.Date(input$opDate), input$opType, as.integer(input$opSize), as.numeric(input$opPrice), as.numeric(input$opCost))
    else
      print("invalid input")

    values$operations <- getOperations(decreasing = TRUE)
  })

  observe({
    invalidateLater(300000, session)

    updateSelectizeInput(session, "symbolAlerts",
                         label = "Symbols",
                         choices = getSymbolNames(),
                         selected = input$symbolAlerts
    )

    alerts.table <- getAlertsResults(getAlerts(input$numAlerts, input$symbolAlerts, input$typeAlerts))
    alerts.table <- data.table(alerts.table, key=c("symbol", "timeframe"))

    if(nrow(alerts.table) > 0)
    {
      alerts <- data.table(alerts.table[!duplicated(alerts.table[,c("symbol","timeframe")])], key=c("symbol", "timeframe"))[order(-datetime)]

      for(i in 1:nrow(alerts))
      {
        local({
          my_i <- i
          drops <- c("symbol", "timeframe")
          alerts.table[, !drops, with=FALSE]

          output[[paste0("alertsResults", my_i)]] <- renderDataTable({alerts.table[alerts[my_i, c("symbol", "timeframe")], !drops, with=FALSE]}, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
          output[[paste0("alerts", my_i)]] <- renderPlot({ make_chart(unique(alerts[my_i]$symbol), intervals = as.integer(input$numIntervals), timeFrame = unique(alerts[my_i]$timeframe), mode = "none") })
        })
      }
    }
  })

  observe({
    invalidateLater(300000, session)

    wallet <- getWallet()
    numWallet <- length(wallet)
    balance <- getBalance()
    balance$open <- as.character.Date(balance$open)

    if(numWallet > 0)
    {
      for(i in 1:numWallet)
      {
        local({
          my_i <- i
          startDate <- input$walletDateRange[1]
          endDate   <- input$walletDateRange[2]

          output[[paste0("balance", my_i)]] <- renderTable({balance[balance$symbol == wallet[my_i], ]})
          output[[paste0("wallet", my_i)]] <- renderPlot({ make_chart(wallet[[my_i]], startDate = startDate, endDate = endDate, timeFrame = input$walletTimeFrame) })
        })
      }
    }
  })

  observe({
    updateSelectizeInput(session, "filterSymbol",
                         label = "Symbols",
                         choices = as.vector(unique(mMergeBacktest()$symbol)),
                         selected = input$filterSymbol
                         )
  })

  observe({
    updateSelectizeInput(session, "opSymbol",
                         label = "Symbols",
                         choices = getSymbolNames(),
                         selected = NULL,
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
  })

  observe({
    invalidateLater(300000, session)

    updateSelectizeInput(session, "symbolNames",
                         label = "Symbols",
                         choices = getSymbolNames(),
                         selected = input$symbolNames
                         )

    numCharts <- length(input$symbolNames)

    if(numCharts > 0)
    {
      for(i in 1:numCharts)
      {
        local({
          my_i <- i
          my_symbol <- input$symbolNames[[my_i]]
          startDate <- input$chartsDateRange[1]
          endDate   <- input$chartsDateRange[2]

          plotname <- paste("charts", my_i, sep="")
          output[[plotname]] <- renderPlot({ make_chart(my_symbol, startDate = startDate, endDate = endDate, timeFrame = input$chartsTimeFrame) })
        })
      }
    }
  })

  output$alerts <- renderUI({
    outputList <- lapply(1:input$numAlerts, function(i) {
      list(
          dataTableOutput(paste0("alertsResults", i)),
          plotOutput(paste0("alerts", i))
      )
    })

    do.call(tagList, outputList)
  })

  output$charts <- renderUI({
    if(length(input$symbolNames) > 0)
    {
      outputList <- lapply(1:length(input$symbolNames), function(i) {
        list(
          plotOutput(paste("charts", i, sep=""))
        )
      })
      do.call(tagList, outputList)
    }
  })

  output$wallet <- renderUI({
    wallet <- getWallet()
    if(length(wallet) > 0)
    {
      outputList <- lapply(1:length(wallet), function(i) {
        tagList(tags$hr(), tableOutput(paste0("balance", i)), plotOutput(paste0("wallet", i)))
      })

      do.call(tagList, outputList)
    }
  })

  output$operations <- renderTable({
    values$operations
  })

  tableValues <- reactive({
    dataTable <- mMergeBacktest(path = "result/")

    if(is.null(dataTable) || nrow(dataTable) == 0)
      return(NULL)

    if(xor(input$open, input$closed))
    {
      if(input$open)
        dataTable <- dataTable[dataTable$state == "open"]

      if(input$closed)
        dataTable <- dataTable[dataTable$state == "closed"]
    }

    if(xor(input$long, input$short))
    {
      if(input$long)
        dataTable <- dataTable[dataTable$type == "long"]

      if(input$short)
        dataTable <- dataTable[dataTable$type == "short"]
    }

    if(!is.null(input$timeFrames) && !is.na(input$timeFrames))
      dataTable <- dataTable[(dataTable$timeframe  %in% input$timeFrames)]

    dataTable <- dataTable[(dataTable$smaPeriod     >= input$smaPeriod[1]     & dataTable$smaPeriod     <= input$smaPeriod[2])      | is.na(dataTable$smaPeriod)]
    dataTable <- dataTable[(dataTable$upperBand     >= input$upperBand[1]     & dataTable$upperBand     <= input$upperBand[2])      | is.na(dataTable$upperBand)]
    dataTable <- dataTable[(dataTable$lowerBand     >= input$lowerBand[1]     & dataTable$lowerBand     <= input$lowerBand[2])      | is.na(dataTable$lowerBand)]
    dataTable <- dataTable[(dataTable$lowLimit      >= input$lowLimit[1]      & dataTable$lowLimit      <= input$lowLimit[2])       | is.na(dataTable$lowLimit)]
    dataTable <- dataTable[(dataTable$stopGainLong  >= input$stopGainLong[1]  & dataTable$stopGainLong  <= input$stopGainLong[2])   | is.na(dataTable$stopGainLong)]
    dataTable <- dataTable[(dataTable$stopGainShort >= input$stopGainShort[1] & dataTable$stopGainShort <= input$stopGainShort[2])  | is.na(dataTable$stopGainShort)]
    dataTable <- dataTable[(dataTable$stopLoss      >= input$stopLoss[1]      & dataTable$stopLoss      <= input$stopLoss[2])       | is.na(dataTable$stopLoss)]
    dataTable <- dataTable[(dataTable$profit_pp     >= input$grade[1]         & dataTable$profit_pp     <= input$grade[2])          | is.na(dataTable$profit_pp)]

    if(!is.null(input$filterSymbol) && !is.null(intersect(input$filterSymbol, unique(dataTable$symbol))))
      dataTable <- dataTable[dataTable$symbol %in% input$filterSymbol]

    dataTable
  })

  output$values <- renderTable({
    nrow(tableValues())
  })

  output$parameters <- renderPlot({
    tv <- tableValues()
    if(!is.null(tv) && nrow(tv) > 0)
    {
      grid.arrange(
        showPlot(tv, c("smaPeriod", "grade"), input$group),
        showPlot(tv, c("lowerBand", "grade"), input$group),
        showPlot(tv, c("upperBand", "grade"), input$group),
        showPlot(tv, c("lowLimit", "grade"), input$group),
        showPlot(tv, c("highLimit", "grade"), input$group),
        showPlot(tv, c("stopGainLong", "grade"), input$group),
        showPlot(tv, c("stopGainShort", "grade"), input$group),
        showPlot(tv, c("stopLoss", "grade"), input$group),
        ncol = 2
      )
    }
  })

  output$dataTable <- renderDataTable({
    tv <- tableValues()
    if(!is.null(tv) && nrow(tv) > 0)
      showReport(tv)
    },
    options = list(paging = FALSE))
})

#' @export
runShinyApp <- function(options = list(host="127.0.0.1", port=8000))
{
  shinyApp(ui = ui, server = server, options = options)
}
