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
                                selectInput("opType", "Operation type", c("",  "C", "V")),
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

                              numericInput("numAlerts", "Alerts:", 5, min = 0, max = 30),

                              selectizeInput('typeAlerts', 'Type', choices = c("buy", "sell"), selected = c("buy", "sell"), multiple = TRUE),

                              numericInput("numIntervals", "Intervals:", 730)
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
                              selectizeInput("filterSymbol", "Symbols", choices = NULL, multiple = TRUE),
                              selectizeInput("timeFrames", "Time Frames", choices = timeFrameChoices, selected = timeFrameChoices, multiple = TRUE),
                              sliderInput("smaPeriod",  "Sma Period:",  min =100, max =1000, value = c(300,500), step = 5),
                              sliderInput("upperBand",  "Upper Band:",  min = -2, max =   4, value = c(0.5,2.5), step= 0.01),
                              sliderInput("lowerBand",  "Lower Band:",  min = -4, max =   2, value = c(-2.5,-0.5), step= 0.01),
                              sliderInput("downChange", "Down Change:", min = -8, max =   0, value = c(-8,0), step= 0.01),
                              sliderInput("upChange",   "Up Change:",   min =  0, max =   8, value = c(0,8), step= 0.01),
                              sliderInput("lowLimit",   "Low Limit:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("stopGain",   "Stop Gain:",   min =  1, max =   5, value = c(1,5), step= 0.01),
                              sliderInput("stopLoss",   "Stop Loss:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("bullBuy",    "Bull Buy:",    min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bullSell",   "Bull Sell:",   min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bearBuy",    "Bear Buy:",    min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bearSell",   "Bear Sell:",   min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("profit",     "Profit:",      min = -1, max =   5, value = c(-1,5), step= 0.01)
                            ),
                            mainPanel(
                              tableOutput("values"),

                              plotOutput("parameters", height = "1600px"),
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
    alerts <- data.table(getAlertsResults(getAlerts(input$numAlerts, input$typeAlerts)), key=c("symbol","timeframe","alert"))
    symbDf <- head(alerts[!duplicated(alerts[,c("symbol","timeframe")]), c("symbol","timeframe","alert")], input$numAlerts)
    alerts <- alerts[symbDf]

    numAlerts <- nrow(symbDf)
    wallet <- getWallet()
    numWallet <- length(wallet)
    balance <- getBalance()
    balance$open <- as.character.Date(balance$open)

    updateSelectizeInput(session, "filterSymbol",
                         label = "Symbols",
                         choices = as.vector(unique(mMergeBacktest()$name)),
                         selected = input$filterSymbol
                         )

    updateSelectizeInput(session, "symbolNames",
                         label = "Symbols",
                         choices = getSymbolNames(),
                         selected = input$symbolNames
                         )

    updateSelectizeInput(session, "opSymbol",
                         label = "Symbols",
                         choices = getSymbolNames(),
                         selected = NULL,
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
                         )

    if(numAlerts > 0)
    {
      for(i in 1:numAlerts)
      {
        local({
          my_i <- i

          output[[paste0("alertsResults", my_i)]] <- renderTable({ alerts[symbDf[my_i]] })
          output[[paste0("alerts", my_i)]] <- renderPlot({ make_chart(unique(alerts[symbDf[my_i]]$symbol), intervals = input$numIntervals, timeFrame = unique(alerts[symbDf[my_i]]$timeframe), mode = "simulation") })
        })
      }
    }

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
      tagList(tags$hr(), tableOutput(paste0("alertsResults", i)), plotOutput(paste0("alerts", i)))
    })

    do.call(tagList, outputList)
  })

  output$charts <- renderUI({
    if(length(input$symbolNames) > 0)
    {
      outputList <- lapply(1:length(input$symbolNames), function(i) {
        plotname <- paste("charts", i, sep="")
        plotOutput(plotname)
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

    if(!is.null(input$timeFrames) && !is.na(input$timeFrames))
      dataTable <- dataTable[(dataTable$timeframe  %in% input$timeFrames)]

    dataTable <- dataTable[(dataTable$smaPeriod  >= input$smaPeriod[1]  & dataTable$smaPeriod  <= input$smaPeriod[2])  | is.na(dataTable$smaPeriod)]
    dataTable <- dataTable[(dataTable$upperBand  >= input$upperBand[1]  & dataTable$upperBand  <= input$upperBand[2])  | is.na(dataTable$upperBand)]
    dataTable <- dataTable[(dataTable$lowerBand  >= input$lowerBand[1]  & dataTable$lowerBand  <= input$lowerBand[2])  | is.na(dataTable$lowerBand)]
    dataTable <- dataTable[(dataTable$downChange >= input$downChange[1] & dataTable$downChange <= input$downChange[2]) | is.na(dataTable$downChange)]
    dataTable <- dataTable[(dataTable$upChange   >= input$upChange[1]   & dataTable$upChange   <= input$upChange[2])   | is.na(dataTable$upChange)]
    dataTable <- dataTable[(dataTable$lowLimit   >= input$lowLimit[1]   & dataTable$lowLimit   <= input$lowLimit[2])   | is.na(dataTable$lowLimit)]
    dataTable <- dataTable[(dataTable$stopGain   >= input$stopGain[1]   & dataTable$stopGain   <= input$stopGain[2])   | is.na(dataTable$stopGain)]
    dataTable <- dataTable[(dataTable$stopLoss   >= input$stopLoss[1]   & dataTable$stopLoss   <= input$stopLoss[2])   | is.na(dataTable$stopLoss)]
    dataTable <- dataTable[(dataTable$bearBuy    >= input$bearBuy[1]    & dataTable$bearBuy    <= input$bearBuy[2])    | (is.na(dataTable$bullBuy)  & is.na(dataTable$bullSell))]
    dataTable <- dataTable[(dataTable$bearSell   >= input$bearSell[1]   & dataTable$bearSell   <= input$bearSell[2])   | (is.na(dataTable$bearSell) & is.na(dataTable$bearBuy))]
    dataTable <- dataTable[(dataTable$bullBuy    >= input$bullBuy[1]    & dataTable$bullBuy    <= input$bullBuy[2])    | (is.na(dataTable$bullBuy)  & is.na(dataTable$bullSell))]
    dataTable <- dataTable[(dataTable$bullSell   >= input$bullSell[1]   & dataTable$bullSell   <= input$bullSell[2])   | (is.na(dataTable$bearSell) & is.na(dataTable$bearBuy))]
    dataTable <- dataTable[(dataTable$profit_pp  >= input$profit[1]     & dataTable$profit_pp  <= input$profit[2])     | is.na(dataTable$profit_pp)]

    if(!is.null(input$filterSymbol) && !is.null(intersect(input$filterSymbol, unique(dataTable$name))))
      dataTable <- dataTable[dataTable$name %in% input$filterSymbol]

    dataTable
  })

  output$values <- renderTable({
    nrow(tableValues())
  })

  output$parameters <- renderPlot({
    tv <- tableValues()
    if(!is.null(tv) && nrow(tv) > 0)
    {
      grid.arrange(showPlot(tv, c("smaPeriod", "profit_pp")),
                   showPlot(tv, c("lowerBand", "profit_pp")),
                   showPlot(tv, c("upperBand", "profit_pp")),
                   showPlot(tv, c("downChange", "profit_pp")),
                   showPlot(tv, c("upChange", "profit_pp")),
                   showPlot(tv, c("lowLimit", "profit_pp")),
                   showPlot(tv, c("stopGain", "profit_pp")),
                   showPlot(tv, c("stopLoss", "profit_pp")),
                   showPlot(tv, c("bullBuy", "profit_pp")),
                   showPlot(tv, c("bullSell", "profit_pp")),
                   showPlot(tv, c("bearSell", "profit_pp")),
                   showPlot(tv, c("bearBuy", "profit_pp")),
                   nrow=4, ncol=3)

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
