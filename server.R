library("shiny")
library("TraderBot")
source("R/report.R")
source("R/chart.R")

# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame())
{
  if (is.null(envir[[symbol]]))
  {
    envir[[symbol]] <- startProbe(symbol, FALSE) #TRUE?
    #symbol <- filterGap(symbol, lastTradingSession())
  }

  envir[[symbol]]
}

shinyServer(function(input, output)
{
  symbol_env <- new.env()

  make_chart <- function(symbol, startDate, endDate, timeFrame) {
    symbol_data <- require_symbol(symbol, symbol_env)

    if(!is.null(symbol))
      chartSymbols(Symbols=symbol, startDate = startDate, endDate = endDate,
                   timeFrame = timeFrame)
  }

  observe({
    alerts <- getAlerts(input$numAlerts)
    symbols <- unique(as.vector(alerts$symbol))
    numAlerts <- min(length(symbols), input$numAlerts)

    if(numAlerts > 0)
    {
      for(i in 1:numAlerts)
      {
        local({
          my_i <- i
          startDate <- input$alertsDateRange[1]
          endDate   <- input$alertsDateRange[2]

          plotname <- paste("alerts", my_i, sep="")
          output[[plotname]] <- renderPlot({ make_chart(symbols[[my_i]], startDate, endDate, input$alertsTimeFrame) })
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
          output[[plotname]] <- renderPlot({ make_chart(my_symbol, startDate, endDate, input$chartsTimeFrame) })
        })
      }
    }
  })

  output$alerts <- renderUI({
    plot_output_list <- lapply(1:input$numAlerts, function(i) {
      plotname <- paste("alerts", i, sep="")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
  })

  output$charts <- renderUI({
    if(length(input$symbolNames) > 0)
    {
      plot_output_list <- lapply(1:length(input$symbolNames), function(i) {
        plotname <- paste("charts", i, sep="")
        plotOutput(plotname)
      })

      do.call(tagList, plot_output_list)
    }
  })

  tableValues <- reactive({
    dataTable <- mergeBacktest()

    dataTable <- dataTable[(dataTable$smaPeriod  >= input$smaPeriod[1]  & dataTable$smaPeriod <= input$smaPeriod[2])   | is.na(dataTable$smaPeriod)]
    dataTable <- dataTable[(dataTable$upperBand  >= input$upperBand[1]  & dataTable$upperBand <= input$upperBand[2])   | is.na(dataTable$upperBand)]
    dataTable <- dataTable[(dataTable$lowerBand  >= input$lowerBand[1]  & dataTable$lowerBand <= input$lowerBand[2])   | is.na(dataTable$lowerBand)]
    dataTable <- dataTable[(dataTable$downChange >= input$downChange[1] & dataTable$downChange <= input$downChange[2]) | is.na(dataTable$downChange)]
    dataTable <- dataTable[(dataTable$upChange   >= input$upChange[1]   & dataTable$upChange <= input$upChange[2])     | is.na(dataTable$upChange)]
    dataTable <- dataTable[(dataTable$lowLimit   >= input$lowLimit[1]   & dataTable$lowLimit <= input$lowLimit[2])     | is.na(dataTable$lowLimit)]
    dataTable <- dataTable[(dataTable$stopGain   >= input$stopGain[1]   & dataTable$stopGain <= input$stopGain[2])     | is.na(dataTable$stopGain)]
    dataTable <- dataTable[(dataTable$stopLoss   >= input$stopLoss[1]   & dataTable$stopLoss <= input$stopLoss[2])     | is.na(dataTable$stopLoss)]

    if(!is.null(input$filterSymbol) && !is.null(intersect(input$filterSymbol, unique(dataTable$symbol))))
      dataTable <- dataTable[dataTable$symbol %in% input$filterSymbol]

    dataTable
  })

  output$values <- renderTable({
    nrow(tableValues())
  })

  output$parameters <- renderPlot({par(mfrow=c(2,4))
                                           showSmaPeriod(tableValues())
                                           showLowerBand(tableValues())
                                           showUpperBand(tableValues())
                                           showDownChange(tableValues())
                                           showUpChange(tableValues())
                                           showLowerLimit(tableValues())
                                           showStopGain(tableValues())
                                           showStopLoss(tableValues())})

  output$dataTable <- renderDataTable({showReport(tableValues())}, options = list(paging = FALSE))
})
