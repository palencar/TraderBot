library("shiny")
library("TraderBot")
source("R/report.R")
source("R/chart.R")

shinyServer(function(input, output)
{
  make_chart <- function(symbol, startDate, endDate, timeFrame)
  {
    symbolTimeFrame <- unlist(strsplit(symbol, "[.]"))

    if(length(symbolTimeFrame) == 2)
    {
      timeFrame = symbolTimeFrame[2]
      #TODO what to do if timeFrame != symbolTimeFrame[2]?
    }

    if(timeFrame %in% c("weekly", "daily"))
    {
      symbol <- getSymbolsDaily(symbol, FALSE)
    }
    else
    {
      symbol <- getSymbolsIntraday(symbolTimeFrame[1], timeFrame)
    }

    if(!is.null(symbol))
      chartSymbols(Symbols=symbol, startDate = startDate, endDate = endDate, timeFrame = timeFrame)
  }

  observe({
    alerts <- getAlerts(input$numAlerts, input$alertsTimeFrame)
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

    dataTable <- dataTable[dataTable$state == "closed"]

    dataTable <- dataTable[(dataTable$smaPeriod  >= input$smaPeriod[1]  & dataTable$smaPeriod <= input$smaPeriod[2])   | is.na(dataTable$smaPeriod)]
    dataTable <- dataTable[(dataTable$upperBand  >= input$upperBand[1]  & dataTable$upperBand <= input$upperBand[2])   | is.na(dataTable$upperBand)]
    dataTable <- dataTable[(dataTable$lowerBand  >= input$lowerBand[1]  & dataTable$lowerBand <= input$lowerBand[2])   | is.na(dataTable$lowerBand)]
    dataTable <- dataTable[(dataTable$downChange >= input$downChange[1] & dataTable$downChange <= input$downChange[2]) | is.na(dataTable$downChange)]
    dataTable <- dataTable[(dataTable$upChange   >= input$upChange[1]   & dataTable$upChange <= input$upChange[2])     | is.na(dataTable$upChange)]
    dataTable <- dataTable[(dataTable$lowLimit   >= input$lowLimit[1]   & dataTable$lowLimit <= input$lowLimit[2])     | is.na(dataTable$lowLimit)]
    dataTable <- dataTable[(dataTable$stopGain   >= input$stopGain[1]   & dataTable$stopGain <= input$stopGain[2])     | is.na(dataTable$stopGain)]
    dataTable <- dataTable[(dataTable$stopLoss   >= input$stopLoss[1]   & dataTable$stopLoss <= input$stopLoss[2])     | is.na(dataTable$stopLoss)]
    dataTable <- dataTable[(dataTable$bullish    >= input$bullish[1]    & dataTable$bullish <= input$bullish[2])       | is.na(dataTable$bullish)]
    dataTable <- dataTable[(dataTable$bearish    >= input$bearish[1]    & dataTable$bearish <= input$bearish[2])       | is.na(dataTable$bearish)]
    dataTable <- dataTable[(dataTable$proffit_pp >= input$proffit[1]    & dataTable$proffit_pp <= input$proffit[2])    | is.na(dataTable$proffit_pp)]

    if(!is.null(input$filterSymbol) && !is.null(intersect(input$filterSymbol, unique(dataTable$symbol))))
      dataTable <- dataTable[dataTable$symbol %in% input$filterSymbol]

    dataTable
  })

  output$values <- renderTable({
    nrow(tableValues())
  })

  output$parameters <- renderPlot({par(mfrow=c(4,3))
                                   showPlot(tableValues(), c("smaPeriod", "proffit_pp"))
                                   showPlot(tableValues(), c("lowerBand", "proffit_pp"))
                                   showPlot(tableValues(), c("upperBand", "proffit_pp"))
                                   showPlot(tableValues(), c("downChange", "proffit_pp"))
                                   showPlot(tableValues(), c("upChange", "proffit_pp"))
                                   showPlot(tableValues(), c("lowLimit", "proffit_pp"))
                                   showPlot(tableValues(), c("stopGain", "proffit_pp"))
                                   showPlot(tableValues(), c("stopLoss", "proffit_pp"))
                                   showPlot(tableValues(), c("bullish", "proffit_pp"))
                                   showPlot(tableValues(), c("bearish", "proffit_pp"))
                                   #showPlot(tableValues(), c("smaPeriod", "mProffit"))
                                   #showPlot(tableValues(), c("lowerBand", "mProffit"))
                                   #showPlot(tableValues(), c("upperBand", "mProffit"))
                                   #showPlot(tableValues(), c("downChange", "mProffit"))
                                   #showPlot(tableValues(), c("upChange", "mProffit"))
                                   #showPlot(tableValues(), c("lowLimit", "mProffit"))
                                   #showPlot(tableValues(), c("stopGain", "mProffit"))
                                   #showPlot(tableValues(), c("stopLoss", "mProffit"))
                                   #showPlot(tableValues(), c("bullish", "mProffit"))
                                   #showPlot(tableValues(), c("bearish", "mProffit"))
                                 })

  output$dataTable <- renderDataTable({showReport(tableValues())}, options = list(paging = FALSE))
})
