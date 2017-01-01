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

    dataTable <- dataTable[(dataTable$V1 >= input$V1[1] & dataTable$V1 <= input$V1[2]) | is.na(dataTable$V1)]
    dataTable <- dataTable[(dataTable$V2 >= input$V2[1] & dataTable$V2 <= input$V2[2]) | is.na(dataTable$V2)]
    dataTable <- dataTable[(dataTable$V3 >= input$V3[1] & dataTable$V3 <= input$V3[2]) | is.na(dataTable$V3)]
    dataTable <- dataTable[(dataTable$V4 >= input$V4[1] & dataTable$V4 <= input$V4[2]) | is.na(dataTable$V4)]
    dataTable <- dataTable[(dataTable$V5 >= input$V5[1] & dataTable$V5 <= input$V5[2]) | is.na(dataTable$V5)]
    dataTable <- dataTable[(dataTable$V6 >= input$V6[1] & dataTable$V6 <= input$V6[2]) | is.na(dataTable$V6)]
    dataTable <- dataTable[(dataTable$V7 >= input$V7[1] & dataTable$V7 <= input$V7[2]) | is.na(dataTable$V7)]
    dataTable <- dataTable[(dataTable$V8 >= input$V8[1] & dataTable$V8 <= input$V8[2]) | is.na(dataTable$V8)]

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
