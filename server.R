library("shiny")
library("TraderBot")

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

  make_chart <- function(symbol, startDate, endDate) {
    symbol_data <- require_symbol(symbol, symbol_env)

    if(!is.null(symbol))
      chartSymbols(Symbols=symbol, startDate = startDate, endDate = endDate,
                   timeFrame = input$alertsTimeFrame)
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
          output[[plotname]] <- renderPlot({ make_chart(symbols[[my_i]], startDate, endDate) })
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
          output[[plotname]] <- renderPlot({ make_chart(my_symbol, startDate, endDate) })
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

  output$parameters     <- renderPlot({par(mfrow=c(2,4))
                                           showSmaPeriod()
                                           showLowerBand()
                                           showUpperBand()
                                           showDownChange()
                                           showUpChange()
                                           showLowerLimit()
                                           showStopGain()
                                           showStopLoss()})

  output$dataTable <- renderDataTable({showReport(mergeBacktest())}, options = list(paging = FALSE))
})
