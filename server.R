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

  make_chart <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env)

    if(!is.null(symbol))
      chartSymbols(Symbols=symbol, startDate = input$daterange[1], endDate = input$daterange[2],
                   timeFrame = input$time_frame)
  }

  observe({
    alerts <- getAlerts(input$numAlerts)
    symbols <- unique(as.vector(alerts$symbol))
    numCharts <- min(length(symbols), input$numAlerts)

    for (i in 1:numCharts) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        output[[plotname]] <- renderPlot({ make_chart(symbols[[my_i]]) })
      })
    }
  })

  output$charts <- renderUI({
    plot_output_list <- lapply(1:input$numAlerts, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname)
    })

    do.call(tagList, plot_output_list)
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
