source("R/report.R")
source("R/chart.R")

mMergeBacktest <- memoise(mergeBacktest, ~timeout(600))

ui <- shinyUI(navbarPage("TraderBot",
                   tabPanel("Backtest",
                            sidebarPanel(
                              headerPanel("Filters"),
                              selectizeInput("filterSymbol", "Symbols", choices = as.vector(unique(mMergeBacktest()$name)), multiple = TRUE),
                              sliderInput("smaPeriod",  "Sma Period:",  min =100, max =1000, value = c(300,500), step = 5),
                              sliderInput("upperBand",  "Upper Band:",  min = -2, max =   4, value = c(0.5,2.5), step= 0.01),
                              sliderInput("lowerBand",  "Lower Band:",  min = -4, max =   2, value = c(-2.5,-0.5), step= 0.01),
                              sliderInput("downChange", "Down Change:", min = -8, max =   0, value = c(-8,-4), step= 0.01),
                              sliderInput("upChange",   "Up Change:",   min =  0, max =   8, value = c(4,8), step= 0.01),
                              sliderInput("lowLimit",   "Low Limit:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("stopGain",   "Stop Gain:",   min =  1, max =   5, value = c(1,5), step= 0.01),
                              sliderInput("stopLoss",   "Stop Loss:",   min =  0, max =   1, value = c(0,1), step= 0.01),
                              sliderInput("bullBuy",    "Bull Buy:",    min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bullSell",   "Bull Sell:",   min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bearBuy",    "Bear Buy:",    min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("bearSell",   "Bear Sell:",   min =  0, max =   1, value = c(0.0,1.0), step= 0.01),
                              sliderInput("proffit",    "Proffit:",     min = -1, max =   5, value = c(-1,5), step= 0.01)
                            ),
                            mainPanel(
                              tableOutput("values"),

                              plotOutput("parameters", height = "1600px"),
                              dataTableOutput("dataTable"))
                   ),

                   tabPanel("Alerts",
                            sidebarPanel(
                              headerPanel("Options"),

                              numericInput("numAlerts", "Number of alerts to view:", 5, min = 0, max = 30),

                              selectInput(inputId = "alertsTimeFrame",
                                          label = "Time frame",
                                          choices = c("1D" = "daily",
                                                      "1W" = "weekly",
                                                      "1M" = "1M",
                                                      "5M" = "5M",
                                                      "10M" = "10M",
                                                      "15M" = "15M",
                                                      "30M" = "30M",
                                                      "1H" = "1H"),
                                          selected = "1D"),

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
                                          choices = c("1D" = "daily",
                                                      "1W" = "weekly",
                                                      "1M" = "1M",
                                                      "5M" = "5M",
                                                      "10M" = "10M",
                                                      "15M" = "15M",
                                                      "30M" = "30M",
                                                      "1H" = "1H"),
                                          selected = "1D"),

                              dateRangeInput(inputId = "chartsDateRange", label = "Date range",
                                             start = Sys.Date() - 730, end = Sys.Date())
                            ),

                            mainPanel(
                              uiOutput("charts")
                            )
                   )
))

server <- shinyServer(function(input, output)
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
    dataTable <- mMergeBacktest(path = "result/")

    dataTable <- dataTable[dataTable$state == "closed"]

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
    showPlot(tableValues(), c("bullBuy", "proffit_pp"))
    showPlot(tableValues(), c("bullSell", "proffit_pp"))
    showPlot(tableValues(), c("bearSell", "proffit_pp"))
    showPlot(tableValues(), c("bearBuy", "proffit_pp"))
  })

  output$dataTable <- renderDataTable({showReport(tableValues())}, options = list(paging = FALSE))
})

#' @export
runShinyApp <- function(options = list(host="127.0.0.1", port=8000))
{
  shinyApp(ui = ui, server = server, options = options)
}
