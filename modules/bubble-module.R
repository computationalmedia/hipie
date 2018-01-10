library(shiny)
library(plotly)

bubbleOutput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(
    fluidRow(plotlyOutput(ns("bubblePlot"))),
    fluidRow(actionButton(ns("toggle.slider"), "Change Shares %")),
    fluidRow(column(10, uiOutput(ns("slider"))),
             column(2, textOutput(ns("new.view.per"))))
  )
}

bubble <- function(input, output, session, selectedData, artists = FALSE, sidebarBug = NULL, videosFound = NULL) {
  modifiedColor <- reactiveValues(extra = rep(0, length(selectedData$YoutubeID)))
  modifiedSize <<- reactiveValues(extra = rep(0, length(selectedData$YoutubeID)))
  showSlider <- reactiveValues(show = FALSE)
  # plotting
  output$bubblePlot <- renderPlotly({
    # for temperarily solving sidebarbug
    sidebarBug()
    # for highlighting searched videos
    if (!is.null(videosFound)) {
      if (nrow(selectedData) == nrow(videosFound)) {
        plotBubble <- plot_ly(data = selectedData, x = ~endo, y = ~mus, text = ~text, key = ~YoutubeID, type = 'scatter', mode = 'markers', name = '',
                              marker = list(size = ~totalViewcount_perc + 5, opacity = 0.6, color = 'black'), source = "source") %>% hide_colorbar()    
      } else {
        selectedData$`Shares %`[selectedData$YoutubeID %in% videosFound$YoutubeID] = 1
        selectedData$`Shares %`[!(selectedData$YoutubeID %in% videosFound$YoutubeID)] = 0
        selectedData <- selectedData[order(selectedData$`Shares %`), ]
        plotBubble <- plot_ly(data = selectedData, x = ~endo, y = ~mus, text = ~text, key = ~YoutubeID, type = 'scatter', mode = 'markers', color = ~`Shares %`, colors = c("gray","black"),
                              name = '', marker = list(size = ~totalViewcount_perc + 5, opacity = 0.6), source = "source") %>% hide_colorbar()
      }
    } else {
      if (artists) {
        plotBubble <- plot_ly(data = selectedData, x = ~endo, y = ~mus, text = ~text, key = ~YoutubeID, type = 'scatter', mode = 'markers', color = ~channelTitle,
                              name = '', marker = list(size = ~totalViewcount_perc + 5, opacity = 0.6), source = "source")
      } else {
        plotBubble <- plot_ly(data = selectedData, x = ~endo, y = ~mus, text = ~text, key = ~YoutubeID, type = 'scatter', mode = 'markers', color = ~`Shares %` + modifiedColor$extra, colors = 'Reds',
                              name = '', marker = list(size = ~totalViewcount_perc + 5 + modifiedSize$extra, opacity = 0.6), source = "source") %>% colorbar(title = 'Shares %')
      }
    }
    plotBubble %>%  layout(xaxis = list(title = "Endogenous response", showgrid = TRUE, type = "log"),
                           yaxis = list(title = "Exogenous sensitivity", showgrid = TRUE, type = "log"),
                           dragmode = 'zoom', plot_bgcolor = 'rgb(180,180,180,180)')
  })
  
  selectedOne <- reactive({
    e <- event_data("plotly_click", source = "source")
    showSlider$show <- FALSE
    modifiedColor$extra <- rep(0, length(selectedData$YoutubeID))
    modifiedSize$extra <<- rep(0, length(selectedData$YoutubeID))
    
    # for the wrong clicking bug
    if (!is.null(e) && is.null(e$key) && !is.null(e$x)) {
      return(selectedData[e$pointNumber + 1, ])
    }
    if (is.null(e) && !is.null(videosFound)) {
      return(videosFound[1, ])
    }
    if (is.null(e) || !(e$key %in% selectedData$YoutubeID)) {
      return(selectedData[1,])
    } else {
      return(selectedData[selectedData$YoutubeID == e$key,])
    }
  })
  
  output$slider <- renderUI({
    if (showSlider$show) {
      sliderInput(session$ns("slider.value"), label = "Share %", min = 0, max = 1, value = selectedOne()$`Shares %`, step = 0.025)
    }
  })
  output$new.view.per <- renderText({
    if (showSlider$show) {
      paste("New View Percentile: ", (selectedOne()$totalViewcount_perc + modifiedSize$extra[selectedData$YoutubeID %in% selectedOne()$YoutubeID]) / 30)
    }
  })
  
  observeEvent(input$toggle.slider, {
    showSlider$show <- !showSlider$show
    modifiedColor$extra <- rep(0, length(selectedData$YoutubeID))
    modifiedSize$extra <<- rep(0, length(selectedData$YoutubeID))
  })
  
  changedSharePer <- reactive({
    if (!showSlider$show) {
      return(0)
    }
    if (exists('selectedData') && !is.null(input$slider.value)) {
      tryCatch({
        diff <- input$slider.value - selectedOne()$`Shares %`
        modifiedColor$extra[selectedData$YoutubeID %in% selectedOne()$YoutubeID] <- diff
      },error = function(e) {
        browser()
      })
      return(diff)
    }
  })
  
  return(list(selectedOne, changedSharePer))
}