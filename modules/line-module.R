library(shiny)
source("scripts/functions-predictive-power.R")

# there seems to be a wired bug, the hover event will be called twice and the latter one is empty event..
# So I have to use the following two values to deal with that bug.
lastID <<- ""

linePlotOutput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  plotlyOutput(ns("linePlot"))
}

linePlot <- function(input, output, session, selectedVideo, data, selectedData, sidebarBug = sidebarBug, changedSharePer = NULL) {
  
  output$linePlot <- renderPlotly({
    # for temperarily solving sidebarbug
    sidebarBug()
    if (selectedVideo()$YoutubeID != lastID) {
      lastID <<- selectedVideo()$YoutubeID
    }
    
    tmpData = selectedVideo()
    if (is.numeric(changedSharePer()) && changedSharePer() != 0) {
      share <- popularity_scale$perc_shares[[1]][as.numeric(cut(x = changedSharePer() + selectedVideo()$`Shares %`, breaks = c(0, unlist(popularity_scale$perc_val_shares)), include.lowest = T))]
      added_share <- share - sum(selectedVideo()$numShare[[1]][1:120])
      # split added_share and add to tmpData
      MAX <- 10000
      if (abs(added_share) < MAX) {
        for (i in sample(1:90, abs(added_share), replace = TRUE)) {
          tmpData$numShare[[1]][i] <- tmpData$numShare[[1]][i] + sign(added_share)
        }
      } else {
        tmpData$numShare[[1]][1:90] <- tmpData$numShare[[1]][1:90] + added_share / 90
      }
      # tmpData$numShare[[1]][1:90][tmpData$numShare[[1]][1:90] < 0] <- 0 # no negtive shares allowed
    }
    res <- predict.get_confidence_intervals(videoID = selectedVideo()$YoutubeID, data = tmpData, external_info_type = "shares", plot_graphic = T, print_parameters = F, plot_confidence_interval = F, plot_tunning = F, currentPosition = straghtLineX)
    # plotting
    data.to.plot <- data.frame(x = res$dates, real = res$real, test = res$test, fitted = res$fitted, infl = res$infl)
    right.y.axis <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "External influence (#shares)",
          range = list(min(data.to.plot$infl), 4 * max(data.to.plot$infl) - 3 * min(data.to.plot$infl)),
          rangemode = 'tozero'
         )
    p <- plot_ly(data.to.plot, x = ~x, y = ~real, name = 'Observed #views', type = 'scatter', mode = 'lines', line = list(dash = 'dot', width = 1)) %>%
      add_trace(x = ~x, y = ~test, name = 'Predicted viewcounts', type = 'scatter', mode = 'lines', line = list(dash = 'dashed', width = 1)) %>%
      add_trace(x = ~x, y = ~fitted, name = 'Fitted #views', type = 'scatter', mode = 'lines', line = list(dash = 'dashed', width = 1)) %>%
      add_trace(x = ~x, y = ~infl, name = 'Exogenous stimuli(#shares)', type = 'scatter', mode = 'lines', yaxis = 'y2', line = list(dash = 'dashed', width = 1)) %>%
      layout(yaxis2 = right.y.axis, title = sprintf("%s: Observed, fitted and predicted popularity", tmpData$YoutubeID),
             yaxis = list(title = '#views', rangemode = 'tozero'), xaxis = list(title = ''), legend = list(orientation = 'h', font = list(size = 10)),
             margin = list(r = 40, l = 50))
    new.viewcount <- res$viewcount
    if (is.numeric(changedSharePer()) && round(changedSharePer(), digits = 3) != 0) {
      new.view.per <- popularity_scale$perc_val_views[[1]][as.numeric(cut(x = new.viewcount, breaks = unlist(popularity_scale$perc_views), include.lowest = T))]
      modifiedSize$extra[selectedData$YoutubeID %in% selectedVideo()$YoutubeID] <<- new.view.per * 30 - selectedVideo()$totalViewcount_perc
    }
    p
  })
}