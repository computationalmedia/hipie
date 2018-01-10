library(shiny)
library(shinydashboard)
source('modules/bubble-module.R')
source('modules/line-module.R')
source('modules/table-module.R')
source('modules/video-module.R')

normalPageUI <- function(id, artists = FALSE) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # determine the first plot
  if (artists) {
    bubbleP <- bubbleOutput(ns("bubblePlotForVEVO"))
    titleForBubble <- "The endo-exo map for VEVOs"
  } else {
    bubbleP <- bubbleOutput(ns("bubblePlotForMain"))
    titleForBubble <- "The endo-exo map"
  }
  
  fluidRow(
    column(width = 6,
           box(width = NULL, title = titleForBubble, status = "primary", solidHeader = TRUE,
               bubbleP),
           box(width = NULL, title = "Video", status = "primary", solidHeader = TRUE,
               videoOutput(ns("videoIframe")))
    ),
    column(width = 6,
           box(width = NULL,  status = "warning", title = "Popularity series plot", solidHeader = TRUE,
               column(width = 12, linePlotOutput(ns("linePlot")))),
           box(width = NULL, status = "warning", solidHeader = TRUE,
               title = "Information about this video", tableModuleOutput(ns("table"))))
  )
}

normalPage <- function(input, output, session, selectedData, data, artists = FALSE,
                       sidebarBug = NULL, height = NULL, selectedVideo = NULL, videosFound = NULL) {
  if (artists) {
    reactiveComponent <- callModule(bubble, id = "bubblePlotForVEVO", selectedData = selectedData,
                             artists = TRUE, sidebarBug = sidebarBug, videosFound = videosFound)
    clickVideo <- reactiveComponent[[1]]
    changedSharePer <- reactiveComponent[[2]]
  } else {
    reactiveComponent <- callModule(bubble, id = "bubblePlotForMain", selectedData = selectedData,
                             artists = FALSE, sidebarBug = sidebarBug, videosFound = videosFound)
    clickVideo <- reactiveComponent[[1]]
    changedSharePer <- reactiveComponent[[2]]
  }

  # check if the tab has been chenged or not
  checkVideo <- reactive({
    if (clickVideo()$YoutubeID == selectedData[1, ]$YoutubeID && !is.null(selectedVideo) && selectedVideo %in% selectedData$YoutubeID) {
      returnData <- selectedData[selectedVideo == selectedData$YoutubeID, ]
      selectedVideo <<- NULL
      print('showing new trained video')
      return(returnData)
    }
    if (!clickVideo()$YoutubeID %in% selectedData$YoutubeID) {
      return(selectedData[1,])
    } else {
      clickVideo()
    }
  })
  
  callModule(video, id = "videoIframe", selectedVideo = checkVideo, sidebarBug = sidebarBug, height = height)
  callModule(linePlot, id = "linePlot", selectedVideo = checkVideo, data = data, selectedData = selectedData, sidebarBug = sidebarBug, changedSharePer = changedSharePer)
  callModule(tableModule, id = "table", selectedVideo = checkVideo, sidebarBug = sidebarBug)
}