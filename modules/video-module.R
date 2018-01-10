library(shiny)

videoOutput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  htmlOutput(ns("video"))
}

video <- function(input, output, session, selectedVideo, sidebarBug = sidebarBug, height = NULL) {
  
  output$video <- renderText({
    # for temperarily solving sidebarbug
    sidebarBug()
    
    # video height
    vHeight <- height() * 0.6
    paste("<iframe src=\"https://www.youtube.com/embed/", selectedVideo()$YoutubeID, "\" width=\"100%\" height=\"", 400,"px\"", " frameborder=\"0\" allowfullscreen>", selectedVideo()$YoutubeID, "</iframe>", sep = "")
  })
}