library(shiny)

tableModuleOutput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dataTableOutput(ns("table"))
}

tableModule <- function(input, output, session, selectedVideo, sidebarBug = sidebarBug) {
  output$table <- renderDataTable({
    # for temperarily solving sidebarbug
    sidebarBug()
    youtubeLink <- paste("<a href=\"https://www.youtube.com/watch?v=", selectedVideo()$YoutubeID, "\" target=\"_blank\">", selectedVideo()$YoutubeID, "</a>", sep = "")
    tableData <- data.frame(c("YoutubeID",
                              "Title",
                              "Author",
                              "Category",
                              "Upload date",
                              "#views",
                              "#shares",
                              "#tweets",
                              "Endogenous response",
                              "Exogenous sensitivity"),
                            c(youtubeLink,
                              selectedVideo()$title,
                              selectedVideo()$channelTitle,
                              selectedVideo()$category,
                              selectedVideo()$uploadDate,
                              selectedVideo()$totalViewcount,
                              selectedVideo()$totalShare,
                              selectedVideo()$totalTweet,
                              round(selectedVideo()$endo, digits = 2),
                              round(selectedVideo()$mus, digits = 2)))
    names(tableData) <- c("Video property", "Property value")
    tableData
  }, options = list(paging = FALSE, searching = FALSE, ordering = FALSE), escape = FALSE)
}