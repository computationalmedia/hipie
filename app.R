library(shiny)
library(jsonlite)
library(plotly)
library(shinydashboard)
library(shinyjs)

jsCode <- readChar('js/main.js', file.info('js/main.js')$size)

# js code for shinyjs functions
shinyjsCode <- readChar('js/shinyjs.js', file.info('js/shinyjs.js')$size)

# bootstrap modal
modal <- tags$div(class="modal fade", id="youtubeid", tabindex="-1", role="dialog", `aria-labelledby`="myModalLabel",
                  tags$div(class="modal-dialog", role="document",
                           tags$div(class="modal-content",
                                    tags$div(class="modal-header",
                                             tags$button(type="button", class="close", `data-dismiss`="modal", `aria-label`="Close"),
                                             tags$h4(class="modal-title", id="myModalLabel", "Progress window")
                                    ),
                                    tags$div(class="modal-body",
                                             tags$div(class = "input-group",
                                                      tags$input(id = "idinput", type="text", class="form-control", placeholder="Enter YouTube ID to estimate its HIP model"),
                                                      tags$span(class="input-group-btn", tags$button(id = "youtubeidclick", type="button", class="btn btn-primary",
                                                                                                     `data-loading-text`="<i class='fa fa-spinner fa-spin '></i> Processing",
                                                                                                     "Add"))
                                             ),
                                             tags$div(id = "progressContainer", class="container-fluid")                                             ),
                                    tags$div(class="modal-footer",
                                             tags$button(type="button", id = "closeButton", class="btn btn-default", `data-dismiss`="modal", "Close"))
                           )
                  )
)

############################## start of the main program ##############################
# loading libraries and datasets
## load all the data that we need
load('data/init.dat', envir = .GlobalEnv)
source("scripts/functions-predictive-power.R")
source("modules/page-module.R")
source("scripts/util.R")
load("data/new-videos.dat")
load("data/datasets.dat")
## clear absent videos in datasets
if (length(datasets) > 5) {
  for (i in 6:length(datasets)) {
    if (length(datasets[i][[1]]) > 0) {
      for (idNo in 1:length(datasets[i][[1]])) {
        if (!(datasets[i][[1]][idNo] %in% newVideoFinalData$YoutubeID)) {
          datasets[i][[1]] <- datasets[i][[1]][-idNo]
          save(datasets, file = "data/datasets.dat")
        }
      }
    }
  }
}

## create progress files if absent
if (!file.exists('data/videos-progress.dat')) {
  videosProgress <- data.frame(matrix(data = NA, nrow = 0, ncol = 3))
  names(videosProgress) <- c("YoutubeID", "progress", "whichDataset")
  save(videosProgress, file = 'data/videos-progress.dat')
}

startTime <- file.info("data/new-videos.dat")$mtime
startTimeProgress <- file.info('data/videos-progress.dat')$mtime

## create the folder data/crawled
if (!file.exists("data/crawled")){
  dir.create("data/crawled")
}

artists_1 <- c("JustinBieberVEVO", "RihannaVEVO", "KatyPerryVEVO", "TaylorSwiftVEVO",
               "EminemVEVO", "shakiraVEVO", "David Guetta", "EnriqueIglesiasVEVO", "OneDirectionVEVO", "Maroon5VEVO")
artists_2 <- c("SamSmithWorldVEVO", "AerosmithVEVO", "DisneyMusicVEVO", "kanygarciaVEVO",
               "Maroon5VEVO", "TiestoVEVO", "5SOSVEVO", "michaeljacksonVEVO", "PorterRobinsonVEVO", "RickyMartinVEVO")
artists <- artists_2

############################ for parallel ###########################
workerNo <- trunc(detectCores() / 8)  + 1
nodes <- list()
for (i in 1:workerNo) {
  nodes[[i]] <- makePSOCKcluster("localhost")
}
nodesInUse <- rep(NA, workerNo)
waitingQueue <- character()
waitingDatasetQueue <- numeric()
tmpTime <- round(as.numeric(Sys.time()))
getSemaphore <- function() {
  system(paste('lockfile -1 /tmp/lock', '-', tmpTime, sep = ''))
}
releaseSemaphore <- function() {
  system(paste('rm -f /tmp/lock', '-', tmpTime, sep = ''))
}
# check if command exists
if (system('command -v lockfile >/dev/null 2>&1 || { echo >&2 "Lockfile is required but it\'s not installed.  On ubuntu use \'sudo apt install procmail\'. Aborting."; exit 1; }')) {
  quit(status = 1)
}
############################## ui part ##############################
ui <- dashboardPage(title = 'HIP-DEMO',
  dashboardHeader(title = "YouTube"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$style(HTML('.box.box-solid.box-primary>.box-header {color: #fff;background: #222d32;}
                    .box.box-solid.box-primary {border: 1px solid #222d32;}
                    .box.box-solid.box-warning>.box-header {color: #fff;background: gray;}
                    .box.box-solid.box-warning {border: 1px solid gray}')),
    HTML(jsCode),
    useShinyjs(),
    extendShinyjs(text = shinyjsCode, functions = c("progress", "emptyVideoDataSetCover",
                                                    "removeCover", "completeOneVideo", "showWarning",
                                                    "createProgressElement", "tablistener", "removeOneVideo")),
    modal,
    uiOutput("content")
  ),
  skin = "black"
)

############################## server part ##############################
server <- function(input, output, session) {
  # Menu output
  output$menu <- renderMenu({
    sidebarMenu(.list = {
      # items in left sidebar
      menuLists <- list()
      i <- 0
      for (dataset in names(datasets)) {
        menuLists <- list(menuLists, menuItem(dataset, tabName = paste(i, sep = "")))
        i <- i + 1
      }
      menuLists
    })
  })
  
  # inputs from JS
  sidebarBug <- reactive({
    input$resize
  })
  windowHeight <- reactive({
    input$height
  })
  
  # loading up datasets
  constructSelectedData <- function(selectedData, selectedPercentiles) {
    # Get percentiles
    selectedData$totalViewcount_perc <- selectedPercentiles$views * 30
    selectedData$`Shares %` <- selectedPercentiles$shares
    
    selectedData$text <- paste("YoutubeID:", selectedData$YoutubeID, "<br>Author:", selectedData$channelTitle,
                               "<br>Title:", selectedData$title, "<br>TotalViewcount percentile(size):",
                               selectedData$totalViewcount_perc / 30, "<br>Shares percentile:",
                               selectedData$`Shares %`)
    selectedData
  }
  
  loadingFromJSON <- function(datasetNo) {
    load('data/datasets.dat')
    selectedYoutubeID <- array(datasets[datasetNo])[[1]]
    if (datasetNo <= 5) {
      selectedData <- data[data$YoutubeID %in% selectedYoutubeID, ]
      selectedPercentiles <- percentiles[percentiles$YoutubeID %in% selectedYoutubeID, ]
      return(constructSelectedData(selectedData, selectedPercentiles))
    } else {
      getSemaphore()
      load("data/new-videos.dat")
      releaseSemaphore()
      selectedData <- newVideoFinalData[newVideoFinalData$YoutubeID %in% selectedYoutubeID, ]
      return(selectedData)
    }
  }
  
  # updating ui
  updateUI <- function(videosFound = NULL) {
    output$content <- renderUI({
      if (input$chosentab == "4") {
        return(normalPageUI(input$chosentab, artists = TRUE))
      } else {
        return(normalPageUI(input$chosentab))
      }
    })
    if (input$chosentab == "4") {
      callModule(normalPage, id = input$chosentab, selectedData = loadingFromJSON(as.numeric(input$chosentab) + 1),
                 data = data, height = windowHeight, sidebarBug = sidebarBug, artists = TRUE, videosFound = videosFound)
    } else if (as.numeric(input$chosentab) + 1 <= 5) {
      callModule(normalPage, id = input$chosentab, selectedData = loadingFromJSON(as.numeric(input$chosentab) + 1),
                 data = data, height = windowHeight, sidebarBug = sidebarBug, videosFound = videosFound)
    } else {
      # if dataset is empty
      if (length(datasets[as.numeric(input$chosentab) + 1][[1]]) == 0) {
        js$emptyVideoDataSetCover()
        return()
      }
      getSemaphore()
      load('data/new-videos.dat')
      releaseSemaphore()
      callModule(normalPage, id = input$chosentab, selectedData = loadingFromJSON(as.numeric(input$chosentab) + 1),
                 data = newVideoFinalData, height = windowHeight, sidebarBug = sidebarBug, videosFound = videosFound)
    }
  }
  # observing tab changing events
  observeEvent(input$chosentab, {
    updateUI()
  })
  
  # observing on add video button created event
  observeEvent(input$onAddVideoButtonCreated, {
    getSemaphore()
    load('data/videos-progress.dat')
    releaseSemaphore()
    if (nrow(videosProgress) > 0) {
      startTimeProgress <<- 0
    }
    
    for (i in videosProgress$YoutubeID) {
      crawledDataPath <- paste('./data/crawled/', i, '.json', sep = '')
      crawledData <- fromJSON(crawledDataPath)
      js$createProgressElement(i, crawledData$snippet$title, crawledData$snippet$channelTitle, crawledData$snippet$description)
    }
  })
  
  ################# progress updater and video updater ##################
  progressUpdate <- reactiveTimer(2000)
  observe({
    progressUpdate()
    if (file.info('data/videos-progress.dat')$mtime > startTimeProgress) {
      startTimeProgress <<- file.info('data/videos-progress.dat')$mtime
      getSemaphore()
      load('data/videos-progress.dat')
      releaseSemaphore()
      for (i in videosProgress$YoutubeID) {
        crawledDataPath <- paste('./data/crawled/', i, '.json', sep = '')
        crawledData <- fromJSON(crawledDataPath)
        if (videosProgress[videosProgress$YoutubeID == i,]$progress == 100) {
          print(videosProgress)
          js$progress(videosProgress[videosProgress$YoutubeID == i,]$YoutubeID, 100)
          
          # Add new video into corresponding dataset
          no <- as.numeric(videosProgress[videosProgress$YoutubeID == i, ]$whichDataset)
          datasets[no][[1]][length(datasets[no][[1]]) + 1] <<- i
          save(datasets, file = 'data/datasets.dat')
          print(paste('adding to dataset ', no))
          
          print("Removing one from videosProgress")
          videosProgress <- videosProgress[!(videosProgress$YoutubeID == i), ]
          save(videosProgress, file = 'data/videos-progress.dat')
          js$completeOneVideo(i)
          
          print("test here in changing")
          startTime <<- file.info('data/new-videos.dat')$mtime
          
          nodesInUse[nodesInUse %in% i] <<- NA
          if (as.numeric(input$chosentab) + 1 == no && length(datasets[as.numeric(input$chosentab) + 1][[1]]) > 0) {
            js$removeCover()
            getSemaphore()
            load('data/new-videos.dat')
            releaseSemaphore()
            callModule(normalPage, id = input$chosentab, selectedData = loadingFromJSON(as.numeric(input$chosentab) + 1), data = newVideoFinalData,
                       height = windowHeight, sidebarBug = sidebarBug, selectedVideo = i)
          }
        } else {
          js$progress(videosProgress[videosProgress$YoutubeID == i,]$YoutubeID, videosProgress[videosProgress$YoutubeID == i,]$progress)
        }
      }
    }

    # Checking status, start new worker
    if (NA %in% nodesInUse && length(waitingQueue) > 0) {
      index <- match(TRUE, nodesInUse %in% NA)
      id <- waitingQueue[1]
      no <- waitingDatasetQueue[1]
      waitingQueue <<- waitingQueue[-1]
      waitingDatasetQueue <<- waitingDatasetQueue[-1]
      clusterExport(nodes[[index]], varlist = c("fromJSON", "constructingDataFromCrawledData", "dataFitting",
                                    "constructingFinalData", "makeCluster", "clusterExport", "parLapplyLB", 
                                    "stopCluster", "predict.gather_regularization_results", "error_function_gradient", 
                                    "error_function", "generate_simulated_data", "predict_theoretical_lambda", "grad_lambda", 
                                    "predict.get_folder", "predict.get_file_names", "predict.train_regularizer", 
                                    ".check_fix_mus_ext_infl", ".predict.construct_external_info", 
                                    ".predict.fit_videoinfo.construct_work_params", ".correct_names", "fit_series", 
                                    "detectCores", "get_endogenous_response", ".get_n", "get_n"))
      parallel:::sendCall(nodes[[index]][[1]], fun = startNewVideoWorker, args = list(id, popularity_scale, getSemaphore, releaseSemaphore, no))
      nodesInUse[index] <<- id
    }
  })
  
  autoUpdate <- reactiveTimer(10000)
  observe({
    autoUpdate()
    if (file.info('data/new-videos.dat')$mtime > startTime) {
      
      print("test here in mtime")
      startTime <<- file.info('data/new-videos.dat')$mtime
      getSemaphore()
      load("data/new-videos.dat")
      releaseSemaphore()
      if (length(datasets[as.numeric(input$chosentab) + 1][[1]]) > 0) {
        js$removeCover()
        callModule(normalPage, id = input$chosentab, selectedData = loadingFromJSON(as.numeric(input$chosentab) + 1), data = newVideoFinalData,
                   height = windowHeight, sidebarBug = sidebarBug)
      }
    }
  })
  
  ####################### video related event ##########################
  
  # For generating on the fly, observe the input event of youtube id
  observeEvent(input$inputID, {
    # determine which dataset to add
    if (as.numeric(input$chosentab) + 1 < 5) {
      js$showWarning("Sorry, you can't add data into default dataset.")
      return()
    }
    # Checking the input id
    id <- substr(input$inputID, 1, nchar(input$inputID) - 4)
    print(id)
    if (startsWith(id, "https")) {
      id <- sub(".*?v=([^&]+).*", "\\1", id)
    }
    if (!length(grep('^[0-9|\\_|a-z|A-Z|\\-]*$',id))) {
      js$showWarning("Please enter a valid Youtube id.")
      return()
    }
    # PYTHONEXEC <- "python"
    PYTHONEXEC <- "/usr/bin/python"
    # Applying crawler
    command <- paste(PYTHONEXEC, ' scripts/youtube_crawler.py -i=\'', id, '\' --output=\'./data/crawled/', id, '.json\'', sep = '')
    res <- system(command)
    if (res) {
      js$showWarning("There is an error when crawling the data.")
      return()
    }
    crawledDataPath <- paste('./data/crawled/', id, '.json', sep = '')
    crawledData <- fromJSON(crawledDataPath)
    if (is.null(crawledData$insights)) {
      js$showWarning("Popularity history data for this video is not available, please choose another video.")
      return()
    }
    if (length(strsplit(crawledData$insights$dailyShare, ",")[[1]]) < 120) {
      js$showWarning("The history of this video is shorter than 120 days, please choose another video for a more reliable estimate.")
      return()
    }
    if (id %in% datasets[as.numeric(input$chosentab) + 1][[1]] && id %in% newVideoFinalData$YoutubeID) {
      js$showWarning("This video is already in this dataset.")
      return()
    }
    js$createProgressElement(id, crawledData$snippet$title, crawledData$snippet$channelTitle, crawledData$snippet$description)
    
    # Add new video into the waiting queue
    waitingQueue[length(waitingQueue) + 1] <<- id
    waitingDatasetQueue[length(waitingDatasetQueue) + 1] <<- as.numeric(input$chosentab) + 1
  })
  
  # remove current video event
  observeEvent(input$removeCurrentVideo, {
    tmp <- datasets[as.numeric(input$chosentab) + 1][[1]]
    datasets[as.numeric(input$chosentab) + 1][[1]] <<- tmp[!(tmp %in% input$removeCurrentVideo)]
    save(datasets, file ='data/datasets.dat')
    updateUI()
  })
  
  # stop training video event
  observeEvent(input$stopTrainingVideo, {
    js$removeOneVideo(input$stopTrainingVideo)
    
    if (input$stopTrainingVideo %in% waitingQueue) {
      waitingQueue <<- waitingQueue[!(waitingQueue %in% input$stopTrainingVideo)]
      waitingDatasetQueue <<- waitingDatasetQueue[!(waitingQueue %in% input$stopTrainingVideo)]
    } else if (input$stopTrainingVideo %in% nodesInUse) {
      index <- match(TRUE, nodesInUse %in% input$stopTrainingVideo)
      cl <- nodes[[index]]
      stopCluster(cl)
      nodes[[index]] <<- makePSOCKcluster("localhost")
      nodesInUse[index] <<- NA
      
      # remove progress
      getSemaphore()
      load('data/videos-progress.dat')
      videosProgress <- videosProgress[!(videosProgress$YoutubeID %in% input$stopTrainingVideo), ]
      save(videosProgress, file = 'data/videos-progress.dat')
      releaseSemaphore()
    }
  })
  
  # observing video search event
  observeEvent(input$search, {
    keyword <- substr(input$search, 1, nchar(input$search) - 4)
    dataset <- loadingFromJSON(as.numeric(input$chosentab) + 1)
    if (nrow(dataset) == 0) {
      return()
    }
    videosFound <- searchForVideos(keyword, dataset)
    if (nrow(videosFound) == 0) {
      js$showWarning('No videos found! Please try another keyword.')
      updateUI()
      return()
    }
    updateUI(videosFound = videosFound)
  })
  
  # observing video search cancel event
  observeEvent(input$cancelSearch, {
    updateUI()
  })
  
  ######## Dataset mofication groups ##########
  
  # observing adding new dataset
  observeEvent(input$newDatasetName, {
    datasets$tmp <<- character(length = 0)
    names(datasets)[length(datasets)] <<- input$newDatasetName
    save(datasets, file = 'data/datasets.dat')
    output$menu <- renderMenu({
      sidebarMenu(.list = {
        # items in left sidebar
        menuLists <- list()
        i <- 0
        for (dataset in names(datasets)) {
          menuLists <- list(menuLists, menuItem(dataset, tabName = paste(i, sep = "")))
          i <- i + 1
        }
        menuLists
      })
    })
  })
  
  # observing changing dataset name
  observeEvent(input$changeDatasetName, {
    names(datasets)[as.numeric(input$chosentab) + 1] <<- input$changeDatasetName
    save(datasets, file = 'data/datasets.dat')
  })
  
  # observing deleting dataset
  observeEvent(input$deleteDataset, {
    datasets <<- datasets[-(as.numeric(input$deleteDataset) + 1)]
    save(datasets, file = 'data/datasets.dat')
  })
}

shinyApp(ui = ui, server = server)
