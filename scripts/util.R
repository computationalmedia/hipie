require(parallel)
source("scripts/functions-predictive-power.R")

dataFitting <- function(mydataset, updateProgress = NULL, id = NULL) {
  
  ## sanity check -- only get one video
  if (nrow(mydataset) != 1)
    stop("[dataFitting] mydataset object contains more than one video id! FIXME!")
  
  ## TODO: check that views and shares are at least 120days long, or adjust train and test accordingly.
  predict.train_regularizer(videoID = unlist(mydataset$YoutubeID), 
                            data = mydataset, 
                            parallel = T, 
                            external_info_type = "shares", 
                            points_to_hold_for_test = 30,
                            points_to_train = 90,
                            regularize = F,
                            dataset_name = "on-the-fly-fits")
  updateProgress(id, 60)

  ## gather results of fitting
  fitted.data <- predict.gather_regularization_results(data = mydataset, dataset_name = "on-the-fly-fits")
  mydataset <- cbind(mydataset, fitted.data)
  updateProgress(id, 70)
  
  ## compute endo reaction
  mydataset$endo <- lapply(X = mydataset$YoutubeID,
                           FUN = function(x) {
                             params <- mydataset[mydataset$YoutubeID == x,]$predict.shares[[1]]$fitted_params
                             res <- get_endogenous_response(params = params)
                             res$endo
                           })[[1]]
  ### done, stopping cluster
  updateProgress(id, 85)
  return(mydataset)
}

constructingDataFromCrawledData <- function(crawledData) {
  categoryMapping <- c("Film & Animation", "Cars & Vehicles", "Music", "Pets & Animals", "Sports", "Travel & Events",
                       "Gaming", "People & Blogs", "Comedy", "Entertainment", "News & Politics", "How-to & Style",
                       "Education", "Science & Technology", "Non-profits & Activism")
  names(categoryMapping) <- c("1", "2", "10", "15", "17", "19", "20", "22", "23", "24", "25", "26", "27", "28", "29")
  # constructing dataset
  YoutubeID <- crawledData$id
  description <- crawledData$snippet$description
  title <- crawledData$snippet$title
  channelId <- crawledData$snippet$channelId
  channelTitle <- crawledData$snippet$channelTitle
  category <- categoryMapping[crawledData$snippet$categoryId]
  names(category) <- NULL
  uploadDate <- crawledData$snippet$publishedAt
  duration <- crawledData$contentDetails$duration
  definition <- crawledData$contentDetails$definition
  dimension <- crawledData$contentDetails$dimension
  caption <- crawledData$contentDetails$caption
  totalShare <- as.numeric(crawledData$insights$totalShare)
  totalViewcount <- as.numeric(crawledData$insights$totalView)
  totalTweet <- NA
  numTweet <- NA
  res <- data.frame(YoutubeID = YoutubeID, description,
                    title, channelId, channelTitle, category, uploadDate, duration, definition, dimension,
                    caption, totalShare, totalViewcount, totalTweet, numTweet, stringsAsFactors = FALSE, check.rows = TRUE)
  res$numShare <- list(lapply(strsplit(crawledData$insights$dailyShare, ","), function (x) {as.numeric(x)})[[1]])
  res$numSubscriber <- list(lapply(strsplit(crawledData$insights$dailySubscriber, ","), function (x) {as.numeric(x)})[[1]])
  res$watchTime <- list(lapply(strsplit(crawledData$insights$dailySubscriber, ","), function (x) {as.numeric(x)})[[1]])
  res$dailyViewcount <- list(lapply(strsplit(crawledData$insights$dailyView, ","), function (x) {as.numeric(x)})[[1]])
  res$regionRestriction.blocked <- list(crawledData$contentDetails$regionRestriction$blocked)
  res$relevantTopicIds <- list(crawledData$topicDetails$relevantTopicIds)
  res
}

constructingFinalData <- function(data, popularity_scale) {
  age <- 120
  pars <- data.frame(YoutubeID = data$YoutubeID) #endo_int = data$endo_int,
  pars$promo_score <- data$endo ## Initially: - 1. Now changed.
  pars$promo_score[pars$promo_score < 1] <- 1
  pars$YoutubeID <- as.character(pars$YoutubeID)
  varname <- "predict.shares"
  pars$views <- unlist(lapply(X = data$dailyViewcount, FUN = function(x) sum(as.numeric(x)[1:age], na.rm = T) ))
  pars$shares <- unlist(lapply(X = data$numShare, FUN = function(x) sum(as.numeric(x)[1:age], na.rm = T) ))
  pars$num_tweets <- unlist(lapply(X = data$dailyTweets, FUN = function(x) sum(x[1:age], na.rm = T) ))
  pars$gammas <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$gamma[[1]]) else return(NA) ))
  pars$etas <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$eta[[1]]) else return(NA) ))
  pars$mus <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$mu1[[1]]) else return(NA) ))
  pars$Ks <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$K[[1]]) else return(NA) ))
  # pars$betas <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!any(is.na(x))) return(x$fitted_params$beta[[1]]) else return(NA) ))browser()
  pars$cs <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$c[[1]]) else return(NA) ))
  pars$thetas <- unlist(lapply(X = data[[varname]], FUN = function(x) if (!all(is.na(x))) return(x$fitted_params$theta[[1]]) else return(NA) ))
  pars$mu[pars$mus <= 0] <- .Machine$double.eps
  pars$promo_score <- pars$promo_score * pars$mus
  if ("promo_score" %in% names(data))
    data$promo_score <- NULL
  data <- merge(x = data, y = pars, by = "YoutubeID")
  # construct percentiles -- TODO: don't think this is correct!
  data$totalViewcount_perc <- unlist(lapply(X = data$views, FUN = function(x) {
    popularity_scale$perc_val_views[[1]][as.numeric(cut(x = x, breaks = unlist(popularity_scale$perc_views), include.lowest = T))]
  })) * 30
  data$`Shares %` <- unlist(lapply(X = data$shares, FUN = function(x) {
    popularity_scale$perc_val_shares[[1]][as.numeric(cut(x = x, breaks = unlist(popularity_scale$perc_shares), include.lowest = T))]
  }))
  data$text <- paste("YoutubeID:", data$YoutubeID, "<br>Author:", data$channelTitle, "<br>Title:", 
                     data$title, "<br>TotalViewcount percentile(size):", data$totalViewcount_perc / 30,
                     "<br>Shares percentile:", data$`Shares %`)
  data
}

startNewVideoWorker <- function(id, popularity_scale, getSemaphore, releaseSemaphore, datasetNo) {
  updateProgress <- function(id, percent) {
    getSemaphore()
    load('data/videos-progress.dat')
    if (id %in% videosProgress$YoutubeID) {
      videosProgress[videosProgress$YoutubeID == id, ]$progress <- percent
    } else {
      videosProgress[nrow(videosProgress) + 1, ] <- c(id, percent, datasetNo)
    }
    save(videosProgress, file = 'data/videos-progress.dat')
    releaseSemaphore()
  }
  
  # if video exists then return
  getSemaphore()
  load("data/new-videos.dat")
  releaseSemaphore()
  if (id %in% newVideoFinalData$YoutubeID) {
    updateProgress(id, 100)
    return()
  }
  
  # Reading crawled data
  crawledDataPath <- paste('./data/crawled/', id, '.json', sep = '')
  crawledData <- fromJSON(crawledDataPath)
  crawledData <- constructingDataFromCrawledData(crawledData)
  updateProgress(id, 30)
  # Training
  trainedData <- dataFitting(crawledData, updateProgress, id)
  finalData <- constructingFinalData(trainedData, popularity_scale)
  # # Setting up percentile
  getSemaphore()
  load("data/new-videos.dat")
  if (! finalData$YoutubeID %in% newVideoFinalData$YoutubeID) {
    newVideoFinalData <- rbind(newVideoFinalData, finalData)
  }
  save(newVideoFinalData, file = "data/new-videos.dat")
  releaseSemaphore()
  updateProgress(id, 100)
}

searchForVideos <- function(keyword, dataset) {
  res <- logical()
  for (i in 1:nrow(dataset)) {
    video <- dataset[i, ]
    if (keyword == video$YoutubeID) {
      res[i] <- TRUE
      next()
    }
    if ((!is.na(video$title)) && length(grep(keyword, video$title, ignore.case = TRUE))) {
      res[i] <- TRUE
      next()
    }
    if ((!is.na(video$channelTitle)) && length(grep(keyword, video$channelTitle, ignore.case = TRUE))) {
      res[i] <- TRUE
      next()
    }
    if ((!is.na(video$description)) && length(grep(keyword, video$description, ignore.case = TRUE))) {
      res[i] <- TRUE
      next()
    }
    if ((!is.na(video$category)) && length(grep(keyword, video$category, ignore.case = TRUE))) {
      res[i] <- TRUE
      next()
    }
    res[i] <- FALSE
  }
  return(dataset[res, ])
}