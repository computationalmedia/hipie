# import necesarry function
# the next importation is from another project, to to work with the Youtube segmented data (second version version)
source('scripts/functions-fitting-data.R')
# source('scripts/functions-fit-and-plot-youtube-data.R')

.predict.construct_external_info <- function(external_info_type = "", videoID = NULL, data = NULL) {
  types <- c("none", "shares", "tweets", "sharesNtweets", "neighboorhoodTweets", "neighboorhoodShares", "neighboorhoodViews" )
  correct = (external_info_type %in% types)
  if ( (external_info_type %in% c("neighboorhoodTweets", "neighboorhoodShares", "neighboorhoodViews", "sharesNtweets")) & is.null(videoID)) {
    warning(sprintf("You asked for '%s' external info, but no videoID supplied!", external_info_type))
    correct <- F
  }
  
  ext_infl <- NULL
  if (correct & !is.null(data)) {
    vc <- unlist(data[data$YoutubeID == videoID,]$dailyViewcount)
    sc <- unlist(data[data$YoutubeID == videoID,]$numShare)
    tc <- unlist(data[data$YoutubeID == videoID,]$dailyTweets)
    
    
    # construct the external influence
    if (external_info_type == "none")
      ext_infl <- list(rep(0, times = length(vc)))
    if (external_info_type == "shares") 
      ext_infl <- list(shares = sc)
    if (external_info_type == "tweets") 
      ext_infl <- list(shares = tc)
    if (external_info_type == "sharesNtweets") 
      ext_infl <- list(shares = sc, tweets = tc)
    if (external_info_type == "neighboorhoodTweets") 
      ext_infl <- sapply(X = data[data$YoutubeID == videoID,]$neighbourhood, FUN = function(x) return( data[x,]$dailyTweets))
    if (external_info_type == "neighboorhoodShares") 
      ext_infl <- sapply(X = data[data$YoutubeID == videoID,]$neighbourhood, FUN = function(x) return( data[x,]$numShare))
    if (external_info_type == "neighboorhoodViews") 
      ext_infl <- sapply(X = data[data$YoutubeID == videoID,]$neighbourhood, FUN = function(x) return( data[x,]$dailyViewcount))
  }
  
  return(list(correct = correct, types = types, ext_infl = ext_infl ))
}

predict.get_file_names <- function(folder, videoID) {
  return( list(pdf = sprintf("%s/fitted-viewcounts-%s.pdf", folder, videoID), 
               csv = sprintf("%s/fitted-viewcounts-%s.csv", folder, videoID),
               exoendo = sprintf("%s/exo-endo-%s.pdf", folder, videoID)) )
  
}

predict.get_folder <- function(external_info_type = "sharesNtweets", disable_relaxation_kernel = F, dataset_name = "") {
  
  if ( ! (external_info_type %in% .predict.construct_external_info()$types))
    stop(sprintf("Error: External information type '%s' is not known.", external_info_type))
  
  # compute the names of folder in which to save
  stext <- external_info_type
  if (nchar(dataset_name) > 0) {
    # check if it ends in "/"
    if (substr(dataset_name, start=nchar(dataset_name), stop = nchar(dataset_name)) != "/")
      dataset_name <- paste(dataset_name, "/", sep = "")
  }
  folder <- sprintf("predictive-power/%sfitting-predictive", dataset_name) #1-subset-680videos/
  plot_folder <- sprintf("%s-plots", folder)
  folder <- sprintf("%s-%s", folder, stext)
  reg_training <- sprintf("regularizer-training/%sfitting-predictive-%s", dataset_name, stext)
  if (disable_relaxation_kernel)
    folder <- sprintf("%s-no-relaxation", folder)
  
  return(list(folder = folder, 
              stext = stext, 
              plot_folder = plot_folder,
              reg_training = reg_training))
}

.predict.fit_videoinfo.construct_work_params <- function(initial_params) {
  params <- initial_params
  rownames(params) <- NULL
  #   params <- rbind(params, c(gamma = 10, eta = 0.1, K = 0.0024, beta = -0.146, c = -0.73, theta = 3.35))
  #   params <- rbind(params, c(gamma = 0.1, eta = 0.1, K = 0.1, beta = 0.1, c = 0.1, theta = 0.1))
  #   params <- rbind(params, c(gamma = 100, eta = -2, K = 3, beta = 2, c = 5, theta = 10))
  # params <- rbind(params, c(gamma = 100, eta = 10, K = 0.0024, beta = 0.1, c = -0.73, theta = 3.35, mu1 = 1))
  params <- rbind(params, c(gamma = 100, eta = 10, K = 0.0024, beta = 0.1, c = 0.0001, theta = 3.35, mu1 = 1))
  #   params <- rbind(params, c(gamma = 0.1, eta = 0.1, K = 0.1, beta = 0.1, c = 0.1, theta = 0.1, mu1 = 0.1 ))
  # params <- rbind(params, c(gamma = 10000, eta = 1000, K = 50, beta = 0.1, c = 1.5, theta = -2, mu1 = 5 ))
  params <- rbind(params, c(gamma = 10000, eta = 1000, K = 50, beta = 0.1, c = 1.5, theta = 0.00001, mu1 = 5 ))
  # add 5 more random choices from the definition interval
  more_parms <- matrix(data = NA, nrow = 5, ncol = ncol(params))
  maxValue <- .Machine$double.xmax - 1
  lowerBound = c(gamma = 0, eta = 0, K = 0, beta = 0.1, c = .Machine$double.eps, theta = 2.3, mu1 = 0) #before -200
  upperBound = c(gamma = 9947, eta = 289.2, K = 52.9, beta = 0.1, c = 4, theta = 67.7, mu1 = 505.90)
  colnames(more_parms) <- names(lowerBound)
  for (varname in names(lowerBound))
    more_parms[,varname] <- runif(n = nrow(more_parms), min = lowerBound[varname], max = upperBound[varname])
  more_parms[is.infinite(more_parms)] <- 1
  params <- rbind(params, more_parms)
  rownames(params) <- NULL
  
  return(params)
}

#' @param external_info_type (default "shares"): can be "shares", "tweets" or 
#'   "none"
#' @param force_recompute (default F): if the CSV file containing the fitting is
#'   found in the output folder, should we force recompute it? if not, just load
#'   it, regenerate graphic and return fitting
#' @param points_to_train (default NULL) on how many points to train? If NULL, 
#'   then train on all available point, except the last points_to_hold_for_test.
#'   Note that if points_to_train + points_to_hold_for_test > number of points, 
#'   then points_to_train is adjusted accordingly with a warning
#' @param points_to_hold_for_test (default 30) on how many points to test?
#' @param points_to_train (default 90) how many points to be used for training. 
#'   If not enough points exist, this parameter will be adjusted with warning.
#' @param regularize (default T) -  wheather we should regularize the error 
#'   function to avoid overfitting
#' @param regularizer_val (default null) the value for the alpha regularizer. If
#'   NULL, we use linear search for best value, but it will be lengthy. This
#'   parameter can be an array and each value will be tested and searched.
#' @param points_to_holdout (default 15) the number of points in the holdout
#'   tune the regularizer
predict.train_regularizer <- function(videoID, data, initial_params = c(gamma = 10, eta = 100, K = 3, beta = 0.1, c = 4.5, theta = 10, mu1 = 0.2), plot_fitting = F,
                                      save_results_to_file = T, parallel = T, external_info_type = "sharesNtweets", force_recompute = F, disable_relaxation_kernel = F,
                                      points_to_hold_for_test = 30, points_to_train = 90, dataset_name = NULL, regularize = T, regularizer_val = NULL, points_to_holdout = 15) {
  
  if ( ! (external_info_type %in% .predict.construct_external_info()$types))
    stop(sprintf("External error type '%s' is not known.", external_info_type))
  
  folder <- predict.get_folder(external_info_type = external_info_type, 
                               disable_relaxation_kernel = disable_relaxation_kernel, 
                               dataset_name = dataset_name)
  filenames <- predict.get_file_names(folder = folder$reg_training, videoID = videoID)
  dir.create(folder$reg_training, showWarnings = F, recursive = T)
  
  if ( !force_recompute  & file.exists(filenames$csv) ) {
    fitting_results <- read.delim(filenames$csv)
    return(fitting_results)
  }
  
  # extract info
  video_info <- data[data$YoutubeID == videoID,]
  vc = unlist(video_info$dailyViewcount) ;
  
  ############### process the train/test division
  if ( !is.finite(points_to_train)) {
    if (length(vc)-points_to_hold_for_test <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test, length(vc)))
    train_points <- 1:(length(vc)-points_to_hold_for_test)
    test_points <- (length(vc)-points_to_hold_for_test+1):length(vc) 
  } else {
    # means we got a number of points to learn on
    if (length(vc)-points_to_hold_for_test <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test, length(vc)))
    
    if ( (points_to_train + points_to_hold_for_test) > length(vc)) {
      warning(sprintf("Video '%s': cannot learn on %d points and test on %d, when only %d are available. Adjusting learning to %d", 
                      videoID, points_to_train, points_to_hold_for_test, length(vc), length(vc)-points_to_hold_for_test))
      points_to_train <- length(vc)-points_to_hold_for_test
    }
    
    train_points <- 1:points_to_train
    test_points <- (points_to_train+1):(points_to_train+points_to_hold_for_test)
  }
  holdout_points <- (length(train_points) - points_to_holdout + 1):length(train_points)
  train_points <- train_points[-holdout_points]
  points_to_train <- points_to_train - points_to_holdout
  
  ########################################### If here, it means that we need to fit segments
  vc_tunning <- vc[holdout_points]
  vc <- vc[train_points]
  sc = unlist(video_info$numShare)[train_points] ;
  tc = unlist(video_info$dailyTweets)[train_points] ;
  #dn = (as.Date(video_info$uploadDate) + 1:length(vc) - 1)[train_points] ;  
  
  ext_info <- .predict.construct_external_info(external_info_type = external_info_type, videoID = videoID, data = data)
  if (ext_info$correct) {
    ext_infl <- ext_info$ext_infl
  } else stop("Incorrect external info type or information provided!")
  
  # construct errors data frame
  fitting_results <- data.frame()
  
  ## generate the configurations to start the search with
  method = "nloptr"
  params <- .predict.fit_videoinfo.construct_work_params(initial_params)
  
  work_configs <- list(nrow(params))
  for (ln in 1:nrow(params)) {
    current_config = list(series = list(vc), ext_infl = ext_infl, param = params[ln,])
    work_configs[[ln]] <- current_config
  }
  
  ######## calculating bounds
  ## only gamma and eta
  #   lowerBound = c(gamma = 0, eta = 0, K = -Inf, beta = 0.1, c = -Inf, theta = -Inf, mu1 = -Inf)
  #   upperBound = c(gamma = Inf, eta = Inf, K = Inf, beta = 0.1, c = 5, theta = 100, mu1 = Inf)
  #   ## completely bounded
  lowerBound = c(gamma = 0, eta = 0, K = 0, beta = 0.1, c = .Machine$double.eps, theta = .Machine$double.eps) # limit external influence to lower 0
  upperBound = c(gamma = Inf, eta = Inf, K = Inf, beta = 0.1, c = 5, theta = 100)
  # all subsequent mus need to be bounded min to zero
  for (i in 1:length(ext_infl)) {
    varname <- sprintf("mu%d", i)
    lowerBound[[varname]] <- 0
    upperBound[[varname]] <- Inf
  }
  
  ################# start the fitting
  
  if (parallel) {
    # means that we fit the different configurations in parallel
    require("parallel");
    .cl <- makeCluster( min(detectCores(), 8), type = "PSOCK") ## TODO: this needs testing so that all objects are passed here below.
    ## Make sure the nodes can see these functions & other objects as called by the optimizer
    ## NOT NEEDED WITH FORK CLUSTER. BUT FORK CLUSTERS DON'T WORK ON WINDOWS - also fork has faster startup
    clusterExport(.cl,
                  varlist = c("error_function_gradient", "error_function", "generate_simulated_data", "predict_theoretical_lambda",
                              "grad_lambda", "predict.get_folder", "predict.get_file_names", "predict.train_regularizer",
                              ".check_fix_mus_ext_infl", ".predict.construct_external_info", ".predict.fit_videoinfo.construct_work_params",
                              ".correct_names", "fit_series"),
                  envir = environment())
    result <- parLapplyLB(cl = .cl,
                          X = work_configs,
                          fun = function(x) fit_series(data_series = as.numeric(unlist(x$series)),
                                                       ext_infl = x$ext_infl,
                                                       initial_params = x$param, 
                                                       disable_relaxation_kernel = disable_relaxation_kernel,
                                                       lowerBound = lowerBound,
                                                       upperBound = upperBound,
                                                       method = method) )
    ### done, stopping cluster
    stopCluster(cl = .cl)
  } else {
    result <- lapply(X = work_configs, 
                     FUN = function(x) fit_series(data_series = as.numeric(unlist(x$series)),
                                                  ext_infl = x$ext_infl,
                                                  initial_params = x$param, 
                                                  disable_relaxation_kernel = disable_relaxation_kernel,
                                                  lowerBound = lowerBound,
                                                  upperBound = upperBound,
                                                  method = method) )
  }
  
  # calculate square error for all configs
  my_model <- NULL
  init_params <- NULL
  error <- Inf
  error_reg <- Inf
  alpha_regularizer <- NA
  alpha_regularizer_perc <- NA
  
  for (ln in 1:nrow(params)) {
    # series is the predicted series with the current fitted parameters on the holdout set
    series <- generate_simulated_data(params = result[[ln]]$model$par, 
                                      time = max(holdout_points)-1, 
                                      ext_infl = ext_infl, 
                                      prefix = vc[train_points])$Count
    score = sum((vc_tunning - series[holdout_points])^2) / 2
    # while the solution will be chosen on the holdout set, we need to express
    # the regularization hyperparameter as a percentage of the error score
    # obtained on the training set. So, I need to store that one also.
    series <- result[[ln]]$fitted_counts
    if ( is.numeric(result)) series <- result[,j]
    score_regularizer = sum((vc - series)^2) / 2
    
    if ( is.finite(score) && (score < error)) {
      my_model <- result[[ln]]$model
      init_params <- params[ln,]
      error <- score
      error_reg <- score_regularizer
    }
  }
  
  ## tune the hyperparameter, by calculating the errors obtained on the holdout
  ## set for all work configs
  # if we need to regularize, then we need to use the resulted parameters only as normalization and redo the procedure for 5 values of alpha.
  if (regularize) {
    work_configs <- list()
    
    if (is.null(regularizer_val) )
      regularizer_val <- c(0.0001, 0.001, 0.01, 0.1, 1, 10)
    
    for (alpha_regularizer in regularizer_val ) {
      start_from <- length(work_configs)
      params <- .predict.fit_videoinfo.construct_work_params(initial_params)
      
      for (ln in 1:nrow(params)) {
        current_config = list(series = list(vc), 
                              ext_infl = ext_infl, 
                              param = params[ln,], 
                              alpha_regularizer_perc = alpha_regularizer,
                              alpha_regularizer = alpha_regularizer * error_reg)
        work_configs[[start_from + ln]] <- current_config
      }
    }
    
    ######## calculating bounds
    ## only gamma and eta
    #   lowerBound = c(gamma = 0, eta = 0, K = -Inf, beta = 0.1, c = -Inf, theta = -Inf, mu1 = -Inf)
    #   upperBound = c(gamma = Inf, eta = Inf, K = Inf, beta = 0.1, c = 5, theta = 100, mu1 = Inf)
    #   ## completely bounded
    lowerBound = c(gamma = 0, eta = 0, K = 0, beta = 0.1, c = .Machine$double.eps, theta = .Machine$double.eps) 
    upperBound = c(gamma = Inf, eta = Inf, K = Inf, beta = 0.1, c = 5, theta = 100)
    ## limit external influence to lower 0. all subsequent mus need to be bounded min to zero
    for (i in 1:length(ext_infl)) {
      varname <- sprintf("mu%d", i)
      lowerBound[[varname]] <- 0
      upperBound[[varname]] <- Inf
    }
    
    ################# start the fitting
    if (parallel) {
      # means that we fit the different configurations in parallel
      require("parallel");
      .cl <- makeCluster(detectCores() + 1, type = "FORK");
      result <- parLapplyLB(cl = .cl,
                            X = work_configs,
                            fun = function(x) fit_series(data_series = as.numeric(unlist(x$series)), 
                                                         ext_infl = x$ext_infl,
                                                         initial_params = x$param, 
                                                         disable_relaxation_kernel = disable_relaxation_kernel,
                                                         lowerBound = lowerBound,
                                                         upperBound = upperBound,
                                                         alpha_regularizer = x$alpha_regularizer,
                                                         param_scaling = as.list(my_model$par),
                                                         method = method) )
      ### done, stopping cluster
      stopCluster(cl = .cl)
    } else {
      result <- lapply(X = work_configs, 
                       FUN = function(x) fit_series(data_series = as.numeric(unlist(x$series)),
                                                    ext_infl = x$ext_infl,
                                                    initial_params = x$param, 
                                                    disable_relaxation_kernel = disable_relaxation_kernel,
                                                    lowerBound = lowerBound,
                                                    upperBound = upperBound,
                                                    alpha_regularizer = x$alpha_regularizer,
                                                    param_scaling = as.list(my_model$par),
                                                    method = method) )
    }
    
    # calculate square error for all configs
    my_model <- NULL
    init_params <- NULL
    error <- Inf
    alpha_regularizer <- NA
    alpha_regularizer_perc <- NA
    
    for (ln in 1:length(work_configs)) {
      # series is the predicted series with the current fitted parameters on the holdout set
      series <- generate_simulated_data(params = result[[ln]]$model$par, 
                                        time = max(holdout_points)-1, 
                                        ext_infl = ext_infl, 
                                        prefix = vc[train_points])$Count
      score = sum((vc_tunning - series[holdout_points])^2) / 2
      
      if ( is.finite(score) && (score < error)) {
        my_model <- result[[ln]]$model
        init_params <- work_configs[[ln]]$param
        error <- score
        alpha_regularizer <- work_configs[[ln]]$alpha_regularizer
        alpha_regularizer_perc <- work_configs[[ln]]$alpha_regularizer_perc
      }
    }
  }
  
  # the corresponding results line
  fitting_results <- list(VideoID = videoID, external_info_type = external_info_type, 
                          points_to_train = points_to_train, points_to_holdout = points_to_holdout, points_to_hold_for_test = points_to_hold_for_test,
                          initial_params = as.list(init_params), fitted_params = as.list(my_model$par), 
                          alpha_regularizer = alpha_regularizer, alpha_regularizer_perc = alpha_regularizer_perc )
  
  if (plot_fitting) {
    varname <- paste("predict.", external_info_type, sep="")
    if (!varname %in% names(data)) data[[varname]] <- rep(NA, times = nrow(data))
    data[data$YoutubeID == videoID,][[varname]] <- list(fitting_results)
    .predict.plot_one_video(videoID = videoID, data = data, ext_infl_in_test = F, dataset_name = dataset_name)
  }
  
  if (save_results_to_file) {
    dir.create(path = folder$reg_training, recursive = T, showWarnings = F)
    write.table(x = fitting_results, file = filenames$csv, sep = "\t", quote = F, row.names = F, col.names = T)
  }
  
  return(fitting_results)
}

#' Loads the results of the fitting into the big data frame
predict.gather_regularization_results <- function(data, dataset_name = "") {
  
  infotypes <- "shares"
  varnames <- paste("predict.", infotypes, sep="")
  new_data <- data.frame(matrix(data = NA, nrow = nrow(data), ncol = length(varnames)))
  names(new_data) <- varnames
  
  j = 0
  
  for (external_info_type in infotypes) {
    #     for (disable_relaxation_kernel in c(F, T)) {
    disable_relaxation_kernel <- F
    j = j + 1
    dir <- predict.get_folder(external_info_type = external_info_type, 
                              disable_relaxation_kernel = disable_relaxation_kernel, 
                              dataset_name = dataset_name)
    
    for (i in 1:nrow(data)) {
      files <- predict.get_file_names(folder = dir$reg_training, videoID = data$YoutubeID[[i]])
      
      fitting_results_raw <- NULL
      tryCatch(fitting_results_raw <- read.delim(files$csv),
               error = function(e) foo <- 1)
      
      ## got a weird bug where I have multiple lines in a result file.
      ## Ignoring these ones
      if (!is.null(fitting_results_raw) && nrow(fitting_results_raw) != 1)
        fitting_results_raw <- NULL
      
      if( !is.null(fitting_results_raw) ) {
        splitting <- strsplit(names(fitting_results_raw), "[.]")
        fitting_results <- list()
        initial_params <- list()
        fitted_params <- list()
        
        k = 0
        for (parm in splitting) {
          k <- k + 1
          if (length(parm) > 1) {
            if (parm[[1]] == "initial_params") {
              initial_params[[parm[[2]]]] <- fitting_results_raw[[k]]
            } else {
              fitted_params[[parm[[2]]]] <- fitting_results_raw[[k]]
            }
          } else {
            fitting_results[[parm]] <- fitting_results_raw[[parm]]
          }
        }
        fitting_results[["initial_params"]] <- initial_params
        fitting_results[["fitted_params"]] <- fitted_params
        
        new_data[i,j][[1]] <- list(fitting_results)
      }
    }
    #     }
  }
  
  return(new_data)
}

#' Wrapper for the next function, to make access easier to one video data
predict.get_series <- function(videoID, data, external_info_type = "sharesNtweets", 
                               ext_infl_in_test = F, points_to_train = NULL, points_to_hold_for_test = NULL, ...) {
  video_info <- data[ data$YoutubeID == videoID,]
  return(.predict.get_series(video_info = video_info, data = data, external_info_type = external_info_type,
                             ext_infl_in_test =  ext_infl_in_test, points_to_train = points_to_train, points_to_hold_for_test = points_to_hold_for_test, ...))
}

.predict.get_series <- function(video_info, external_info_type = "sharesNtweets", disable_relaxation_kernel = F, ext_infl_in_test = F, 
                                points_to_hold_for_test = NULL, points_to_train = NULL, varname = NULL, data = NULL,
                                learn_ext_infl = NULL, test_ext_infl = NULL) {
  if ( ! (external_info_type %in% .predict.construct_external_info()$types))
    stop(sprintf("External error type '%s' is not known.", external_info_type))
  videoID <- video_info$YoutubeID
  
  if (is.null(varname))
    varname <- sprintf("predict.%s", external_info_type)
  fitting_results <- video_info[[varname]]
  if (length(fitting_results) == 1) fitting_results <- fitting_results[[1]]
  
  # the existing series
  vc = unlist(video_info$dailyViewcount) ;
  sc = unlist(video_info$numShare) ;
  tc = unlist(video_info$dailyTweets) ;
  dn = as.Date(video_info$uploadDate) + 1:length(vc) - 1 ;  
  
  if ((!varname %in% names(video_info)) || is.na(video_info[varname])) 
    return(list(train_series = rep(NA, times = length(vc)), 
                train_error = NA,
                predicted_series = rep(NA, times = length(vc)), 
                predicted_error = NA))
  
  mydata <- video_info
  if (!is.null(data))
    mydata <- data
  ext_info <- .predict.construct_external_info(external_info_type = external_info_type, videoID = video_info$YoutubeID, data = mydata)
  if (ext_info$correct) {
    ext_infl <- ext_info$ext_infl
  } else stop("Incorrect external info type or information provided!") 
  
  if ( is.null(points_to_hold_for_test)) points_to_hold_for_test <- fitting_results$points_to_hold_for_test
  if ( is.null(points_to_train) & ("points_to_train" %in% names(fitting_results))) {
    # put in the training also the holdout collection
    points_to_train <- fitting_results$points_to_train + fitting_results$points_to_holdout
  }
  
  if ( !is.finite(points_to_train)) {
    if (length(vc)-points_to_hold_for_test <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test, length(vc)))
    
    train_set <- 1:(length(vc)-points_to_hold_for_test)
    test_set <- (length(vc)-points_to_hold_for_test+1):length(vc) 
  } else {
    # means we got a number of points to learn on
    if (length(vc)-points_to_hold_for_test <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test, length(vc)))
    
    if ( (points_to_train + points_to_hold_for_test) > length(vc)) {
      warning(sprintf("Video '%s': cannot learn on %d points and test on %d, when only %d are available. Adjusting learning to %d", 
                      videoID, points_to_train, points_to_hold_for_test, length(vc), length(vc)-points_to_hold_for_test))
      points_to_train <- length(vc)-points_to_hold_for_test
    }
    
    train_set <- 1:points_to_train
    test_set <- (points_to_train+1):(points_to_train+points_to_hold_for_test)
  }
  
  # limit the external influence to only the known part (take out the points_to_hold_for_test in the fitting_results)
  if ( !ext_infl_in_test)
    ext_infl <- lapply(X = ext_infl, FUN = function(x) { x[test_set] <- NA ; return(x) }) # if this line is commented, then external influence is used in predicting the future
  
  if (!is.null(test_ext_infl))
    ext_infl <- lapply(X = ext_infl, FUN = function(x) { x[test_set] <- test_ext_infl ; return(x) })
  
  ## now for the calculation
  hv <- vector()
  
  # generate the series for the current segment
  params = unlist(fitting_results$fitted_params)
  
  # calculate squarre error for all configs
  hv <- generate_simulated_data(params = params, time = max(train_set)-1, ext_infl = ext_infl)$Count
  hv_pred <- generate_simulated_data(params = params, time = max(test_set)-1, ext_infl = ext_infl, prefix = vc[train_set])$Count
  
  # now, the last fitting_results$points_to_hold_for_test are the prediction. Let's calculate the 2 errors
  train_error <- sum((vc[train_set] - hv[train_set])^2) / 2
  test_error <- sum((vc[test_set] - hv_pred[test_set])^2) / 2
  
  return(list(train_series = hv[train_set], train_error = train_error, predicted_series = hv_pred[test_set], predicted_error = test_error))
}

predict.get_Honglin_series <- function(videoID, data, lambda_val = NULL) {
  varname <- "segs"
  if (!is.null(lambda_val)) varname <- paste(varname, as.character(lambda_val), sep = ".")
  
  video_info <- data[data$YoutubeID == videoID,]
  video_info.segs <- video_info[[varname]][[1]]
  if (typeof(video_info.segs) != "list")
    return(list(train_series = NA, train_error = NA, no_segments = NA))
  
  video_info.segs$HeadIndex <- video_info.segs$HeadIndex + 1
  video_info.segs$TailIndex <- video_info.segs$TailIndex + 1
  
  # plot view count
  vc = unlist(video_info$dailyViewcount) ;
  dn = as.Date(video_info$uploadDate) + 1:length(vc) - 1 ;  
  
  # compute Honglin's curve - translated from Honglin's matlab code
  fv <- vector()
  ferror <- 0
  fhead = matrix(0, nrow = 1, ncol = nrow(video_info.segs)); 
  maximIndex = video_info.segs$TailIndex[nrow(video_info.segs)]
  for (i  in  1:nrow(video_info.segs)) {
    s = video_info.segs$HeadIndex[i]
    e = video_info.segs$TailIndex[i]
    x = video_info.segs$A[i] * (1: (e-s+1)) ^ video_info.segs$B[i] + video_info.segs$C[i] 
    current = x/100 * max(vc[1:maximIndex]); # WHAT IS THIS FOR? WHY IS IT NORMALIZED?
    # check the fitting direction, maybe it needs reverting
    if (video_info.segs$FitDirection[i] == 1)
      current <- current[length(current):1]
    
    # calculate error
    error <- sum((vc[s:e] - current)^2)/2
    ferror <- ferror + error
    
    fv <- c(fv, current)
    fhead[i] = s
  }
  
  return(list(train_series = fv, train_error = ferror, no_segments = nrow(video_info.segs)))
}

.predict.plot_one_video <- function(videoID, data, ext_infl_in_test = F, dataset_name = "", baseline_prediction = NULL){
  res <- predict.get_folder(external_info_type = "none", disable_relaxation_kernel = F, dataset_name = dataset_name)
  dir.create(res$plot_folder, showWarnings = F, recursive = T)
  
  video_info <- data[data$YoutubeID == videoID,]
  vc = unlist(video_info$dailyViewcount) ;
  dn = as.Date(video_info$uploadDate) + 1:length(vc) - 1 ;  
  
  ## start plotting. First the graphic with all original series, Honglin and 3 fittings in one graphic
  files <- predict.get_file_names(folder = res$plot_folder, videoID = videoID)
  
  print_legend <- rep(T, times = 6)
  series_none <- predict.get_series(videoID = videoID, data = data, external_info_type = "none", disable_relaxation_kernel = F, ext_infl_in_test = ext_infl_in_test)
  if (is.na(series_none$predicted_error) ) {
    series_none$predicted_error <- NULL
    print_legend[1] <- F
  }
  series_shares <- predict.get_series(videoID = videoID, data = data, external_info_type = "shares", disable_relaxation_kernel = F, ext_infl_in_test = ext_infl_in_test)
  if (is.na(series_shares$predicted_error) ) {
    series_shares$predicted_error <- NULL
    print_legend[2] <- F
  }
  series_tweets <- predict.get_series(videoID = videoID, data = data, external_info_type = "tweets", disable_relaxation_kernel = F, ext_infl_in_test = ext_infl_in_test)
  if (is.na(series_tweets$predicted_error) ) {
    series_tweets$predicted_error <- NULL
    print_legend[3] <- F
  }
  series_sharesNtweets <- predict.get_series(videoID = videoID, data = data, external_info_type = "sharesNtweets", disable_relaxation_kernel = F, ext_infl_in_test = ext_infl_in_test)
  if (is.na(series_sharesNtweets$predicted_error) ) {
    series_sharesNtweets$predicted_error <- NULL
    print_legend[4] <- F
  }
  series_neighboorhoodTweets <- predict.get_series(videoID = videoID, data = data, external_info_type = "neighboorhoodTweets", disable_relaxation_kernel = F, ext_infl_in_test = ext_infl_in_test)
  if (is.na(series_neighboorhoodTweets$predicted_error) ) {
    series_neighboorhoodTweets$predicted_error <- NULL
    print_legend[5] <- F
  }
  ref_series <- series_sharesNtweets
  train <- 1:(length(ref_series$train_series))
  test <- (length(ref_series$train_series) + 1 ):(length(ref_series$train_series) + length(ref_series$predicted_series))
  
  baseline <- NA
  baseline_error <- NULL
  if (!is.null(baseline_prediction) & (sum(baseline_prediction$YoutubeID == videoID) > 0)) {
    baseline = unlist(baseline_prediction[baseline_prediction$YoutubeID == videoID, 2:ncol(baseline_prediction)])
    if (length(baseline) != length(test))
      warning( sprintf("Baseline length %d is different from testing length %d.", length(baseline), length(test)) )
    
    names(baseline) <- NULL
    baseline <- c(rep(NA, times = length(train)), baseline)
    if (length(baseline) != (length(test) + length(train)) ) {
      missing_vals <- length(test) + length(train) - length(baseline)
      baseline <- c(baseline, rep(NA, times = missing_vals))
    }
    baseline_error <- sum( (vc[test] - baseline[test])^2 ) / 2
  }
  if (is.null(baseline_error)) print_legend[6] <- F
  
  # do the actual plotting
  pdf(files$pdf, width = 11, height = 8.5)
  series <- c(vc, 
              series_none$train_series, series_none$predicted_series,
              series_sharesNtweets$train_series, series_sharesNtweets$predicted_series,
              series_neighboorhoodTweets$train_series,  series_neighboorhoodTweets$predicted_series,
              baseline)
  series[ is.infinite(series)] <- NA
  ylimit = c( min(series, na.rm = T), 
              max(series, na.rm = T)+1)
  plot(x=dn, y=vc, type="l", 
       xlab = "Timeslice date", ylab = "Viewcounts", main=sprintf("Real and predicted viewcounts \n - video '%s'", videoID), ylim = ylimit)
  
  # none plot
  lines(x = dn[train], y = series_none$train_series[1:length(train)], type="l", col="blue", lty = 2) #,lwd=3
  abline(v = dn[test[1]], col = "gray60", lty = 2)
  lines(x = dn[test], y = series_none$predicted_series[1:length(test)], col="blue")
  
  # shares plot
  lines(x = dn[train], y = series_shares$train_series[1:length(train)], type="l", col="darkcyan", lty = 2) #,lwd=3
  lines(x = dn[test], y = series_shares$predicted_series[1:length(test)], col="darkcyan")
  
  # tweets plot
  lines(x = dn[train], y = series_tweets$train_series[1:length(train)], type="l", col="magenta", lty = 2) #,lwd=3
  lines(x = dn[test], y = series_tweets$predicted_series[1:length(test)], col="magenta")
  
  # sharesNtweets plot
  lines(x = dn[train], y = series_sharesNtweets$train_series[1:length(train)], type="l", col="red", lty = 2) #,lwd=3
  lines(x = dn[test], y = series_sharesNtweets$predicted_series[1:length(test)], col="red")
  
  # series_neighboorhoodTweets plot
  lines(x = dn[train], y = series_neighboorhoodTweets$train_series[1:length(train)], type="l", col="darkolivegreen4", lty = 2) #,lwd=3
  lines(x = dn[test], y = series_neighboorhoodTweets$predicted_series[1:length(test)], col="darkolivegreen4")
  
  # baseline plot
  lines(x = dn[test], y = baseline[test], col="gray70")
  
  # beautification
  legend("topright", 
         legend = c("Real views", 
                    sprintf("Hawkes's fit (<none>) (p.err = %.2e)", series_none$predicted_error ),
                    sprintf("Hawkes's fit (#shares) (p.err = %.2e)", series_shares$predicted_error ),
                    sprintf("Hawkes's fit (#tweets) (p.err = %.2e)", series_tweets$predicted_error ),
                    sprintf("Hawkes's fit (#shares & #tweets) (p.err = %.2e)", series_sharesNtweets$predicted_error ),
                    sprintf("Hawkes's fit (neigh. #tweets) (p.err = %.2e)", series_neighboorhoodTweets$predicted_error ),
                    sprintf("Baseline (p.err = %.2e)", baseline_error )),
         col = c("black", "blue", "darkcyan", "magenta", "red", "darkolivegreen4", "gray70")[c(T, print_legend)], lwd=c(2.5, 2.5, 2.5, 2.5, 2.5), lty=c(1, 1, 1, 1, 1) ) #, 
  
  #     ######### second graphic, tweets
  #     series <- c(vc, 
  #                 series_none$train_series, series_none$predicted_series,
  #                 series_tweets$train_series, series_tweets$predicted_series,
  #                 series_tweets_norelax$train_series, series_tweets_norelax$predicted_series)
  #     series[ is.infinite(series)] <- NA
  #     ylimit = c( min(series, na.rm = T), 
  #                 max(series, na.rm = T)+1)
  #     plot(x=dn, y=vc, type="l", 
  #          xlab = "Timeslice date", ylab = "Viewcounts", main=sprintf("Real and predicted viewcounts \n - video '%s'", videoID), ylim = ylimit)
  #     
  #     # none plot
  #     lines(x = dn[train], y = series_none$train_series, type="l", col="blue", lty = 2) #,lwd=3
  #     abline(v = dn[test[1]], col = "gray60", lty = 2)
  #     lines(x = dn[test], y = series_none$predicted_series, col="blue")
  #     
  #     # tweets plot
  #     lines(x = dn[train], y = series_tweets$train_series, type="l", col="red", lty = 2) #,lwd=3
  #     lines(x = dn[test], y = series_tweets$predicted_series, col="red")
  #     
  #     # baseline plot
  #     lines(x = dn[test], y = baseline[test], col="darkcyan")
  #     
  #     # shares norelax plot
  #     lines(x = dn[train], y = series_tweets_norelax$train_series, type="l", col="darkolivegreen4", lty = 2) #,lwd=3
  #     lines(x = dn[test], y = series_tweets_norelax$predicted_series, col="darkolivegreen4")
  #     
  #     # beautification
  #     legend("topright", 
  #            legend = c("Real views", 
  #                       sprintf("Hawkes's fit (<none>) (p.err = %.2e)", series_none$predicted_error ),
  #                       sprintf("Hawkes's fit (#tweets) (p.err = %.2e)", series_tweets$predicted_error ),
  #                       sprintf("Baseline (p.err = %.2e)", baseline_error ),
  #                       sprintf("Pred. no relax (#tweets) (p.err = %.2e)", series_tweets_norelax$predicted_error )), 
  #            col = c("black", "blue", "red", "darkolivegreen4"), lwd=c(2.5, 2.5, 2.5, 2.5), lty=c(1, 1, 1, 1) )
  
  
  dev.off()
  
  return(c(YoutubeID = videoID, 
           none = series_none$predicted_error, 
           shares = series_shares$predicted_error, 
           tweets = series_tweets$predicted_error, 
           sharesNtweets = as.numeric(series_sharesNtweets$predicted_error)) )
  
}

predict.plot_predicted_curves <- function(data, ext_infl_in_test = F, dataset_name = "", baseline_prediction = NULL) {
  res <- predict.get_folder(external_info_type = "none", disable_relaxation_kernel = F, dataset_name = dataset_name)
  dir.create(res$plot_folder, showWarnings = F, recursive = T)
  
  #   require("parallel");
  #   .cl <- makeCluster(detectCores() + 5);  #, type = "FORK"
  #   clusterExport(.cl, 
  #                 varlist = c("fit_series", "error_function_gradient", "error_function", "generate_simulated_data", 
  #                             "predict_theoretical_lambda", "grad_lambda", "data", "predict.get_folder", 
  #                             "predict.get_file_names", "predict.fit_videoinfo",
  #                             ".predict.plot_one_video", ".predict.get_series", "predict.get_file_names"),
  #                 envir=environment());
  #   
  #   rest <- parLapply(cl = .cl, X = data$YoutubeID, 
  #                     fun = function(x) .predict.plot_one_video(x, data = data, ext_infl_in_test = ext_infl_in_test) )
  #   
  #   ### done, stopping cluster
  #   stopCluster(cl = .cl)
  
  ## serial version
  rest <- lapply(X = data$YoutubeID, 
                 FUN = function(x) .predict.plot_one_video(x, data = data, 
                                                           ext_infl_in_test = ext_infl_in_test, 
                                                           dataset_name = dataset_name, 
                                                           baseline_prediction = baseline_prediction) )
  result <- t(data.frame(rest))
  rownames(result) <- NULL
  
  write.table(x = result, file = sprintf("%s/prediction-errors.csv", res$plot_folder), quote = F, sep = "\t", row.names = F, col.names = T)
  
  return(as.data.frame(result))
}

.predict.polyfit <- function(train_series, test_series, degrees = 7) {
  res <- lm(train_series ~ poly(1:90, degrees, raw=TRUE))
  
  val <- function(x) {
    val <- 0
    for (i in 1:length(res$coefficients)) {
      val <- val + res$coefficients[i] * x ^ (i-1)
    }
    return(val)
  }
  
  return(list(fitted = predict(res), predicted = val(test_series)) )
}

predict.create_neighbourhoods <- function(data, default_neighbourhood_size = 100, train_size = 90, folds = 5) {
  dist_matr <- t(sapply(X = data$dailyViewcount, FUN = function(x) return(x[1:train_size])))
  max_training <- apply(X = dist_matr, MARGIN = 1, FUN = max, na.rm = T)
  dist_matr <- dist_matr / max_training
  
  # calculate distances and folds
  distances <- as.matrix(dist(x = dist_matr, method = "euclidean"))  
  require(caret)
  splt <- createFolds(y = 1:nrow(data), k = folds, list = TRUE, returnTrain = FALSE)
  for (spt in splt) {
    distances[spt, spt] <- NA
  }
  
  # get first default_neighbourhood_size individuals
  distances <- lapply(X = 1:nrow(data), FUN = function(x) return(distances[x,]))
  require("parallel");
  .cl <- makeCluster(detectCores() + 1, type = "FORK");
  result <- parLapply(cl = .cl,
                      X = distances, fun = function(x) return(order(unlist(x), na.last = NA)[1:default_neighbourhood_size]) )
  stopCluster(cl = .cl)
  
  folds <- rep(x = "outlier", times = nrow(data))
  for (fld in names(splt)) {
    folds[splt[[fld]]] <- fld
  }
  
  return(list(fold = folds, neighbourhood = result))
}

#' Computes the confidence intervals of the forecasting, for a given video. 
#' Using the fitted data and the external influence series, by following the 
#' assumption that the variance on the testing set is similar to the variance 
#' observed on the training (or even better), the tunning holdout set. This is 
#' based on the (rather simplistic) idea that the popularity points observed 
#' durint testing/tunning are instances of a random variable of a given 
#' variance. Furthermore, the testing points are also sampled from the same 
#' random variable and share, therefore, the same variance.
#' @param videoID - the ID of the video to output confidence intervals
#' @param data - the data structure containing fitted videos
#' @param external_info_type [default "shares"] - the type of external 
#'   influence to use (option are "tweets", "shares" and "sharesNtweets")
#' @param points_to_hold_for_test
#' @param points_to_train
#' @param points_to_holdout [default NULL] - number of points to use fo testing,
#'   training, and tunning. If NULL, the stored values will be used.
#' @param plot_graphic [default FALSE] - whether a graphic with the fitting 
#'   should be plotted.
#' @param plot_tunning [default TRUE] - if plotting the graphic, should we also
#'   plot the tunning part?
predict.get_confidence_intervals <- function(videoID, data, external_info_type = "shares",
                                             points_to_hold_for_test = NULL, points_to_train = NULL, points_to_holdout = NULL,
                                             plot_graphic = F, plot_forecast = T, plot_tunning = T, plot_confidence_interval = T, print_parameters = T, currentPosition = NULL) {
  
  if ( ! (external_info_type %in% .predict.construct_external_info()$types))
    stop(sprintf("External error type '%s' is not known.", external_info_type))
  video_info <- data[data$YoutubeID == videoID,]
  
  varname <- sprintf("predict.%s", external_info_type)
  fitting_results <- video_info[[varname]]
  if (length(fitting_results) == 1) fitting_results <- fitting_results[[1]]
  
  # the existing series
  vc = unlist(video_info$dailyViewcount) ;
  
  if ((!varname %in% names(video_info)) || is.na(video_info[varname])) 
    return(confidence = NA)
  
  ext_info <- .predict.construct_external_info(external_info_type = external_info_type, videoID = video_info$YoutubeID, data = data)
  if (ext_info$correct) {
    ext_infl <- ext_info$ext_infl
  } else stop("Incorrect external info type or information provided!") 
  
  if ( is.null(points_to_hold_for_test)) points_to_hold_for_test <- fitting_results$points_to_hold_for_test
  if ( is.null(points_to_holdout)) points_to_holdout <- fitting_results$points_to_holdout
  if ( is.null(points_to_train) & ("points_to_train" %in% names(fitting_results))) {
    points_to_train <- fitting_results$points_to_train
  }
  
  if ( !is.finite(points_to_train)) {
    if (length(vc)-points_to_hold_for_test-points_to_holdout <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test+points_to_holdout, length(vc)))
    
    points_to_train <- length(vc) - points_to_hold_for_test - points_to_holdout
  } else {
    # means we got a number of points to learn on
    if (length(vc)-points_to_hold_for_test <= 0)
      stop(sprintf("Video '%s': you asked to test on too many points (%d), when only %d are available.", videoID, points_to_hold_for_test, length(vc)))
    
    if ( (points_to_train + points_to_holdout + points_to_hold_for_test) > length(vc)) {
      warning(sprintf("Video '%s': cannot learn on %d points and test on %d, when only %d are available. Adjusting learning to %d", 
                      videoID, points_to_train, points_to_hold_for_test, length(vc), length(vc)-points_to_hold_for_test))
      points_to_train <- length(vc) - points_to_hold_for_test - points_to_holdout
    }
  }
  train_set <- 1:points_to_train
  holdout_set <- (points_to_train+1):(points_to_train+points_to_holdout)
  test_set <- (points_to_train+points_to_holdout+1):(points_to_train+points_to_holdout+points_to_hold_for_test)
  
  # generate the series starting from the fitted params and compute the residual
  params = unlist(fitting_results$fitted_params)
  hv <- generate_simulated_data(params = params, time = max(train_set)-1, ext_infl = ext_infl)$Count
  residual <- vc[train_set] - hv[train_set]
  res_var <- var(residual)
  
  # now do the same for the holdout set
  hv_holdout <- generate_simulated_data(params = params, 
                                        time = max(holdout_set)-1, 
                                        ext_infl = ext_infl, 
                                        prefix = vc[train_set])$Count
  residual_holdout <- vc[holdout_set] - hv_holdout[holdout_set]
  res_holdout_var <- var(residual_holdout)
  
  filename <- NULL
  if (plot_graphic) {
    folder <- "plots/fitted-videos/fitted-popularity-curves"
    dir.create(path = folder, recursive = T, showWarnings = F)
    filename <- sprintf("%s/%s.pdf", folder, videoID)
    # pdf(file = filename, width = 10, height = 7.5) ## initially 12 x 7 (wide format)
    op <- par(mar = c(5,4.5,4,4) + 0.1)
    dn <- as.Date(video_info$uploadDate) + 1:length(vc) - 1 ; 
    
    ## real viewcounts
    to_plot <- data.frame(real = vc[c(train_set, holdout_set, test_set)])
    to_plot$fitted <- rep(x = NA, times = nrow(to_plot))
    
    legends <- list()
    conf_title <- " with confidence interval"
    if (!plot_confidence_interval)
      conf_title <- ""
    if (plot_tunning) {   
      ## if we are plotting the tunning part, everything is already calculated
      
      ## fitted viewcounts, on training
      to_plot$fitted[train_set] <- hv
      to_plot$tunning <- rep(x = NA, times = nrow(to_plot))
      to_plot$tunning[holdout_set] <- hv_holdout[holdout_set]
      
      # legends$lwd <- c(1, 3, 3, 3, 1, 1)
      # legends$legend <- c("Observed #views", "Fitted #views on training set", "Tunned #views on holdout set", 
      #                     "Predicted viewcounts", "Predicted confidence intervals", sprintf("Exogenous stimuli (#%s)", external_info_type) )
      # legends$lty <- c(2, 1, 1, 1, 2, 2)
      # legends$col <- c("black", "blue", "darkolivegreen4", "darkmagenta", "darkmagenta", "darkmagenta")
      # legends$title <- sprintf("%s: Observed, fitted and predicted popularity\n%s", videoID, conf_title)
    } else {
      ## if we don't want the tunning part, then we need to recompute the fitted part
      hv <- generate_simulated_data(params = params, time = max(holdout_set)-1, ext_infl = ext_infl)$Count
      to_plot$fitted[c(train_set, holdout_set)] <- hv
      
      # legends$lwd <- c(2, 2, 2, 2)
      # legends$legend <- c("Observed #views", "Fitted #views", 
      #                     "Predicted viewcounts", sprintf("Exogenous stimuli (#%s)", external_info_type) )
      # legends$lty <- c(2, 1, 1, 1)
      # legends$col <- c("black", "blue", "darkmagenta", "red")
      # legends$title <- sprintf("%s: Observed and predicted popularity%s", videoID, conf_title)
    }
    
    ## generate testing part
    hv_test <- generate_simulated_data(params = params, 
                                       time = max(test_set)-1, 
                                       ext_infl = ext_infl, 
                                       prefix = vc[c(train_set, holdout_set)])$Count
    to_plot$upperConf <- to_plot$lowerConf <- to_plot$test <- rep(x = NA, times = nrow(to_plot))    
    to_plot$test[test_set] <- hv_test[test_set]
    if (plot_confidence_interval) {
      to_plot$upperConf[test_set] <- hv_test[test_set] + 1.96 * sqrt(res_holdout_var)
      to_plot$lowerConf[test_set] <- hv_test[test_set] - 1.96 * sqrt(res_holdout_var)
    }
    
    # ## and now plot them all at once - add x time labels later, every 14 days
    # matplot(dn[c(train_set, holdout_set, test_set)], to_plot, type = "l", xaxt = "n",
    #         col = legends$col, lty = legends$lty, lwd = legends$lwd,
    #         cex.main = 1.5, cex.axis = 1, cex.lab = 1.5,
    #         xlab = "", ylab = "#views", main = legends$title)
    # axis(1, at=dn[c(train_set, holdout_set, test_set)][seq(from = 1, to = max(test_set), by = 14)], cex.axis = 1,
    #      labels=dn[c(train_set, holdout_set, test_set)][seq(from = 1, to = max(test_set), by = 14)])
    # 
    if (plot_tunning)
      abline(v = dn[holdout_set[1]], col = "gray60", lty = 2)
    
    # and now the external influence on another axis
    par(new=TRUE)
    infl <- unlist(ext_infl)
    y_limit <- range(infl)
    # scale it down
    y_limit[2] <- y_limit[2] * 3
    # plot(x = dn[c(train_set, holdout_set, test_set)], y = infl[c(train_set, holdout_set, test_set)], 
    #      type="l", col="red", xaxt="n", yaxt="n", xlab="", ylab="", lwd=1.5, lty = 1, ylim = y_limit,
    #      cex.main = 1.5, cex.axis = 1, cex.lab = 1.5)
    # axis(4, cex.main = 1.5, cex.axis = 1, cex.lab = 1.5, 
    #      at = round(seq(from = range(infl[c(train_set, holdout_set, test_set)])[1], 
    #                     to = range(infl[c(train_set, holdout_set, test_set)])[2], 
    #                     length.out = 6)) )
    # mtext(sprintf("External influence (#%s)", external_info_type), side=4, line=2, cex = 1.5, col = "red")
    
    # if (!is.null(currentPosition) && !currentPosition == 0) {
    #   nearestPoint <- max(dn[dn <= currentPosition])
    #   abline(v = nearestPoint, col = "gray60", lty = 2)
    #   legend("topright", legend = c(paste("Oberved #views: ", to_plot$real[which(dn == nearestPoint)]),
    #                                 paste("Fitted #views: ", round(to_plot$fitted[which(dn == nearestPoint)], digits = 0)),
    #                                 paste("Predicted viewcounts: ", round(to_plot$test[which(dn == nearestPoint)], digits = 0)),
    #                                 paste("Exogenous stimuli(#shares): ", infl[dn == nearestPoint])),
    #                       bty = "n", lwd = legends$lwd, lty = legends$lty, col = legends$col)
    # }
    # legend(x = min(dn), y = (min(infl) - max(infl)) / 2, xpd = TRUE, lwd = legends$lwd, legend = legends$legend, lty = legends$lty, col = legends$col, bty = "n", cex = 1, ncol = 2)
    
    ##### if needed, print parameters
    if (print_parameters) {
      ## compute some measures
      mu <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$mu1
      theta <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$theta
      gamma <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$gamma
      eta <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$eta
      c <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$c
      K <- data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params$K
      bigC <- K * 1.016 / (1.016 - 0.1)
      if ("endo" %in% names(data))
        endo <- data$endo[data$YoutubeID == videoID]
      else
        endo <- get_endogenous_response(params = data$predict.shares[data$YoutubeID == videoID][[1]]$fitted_params)$endo
      
      # printing it
      startLine <- -2
      mtext("Fitted values:  ", side = 3, line = startLine, adj = 1, cex = 1.6 )
      mtext(bquote(mu == .(mu) ~ ", " ~ 
                     theta == .(theta) ~ " "), line = startLine-1.5, adj = 1, cex = 1.3 )
      mtext(bquote("c" == .(c) ~ ", " ~ 
                     "C" == .(bigC) ~ " "), line = startLine-3, adj = 1, cex = 1.3 )
      mtext(bquote(gamma == .(gamma) ~ ", " ~ 
                     eta == .(eta) ~ " "), line = startLine-4.5, adj = 1, cex = 1.3 )
      mtext(bquote(A[widehat(xi)] == .(endo) ~ " "), line = startLine-6, adj = 1, cex = 1.3 )
    }
    # dev.off()
  }
  return(list(dates = dn[c(train_set, holdout_set, test_set)], real = to_plot$real, fitted = to_plot$fitted, test = to_plot$test,
              infl = infl[c(train_set, holdout_set, test_set)], viewcount = sum(to_plot$fitted, to_plot$test, na.rm = TRUE)))
}

#' Converts data structure containing results from v1 format (old fitting, but
#' still interesting for some plots) to v2
predict.convert_data_v1_to_v2 <- function(data) {
  # new_data <- data.frame(YoutubeID = data$YoutubeID)
  # new_data$YoutubeID <- as.character(new_data$YoutubeID)
  
  for (varname in c("tweets", "shares")) {
    # varname <- "shares"
    oldvarname <- sprintf("fit.entire.%s", varname)
    newvarname <- sprintf("predict.%s", varname)
    
    ## if old fitting contains the curent variable
    if ( oldvarname %in% names(data)) {
      for (i in 1:nrow(data)) {
        oldfit <- data[[oldvarname]][[i]]
        fitting_results <- list(VideoID = data$YoutubeID[i], 
                                external_info_type = "varname",
                                points_to_train = 75, ## not true, but need to fake it for implicit values other functions expect
                                points_to_holdout = 15,
                                points_to_hold_for_test = 30,
                                alpha_regularizer = NA,
                                alpha_regularizer_perc = NA)
        initial_params <- list()
        fitted_params <- list()
        oldfit <- oldfit[1,]
        
        splitting <- strsplit(names(oldfit), "[.]")
        
        ## iterate through the old fit parameters
        k = 0
        for (parm in splitting) {
          k <- k + 1
          if (length(parm) > 1 & parm[[1]] == "HS") {
            init_split <- strsplit(parm[[2]], "[_]")
            if (length(init_split[[1]]) > 1 & init_split[[1]][[1]] == "init") {
              initial_params[[init_split[[1]][[2]]]] <- oldfit[[1, k]]
            } else {
              if (parm[[2]] == "error")
                fitting_results$error <- oldfit[[1, k]]
              else
                fitted_params[[parm[[2]]]] <- oldfit[[1, k]]
            }
          } 
        }
        
        initial_params$mu1 <- initial_params$mu ; initial_params$mu <- NULL
        fitted_params$mu1 <- fitted_params$mu ; fitted_params$mu <- NULL
        fitting_results[["initial_params"]] <- initial_params
        fitting_results[["fitted_params"]] <- fitted_params
        
        data[[newvarname]][i] <- list(fitting_results)
      }
    }
  }
  
  return(data)
  
  ## Example run:
  # load("~/Work/ResearchRepos/youtube-popularity-twitter-current/youtube-data/16-fit-model-series-data/data/epic-figures.dat")
  # data <- predict.convert_data_v1_to_v2(data = data)
}

#' Computes a series of error measure for the prediction of the time series 
#' #views. The following error measures are computed: SSR/SSE (Squared Sum of 
#' Residuals/Errors), MSE (Mean Squared Error), RMSE (Root Mean Squared Error), 
#' MSSE (Means Squared Scalled Error). Only MSSE is scale-free and can be used
#' to compare different series of different scales.
#' SOURCE: https://www.otexts.org/fpp/2/5
#' 
#' @param real_series - the observed series of data
#' @param predicted_series - the series predicted by the model
predict.compute_prediction_errors <- function(real_series, predicted_series) {
  real_series <- unlist(real_series)
  predicted_series <- unlist(predicted_series)
  if (length(real_series) != length(predicted_series)) {
    warning(sprintf("The two series have different lenghts: observed=%d, predicted=%d. Is this correct?", length(real_series), length(predicted_series)) )
    return(list(SSR = NA, MSE = NA, RMSE = NA, MSSE = NA))
  }
    
  
  # let's start computing. First scale-dependent measure
  SR <- (real_series - predicted_series)^2
  SSR <- sum(SR, na.rm = T)
  MSE <- mean(SR, na.rm = T)
  RMSE <- sqrt(MSE)
  
  # and finally MSSE, which is MSE / scaler
  scaler <- mean(diff(real_series)^2, na.rm = T)
  MSSE <- MSE / scaler
  
  ## return the computed values
  return(list(SSR = SSR, MSE = MSE, RMSE = RMSE, MSSE = MSSE))
}

