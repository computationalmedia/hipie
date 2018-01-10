## initially called "2015-08-10-endo-response-char-time.R" in folder "19-..."

source('scripts/functions-predictive-power.R')
# require("car")
# require("MASS")
# require("fields")
require("parallel")
# require("colorspace")
# require("grDevices")

##### config
extrabit <- "" ## for initial publication dataset use this one!
# extrabit <- "defined-N" ## use this one for the defined N fitting (where all params respect the strict bounding boxes)

if ( ! exists("external_info_type")) {
  external_info_type <- "shares"
  # external_info_type <- "tweets"
  # external_info_type <- "sharesNtweets"
}

dataset_base_name <- "regression-period-90-30-regularization" # this is the forecasting exercice version: fit on 1-90 days, with regularizer
# dataset_base_name <- "regression-period-classic-fit" # this is the classic fit (only the ALL period, no regularizer)
# dataset_base_name <- "regression-period-one-step-fit" # this is the one step fit

#### done config

## load files. If you want any regeneration of the prediction errors, use the 
## "18-.../analysis-popularity-JUMP-quantitative.R"
## to regenerate endo response, use routine 1. from above
## Data loading and merging. Need the json containing metadata, the regression period dataset and endo-response calculation.
## See in project 18-...

# this is the forecasting exercise version: fit on 1-90 days, with regularizer
load(file = sprintf("data/%s/%s.dat", extrabit, dataset_base_name))

## load the endo response -- use "reconstruct-endo-promo-score.R" to regenerate these files
load(file =  sprintf("data/%s/%s-endo-response-COMPLETE.dat", extrabit, dataset_base_name))
## select what to load
response <- response.complete[, c("YoutubeID", 
                                  names(response.complete)[grepl(pattern = paste(external_info_type, "$", sep = ""), x = names(response.complete))]
)]
names(response) <- gsub(pattern = paste(".", external_info_type, sep = ""), replacement = "", x = names(response))

## and now load error predictions
data <- merge(x = data, y = response, by = "YoutubeID")
rm(response, response.complete)

## load the prediction error - for now I'm interested only in the #shares MSSE
load(file = sprintf("data/%s/%s-prediction-errors.dat", extrabit, dataset_base_name))
data <- merge(x = data, y = prediction[c("YoutubeID", "shares_MSSE")], by = "YoutubeID")
rm(prediction)

## load metadata from the json
# require("jsonlite"); dataset <- fromJSON(txt = "data/active-dataset.json.bz2", flatten = T) ; save(dataset, file = "data/active-dataset.dat", compress = "bzip2")
load(file = "data/active-dataset.dat") ## load into var dataset
dataset <- dataset[, c("YoutubeID", "description", "title", "relevantTopicIds", "topicIds", "channelId", "channelTitle")]
data <- merge(x = data, y = dataset, by = "YoutubeID")
rm(dataset)
# browser()
######### What do we plot? Identify the videos that respect the hypothesis from 2015-04-23-easily-promotable-videos
# extract fitted parameters
age <- 120
pars <- data.frame(YoutubeID = data$YoutubeID, endo = data$endo, n = data$n, 
                   characteristic_time = data$characteristic_time, shares_fit_error = data$shares_MSSE) #endo_int = data$endo_int,
pars$promo_score <- pars$endo ## Initially: - 1. Now changed.
pars$promo_score[pars$promo_score < 1] <- 1
pars$YoutubeID <- as.character(pars$YoutubeID)
varname <- sprintf("predict.%s", external_info_type)
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
pars$mu[pars$mus < 0] <- 0
pars$promo_score <- pars$promo_score * pars$mus
if ("promo_score" %in% names(data))
  data$promo_score <- NULL
data <- merge(x = data, y = pars[, c("YoutubeID", "promo_score")], by = "YoutubeID")

# construct percentiles
pop_step <- 0.05
percentiles <- list()
percentiles$YoutubeID <- data$YoutubeID
for (varname in names(pars)) {
  if (!varname %in% c("YoutubeID", "characteristic_time", "promo_score") ) {
    if (varname == "endo") {
      pop_step <- 0.2
    } else {
      pop_step <- 0.05
    }
    tryCatch({
      brks <- quantile(pars[[varname]], probs=seq(0, 1, by=pop_step), na.rm = T)
      lvls <- data.frame(YoutubeID = pars$YoutubeID, 
                         varname = as.numeric(cut(x = pars[[varname]], breaks = brks, include.lowest = T)) * pop_step )
      names(lvls) <- c("YoutubeID", varname)
      percentiles <- merge(percentiles, lvls, by = "YoutubeID")
    }, error=function(e){print(sprintf("Non-unique values in brks in %s", varname))})
  }
  
  if (varname == "promo_score") {
    lvls <- data.frame(YoutubeID = pars$YoutubeID, 
                       varname = rank(pars[[varname]], na.last = "keep", ties.method = "min") / nrow(pars))
    names(lvls) <- c("YoutubeID", varname)
    percentiles <- merge(percentiles, lvls, by = "YoutubeID")
  }
}
rm(lvls)

## add channel title in percentiles, to make it easier
percentiles <- merge(x = percentiles, y = data[, c("YoutubeID", "title", "channelTitle", "category")])

## compute popularities
pop_step <- 0.025
popularity_scale <- data.frame(matrix(data = NA, nrow = 1, ncol = 0))
for (viewvar in c("views", "shares")) {
  popvar <- sprintf("pop_%s", viewvar)
  percvar <- sprintf("perc_%s", viewvar)
  perc_valvar <- sprintf("perc_val_%s", viewvar)
  res <- t(unique( data.frame(quantile(pars[[viewvar]], probs=seq(0,1, by=pop_step))), fromLast = T))
  res[1] <- 0
  res[length(res)] <- Inf
  rownames(res) <- NULL
  perc_vals <- (as.numeric(sub("%", "", colnames(res))) / 100) ## because the value that I want is the end of the interval
  perc_vals <- perc_vals[-length(perc_vals)] + pop_step
  ## first compute the levels of popularity, but observing unique values, we transform to numerical afterwards
  pop_perc_factor <- cut(x = pars[[viewvar]], 
                         breaks = res, 
                         include.lowest=TRUE)
  popularity_scale[[percvar]] <- list(res)
  popularity_scale[[perc_valvar]] <- list(perc_vals)
}

## Usage of popularity scale:
# myviews <- 568456
# myshares <- 12560
# views_percentage <- popularity_scale$perc_val_views[[1]][as.numeric(cut(x = myviews, breaks = unlist(popularity_scale$perc_views), include.lowest = T))]
# shares_percentage <- popularity_scale$perc_val_shares[[1]][as.numeric(cut(x = myshares, breaks = unlist(popularity_scale$perc_shares), include.lowest = T))]
