.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my demographics package")
}

.onLoad <- function(libname, pkgname) {
}

AddMonths <- function(date, n) {
  seq(as.POSIXct.Date(date), by = paste(n, "months"), length = 2)[2]
}

AddNewInvestorFlag <- function(inputData, months) {
  RelevantDate <- AddMonths(Sys.Date() , -1 * months)
  inputData[ ,paste0('NewInvestor',months,'Months')] <- 0
  inputData[ inputData$InceptionDate >= RelevantDate, paste('NewInvestor',months,'Months', sep = '')] <- 1
  return(inputData)
}

AddAgeGroups <- function(inputData) {
  inputData$AgeGroup <- 'unknown'
  agebreaks <- c(0,30,50,60,70,80,500)
  agelabels <- c("<30","30-49","50-59","60-69","70-79","80+")
  inputData[, AgeGroup := cut(inputData$Age, breaks = agebreaks, right = FALSE, labels = agelabels)]
  return(inputData)
}

#this should go from BQ as aggregated columns or can be derived in app that uses it?
AddDerivedData <- function(demographicsData) {
  demographicsData[, InceptionDateYearMonth := format(demographicsData$InceptionDate, "%Y-%m")]
  demographicsData <- AddAgeGroups(demographicsData)
  demographicsData <- AddNewInvestorFlag(demographicsData, 1)
  demographicsData <- AddNewInvestorFlag(demographicsData, 6)
  demographicsData <- AddNewInvestorFlag(demographicsData, 12)
  return(demographicsData)
}

#' Derive demographics dataset - this would be a function that is called when CRON is running
#' @export
#' @import data.table
#' @import mongolite
DeriveData <- function() {
  demographics <- mongolite::mongo(collection = "demographics", db = "datasets", url = "mongodb://localhost")
  demographicsData <- demographics$find()
  demographicsData <- data.table::as.data.table(demographicsData)
  demographicsData <- AddDerivedData(demographicsData)
  demographicsDerived <- mongolite::mongo(collection = "demographics_derived", db = "datasets", url = "mongodb://localhost")
  demographicsDerived$drop()
  demographicsDerived$insert(demographicsData)
}

#' Get demographics dataset
#' @return The demographics dataset with age group breakdown and new insvestor flag data.table
#' @export
#' @import data.table
#' @import mongolite
GetDataset <- function(platform = NULL) {
  demographics <- mongolite::mongo(collection = "demographics_derived", db = "datasets", url = "mongodb://localhost")
  query <- '{}';
  if (!is.null(platform)) {
    query <- paste0('{"Platform" : "', platform, '"}');
  }
  demographicsData <- demographics$find(query)
  rm(demographics)
  demographicsData <- data.table::as.data.table(demographicsData)
  return(demographicsData)
}

#' Get inception year distribution
#' @return The inception year distribution data.table
#' @export
#' @import data.table
#' @import mongolite
GetInceptionYearDistribution <- function(platform = NULL) {
  demographics <- mongolite::mongo(collection = "demographics_derived", db = "datasets", url = "mongodb://localhost")
  if (!is.null(platform)) {
    query <- paste0('[
      {"$match" : { "Platform": "', platform, '" } },
      {"$group": {"_id":"$InceptionYear", "Count": {"$sum":1}}},
      {"$sort" : { "_id": 1 } }
    ]');
  }
  else{
    query <- '[
      {"$group": {"_id":"$InceptionYear", "Count": {"$sum":1}}},
      {"$sort" : { "_id": 1 } }
    ]';
  }
  demographicsData <- demographics$aggregate(query)
  rm(demographics)
  return(data.table::data.table(InceptionYear = demographicsData$`_id`, Count = demographicsData$Count))
}
