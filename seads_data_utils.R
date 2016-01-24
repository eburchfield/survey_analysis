#rm(list = ls())
#could add data processing/cleaning functions for spss dataset


#work on extract_q, and extract_data_frame

library(memisc)
library(stringr)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)

#add headers for VOI
community <- 'community'
gn <- 'gn'
gender <- 'gender'
irr_type <- 'IrrigationType'
aid <- 'adaptID'

data.dir = './data'

#loads .Rda file - WORKING
load_data <- function(force_reload = FALSE, force_reprocess = FALSE, data.dir = './data') {
  if (force_reprocess) {
    source('load_seads_data.R')
    process_file()
  } else if (force_reload || ! exists('seads_data')){
    print("Loading data")
    load('seads_data.Rda', envir= globalenv() )
  }
}

#loads idx table - WORKING
load_idx <- function(force = FALSE, data.dir = './data') {
  if (force || ! exists('idx')) {
    idx <- read.csv(file.path(data.dir, 'info2.csv'),stringsAsFactors=FALSE)
    idx <<- idx
  }
}

#works
load_if_necessary <- function(force = FALSE) {
  load_data(force_reload = force)
  load_idx(force = force)
  #load_religion(force = force)
}



#summary for each community of variable
#??? will this work if my data aren't factors but characters stringsAsFactors = False
summarize_columns <- function(ds, col) {
  x <- data.frame(site = unique(ds$community))
  for (lev in levels(df[,col])) {
    y <- list(unlist(lapply(x$site, function(site) {
      sum(df$community == site & df[,col] == lev, na.rm=T)
    })))
    names(y) <- lev
    x <- cbind(x, y)
  }
  invisible(x)
}


#extracting variables based on values in idx spreadsheet
#isn't working
extract_q <- function(idx, ...) {
  idx[with(idx, ...),]
}


extract_data_frame <- function(ds, idx, include_site_no = FALSE, include_gender = FALSE, 
                               include_irr_type = FALSE, include_aid = FALSE) {
  if (is.null(idx)) {
    columns <- NULL
  } else {
    columns <- idx$vname
  }
  #add additional data to dataframe
  if (! adaptID %in% columns) {
    columns <- c(adaptID,columns)
  }
  if (include_site_no && ! community %in% columns) {
    columns <- c(community,columns)
  }
  if (include_gender && ! gender %in% columns) {
    columns <- c(gender, columns)
  }
  if (include_irr_type && ! irr_type %in% columns) {
    columns <- c(irr_type, columns)
  }
  
  df <- as.data.frame(columns)
  row.names(df) <- ds$adaptID
  if (! include_aid && ! adaptID %in% idx$vname) {
    df <- df[,names(df) != adaptID, drop=FALSE]
  } #else if (short_barcode_name) {
    #names(df)[names(df) == barcode_header] <- short_barcode_header
  #}
  invisible(df)
}



filter_missing <- function(df, idx = NULL) {
  if (is.null(idx)) {
    columns <- names(df)
  } else {
    columns <- idx$vname
  }
  invisible(df)
  }

filter_quantitative <- function(df, idx, max = NULL, min = NULL) {
  quant_questions <- idx[idx$class %in% c('Float','Integer'),]
  df <- filter_missing(df, quant_questions)  #filters none for now
  for(c in quant_questions$vname) {
    df[,c] <- as.numeric(levels(df[,c])[df[,c]])
    if (! is.null(max)) {
      df[! is.na(df[,c]) & df[,c] > max,c] <- NA
    }
    if (! is.null(min)) {
      df[! is.na(df[,c]) & df[,c] < min,c] <- NA
    }
  }
  invisible(df)
}