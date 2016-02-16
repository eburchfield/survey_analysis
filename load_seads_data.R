#rm(list = ls())

#add SPSS if statement for future files

library(memisc)
library(dplyr)
library(stringr)


source('seads_data_utils.R') 

process_p <- function(filename = 'pilot.csv', data.dir = './data') {
  if (! is.null(data.dir)) filename = file.path(data.dir,filename)
  
  #add if/then for sav file
  #ds <- as.data.set(spss.system.file(filename))

  ds <- tbl_df(read.csv("./data/pilot.csv", stringsAsFactors = FALSE))
  save(ds, file="p_data.Rda")
  
  numbers <- unlist(lapply(str_split(names(ds), ' '), function(x) x[[length(x)]]))
  names(numbers) <- 1:length(numbers)
  #numbers <- numbers[grepl('^[0-9]', numbers)]
  
  #print(names(ds)[as.integer(names(numbers))])
  write.table(names(ds)[as.integer(names(numbers))], file = 'p_names.txt', row.names=FALSE, col.names=FALSE)
  
  p <<- ds
  p_names <<- names(ds)
}

process_c1 <- function(filename = 'c1.csv', data.dir = './data'){
  if (! is.null(data.dir)) filename = file.path(data.dir,filename)
  
  #add if/then for sav file
  #ds <- as.data.set(spss.system.file(filename))
  
  ds <- tbl_df(read.csv("./data/c1.csv", stringsAsFactors = FALSE))
  save(ds, file="c1_data.Rda")
  
  numbers <- unlist(lapply(str_split(names(ds), ' '), function(x) x[[length(x)]]))
  names(numbers) <- 1:length(numbers)
  #numbers <- numbers[grepl('^[0-9]', numbers)]
  
  #print(names(ds)[as.integer(names(numbers))])
  write.table(names(ds)[as.integer(names(numbers))], file = 'c1_names.txt', row.names=FALSE, col.names=FALSE)
  
  c1 <<- ds
  c1_names <<- names(ds)
}

process_c1()



