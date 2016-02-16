

library(gdata)
library(dplyr)

d <- tbl_df(read.csv("./data/pilot.csv", stringsAsFactors = FALSE))
idx <- tbl_df(read.csv("./data/info.csv", stringsAsFactors = FALSE))

d$ofcextentm[d$ofcextentm == 95] <- NA
d$ofcextenty[xor(d$ofcextenty == 98, d$ofcextenty == 95)] <- NA
d$mahaearn[d$mahaearn == 2000000] <- NA
d$yalaearn[d$yalaearn == 2000000] <- NA
d$paddyextenty[d$paddyextenty == 98] <- NA
d$paddyyieldm[d$paddyyieldm == 999] <- NA
d$totalincome[d$totalincome == 999] <- NA
d$mahacost[d$mahacost == 750000] <- NA
d$yalacost[d$yalacost == 750000] <- NA


c1 %>% group_by(irrigtype, ADP1_F1) %>% 
  summarise(n = n())

#COMMUNITY GROUPED DATA
d %>% group_by(HI4) %>%
  #summarise_each(funs(mean), AllPaddyAcres, ChenaAcres, HighlandAcres)
  #table(c1$agrowell_owner)
  
  #summarise_each(mean(., na.rm = TRUE), sd(., na.rm = TRUE), c1$agrowell_owner)

  summarise_each(n_distinct(), agrowell_owner)

count(distinct(d), tractor2o, tractor4o)
  
#PLOT/TABLES
write.table(with(c1, table(ADP1_A1, ADP1_B1)), "clipboard", sep="\t", row.names=T)
#ggplot(data = d, aes(x = community, y = bethmaeffective, fill = bethma)) + 
  #geom_bar(stat = "identity", position = position_dodge())

#BETHMA
bm <- select(d, contains("bethma")) 

#DPLYR actions

#count number of rows in each GN and arrange in descending order
d%>%
  group_by(community, gn)%>%
  summarise(gn_count = n())%>%
  arrange(desc(gn_count))
#or simpler version
d%>%
  group_by(community, gn)%>%
  tally(sort=TRUE)

#find the distinct categories that compose column
d%>%
  group_by(community)%>%
  summarise(dwsource = n(), dwo = n_distinct(dwo))

#just group - look at water ownership by group
d%>%
  group_by(HI4)%>%
  select(agrowell_user)%>%
  table() %>%
  head()

#randomly sample fixed number of rows
d %>% sample_n(10)




#extract data by table

extract_data <- functoin(df, table){
  if (is.null(questions)){
    columns <- NULL
  } else {
    columns <- table$
  }
}


#pulling out questionnaire data
extract_data <- function(df, questions, include_site = FALSE, include_gender = FALSE){
  if (is.null(questions)) {
    columns <- NULL
  } else {
    columns <- idx[row.names(idx) == 5]
    columns <- questions$SPSS_header
  }
  if (include_site && ! site_no_header %in% columns) {
    columns <- c(site_no_header,columns)
  }
  if (! barcode_header %in% columns) {
    columns <- c(barcode_header,columns)
  }
  if (include_gender && ! gender_header %in% columns) {
    columns <- c(gender_header, columns)
  }
  fltr <- substitute(! is.na(site_no) &  s == 'Completed',
                     list(s = as.name(interview_status_header)))
  filtered_survey <- data_set[with(data_set, eval(fltr)),]
  df <- as.data.frame(fix_labels(filtered_survey[,columns]))
  row.names(df) <- df$barcode_number_1.21
  if (! include_barcode && ! barcode_header %in% questions$SPSS_header) {
    df <- df[,names(df) != barcode_header, drop=FALSE]
  } else if (short_barcode_name) {
    names(df)[names(df) == barcode_header] <- short_barcode_header
  }
  invisible(df)
}

filter_missing <- function(df, questions = NULL, skip = NULL) {
  if (is.null(skip)) {
    skip <-special_values()
  }
  if (is.null(questions)) {
    columns <- names(df)
  } else {
    columns <- questions$SPSS_header
  }
  for(c in columns) {
    df[df[,c] %in% skip,c] <- NA
  }
  invisible(df)
}




#cohort 1
library(foreign)

c1 <- read.spss('./c1/c1.sav', use.value.labels = F, to.data.frame = F)
c1 <- as.data.set(spss.system.file('./c1/c1.sav'))
write.table(c1, file = './c1/c1.csv')
#duplicate in file system

##extract_data_frame <- takes a dataset adn gives it a list of questions, all fltr stuff (fltr <- substitute), as.name turns quoted string into varname




#plothhfinance
#religion plot (religion by other variables)
#expense plot
#pull categories from Questionnaire Table - process_religion function
#if true/false instead of commenting
#packagename::function to call sub functions
#plot_env_gender look at functions

#plot_env has all the plotting
#edward tufte polysci and graphic arts  #visual display of quantitative data
#visual explanations, envisioning information
###VISUAL EXPLANATIONS
#cognitive style of powerpoint
#communities not answering questions


#new ways to sort/clean data
#look at time series for Martin/John look at data processing and cleaning, using dplyr %>% is like piping in linux, string functions together
#mutate add a new column, 
#lots of little scripts, summarize, group_by

#class example that's a bit messier with dplyr adn tidyr <- four inconsistent spreadsheets, reads in different datasets
#dplyr functins called intersect union and setdif <- set dif prints out the elements that are not in both datasets
#organizes, processes, adn merges into one dataframe

#BA code <- messier
#directory of scripts used to make cool climate 

#ggplot - hadley wickham <- geom_bar
#docs.ggplot2.org

#as.numeric(as.character(factor_var)) <- else factor index is transformed
#ordered factors, ordinal data 

#look at Martin's code - climateModel.R (OOP) The Art of R Programming (S3, S4 OOP) S3 easier
#ask John about OOP in R
#documentation isn't great, better to do in Python
#mostly building functions, variable type more important 
