##################################### Meta ##########################################

# High Frequency Code template
# 10th January 2017
# Contributors : Krishanu Chakraborty
# Originally written in STATA by Saurabh (J-PAL SA)
# Version of code : 1.0.0
# R version : R version 3.3.2 (Sincere Pumpkin Patch)
# Last edited by : Krishanu Chakraborty

#################################### Introduction ###################################

# install packages if necessary 
# install.packages(c("foreign", "haven","dplyr", "VIM", "outliers", "ggplot2", "scales", "grid",    "RColorBrewer", "psych))
# load the required packages

library(foreign)
library(haven)
library(dplyr)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)

# get the name of the OS

get_os <- function()
{ 
  sysinf <- Sys.info()
  if (!is.null(sysinf))
  {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else 
    { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
    }
  tolower(os)
}

os_name <- get_os()

if (os_name == "windows")
{
  print("Your system is Windows")
  setwd("D:/R/HFC/HFC-2")                 # put folder location in Windows format
} else if (os_name == "osx") 
{
  print("Your system is OSX")
  setwd("")                               # put folder location in OSX format
}

# getting the data

dummy_data <- read.dta("dummy_main.dta")

################### Checking for missing Unique IDs #################################

# The Unique ID cannot be missing. This should be taken care of during programming 

# For directly printing 'NA' (missing in R) for the unique variable surveyor_id. Please note that this template uses surveyor_id for demonstration purpose. Please feel free to change as required

dummy_data_summary = summary(dummy_data$surveyor_id)
#dummy_data_summary
paste("surveyor_id has", dummy_data_summary[7] ,"missing value(s)", sep = " ")

# To see all values that are missing for each of the variable.

for (Var in names(dummy_data)) {
  missing <- as.numeric(sum(is.na(dummy_data[,Var])))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# percentage missing

colMeans(is.na(dummy_data))

# alternate way : missing <- sapply(airquality, function(x) sum(is.na(x)));
# listing information for missing surveyor IDs

paste("Row Number =", which(is.na(dummy_data$surveyor_id) == TRUE), "Surveyor ID =", dummy_data$surveyor_id[is.na(dummy_data$surveyor_id) == TRUE], "Start Time =", dummy_data$starttime[is.na(dummy_data$surveyor_id) == TRUE], sep=" ")

# Tip : structure of missing values in the data frame graphically

aggr(dummy_data)

############################ Duplicates in Unique IDs ###############################

# Finding the number of duplicates in surveyor_id

dummy_data <- dummy_data %>%
  arrange(surveyor_id) %>% ## sorted surveyor id
   group_by(surveyor_id) %>%
    mutate(dup=row_number())

# finding the number of duplicates. 

sum(dummy_data$dup > 0)

# Please note that the STATA template shows this as 481
# instead of 483. The same output can also be achieved easily but is not mandatory for high frequency # checks

dummy_data <- dummy_data %>%
  arrange(surveyor_id) %>% ## sorted surveyor id
   group_by(surveyor_id) %>%
    mutate(N=n())

sum(dummy_data$N > 1)

# Arranging the data frame for review

print("Surveys with duplicates")

dummy_data_temp = subset(dummy_data, dummy_data$dup>0 & is.na(dummy_data$surveyor_id)== FALSE)
subset(dummy_data_temp, select= c("surveyor_id", "starttime","dup" ))

# free up memory by using rm(). This is currently not implmented because the dummy_data_temp
# is used as a proxy for master tracking sheet

# rm(dummy_data_temp)                       

############ Unique ID matches with master tracking list & field tracking lists #####

# creating a storeholder for all variables except unique ID

variable_list_master <- names(dummy_data)
variable_list_master <- variable_list_master[variable_list_master != "surveyor_id"]

# creating variable list from tracking sheet. Ensure that the ID variable here has the same name
# as that in your master data

variable_list_using <- names(dummy_data_temp)   # use the tracking sheet here
variable_list_using <- variable_list_using[variable_list_using != "surveyor_id"]

# Checking for variables with same name in master & using. It's a
# good habit to check for such common varnames to avoid losing 
# necessary variables from using data 

for(i in 1:length(variable_list_master)) 
{
  for(j in 1:length(variable_list_using)) 
  {
    if(variable_list_master[i] == variable_list_using[j]) 
    {
      print(paste("Variable", variable_list_master[i] ,"has same name in both master and using", sep= " "))
    }
  }
}

# If such a detailed analysis is not required the follwoing commands can also be used

all.equal(variable_list_master, variable_list_using)
identical(variable_list_master, variable_list_using)
which(variable_list_master != variable_list_using)

# Check the merge results thoroughly and carry out further checks as needed

# merge(dummy_data, dummy_data_temp)
# Please be careful with all.x and all.y.

################################### Date and Time Checks ############################

# Surveys that don't end on the same day as they started

subset(subset(dummy_data, as.Date(dummy_data$starttime) != as.Date(dummy_data$endtime)), select= c("surveyor_id", "starttime","endtime"))

# Surveys where end date/time is before start date/time

subset(subset(dummy_data, as.Date(dummy_data$starttime) > as.Date(dummy_data$endtime)), select= c("surveyor_id", "starttime","endtime"))

subset(subset(dummy_data, strftime(dummy_data$starttime, format="%H:%M:%S") > strftime(dummy_data$endtime, format="%H:%M:%S") & as.Date(dummy_data$starttime) > as.Date(dummy_data$endtime)), select= c("surveyor_id", "starttime","endtime"))

# Surveys that show starttime earlier than first day of data collection

subset(subset(dummy_data, as.Date(dummy_data$starttime, "%m/%d/%y") < as.Date("4/21/16", "%m/%d/%y")), select= c("surveyor_id", "starttime","endtime")) # 21st April 2016 is just an example. Replace it with start date of your data collection

# Surveys that have starttime after system date (current)

subset(subset(dummy_data, as.Date(dummy_data$starttime) > Sys.Date()), select= c("surveyor_id", "starttime","endtime"))

#################################### Distribution ###################################

# Missing Values
# 
# Variables with all observations missing 

missing_all <- sapply(dummy_data, function(x) sum(is.na(x)))
missing_all[missing_all == nrow(dummy_data)]

# Missing value percentages for remaining variables (which don't have all values missing)

# Option : 1
# 
# for(Var in names(dummy_data))
# {
#   if(sapply(dummy_data[, Var], class) == "character")
#    print(colMeans(is.na(dummy_data[, Var])))
# }

print("Displaying percent missing in non-numeric variables") 
colMeans(is.na(dummy_data[sapply(dummy_data, class) == "character"]))*100
colMeans(dummy_data[sapply(dummy_data, class) == "character"]== "")*100 # this is for a non - NA blank field

print("Displaying percent missing in numeric variables") 
colMeans(is.na(dummy_data[sapply(dummy_data, class) != "character"]))*100

################################## Number of distinct values ########################

# Pay attention to variables with very few distinct values. 
# Lack of variation in variables is an important flag to be raised and discussed with the PIs. 

n_distinct_no_na <- function(x) n_distinct(x[!is.na(x)])
sapply(dummy_data, n_distinct_no_na)

########### Distribution of specific coded values (don't know, refused, other etc.) ######

# "-999" is used as and example. Run this for all the codes in your survey

# For numeric variables

colMeans(dummy_data[sapply(dummy_data, class) != "character"] == -999, na.rm = TRUE)*100

# For non-numeric variables

colMeans(dummy_data[sapply(dummy_data, class) == "character"] == "-999", na.rm = TRUE)*100

# Note: if you want to run 
# colMeans(dummy_data[sapply(dummy_data, class) != "character"] == "-999", na.rm = T)
# you might face this error : Error in as.POSIXlt.character(x, tz, ...) : character string is not in a
# standard unambiguous format
# use as.character.POSIXt() on POSIXt variable

################################## Outliers #########################################

# Here z-score is taken. Appropriate functions can be defined

scores_na <- function(x) scores(x[!is.na(x) & x > -1], type = "z")
scores_outliers <- sapply(dummy_data[sapply(dummy_data, class) != "character"], scores_na)

# Run the following line of code if you need only the numeric type variable
# scores_outliers <- scores_outliers[sapply(scores_outliers, class) == "numeric"]
scores_outliers <- scores_outliers[sapply(scores_outliers, length) > 0]

for(i in 1:length(scores_outliers))
{ 
  for(j in 1:length(scores_outliers[[i]]))
  { 
    #cat("i=", i, "j=", j, "\n", sep = " ")
    if(!is.na(scores_outliers[[i]][j])) 
      if(abs(scores_outliers[[i]][j]) > 2) # the value of 2 sds is used here as an example. You will have to come up with a suitable threshold for your data
         cat("Variable = ", names(scores_outliers)[i], "Z-score = ", scores_outliers[[i]][j], "\n")
  }
}  

########################################### Survey Duration #########################

# Use the following four lines of code only if you haven't used the SurveyCTO generated .do file. 
# Then replace the "starttime" and "endtime" variables in duration calculations below with "start" and "end"
# gen start = clock(starttime,"MDYhms",2025)
# format %tc start
# 
# gen end = clock(endtime,"MDY",2025)
# format %tc end
# 

# Let us get the duration of each survey

dummy_data$duration <- round(as.numeric(dummy_data$endtime - dummy_data$starttime))
duration_outliers <- scores(dummy_data$duration[!is.na(dummy_data$duration)], type = "z")

for (i in 1:length(duration_outliers))
{
  if(abs(duration_outliers[][i]) > 2) 
    cat("Z Scores =", duration_outliers[][i], "Duration =", dummy_data$duration[i], "\n")
}

################################ Enumerator checks ##################################

# Enumerator level average survey duration

overall_avg_duration <- mean(dummy_data$duration[!is.na(dummy_data$duration)])

dummy_data %>%
  group_by(surveyor_id) %>%
   summarise(duration_mean = mean(duration), overall_avg_duration, perc_diff_avg = ((duration_mean - overall_avg_duration)/overall_avg_duration)*100)

# Enumerator level distribution checks

dummy_data %>%
  group_by(surveyor_id) %>% 
   summarise_each(funs(sum(is.na(.))))   # for missing values


dummy_data %>%
  group_by(surveyor_id) %>% 
  summarise_each(funs(n_distinct_no_na(.)))  # for distinct values

dummy_data %>%
  group_by(surveyor_id) %>% 
  summarize_each(funs(sum(dummy_data[sapply(dummy_data, class) != "character"] == -999))) 
# -999 is used as and example. Run this for all the codes in your survey

dummy_data %>%
  group_by(surveyor_id) %>% 
    summarize_each(funs(sum(dummy_data[sapply(dummy_data, class) == "character"] == "-999")))
# "-999" is used as and example. Run this for all the codes in your survey

############################################ Productivity ###########################

# Summary of daily average productivity 

dummy_data <- dummy_data %>%
  arrange(surveydate) %>% ## survey date
   group_by(surveydate) %>%
    mutate(surveydate_N=n())

describe(unique(dummy_data$surveydate_N))

# Overall Productivity Histogram

ggplot(dummy_data, aes(dummy_data$surveydate)) +
  geom_histogram(binwidth=1)

# Fun part. let us make the graph more interesting with Five Thirty Eight theme. Thanks to @minimaxir:-)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

ggplot(dummy_data, aes(dummy_data$surveydate)) +
  geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
  fte_theme() +
  labs(title="Number of surveys", x="Date", y="Number of surveys") +
  geom_hline(yintercept=0, size=0.4, color="black")

# Try with other theme plots!

# Enumerator level productivity

dummy_data %>%
  group_by(surveyor_id) %>% 
   summarise(days_worked = length(unique(`surveydate`)), total_surveys_done = n(), daily_average = total_surveys_done/days_worked)

# End
