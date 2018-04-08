### this is the clean processing page

# Author: Sam Beadles
# Email: Sbb5ur@virginia.edu
# Date: 4/2/18

# import relevant libraries
library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# change options

options(stringsAsFactors = FALSE)

### the purpose of this script is to clean/process the data that we scraped
  # this processing is idiosyncratic to this dataset: we will perform more 'regular' 
  # processing in the next post

setwd("/Users/samuelbeadles/Desktop/01_UVA_education_related/00_Sem8")

# read in the semi-processed data
valley_df_clean <- read.csv('valley_df_clean.csv',encoding = "UTF-8")

# Take the index, date, paper, text, URL, and side, Filter any NA values in $text
valley_df_clean <- valley_df_clean %>% subset(select = c('X','date','paper','text','url','side')) %>% 
  filter(!is.na(valley_df_clean$text))

# change the date format
valley_df_clean$date <- as.Date(str_sub(valley_df_clean$date,start=1,end=-2),format = "%Y.%m.%d")
# Extract the year
valley_df_clean$year <- year(valley_df_clean$date)

# create a word count of each document
word_count <- sapply(valley_df_clean$text, function(x) str_count(x))
w <- unname(word_count)
valley_df_clean$words <- w

# Visualize the word count column
ggplot()+aes(word_count)+geom_histogram()+
  ggtitle('Graph of number of words shows most shorter than 4000 words')+
  theme_minimal()

# Eliminate non-ASCII characters
valley_df_clean<- valley_df_clean[-grep("NOT_ASCII", iconv(valley_df_clean$text, "latin1", "ASCII", sub="NOT_ASCII")),]
# these indicies! [1]  384  408  833  847  859 1086
# source: https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files

# try to understand the longest entries
valley_df_clean <- valley_df_clean[valley_df_clean$words!=0,] # seem to be grouped in 3's

# filter all entries that do not have the assigned side
valley_df_clean <- valley_df_clean %>% 
  filter(side %in% c('UN','CF'))

# take only entries with more than 100 words
valley_df_shortened <- valley_df_clean[valley_df_clean$words>100,]

# write out CSV
write.csv(valley_df_shortened,'IMP4500_valley_df_clean.csv')



