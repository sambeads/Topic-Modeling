### this is the clean LDA analysis page

# Author: Sam Beadles
# Email: Sbb5ur@virginia.edu
# Date: 4/2/18

# The purpose of this script is to prepare the data for LDA analysis and run the analysis

# import libraries
library(dplyr)
library(tm)
require(ggplot2)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(LDAvis)
library(RColorBrewer)

setwd('/Users/samuelbeadles/Desktop/01_UVA_education_related_00_Sem8/')

# Section i:import processed document: eliminate documents less than 100 words

valley_df_shortened <- read.csv('/Users/samuelbeadles/Desktop/01_UVA_education_related/00_Sem8/IMP4500_valley_df_clean.csv')

valley_tm <- VCorpus(VectorSource(valley_df_shortened$text))

# convert the text to document term matrix
  # Source: https://rstudio-pubs-static.s3.amazonaws.com/162701_a953e29532f74a4db8a98329de7d323b.html

# use functions in tm() to 
  # lowercase
  # remove punctuation
  # remove numbers
  # strip whitespace
  # optionally: remove stopwords
  # optionally: stem

Corpus_cleaner <- function(tm_obj,stopwords=FALSE){
  tm_obj <- tm_map(tm_obj,content_transformer(tolower))
  tm_obj <- tm_map(tm_obj,removePunctuation)
  tm_obj <- tm_map(tm_obj,removeNumbers)
  tm_obj <- tm_map(tm_obj,stripWhitespace)
  if(stopwords){
    tm_obj <- tm_map(tm_obj,removeWords,c(stopwords("english"),"will",'shall'))
  }
  return(tm_obj)
}

# run the function on the VCorpus to get a cleaner Vcorpus object
clean_withoutstopwords <- Corpus_cleaner(valley_tm,stopwords=TRUE)

# create a DocumentTermMatrix from the VCorpus object
dtm <- DocumentTermMatrix(clean_withoutstopwords)

# section ii: fit the model using tm(). Choose appropriate K.

LDA_topics20   <- LDA(x=dtm,k=20)

# Save and load model

saveRDS(LDA_topics20, "LDA_topics20.rds")
LDA_topics20 <- readRDS("LDA_topics20.rds")

LDA_topics20 <- readRDS('/Users/samuelbeadles/Desktop/01_UVA_education_related/00_Sem8/LDA_topics20model.rds')

# explore the outputs:

terms(LDA_topics20,k=10)
topics(LDA_topics20,k=1,.1)

# take out
# section iii: Visualizing results
  # LDAvis: vignettes? https://cran.r-project.org/web/packages/LDAvis/vignettes/details.pdf
  # Source of large topicmodels_json_ldavis fxn:http://christophergandrud.blogspot.com/2015/05/a-link-between-topicmodels-lda-and.html

terms(LDA_topics20,k=10) # topic 20 looks like gov, topic 14 looks good

topics(LDA_topics20,k=1)[topics(LDA_topics20,k=1)==14]

# take out

  # source: https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html

LDA_td <- tidy(LDA_topics20) # probabilities are very low

top_terms <- LDA_td %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# visualize...

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





