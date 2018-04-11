---
title: "Topic-Modeling"
author: "Sam Beadles"
date: "4/10/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

### Introduction

  I designed this project—part code, part analysis, part history—to understand how statistical approaches to text can supplement and guide historical research. Along the way, I’ll discuss my research method, show how choices in the collection and processing of data inform my results, and suggest areas for future research in the digital humanities.
  In the following posts, I use topic modeling—a probabilistic modeling technique—to 
parse free text. I’ve posted all of my relevant R code and interim datasets to a Github repository. I’ll include snippets of codes in my walkthrough. The codebase along with comments should be read-able on its own. I encourage the reader to use whichever set of resources—be it code or historical sources—best helps them comprehend and build on my research. 
  Each part of my work will appeal to different people. My goal is to create a living document that attracts conversation and criticism from many disciplines. In this sense, my thesis isn’t done: it’s still waiting for a contribution from my readers.

# 1: Source of the data

  I conceived of my project from Edward Ayer’s Valley of the Shadow online repository. This archive, an early 1990’s foray into what would become the discipline of the Digital Humanities, contains an array of digitized source materials. Ayers incorporates sources from maps and GIS data to letters and newspapers. The archive focuses on the ante-and-post bellum periods in two towns in the Shenandoah Valley: Franklin County, PA and Augusta County, VA.
	In particular, I’ve chosen to work with the archives of seven Civil War newspapers. These newspapers run from various intervals from to 1857-1870. Four are based in Franklin, PA, three in Augusta, VA. They represent a variety of political positions from Republican to Democratic.
  These newspapers have several benefits that led me to use them specifically. The papers are relatively consistently published (usually weekly), have clearer politics ties, and are more easily accessible in tabulated form. However, their form also has drawbacks. Most importantly, the newspapers have already been curated. Only certain ‘important’ passages have been transcribed by the historians working on Ayers’ project. Most of the main text entries in the original newspapers—such as weddings and obituaries—have been omitted or summarized.


# 2: Scraping the data

In order to do any analysis, I had to collect the data. I used a technique called web-scraping to access and store each week’s worth of newspaper text. In this post, I’ll outline the specific coding practices I used. At the end of the post, I’ll discuss the importance of some of my decisions and their relation to the rest of my work.

Throughout, I use the Rvest library to access and parse HTML. The basic process is to access each individual paper’s categorization by date, access the hypertext references (hrefs) on each page, and collect the words on those pages. The output is a dataframe with four columns: text, URL, paper, and date. I began by importing the relevant libraries. I used rvest to scrape, stringr to do string manipulations, and dplyr to pipe data. 

For a more technical description of the web-scraping loops, please see the code repository.

# Importing libraries

```{r libraries, echo=TRUE,message=FALSE,warning=FALSE}
library(rvest)
library(dplyr)
library(readr)
library(stringr)
```

I began by creating a function that takes URLs as an input and returns the list of HREFs on that URL. This function will help me crawl the 'tree' structure of one page with many hyperlinks.

```{r HREFs, echo=TRUE,message=FALSE,warning=FALSE}
hrefs <- function(url){
  ht <- read_html(url)
  hrefs <- ht %>% html_nodes("center center a") %>% html_attr('href')
  return(hrefs)
}
```
The second step is to assign variables that contain the paper-specific URL. Luckily, the URL varies in a predictable way: the base URL plus the paper designation—usually a very accessible shortening of the name, e.g. VV for Valley Virginian—brings the web-scraping application to the right spot. 

```{r assign, echo=TRUE,message=FALSE,warning=FALSE}
papers_chr <- c('vv','rv','ss','fr','sd','vr','vs')

for ( i in papers_chr){
  # copy the main/stem URL
  base <- 'http://valley.lib.virginia.edu/news-calendar/?paper='
  # create a character string that is the base URL + the two letter paper designation
  # we have to manually set 'sd' to a variable because assign seems to recognize 'sd'
    # as the base standard deviation function
  if (i == 'sd'){
    sd <-'http://valley.lib.virginia.edu/news-calendar/?paper=sd'
  }else{
    var_input <- paste(base,i,sep='')
    # assign the paper-name string (e.g. 'vv') to a variable in the global environment
    assign(i,var_input,inherits=TRUE)}
}

# create a vector of the variables that represent the base URL + two character paper designation
papers <- c(vv,rv,ss,fr,sd,vr,vs)
```

Notice that creating functions-- instead of copy-and-pasting, or even vectorizing the action-- creates clean, reproducible, and flexible code.

I will initialize the dataframe that will hold the data from each scraping iteration. Note that a loop was necessary because the string "sd" brought up issues-- base R wanted to recognize it as the standard deviation function and failed to assign it a value.

```{r dataframermaker, echo=TRUE,message=FALSE,warning=FALSE}

dataframemaker <- function(pastername,base_name = 'newdf'){
  df <- data.frame(matrix(ncol=4,nrow=0),stringsAsFactors = FALSE)
  colnames(df) <- c('date','paper','text','url')
  thing <- paste(base_name,pastername,sep='')
  assign(thing,df,inherits=TRUE)
  return(df)
}

# run the dataframe making function for each paper two-character name
for (i in papers_chr){
  dataframemaker(i)
}

```

Lastly, I will run my original 'HREFs' scraping function on each of the URLs to create vectors-- all of different lengths-- that contain the HREFs for each newspaper page.

```{r HREFs_action, echo=TRUE,message=FALSE,warning=FALSE}
hrefsvv <- hrefs(vv)
hrefsrv <- hrefs(rv)
hrefsss <- hrefs(ss)
hrefsfr <- hrefs(fr)
hrefssd <- hrefs(sd)
hrefsvr <- hrefs(vr)
hrefsvs <- hrefs(vs)

```

The final step is to implement loops over each newspaper. I will only give an example of one loop along with an explanation: I had to copy the same loop several times and change the parameters for each loop. In order to run this code, see the code repository.

```{r loops, echo=TRUE,message=FALSE}
for (i in 1:length(hrefsvv)){
  tryCatch({
    href = hrefsvv[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'VV',bag,url)
    names(newrow) <- names(newdfvv)
    newdfvv <- rbind(newdfvv,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

```

  Notice, also, that web-scraping forces me to deal with many edge cases. I had to wrap the entire loop in a tryCatch  call because sometimes the URL linked to a 404 Page Not Found error. Looping over all papers at one time would make such an error much harder to identify and fix. 
  
 To finish out my scraping, I did some idiosyncratic cleaning. The HTML text that I scraped had filler text of the following form: “backslash    “ with varying numbers of trailing spaces. I used regex to find and replace all “backslash” characters followed by trailing spaces with an empty string, “”. I also added their ‘side’ to the dataframe: CF for Southern papers and UN for Northern papers. 
 
```{r cleaning, echo=TRUE,message=FALSE, eval=FALSE}
confederates <- rbind(newdfvv,newdfrv,newdfss)
confederates$side <- 'CF'

union <- rbind(newdffr,newdfsd,newdfvr,newdfvs)
union$side <- 'UN'

valley_df <- rbind(confederates,union)

valley_df$text <- gsub("\\s\\s+",' ',valley_df$text)  # eliminate whitespace from idiosyncractic scraping issues
valley_df$text <- trimws(valley_df$text)              # trim whitespace
valley_df$text <- gsub("\\\"",'',valley_df$text)      # replace \" with ''
```

## Discussion

  Choices in this data collection stage have long term impact on the eventual analysis. Specifically, the format of the website limits the granularity of my scraping. The structure of the website prevents me from distinguishing between different entries. With the human eye, the distrinction is obvious: the header “Full Text” heads each transcribed portion. However, the Rvest can only distinguish between paragraphs: we can have either many paragraph-sized entries, or an aggregation of the entire week.
 
  At the end of this script, I wrote out my data into an Excel sheet. This ‘data storage’ approach has two benefits. First, as I described, it saves time by not requiring me to run the script every time I want to work. Second, it allows me to see the data in a format that sometimes—as in my case—leads me to understand some issues.

  Notice that my choices—or my lack of choices—at this point will drive later analysis. Most importantly, my scraping machine forces me to aggregate all entries in a single week and treat them as one document. Second, the curated nature of the documents means that I can use topic modeling not to infer about what 19th century newspapers wrote generally, but only to infer through the lens of what 21st century historians deemed important enough to transcribe. 

