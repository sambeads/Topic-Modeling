### this is the clean scraping page

# Author: Sam Beadles
# Email: Sbb5ur@virginia.edu
# Date: 4/2/18

### the purpose of this script is to scrape the newspaper data from the Valley of the Shadow 

# 1: import relevant libraries
library(rvest)
library(stringr)
library(dplyr)

# 2: create an rvest function that takes a URL and outputs the HREFS on that page
hrefs <- function(url){
  ht <- read_html(url)
  hrefs <- ht %>% html_nodes("center center a") %>% html_attr('href')
  return(hrefs)
}

# 3: create variables that are the two-character string that contain the specified URL
papers_chr <- c('vv','rv','ss','fr','sd','vr','vs')

# Loop over these strings and assign them to variables

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

# use read_html to get the HREFS associated with each URL using function hrefs() that we created above
hrefsvv <- hrefs(vv)
hrefsrv <- hrefs(rv)
hrefsss <- hrefs(ss)
hrefsfr <- hrefs(fr)
hrefssd <- hrefs(sd)
hrefsvr <- hrefs(vr)
hrefsvs <- hrefs(vs)

# 4: initialize dataframe for each paper. 
  # Structure: 3 cols (date, origin, text), N rows for each paper
  # First argument is the specified URL
  # Second argument is the stem of the variable that you want
  # Example output: newdfvv for the Valley Virginian dataframe

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


# 5: Loop over the legnth of the HREFS to scrape the data
  # Structure of loop:
    # Wrap in a tryCatch{} so that errors won't stop the loop
    # Access the first HREF on the specified newspaper page
    # Paste that HREF back onto the 'stem' url: in this case, http://valley.lib.virginia.edu/
    # Use Rvest to read the URL into HTML
    # Subset the date from the XML extension (HREF)
      # Note that this process works even when the XML extension is broken/ returns nothing
    # Read in the CSS selector
    # Initialize a bag-of-words into which we will fill the relevant text
    # Iterate over the length of the nodeset: when there are multiple transcriptions in one week,
      # you have multiple items in the list
    # create a new row with the correct two-character paper designation, standardize the names,
      # and bind back to the overall dataframe

# Note that we most likely could have vectorized this process.

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

for (i in 1:length(hrefsrv)){
  tryCatch({
    href = hrefsrv[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'RV',bag,url)
    names(newrow) <- names(newdfrv)
    newdfrv <- rbind(newdfrv,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (i in 1:length(hrefsss)){
  tryCatch({
    href = hrefsss[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'SS',bag,url)
    names(newrow) <- names(newdfss)
    newdfss <- rbind(newdfss,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (i in 1:length(hrefsfr)){
  tryCatch({
    href = hrefsfr[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'FR',bag,url)
    names(newrow) <- names(newdffr)
    newdffr <- rbind(newdffr,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (i in 1:length(hrefssd)){
  tryCatch({
    href = hrefssd[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'SD',bag,url)
    names(newrow) <- names(newdfsd)
    newdfsd <- rbind(newdfsd,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (i in 1:length(hrefsvs)){
  tryCatch({
    href = hrefsvs[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'VS',bag,url)
    names(newrow) <- names(newdfvs)
    newdfvs <- rbind(newdfvs,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

for (i in 1:length(hrefsvr)){
  tryCatch({
    href = hrefsvr[i]
    url <- paste('http://valley.lib.virginia.edu/',href,sep='') 
    html <- read_html(url)
    date <- str_sub(url,-14,-4)
    nodes <- html_nodes(html,'blockquote p')
    paper_text <- html_text(nodes)
    bag <- ''
    for (i in 1:length(paper_text)){
      bag <- paste(bag,paper_text[i])
    }
    newrow <- data.frame(date,'VR',bag,url)
    names(newrow) <- names(newdfvr)
    newdfvr <- rbind(newdfvr,newrow)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# 6: Do some preliminary cleaning/processing

# i: Assign a designation for side. Bind together all dataframes.

confederates <- rbind(newdfvv,newdfrv,newdfss)
confederates$side <- 'CF'

union <- rbind(newdffr,newdfsd,newdfvr,newdfvs)
union$side <- 'UN'

valley_df <- rbind(confederates,union)

# ii:

valley_df$text <- gsub("\\s\\s+",' ',valley_df$text)  # eliminate whitespace that comes from idiosyncractic scraping issues
valley_df$text <- trimws(valley_df$text)              # trim whitespace
valley_df$text <- gsub("\\\"",'',valley_df$text)      # replace \" with ''


# iii: export valley_df to a file in order not to have to run the loops each time.

write.csv(valley_df,file='valley_df_clean.csv') # you will need to customize your path.
