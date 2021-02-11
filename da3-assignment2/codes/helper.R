#########################################################################################
# DA3 Assignment 2 
# Helper Script
# 
# GOAL:
# Write functions and set variables that are auxiliary to the analysis script.
#
#########################################################################################


# Styling -----------------------------------------------------------------
# Set Plot Size Dimensions
mywidth=7.5
myheight=5.62

theme_set(theme_bw())

# Create Variable Selection Schema File creator function
vss <- function(df) {
  if (!file.exists("data/variable_selection_schema.csv")) {
    vss_df<-data.frame(vars=colnames(df),example=unlist(lapply(unname(df[1,]),as.character)),use=NA,notes=NA)
    vss_df$na_ratio<-unname(apply(df,2,function(x){
      ar<-sum(is.na(x))/length(x)}))
    vss_df<-vss_df[,c("vars","example","na_ratio","use","notes")]
    write_csv(vss_df,"data/variable_selection_schema.csv",na = "")
    print(vss_df)
  }
  else{
    print("Warning! File already exists.")
  }
}



# discord-webhook function ------------------------------------------------

# define webhook function
send_message <- function(webhookurl = url, my_text = text) {
  library(jsonlite)
  library(rvest)
  
  headers = c(
    `Content-type` = 'application/json'
  )
  tryCatch({
    data= toJSON(list("content"= my_text), auto_unbox = T)
    res <- httr::POST(url = webhookurl, httr::add_headers(.headers=headers), body = data)
  },
  warning = function(w) {
    print('warning!')
  }, error = function(e) {
    print('error!')
  }, finally = {
  }
  )
}

# read in credentials
creds <- read.delim('/mnt/DATA/Projects/BA-20-21/danalysis-3/data/creds.txt', sep = ',') # change the path to your creds.txt file
url <- as.character(creds[creds$key == 'webhookurl', 2])
user <- as.character(creds[creds$key == 'user', 2])

text <- paste0('hello ', user, ' your model has finished training')
