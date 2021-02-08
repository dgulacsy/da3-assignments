#########################################################################################
# DA3 Assignment 2 
# Helper Script
# 
# GOAL:
# Write functions and set variables that are auxiliary to the analysis script.
#
#########################################################################################

# Set Plot Size Dimensions
mywidth=7.5
myheight=5.62

# Create Variable Selection Schema File creator function
vss <- function(df) {
  if (!file.exists("data/raw/variable_selection_schema.csv")) {
    vss_df<-data.frame(vars=colnames(df),example=unlist(lapply(unname(df[1,]),as.character)),use=NA,notes=NA)
    vss_df$na_ratio<-unname(apply(df,2,function(x){
      ar<-sum(is.na(x))/length(x)}))
    vss_df<-vss_df[,c("vars","example","na_ratio","use","notes")]
    write_csv(vss_df,"data/raw/variable_selection_schema.csv",na = "")
    print(vss_df)
  }
  else{
    print("Warning! File already exists.")
  }
}


