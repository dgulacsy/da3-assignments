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

# Create Feature Selection Schema File creator function
fss <- function(df) {
  fss_df<-data.frame(vars=colnames(df),example=as.character(df[1,]),use=NA,notes=NA)
  write_csv(fss_df,"data/raw/feature_selection_schema.csv",na = "")
  print(df)
}

