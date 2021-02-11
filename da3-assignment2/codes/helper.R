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

#createLossPlot <- function(r, best_coords, file_name,  mywidth_large=12, myheight_large = 9) {
createLossPlot <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]
  n <- rowSums(best_coords[c("tn", "tp", "fn", "fp")])[1]
  
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  all_coords <- all_coords %>%
    mutate(loss = (fp*FP + fn*FN)/n)
  l <- all_coords[all_coords$threshold == t, "loss"]
  
  loss_plot <- ggplot(data = all_coords, aes(x = threshold, y = loss)) +
    geom_line(color="green", size=0.7) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_vline(xintercept = t , color = "red" ) +
    annotate(geom = "text", x = t, y= min(all_coords$loss),
             label=paste0("best threshold: ", round(t,2)),
             colour="blue", angle=90, vjust = -1, hjust = -0.5, size = 7) +
    annotate(geom = "text", x = t, y= l,
             label= round(l, 2), hjust = -0.3, size = 7)
  loss_plot
}

createRocPlotWithOptimal <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]
  
  roc_plot <- ggplot(data = all_coords, aes(x = specificity, y = sensitivity)) +
    geom_line(color="green", size=0.7) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_x_reverse(breaks = seq(0, 1, by = 0.1)) +
    geom_point(aes(x = sp, y = se)) +
    annotate(geom = "text", x = sp, y = se,
             label = paste(round(sp, 2),round(se, 2),sep = ", "),
             hjust = 1, vjust = -1, size = 7)
  #  + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
  #          axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
  
  #  ggsave(plot = roc_plot, paste0(file_name, ".png"),         width=mywidth_small, height=myheight_small, dpi=1200)
  # cairo_ps(filename = paste0(file_name, ".eps"),           width = mywidth_small, height = myheight_small, pointsize = 12,           fallback_resolution = 1200)
  #print(roc_plot)
  #dev.off()
  
  roc_plot
}

# Calibration plot
create_calibration_plot <- function(data, file_name, prob_var, actual_var, y_lab = "Actual event probability" , n_bins = 10, breaks = NULL) {
  
  if (is.null(breaks)) {
    breaks <- seq(0,1,length.out = n_bins + 1)
  }
  
  binned_data <- data %>%
    mutate(
      prob_bin = cut(!!as.name(prob_var), 
                     breaks = breaks,
                     include.lowest = TRUE)
    ) %>%
    group_by(prob_bin, .drop=FALSE) %>%
    summarise(mean_prob = mean(!!as.name(prob_var)), mean_actual = mean(!!as.name(actual_var)), n = n())
  
  p <- ggplot(data = binned_data) +
    geom_line(aes(mean_prob, mean_actual), color="green", size=0.6, show.legend = TRUE) +
    geom_point(aes(mean_prob,mean_actual), color = "green", size = 1, shape = 16, alpha = 0.7, show.legend=F, na.rm = TRUE) +
    geom_segment(x=min(breaks), xend=max(breaks), y=min(breaks), yend=max(breaks), color="red", size=0.3) +
    labs(x= "Predicted event probability",
         y= y_lab) +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
    expand_limits(x = 0.01, y = 0.01) +
    scale_y_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) +
    scale_x_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) 
  
  # save_fig(file_name, output, "small")
  print(p)
  results=list(plot=p,bins=binned_data)
  return(results)
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
