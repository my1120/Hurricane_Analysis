CrossoverData <- function(criterion = c(), city = c(), control_ratio = 15, 
                          lags = 14, storm_id = NA){
 # print(city)
  require(lubridate)
  require(dplyr)
  
  ## generate the data
  df <- CityStorm(criterion, city)
  
  if(!is.na(storm_id)){
    ## exclude other storms
    df$hurr[df$storm_id != storm_id] <- 0
  }else{
      df <- df
    }
  
  df$time <- 1:length(df$hurr)
  cand_control <- unique(c(which(df$hurr == 1) , which(df$hurr == 1) + 1,
                           which(df$hurr == 1) - 1))
  
  df$cand_control <- TRUE
  df$cand_control[cand_control] <- FALSE
  
  case_dates <- subset(df, hurr == 1) # hurr is exposure
  control_dates <- subset(df, hurr == 0)
  
  for(i in 1:nrow(case_dates)){
    ## choose lags of storm days (lag0)
    lag_dates <- case_dates[i, ]$date + 1:lags
    lag_case <- subset(df, date %in% lag_dates)
    
    ## choose controls for storm days (lag0)
    control_range <- case_dates[i, ]$doy + -3:3 
    control_subset <- subset(control_dates, 
                             control_dates$year != case_dates[i, ]$year &
                             doy %in% control_range & 
                             cand_control) 
    controls <- sample_n(control_subset, control_ratio)
    
    ## lagged controls
    for(j in 1:lags){
      lag_control_dates <- controls$date + j
      lag_control_each <- subset(df, date %in% lag_control_dates)
      
      if(j == 1){
        lag_control <- lag_control_each
      }else{
        lag_control <- rbind(lag_control, lag_control_each)
      }
    }
    
    i_stratum <- rbind(case_dates[i, ], lag_case, controls, lag_control)
    
    stratum <- paste("stratum", i, sep = ".")
    i_stratum$stratum <- stratum
    
    status <- c(rep("case", lags + 1), rep("control", control_ratio*(lags + 1))) # case: 1 storm day + 14 lagged day
    i_stratum$status <- status
    
    lag <- c(0:lags, rep(0:lags, each = control_ratio))
    i_stratum$lag <- lag
    
    if(i == 1){
      new_df <- i_stratum
    }else{
      new_df <- rbind(new_df, i_stratum)
    }
  }
  
  ## rbind a "fake" data frame with lags = 14 rows and same number of columns as "new_df"
  coln <- ncol(new_df)
  be_data <- as.data.frame(matrix(0, nrow = lags, ncol = coln)) 
  colnames(be_data) <- colnames(new_df)
  
  df_to_mod <- rbind(be_data, new_df)
}

# example
# a <- CrossoverData(criterion = "rain75", city = "miam", storm_id = "Irene-1999")
# b <- CrossoverData(criterion = "rain50", city = "det") # one storm
