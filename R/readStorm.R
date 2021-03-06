# `criterion` can take the values "rain125", "rain100", "rain75", "rain50", "wind12", "wind15", "wind18"
readStorm <- function(criterion = c(), city = c()){
  root <- "exposure/"
  file <- paste(root, criterion, "/", city, ".rds", sep = "")
  df <- readRDS(file)
  df <- as.data.frame(df)
  df$date <- lubridate::ymd(df$closest_date)
  
  return(df)
}
# example
# miam_75 <- readStorm(rain = "rain75", city = "miam")