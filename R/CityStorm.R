CityStorm <- function(criterion = c(), city = c()){
  # Get health data
  h.df <- readCity(city)
  # Get storm data
  s.df <- readStorm(criterion, city)
  
  storm_days <- s.df$date
  
  df <- left_join(h.df, s.df, by = "date") %>%
    mutate(hurr = ifelse(date %in% storm_days, 1, 0))
  df$hurr <- as.factor(df$hurr)
  
  # Add abbreviate and full cityname
  df$city <- city
  sub.county <- readRDS("data/sub.county.rds")
  
  cityname <- sub.county$citynameU[sub.county$city == city]
  df$cityname <- cityname
  
  #df$time <- scale(as.numeric(df$date), center = TRUE, scale = FALSE)
  #n.years <- length(unique(as.POSIXlt(df$date)$year))
  df$year <- year(df$date)
  df$doy <- yday(df$date)
  
  # Total death
  df$all <- df$accident + df$death
  
  return(df)
}
# example
# miam_df75 <- CityStorm(criterion = "rain75", city = "miam")