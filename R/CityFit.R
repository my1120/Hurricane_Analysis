CityFit <- function(criterion = c(), city = c(), cause = "all", 
                    control_ratio = 15, lags = 14, storm_id = NA){
  
  df <- CrossoverData(criterion, city, cause, control_ratio, lags, storm_id)
  
  cb <- crossbasis(df$hurr, lag = c(0, lags),
                   argvar = list(fun = "lin"),
                   arglag = list(fun = "ns", knots = c(1, 4, 8)))
  
  city_fit <- glm(df[, cause] ~ cb + ns(year, 2) + stratum, 
             family = quasipoisson(link = log), 
             data = df,
             control = glm.control(epsilon = 10E-8, maxit = 5000))
  
  city_pred <- crosspred(basis = cb, model = city_fit, at = 1)

  fit_city <- list(city_fit = city_fit, city_pred = city_pred)
  return(fit_city)
}

# example
# fit_miam <- CityFit(criterion = "rain75", city = "miam")