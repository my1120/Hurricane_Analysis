
pull_city_coef <- function(CityFit){
  city_coef <- coef(CityFit[["city_pred"]])
  return(city_coef)
}

pull_city_vcov <- function(CityFit){
  city_vcov <- vcov(CityFit[["city_pred"]])
  return(city_vcov)
}

## intercept only meta-regression
UniMetaReg <- function(city_list = c(), criterion = c(), cause = "all", method = "reml"){
  
  city_fit <- lapply(city_list, CityFit, criterion = criterion, cause = cause)
  city_coefs <- lapply(city_fit, pull_city_coef)
  city_coefs <- do.call("rbind", city_coefs)
  
  city_vcovs <- lapply(city_fit, pull_city_vcov)

  meta_fit <- mvmeta::mvmeta(city_coefs ~ 1, S = city_vcovs, method = method)
  
  return(meta_fit)
}

# example
# city_list <- c("no", "lkch", "miam", "jckv", "mobi")
# meta <- UniMetaReg(city_list = city_list, criterion = "rain75", cause = "accident")

# city_list <- gsub(".rds", "", list.files("exposure/rain125"))
# meta125 <- UniMetaReg(city_list = city_list, criterion = "rain125", cause = "accident")
##### Error ######
#Error in chol.default(X[[i]], ...) : 
#  the leading minor of order 29 is not positive definite


pred_meta <- function(meta_model, exposure = c()){
  hurr_basis <- dlnm::crossbasis(x = exposure, 
                                 lag = c(0, 14),
                                 argvar = list(fun = "lin"),
                                 arglag = list(fun = "ns", knots = c(1, 2, 4, 8)))
  meta_pred <- crosspred(hurr_basis, coef = coef(meta_model),
                         vcov = vcov(meta_model), model.link = "log",
                         cen = 0, at = 1)
  return(meta_pred)
}

# example
# exposure <- c(rep(0, 20), 1, rep(0, 20))
# pred <- pred_meta(meta_model = meta, exposure = exposure)

plot_meta <- function(meta_pred, title = c()){
  plot(meta_pred, xlab = "Lag", 
       ylab = "RR compared to non-storm day",
       exp = TRUE, ptype = "slices", var = 1, main = title)
}

MultiMetaReg <- function(city_list = c(), criterion = c(), cause = "all", method = "reml",
                         data = c(), predictor = c()){
  
  city_fit <- lapply(city_list, CityFit, criterion = criterion, cause = cause)
  city_coefs <- lapply(city_fit, pull_city_coef)
  city_coefs <- do.call("rbind", city_coefs)
  
  city_vcovs <- lapply(city_fit, pull_city_vcov)
  
  meta_fit <- mvmeta::mvmeta(city_coefs ~ predictor, S = city_vcovs, data = data, method = method)
  
  return(meta_fit)
}
