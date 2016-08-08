library(dlnm)
library(dplyr)

synt_data <- data.frame(day = 1:29, 
                        hurr = c(rep(0, 8), 1, rep(0, 20)),
                        exp_outcome = c(rep(4, 8), 4 * 8,  rep(4 * 4, 2),
                                        rep(4 * 2, 4), rep(4, 14)))
synt_data <- rbind(synt_data, synt_data, synt_data) %>%
  mutate(obs_outcome = rpois(87, lambda = exp_outcome))

plot(synt_data$obs_outcome, type = "b")
points(synt_data$exp_outcome, col = "blue", type = "l")

## Unconstrained lag
ex_cross <- crossbasis(synt_data$hurr, lag = c(-1, 10),
                       arglag = list(fun = "integer"),
                       argvar = list("lin"))
ex_mod <- glm(obs_outcome ~ ex_cross, family = poisson(), data = synt_data)
ex_pred <- crosspred(ex_cross, ex_mod, cumul = FALSE)
plot(ex_pred, var = 1, ptype = "slices", type = "p", ci = "bars",
     ylim = c(0, 10))
points(c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
       c(1, 8, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1), col = "red")

## Spline for lag
ex_cross <- crossbasis(synt_data$hurr, lag = c(0, 10),
                       arglag = list(fun = "ns", df = 3),
                       argvar = list("lin"))
ex_mod <- glm(obs_outcome ~ ex_cross, family = poisson(), data = synt_data)
ex_pred <- crosspred(ex_cross, ex_mod, cumul = FALSE)
plot(ex_pred, var = 1, ptype = "slices", ci = "bars", ylim = c(0, 10))
points(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
       c(8, 4, 4, 2, 2, 2, 2, 1, 1, 1, 1), col = "red")
