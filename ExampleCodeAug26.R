x <- c(0, 1)
hurr <- sample (x, size = 50, replace = TRUE, prob = c(.9, .1))
ex <- data.frame(hurr = hurr, 
                 yday = 1:10,
                 time = 1:length(hurr))

cand_control <- unique(c(which(ex$hurr == 1) , which(ex$hurr == 1) + 1,
                         which(ex$hurr == 1)- 1))
ex$cand_control <- TRUE
ex$cand_control[cand_control] <- FALSE
control_subset <- dplyr::filter(ex, yday == 3 & cand_control)




ex <- data.frame(hurr = rep(0, 200), 
                 yday = 1:10, 
                 stratum = NA)
ex[c(5, 50, 101),]$hurr <- 1
ex$stratum[ex$yday == 5] <- "stratum.1"
ex$stratum[ex$yday == 10] <- "stratum.2"
ex$stratum[ex$yday == 1] <- "stratum.3"
ex$stratum <- as.factor(ex$stratum)


ex$this_hurr <- 0
ex[5, ]$this_hurr <- 1

cb <- crossbasis(ex$this_hurr, lag = c(0, 13), group = ex$stratum,
                 argvar = list(fun = "lin"),
                 arglag = list(fun = "ns", knots = c(1, 4, 8)))
