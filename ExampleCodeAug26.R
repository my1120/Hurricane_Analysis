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