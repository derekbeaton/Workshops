## Let's focus on specific parts of our data

## a fancy new pipe shows up!
ttc_streetcar_delays_Jan2019 <- 
  ttc_streetcar_delays_Jan2019[,c("Route", "Day", "Incident", "Min Delay", "Min Gap")]

### let's look
ttc_streetcar_delays_Jan2019

## a fancy new pipe shows up!
ttc_streetcar_delays_Jan2019$Route <- as.character(ttc_streetcar_delays_Jan2019$Route)


### let's look again
ttc_streetcar_delays_Jan2019
  ### and now let's look at the names of the variables in the tibble/data.frame
names(ttc_streetcar_delays_Jan2019)

names(ttc_streetcar_delays_Jan2019)[4:5] <- c("Delay","Gap")