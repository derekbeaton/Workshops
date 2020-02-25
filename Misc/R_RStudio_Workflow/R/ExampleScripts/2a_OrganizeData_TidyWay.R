## Let's focus on specific parts of our data

## a fancy new pipe shows up!
ttc_streetcar_delays_Jan2019 %<>%
  dplyr::select(Route, Day, Incident, `Min Delay`, `Min Gap`)

### let's look
ttc_streetcar_delays_Jan2019


## a fancy new pipe shows up!
ttc_streetcar_delays_Jan2019 %<>%
  mutate_at(vars(Route), as.character)

### let's look again
ttc_streetcar_delays_Jan2019

ttc_streetcar_delays_Jan2019 %<>%
  dplyr::rename("Gap" = `Min Gap`, "Delay" = `Min Delay`)

### a final look
ttc_streetcar_delays_Jan2019