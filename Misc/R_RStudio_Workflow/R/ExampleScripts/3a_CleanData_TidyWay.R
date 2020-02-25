## Let's now take a look at some parts of our data and clean it up



## A summarization of character columns by frequency
ttc_streetcar_delays_Jan2019 %>%
  group_by(Route) %>%
  summarise(N = n())

ttc_streetcar_delays_Jan2019 %>%
  group_by(Day) %>%
  summarise(N = n())

ttc_streetcar_delays_Jan2019 %>%
  group_by(Incident) %>%
  summarise(N = n())

## we discover something!
ttc_streetcar_delays_Jan2019 %>%
  summarise(mean = mean(Delay))

## let's understand without NAs
ttc_streetcar_delays_Jan2019 %>%
  summarise(mean = mean(Delay, na.rm = T))


### some help we could have used from DataExplorer
plot_intro(ttc_streetcar_delays_Jan2019)
plot_missing(ttc_streetcar_delays_Jan2019)


ttc_streetcar_delays_Jan2019 %>%
  summarise(delay_mean = mean(Delay, na.rm = T),
            delay_med = median(Delay, na.rm = T),
            delay_min = min(Delay, na.rm = T ),
            delay_max = max(Delay, na.rm = T ))


ttc_streetcar_delays_Jan2019 %>%
  summarise(gap_mean = mean(Gap, na.rm = T),
            gap_med = median(Gap, na.rm = T),
            gap_min = min(Gap, na.rm = T ),
            gap_max = max(Gap, na.rm = T ))

## some possible anomalies and not of interest?
ttc_streetcar_delays_Jan2019 %>%
  filter(Gap == 500 | Delay == 600 | Gap == 0 | Delay == 0)


ttc_streetcar_delays_Jan2019 %>%
  filter(Gap != 500) %>%
  filter(Delay != 600) %>%
  filter(Gap != 0) %>%
  filter(Delay != 0)

official_routes <- c("501","503","504","505","506","508","509","510","511","512",
                     "301","304","306","310")

## Let's start to refine this data set
  ### drop_na will drop any rows with NAs present
ttc_streetcar_delays_Jan2019 %<>%
  filter(Gap != 500) %>%
  filter(Delay != 600) %>%
  filter(Gap != 0) %>%
  filter(Delay != 0) %>%
  drop_na() %>%
  filter(Route %in% official_routes)