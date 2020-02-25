# visualize three ways
  ## first a simple scatter plot in three ways
  ## second we'll do delays (x) by the categories
  ## third is a multivariate analysis of counts

## simple scatter plot
plot(ttc_streetcar_delays_Jan2019$Delay, ttc_streetcar_delays_Jan2019$Gap)
  ## oooof, that correlation makes no sense.

## ggplot of gap vs. delay
ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Gap)) +
  geom_point()

ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Gap, color = Day)) +
  geom_point()


ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Gap, color = Day)) +
  geom_point() +
  facet_wrap(~Day)


## ggplot of gap vs. delay faceted by day of the week

## now some dot plots
ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Route)) +
  geom_point()

ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Day)) +
  geom_point()

ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Incident)) +
  geom_point()

  ### not a particularly good idea but shows the flexibility
ttc_streetcar_delays_Jan2019 %>%
  ggplot(aes(x = Delay, y = Route, color = Day)) +
  geom_point() + 
  facet_wrap(~Incident)


## CA of route x day

route_by_day_counts <- 
  table(ttc_streetcar_delays_Jan2019$Route,ttc_streetcar_delays_Jan2019$Day)

expo_ca_results <- ExPosition::epCA(route_by_day_counts, graphs = F)

factoextra::fviz_ca_biplot(expo_ca_results)
  ## don't ride the 506 on Thursdays, nor the 512 on Fridays
  ## Tuesdays: a mess.
