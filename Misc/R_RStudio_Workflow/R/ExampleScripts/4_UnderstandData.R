
## let's look at some categorical variables
  ### two different packages with two approaches
inspectdf::inspect_cat(ttc_streetcar_delays_Jan2019,show_plot = T)
DataExplorer::plot_bar(ttc_streetcar_delays_Jan2019)


## now let's look at the continuous variables, and get a sense of what they tell us
summarytools::descr(ttc_streetcar_delays_Jan2019[,c("Delay","Gap")],
      stats = c("mean", "sd"))


## and now let's do that by a grouping factor: day of the week
group_descriptives <- summarytools::stby(data = ttc_streetcar_delays_Jan2019[,c("Delay","Gap")],
                           INDICES = ttc_streetcar_delays_Jan2019$Day, 
                           FUN = descr, 
                           stats = c("mean", "sd", "min", "med", "max"), 
                           transpose = TRUE)
group_descriptives
View(group_descriptives)


## this is the tidy way of summarizing data by a grouping factor
  ### first for Delay
ttc_streetcar_delays_Jan2019 %>%
  group_by(Day) %>%
  summarise(Mean = mean(Delay),
            Std.Dev = sd(Delay),
            Min = min(Delay),
            Median = median(Delay),
            Max = max(Delay))

  ### then for Gap
ttc_streetcar_delays_Jan2019 %>%
  group_by(Day) %>%
  summarise(Mean = mean(Gap),
            Std.Dev = sd(Gap),
            Min = min(Gap),
            Median = median(Gap),
            Max = max(Gap))


## maybe a correation between the two?
cor(ttc_streetcar_delays_Jan2019$Delay, ttc_streetcar_delays_Jan2019$Gap)
  ### spoiler alert: this is a terrible idea without doing something more