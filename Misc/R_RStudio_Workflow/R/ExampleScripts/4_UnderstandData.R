

inspect_cat(ttc_streetcar_delays_Jan2019,show_plot = T)


plot_bar(ttc_streetcar_delays_Jan2019)



summarytools::descr(ttc_streetcar_delays_Jan2019[,c("Delay","Gap")],
      stats = c("mean", "sd", "min", "med", "max"))


group_descriptives <- summarytools::stby(data = ttc_streetcar_delays_Jan2019[,c("Delay","Gap")],
                           INDICES = ttc_streetcar_delays_Jan2019$Day, 
                           FUN = descr, 
                           stats = c("mean", "sd", "min", "med", "max"), 
                           transpose = TRUE)
group_descriptives
View(group_descriptives)



ttc_streetcar_delays_Jan2019 %>%
  group_by(Day) %>%
  summarise(Mean = mean(Delay),
            Std.Dev = sd(Delay),
            Min = min(Delay),
            Median = median(Delay),
            Max = max(Delay))

ttc_streetcar_delays_Jan2019 %>%
  group_by(Day) %>%
  summarise(Mean = mean(Gap),
            Std.Dev = sd(Gap),
            Min = min(Gap),
            Median = median(Gap),
            Max = max(Gap))