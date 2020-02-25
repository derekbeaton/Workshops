## Let's now take a look at some parts of our data and clean it up


## A summarization of character columns by frequency
table(ttc_streetcar_delays_Jan2019$Route)

table(ttc_streetcar_delays_Jan2019$Day)

table(ttc_streetcar_delays_Jan2019$Incident)

## or all together through apply
  ### note: This is a list, because it has varying lengths for each vector
apply(ttc_streetcar_delays_Jan2019[,c("Route","Day","Incident")], 2, table)


## quick summary of the numeric variables
summary(ttc_streetcar_delays_Jan2019[,c("Delay","Gap")])


### some help we could have used from DataExplorer
plot_intro(ttc_streetcar_delays_Jan2019)
plot_missing(ttc_streetcar_delays_Jan2019)

official_routes <- c("501","503","504","505","506","508","509","510","511","512",
                     "301","304","306","310")

### let's unpack this a bit.
my_conditional <- which(
  ttc_streetcar_delays_Jan2019$Delay < 600 &
  ttc_streetcar_delays_Jan2019$Gap < 500 &
  ttc_streetcar_delays_Jan2019$Delay > 0 &
  ttc_streetcar_delays_Jan2019$Gap > 0 &
  rowSums(is.na(ttc_streetcar_delays_Jan2019))==0 & 
  ttc_streetcar_delays_Jan2019$Route %in% official_routes
)


ttc_streetcar_delays_Jan2019 <- ttc_streetcar_delays_Jan2019[my_conditional,]