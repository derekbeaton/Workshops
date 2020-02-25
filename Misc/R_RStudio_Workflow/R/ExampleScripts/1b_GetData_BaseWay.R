## we're going to use `opendatatoronto`
### https://sharlagelfand.github.io/opendatatoronto/index.html
## and we'll work with one of the multi-resource examples here:
### https://sharlagelfand.github.io/opendatatoronto/articles/multisheet_resources.html

## I'm switchin it up to the streetcars instead of the busses.
# search_packages("TTC Bus Delay Data") %>%
# filter(name == "ttc-bus-delay-data-2019") %>%


## note the order and the use of the right assigment arrow
TTC_Streetcar_Delay_Data <- opendatatoronto::search_packages("TTC Streetcar Delay Data")
TTC_Streetcar_Delay_Data

TTC_Streetcar_Delay_Data_Resources <- opendatatoronto::list_package_resources(TTC_Streetcar_Delay_Data)
TTC_Streetcar_Delay_Data_Resources

TTC_Streetcar_Delay_Data_Resources$name=="ttc-streetcar-delay-data-2019"
which(TTC_Streetcar_Delay_Data_Resources$name=="ttc-streetcar-delay-data-2019")

TTC_Streetcar_Delay_Data_2019 <- TTC_Streetcar_Delay_Data_Resources[which(TTC_Streetcar_Delay_Data_Resources$name=="ttc-streetcar-delay-data-2019"),]
TTC_Streetcar_Delay_Data_2019

## same as in the other file.
ttc_streetcar_delays_2019 <- opendatatoronto::get_resource(TTC_Streetcar_Delay_Data_2019)

  
## alternatively, *if* you know the resource id:
# ttc_streetcar_delays_2019 <- get_resource("786224cc-860f-4de3-a9b6-8f86753b3174")
  ### this comes from TTC_Streetcar_Delay_Data_2019[,"id"] or TTC_Streetcar_Delay_Data_2019$id


## str() tells us about the structure of the data
  ### it's a nice way to get a quick look
str(ttc_streetcar_delays_2019, max.level = 1)

## note here the use of the left assignment arrow
ttc_streetcar_delays_Jan2019 <- ttc_streetcar_delays_2019$`Jan 2019 `



