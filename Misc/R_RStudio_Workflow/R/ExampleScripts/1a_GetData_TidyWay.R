## we're going to use `opendatatoronto`
### https://sharlagelfand.github.io/opendatatoronto/index.html
## and we'll work with one of the multi-resource examples here:
### https://sharlagelfand.github.io/opendatatoronto/articles/multisheet_resources.html

## I'm switchin it up to the streetcars instead of the busses.
# search_packages("TTC Bus Delay Data") %>%
# filter(name == "ttc-bus-delay-data-2019") %>%


## note the order and the use of the right assigment arrow
opendatatoronto::search_packages("TTC Streetcar Delay Data") %>%
  opendatatoronto::list_package_resources() %>%
  dplyr::filter(name == "ttc-streetcar-delay-data-2019") %>%
  opendatatoronto::get_resource() -> 
  ttc_streetcar_delays_2019

## str() tells us about the structure of the data
  ### it's a nice way to get a quick look
str(ttc_streetcar_delays_2019, max.level = 1)

## note here the use of the left assignment arrow
ttc_streetcar_delays_Jan2019 <- ttc_streetcar_delays_2019$`Jan 2019 `



