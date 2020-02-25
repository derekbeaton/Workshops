## we're going to use `opendatatoronto`
### https://sharlagelfand.github.io/opendatatoronto/index.html
## and we'll work with one of the multi-resource examples here:
### https://sharlagelfand.github.io/opendatatoronto/articles/multisheet_resources.html


## note the order and the use of the right assigment arrow
opendatatoronto::search_packages("TTC Streetcar Delay Data") %>%
  opendatatoronto::list_package_resources() %>%
  dplyr::filter(name == "ttc-streetcar-delay-data-2019") %>%
  opendatatoronto::get_resource() -> 
  ttc_streetcar_delays_2019


## note here the use of the left assignment arrow
ttc_streetcar_delays_Jan2019 <- ttc_streetcar_delays_2019$`Jan 2019 `


### TTC officially listed streetcar routes
official_routes <- c("501","503","504","505","506","508","509","510","511","512",
                     "301","304","306","310")

ttc_streetcar_delays_Jan2019 %<>%
  dplyr::select(Route, Day, Incident, `Min Delay`, `Min Gap`) %>%
  mutate_at(vars(Route), as.character) %>%
  dplyr::rename("Gap" = `Min Gap`, "Delay" = `Min Delay`) %>%
  filter(Gap != 500) %>%
  filter(Delay != 600) %>%
  filter(Gap != 0) %>%
  filter(Delay != 0) %>%
  drop_na() %>%
  filter(Route %in% official_routes)
  

