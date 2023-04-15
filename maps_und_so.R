library(readr)
library(stringr)
personen <- read_delim("data/protected_data/personen.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
firmen <- read_delim("data/protected_data/firmen.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
firmen_hist <- read_delim("data/protected_data/firmen_hist.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
relationen_person_firma_hist <- read_delim("data/protected_data/relationen_person_firma_hist.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
relationen_person_firma <- read_delim("data/protected_data/relationen_person_firma.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Firmen Dataframe 
colnames(firmen)
firmen <- firmen %>% select(-c("amtsgericht_plz","amtsgericht_ort","hr_nummer",          
                               "hr_nummer_alle","hr_nummer_info","gegenstand",
                               "pubid_arr","pdf_id_arr", "wz_code_text"))
str(firmen)
firmen$branche <- gsub(firmen$wz_code, replacement = "", pattern = "\\{|\\}")
firmen$branche <- strsplit(firmen$branche, ",")
firmen$branche <- lapply(firmen$branche, as.numeric)
firmen$branche <- lapply(firmen$branche, min)
firmen$branche <- substr(firmen$branche,1,2)
firmen <- firmen %>% mutate(branche = as.numeric(branche)) %>% 
  mutate(branchen_code = case_when(branche < 05 ~ "A", 
                                   branche < 10 ~ "B", 
                                   branche < 35 ~ "C", 
                                   branche < 45 ~ "E", 
                                   branche < 49 ~ "G", 
                                   branche < 55 ~ "H", 
                                   branche < 58 ~ "I", 
                                   branche < 64 ~ "J",
                                   branche < 68 ~ "K", 
                                   branche < 69 ~ "L", 
                                   branche < 77 ~ "M", 
                                   branche < 84 ~ "N", 
                                   branche < 85 ~ "O", 
                                   branche < 86 ~ "P", 
                                   branche < 90 ~ "Q", 
                                   branche < 97 ~ "R", 
                                   branche < 99 ~ "T", 
                                   branche == 99 ~ "U"))

firmen$mitarbeiter_staffel
firmen$ma <- strsplit(firmen$mitarbeiter_staffel, ",")
firmen$ma <- lapply(firmen$ma, tail, n = 1L)
firmen$ma <- gsub(firmen$ma,  replacement = "", pattern = "\"")
#firmen$year_ma <- lapply(firmen$ma, function(x) if(identical(x, character(0))) NA_character_ else x)
firmen$year_ma <- strsplit(firmen$ma,  "=>")
firmen$ma_range <- lapply(firmen$year_ma, tail, n = 1L)
firmen$year_ma <- lapply(firmen$year_ma, head, n = 1L)


devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(dplyr)
library(osmdata)
install.packages("osmdata")
addresses <- c("123 Main St, Anytown, USA", "456 Oak Ave, Otherville, USA")
osm_data <- osmdata_sf(addresses)
# Define the addresses
addresses <- c("123 Main St, Anytown, USA", "456 Oak Ave, Otherville, USA")

# Retrieve OSM data for addresses
osm_data <- osmdata_query(search_string = addresses, format_out = "sf")


library(tidygeocoder)
library(tibble)
library(dplyr)
library(tidygeocoder)

address_single <- tibble(singlelineaddress = c(
  "Einsteinstraße 131, München",
  "600 Peachtree Street NE, Atlanta"
))
address_components <- tribble(
  ~street, ~cty, ~st,
  "131 Einsteinstr.", "München", "Bavaria",
  "600 Peachtree Street NE", "Atlanta", "GA"
)
osm_s1 <- geo(
  address = firmen$adress_pasted[1:10], method = "osm",
  lat = latitude, long = longitude
)
osm_s1


library(sf)
# for loading our data
library(jsonlite)
library(rgdal)
library(sf)
# for plotting
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(vapoRwave)
# for data wrangling
library(dplyr)
library(ggplot2)

honey_sf <- read_sf(dsn = "data/protected_data" , layer = "OSM_PLZ")
plot(honey_sf)
st_drivers()

library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"data/protected_data") , 
  layer="plz-5stellig",
  verbose=FALSE
)

ggplot(data = honey_sf) + geom_sf()

honey_sf$
