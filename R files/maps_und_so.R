library(readr)
library(stringr)
library(nominatim)
library(dplyr)
library(osmdata)
library(tidygeocoder)
library(tibble)
library(dplyr)
library(tidygeocoder)
library(sf)
library(jsonlite)
library(rgdal)
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(ggplot2)

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
firmen$year_ma <- strsplit(firmen$ma,  "=>")
firmen$ma_range <- lapply(firmen$year_ma, tail, n = 1L)
firmen$year_ma <- lapply(firmen$year_ma, head, n = 1L)

address_single <- tibble(singlelineaddress = c(
 "Einsteinstraße 131, München"
))
# address_components <- tribble(
#   ~street, ~cty, ~st,
#   "131 Einsteinstr.", "München", "Bavaria",
#   "600 Peachtree Street NE", "Atlanta", "GA"
# )
firmen$adress_pasted <- paste0(firmen$stn_hnr, " ", firmen$ort)
osm_s1 <- geo(
   address = firmen$adress_pasted[1:10], method = "osm",
   lat = lat, long = long
)
# osm_s1
library(ggplot2)
library()
bay_bawu_sf <- read_sf(dsn = "data/protected_data" , layer = "OSM_PLZ")

relationen_fuehrung_adv_stat[relationen_fuehrung_adv_stat$firm_id == "900859", ]

ggplot(data = bay_bawu_sf) + geom_sf(aes(fill = einwohner)) + 
  geom_point(data = osm_s1, aes(x = long, y = lat), color = "orange", size = 4) +
  coord_sf(
    crs = st_crs(4326),
    xlim = c(9, 13),
    ylim = c(47, 50)
  ) + scale_color_gradient(low = "blue", high = "red")
  


bay_bawu_sf$einwohner
bay_bawu_sf$einwohner# retrieve relevant plz
relationen_person_firma_hist_fuehrung <- relationen_person_firma_hist %>%
  filter(funktion %in% c("Vorstandsvorsitzender", "Vorstand", "Direktor",
                         "Geschaeftsfuehrer", "Gruender", "Inhaber", "Kommanditist",
                         "Partner"))

relationen_fuehrung_hist <- merge(x = personen, 
                                   y = relationen_person_firma_hist_fuehrung, 
                                   by = "pers_id", 
                                   all.x = FALSE, 
                                   all.y = TRUE) %>%
  filter(geschlecht %in% c("m", "f"), beginn_jahr <= 2022)

relationen_fuehrung_adv <- merge(x = firmen, 
                                   y = relationen_fuehrung_hist, 
                                   by = "firm_id", 
                                   all.x = FALSE, 
                                   all.y = TRUE)


relationen_person_firma_fuehrung <- relationen_person_firma %>%
  filter(funktion %in% c("Vorstandsvorsitzender", "Vorstand", "Direktor",
                         "Geschaeftsfuehrer", "Gruender", "Inhaber", "Kommanditist",
                         "Partner"))

relationen_fuehrung <- merge(x = personen, 
                                   y = relationen_person_firma_fuehrung, 
                                   by = "pers_id", 
                                   all.x = FALSE, 
                                   all.y = TRUE) %>%
  filter(geschlecht %in% c("m", "f"))

relationen_fuehrung_adv_stat <- merge(x = firmen, 
                                   y = relationen_fuehrung, 
                                   by = "firm_id", 
                                   all.x = FALSE, 
                                   all.y = TRUE)

relationen_fuehrung_adv_stat <- relationen_fuehrung_adv_stat %>%
  mutate(alter = 2023 - jahr) %>%
  filter(alter >= 0, alter <= 100)

relationen_fuehrung_adv_stat <- relationen_fuehrung_adv_stat %>%
  group_by(firm_id, pers_id) %>%
  slice(1)

relationen_fuehrung_adv_stat_tmp <- relationen_fuehrung_adv_stat %>%
  group_by(firm_id) %>%
  summarise(diversity_alter = min(c(max(alter) - min(alter)) / 50, 1),
         diversity_geschlecht = 1 - 2 * abs(0.5 - mean(geschlecht == "m")),
         diversity_score = mean(c(diversity_alter,diversity_geschlecht)))







diversity_per_company <- merge(relationen_fuehrung_adv_stat, 
                              relationen_fuehrung_adv_stat_tmp, 
                              by = "firm_id")
colnames(diversity_per_company)


diversity_per_company <- diversity_per_company %>% select(firm_id, plz, diversity_alter, diversity_geschlecht, diversity_score, 
                                ma_range, branchen_code)
diversity_per_company <- diversity_per_company[!duplicated(diversity_per_company), ]
diversity_per_region <- diversity_per_company %>%
  group_by(plz) %>% summarise(mean_diversity_region = mean(diversity_score))
# table(diversity_per_company$plz)
diversity_per_company %>% arrange()

# merge plz and data: 
diversity_shape <- merge(bay_bawu_sf,
  diversity_per_region, by = "plz")

str(diversity_shape)

diversity_per_region_and_branch_L <- diversity_per_company %>% filter(branchen_code == "L") %>% 
  group_by(plz) %>% summarise(mean_diversity_region = mean(diversity_score))

diversity_shape_L <- merge(bay_bawu_sf,
                           diversity_per_region_and_branch_L, by = "plz")

ggplot(data = diversity_shape_L) + geom_sf(aes(fill = mean_diversity_region)) + 
  #geom_point(data = osm_s1, aes(x = long, y = lat), color = "orange", size = 4) +
  coord_sf(
    crs = st_crs(4326),
    #xlim = c(11.34, 11.73),
    #ylim = c(48.07, 48.22)
    xlim = c(10, 13),
    ylim = c(47, 49)
  ) + scale_fill_gradient(low = "white", high = "red")

# Group data by year and gender, and count the number of occurrences
df_counts <- relationen_vorstand %>%
  group_by(beginn_jahr, geschlecht) %>%
  summarize(count = n())

plot(diversity_per_company$diversity_score)

# Calculate the total count for each year
df_totals <- df_counts %>%
  group_by(beginn_jahr) %>%
  summarize(total = sum(count))

# Calculate the relative frequency of each gender for each year
df_freq <- df_counts %>%
  left_join(df_totals, by = "beginn_jahr") %>%
  mutate(freq = count / total)

# Plot the results
ggplot(df_freq, aes(x = beginn_jahr, y = freq, color = geschlecht)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Relative Frequency", color = "Gender")

# average age in boards over time








str(relationen_person_firma)
table(relationen_person_firma$firm_id)
str(personen)

relationen_person_firma_hist[relationen_person_firma_hist$pers_id == "3383943",]
relationen_person_firma_hist 
library(dplyr)
left_join(
  relationen_person_firma_hist[relationen_person_firma_hist$pers_id == "3383943",],
  relationen_person_firma[relationen_person_firma$pers_id == "3383943",],
  by = "pers_id",
  copy = FALSE,
  suffix = c(".x", ".y")
)

table(relationen_person_firma_hist$ende_jahr)


