source("data.R")
library(ggplot2)
library(sf)
library(dplyr)
library(ggmap)
library(osmdata)
library(tidygeocoder)

colnames(firma_score)
bay_bawu_sf <- read_sf(dsn = "data/protected_data" , layer = "OSM_PLZ")
firma_score_maps <- firma_score %>% select(firmenname, firm_id, plz, diversity_alter, diversity_geschlecht, diversity_score, 
                                      mitarbeiter_range, umsatz_range, branche_code, lat, lon, einwohner, note, firmen_ort, 
                                      stn_hnr, firmen_ort) %>%
  mutate(is_city = (einwohner >= 10000), adress_pasted = paste0(stn_hnr, " ", firmen_ort))



score_per_region <- firma_score_maps %>%
  group_by(plz) %>% summarise(mean_diversity_region = mean(diversity_score), einwohner = sum(einwohner), mean_diversity_region_alter = mean(diversity_alter)) %>% 
  mutate(is_rural = einwohner <= 200000)

score_per_region_J <- firma_score_maps %>% filter(branche_code == "L") %>% 
  group_by(plz) %>% summarise(mean_diversity_region = mean(diversity_score, na.rm = T), einwohner = sum(einwohner, na.rm = T)) %>% 
  mutate(is_rural = einwohner <= 200000)
score_shape <- merge(bay_bawu_sf,
                         score_per_region, by = "plz")

score_shape_J <- merge(bay_bawu_sf,
                       score_per_region_I, by = "plz", all.x=TRUE)
#map_bay <- get_map(getbb("bavaria"))
summarised_shape <- bay_bawu_sf %>% summarise()
str(score_shape)

ordered_firma_score <- firma_score_maps[order(-firma_score$diversity_score), ]
ordere_branche_score <- diversity_branche[order(-diversity_branche$mean_diversity_branche), ]

geom_points_best_cmp <- geo(
  address = ordered_firma_score$adress_pasted[1:10], method = "osm",
  lat = lat, long = long
)

###################################### Maps
ggplot(data = score_shape_J) + geom_sf(aes(fill = mean_diversity_region)) + 
  #geom_point(data = osm_s1, aes(x = long, y = lat), color = "orange", size = 4) +
  coord_sf(
    crs = st_crs(4326),
    xlim = c(9, 13),
    ylim = c(47, 50)
  )+ scale_fill_gradient(low = "white", high = "red") 

theme_set(theme_bw())
ggplot(data = score_shape) + geom_sf(aes(fill = mean_diversity_region), lwd = 0) + 
  geom_point(data = geom_points_best_cmp, aes(x = long, y = lat), color = "orchid4", size = 4, shape = 1) +
  #geom_text(data = score_shape, aes(x = lon, y = lat, label = firmenname)) +
  coord_sf(
    crs = st_crs(4326),
    #xlim = c(11.34, 11.73),
    #ylim = c(48.07, 48.22)
    xlim = c(10.5, 13),
    ylim = c(47.25, 49.3)
  ) + scale_fill_gradient(low = "white", high = "orchid4") +
  labs(x = 'Longitude', y = 'Latitude',  fill = 'Cherry Score')
  #theme(legend.position = "bottom")
ggsave("map_final_cherry",width = 9, height = 12)

theme_set(theme_bw())
ggplot(data = score_shape) + geom_sf(aes(fill = mean_diversity_region), lwd = 0) + 
  #geom_point(aes(x = 11.57, y = 48.14), color = "black", size = 4, shape = 1) +
  #geom_text(aes(x = 11.57, y = 48.14), label = "Munich") + 
  coord_sf(
    crs = st_crs(4326),
    #xlim = c(11.34, 11.73),
    #ylim = c(48.07, 48.22)
    xlim = c(10.5, 13),
    ylim = c(47.25, 49.3)
  ) + scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(x = 'Longitude', y = 'Latitude',  fill = 'Age-Diversity in %')
  #theme(legend.position = "bottom") +
  #guides(fill = guide_legend(title.position = "top"))
ggsave("map_final_intro",width = 12, height = 9)

# münchen koordinaten 48.14, 11.57 

# test 1: unterschied zwischen Stadt und Land? -> Stadt vs Land mean 
stadt <- score_per_region[score_per_region$is_rural,]$mean_diversity_region
land <- score_per_region[!score_per_region$is_rural,]$mean_diversity_region
t.test(stadt, land, alternative = "two.sided", var.equal = FALSE)

ggplot(data = score_per_region, aes(x = is_rural, y = mean_diversity_region)) + geom_boxplot()

# test 2: Unterschied branche Bsp. 
diversity_branche <- firma_score_maps %>% group_by(branche_code) %>% summarise(mean_diversity_branche = median(diversity_score), 
                                                                         mean_div_alter = mean(diversity_alter), 
                                                                         mean_div_geschl = mean(diversity_geschlecht))

ggplot(data = diversity_branche, aes(x = branche_code, y = mean_diversity_branche)) + geom_boxplot()
