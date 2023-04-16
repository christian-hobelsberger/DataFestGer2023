library(readr)



firmen_hist <- read_delim("data/protected_data/firmen_hist.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
firmen <- read_delim("data/protected_data/firmen.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
personen <- read_delim("data/protected_data/personen.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
relationen_person_firma <- read_delim("data/protected_data/relationen_person_firma.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
relationen_firma_firma <- read_delim("data/protected_data/relationen_firma_firma.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
personen_hist <- read_delim("data/protected_data/personen_hist.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
relationen_person_firma_hist <- read_delim("data/protected_data/relationen_person_firma_hist.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)



## Maps

library(sf)
honey_sf <- read_sf(dsn = "data/protected_data/Postleitzahlengebiete_-_OSM" , layer = "OSM_PLZ")


ggplot(data = honey_sf) + 
  geom_sf() +
  coord_sf(
    crs = st_crs(4326),
    xlim = c(9, 13),
    ylim = c(47, 50)
  )


### Gender und Branchen

diversity_branche <- function(df, branche) {
  df <- df %>%
    filter(branchen_code == !!branche)
  
  df_counts <- df %>%
    group_by(beginn_jahr, geschlecht) %>%
    summarize(count = n())
  
  # Calculate the total count for each year
  df_totals <- df_counts %>%
    group_by(beginn_jahr) %>%
    summarize(total = sum(count))
  
  # Calculate the relative frequency of each gender for each year
  df_freq <- df_counts %>%
    left_join(df_totals, by = "beginn_jahr") %>%
    mutate(freq = count / total)
  
  # Plot the results
  theme_set(theme_bw())
  ggplot(df_freq, aes(x = beginn_jahr, y = freq, color = geschlecht)) +
    geom_line(size = 3) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
    scale_colour_manual(labels = c("Female", "Male"), values = c("darkred", "lightblue")) +
    scale_x_continuous(breaks = c(2007, 2010, 2015, 2020, 2022)) +
    labs(x = "Year", y = "Relative Frequency", color = "Gender") +
    theme(axis.title.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 17)) +
    guides(size = "none")
    
  ggsave(paste("Plots/diversity_", branche, ".png", sep = ""),
         width = 10, height = 5)
}


branchen_codes <- unique(firmen$branchen_code)
for (branchen_code in branchen_codes) {
  diversity_branche(relationen_fuehrung_adv, branchen_code)
}


