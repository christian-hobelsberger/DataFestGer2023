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

firmen$ma <- strsplit(firmen$mitarbeiter_staffel, ",")
firmen$ma <- lapply(firmen$ma, tail, n = 1L)
firmen$ma <- gsub(firmen$ma,  replacement = "", pattern = "\"")
firmen$year_ma <- strsplit(firmen$ma,  "=>")
firmen$ma_range <- lapply(firmen$year_ma, tail, n = 1L)
firmen$year_ma <- lapply(firmen$year_ma, head, n = 1L)