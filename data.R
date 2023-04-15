library(dplyr)
library(rvest)

firmen_hist <- readr::read_delim("data/protected_data/Firmen_hist.csv",
  delim = ";", n_max = 100000, trim_ws = TRUE
)
firmen_inaktiv <- readr::read_delim("data/protected_data/firmen_inaktiv.csv",
  delim = ";", trim_ws = TRUE
)
firmen <- readr::read_delim("data/protected_data/firmen.csv",
  delim = ";", trim_ws = TRUE
)
person <- readr::read_delim("data/protected_data/personen.csv",
  delim = ";", trim_ws = TRUE
)
person_hist <- readr::read_delim("data/protected_data/personen_hist.csv",
  delim = ";", trim_ws = TRUE
)
relation_firma_hist <- readr::read_delim("data/protected_data/relationen_firma_firma_hist.csv",
  delim = ";", trim_ws = TRUE
)
relation_firma <- readr::read_delim("data/protected_data/relationen_firma_firma.csv",
  delim = ";", trim_ws = TRUE
)
relation_person_firma_hist <- readr::read_delim("data/protected_data/relationen_person_firma_hist.csv",
  delim = ";", trim_ws = TRUE
)
relation_person_firma <- readr::read_delim("data/protected_data/relationen_person_firma.csv",
  delim = ";", trim_ws = TRUE
)
plz_einwohner <- readr::read_csv("data/plz_einwohner.csv")

extract_wz_code <- function(x) {
  lapply(x, function(code) {
    stringr::str_extract_all(code, "\\d+") %>%
      `[[`(1) %>%
      as.numeric() %>%
      min() %>%
      as.character() %>%
      substr(1, 2)
  })
}

branche_name <- c(
  "A" = "Land- und Forstwirtschaft, Fischerei ",
  "B" = "Bergbau und Gewinnung von Steinen und Erden",
  "C" = "Verarbeitendes Gewerbe",
  "E" = "Energieversorgung",
  "G" = "Handel; Instandhaltung und Reparatur von Kraftfahrzeugen",
  "H" = "Verkehr und Lagerei",
  "I" = "Gastgewerbe",
  "J" = "Information und Kommunikation",
  "K" = "Erbringung von Finanz- und Versicherungsdienstleistungen",
  "L" = "Grundstücks- und Wohnungswesen",
  "M" = "Erbringung von freiberuflichen, wissenschaftlichen und technischen Dienstleistungen",
  "N" = "Erbringung von sonstigen wirtschaftlichen Dienstleistungen",
  "O" = "Öffentliche Verwaltung, Verteidigung; Sozialversicherung",
  "P" = "Erziehung und Unterricht",
  "Q" = "Gesundheits- und Sozialwesen",
  "R" = "Kunst, Unterhaltung und Erholung",
  "T" = "Private Haushalte mit Hauspersonal; Herstellung von Waren und Erbringung von Dienstleistungen durch Private Haushalte für den Eigenbedarf ohne ausgeprägten Schwerpunkt",
  "U" = "Exterritoriale Organisationen und Körperschaften"
)

firmen <- firmen %>%
  mutate(
    branche_code = extract_wz_code(wz_code),
    branche_code = case_when(
      branche_code < 05 ~ "A",
      branche_code < 10 ~ "B",
      branche_code < 35 ~ "C",
      branche_code < 45 ~ "E",
      branche_code < 49 ~ "G",
      branche_code < 55 ~ "H",
      branche_code < 58 ~ "I",
      branche_code < 64 ~ "J",
      branche_code < 68 ~ "K",
      branche_code < 69 ~ "L",
      branche_code < 77 ~ "M",
      branche_code < 84 ~ "N",
      branche_code < 85 ~ "O",
      branche_code < 86 ~ "P",
      branche_code < 90 ~ "Q",
      branche_code < 97 ~ "R",
      branche_code < 99 ~ "T",
      branche_code == 99 ~ "U"
    ),
    branche_name = vapply(branche_code, function(x) branche_name[x], character(1)),
    mitarbeiter_year = stringr::str_extract(mitarbeiter_staffel, "\\d{4}"),
    mitarbeiter_range = stringr::str_extract(mitarbeiter_staffel, "\\d+-\\d+"),
    umsatz_year = stringr::str_extract(umsatz_staffel, "\\d{4}"),
    umsatz_range = stringr::str_extract(umsatz_staffel, "\\d+-\\d+")
  )

person_firma <- relation_person_firma %>%
  inner_join(firmen %>% rename(firmen_ort = ort), by = "firm_id") %>%
  inner_join(person %>% rename(person_ort = ort), by = "pers_id") %>%
  left_join(plz_einwohner, by = "plz") %>%
  filter(
    geschlecht %in% c("m", "f"),
    funktion %in% c(
    "Vorstandsvorsitzender", "Vorstand", "Direktor",
    "Geschaeftsfuehrer", "Gruender", "Inhaber", "Kommanditist",
    "Partner"
  ))

firma_score <- person_firma %>%
  mutate(alter = 2023 - jahr) %>%
  filter(between(alter, 0, 100)) %>%
  group_by(firm_id, pers_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(firm_id) %>% 
  mutate(
    diversity_alter = min(c(max(alter) - min(alter)) / 50, 1),
    diversity_geschlecht = 1 - 2 * abs(0.5 - mean(geschlecht == "m")),
    diversity_score = (diversity_alter + diversity_geschlecht) / 2
  ) %>%
  arrange(diversity_score) %>%
  filter(mitarbeiter_range %in% c("51-500", "501-5000", "5001-50000")) %>%
  group_by(firm_id) %>%
  filter(n() >= 2) %>%
  ungroup()
