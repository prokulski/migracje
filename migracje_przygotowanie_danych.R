library(tidyverse)
library(reshape2)

# nazwy powiatów i gmin
# plik TERC podstwowa ze strony
# http://eteryt.stat.gov.pl/eTeryt/rejestr_teryt/udostepnianie_danych/baza_teryt/uzytkownicy_indywidualni/pobieranie/pliki_pelne.aspx?contrast=default
nazwy_gmi <- read_csv2("TERC_Urzedowy_2017-04-28.csv")
nazwy_woj <- nazwy_gmi %>% filter(NAZWA_DOD=="województwo") %>% select(WOJ, NAZWA)
nazwy_pow <- nazwy_gmi %>% filter(NAZWA_DOD %in% c("powiat", "miasto na prawach powiatu", "miasto stołeczne, na prawach powiatu")) %>% select(WOJ, POW, NAZWA) %>% mutate(TERYT=paste0(WOJ, POW))
nazwy_gmi <- nazwy_gmi %>% filter(!is.na(RODZ)) %>% select(-STAN_NA) %>% mutate(TERYT=paste0(WOJ, POW, GMI, RODZ))



# dane o migracjach
# plik ze strony http://demografia.stat.gov.pl/bazademografia/Tables.aspx
# III. MIGRACJE LUDNOŚCI -> Migracje wewnętrzne -> 2015 -> 2g

# obrobiony najpierw w excelu (pozostawiony kod i liczby), a nastepnie:
# migracje <- read_csv2("pl_mig_2015_00_2g_teryt.csv")
# migracje_df <- melt(migracje)
# colnames(migracje_df) <- c("DO", "Z", "Liczba")
# saveRDS(migracje_df, "migracje_df.RDS")

migracje_df <- readRDS("migracje_df.RDS")

# migracje między powiatami
migracje_pow_df <- migracje_df %>%
   filter(Liczba > 0 ) %>%
   group_by(Z=substr(Z, 1, 4), DO=substr(DO, 1, 4)) %>%
   summarise(Liczba = sum(Liczba)) %>%
   ungroup()

powiaty <- nazwy_pow %>%
   rename(NAZWA_POW = NAZWA) %>%
   left_join(nazwy_woj, by="WOJ") %>%
   select(NAZWA_WOJ = NAZWA, NAZWA_POW, TERYT)

saveRDS(powiaty, file = "powiaty_nazwy.RDS")
saveRDS(migracje_pow_df, file = "powiaty_migracje.RDS")


# mapy
library(rgdal)
library(broom)

# dane z http://www.codgik.gov.pl/index.php/darmowe-dane/prg.html
powiaty_mapa <- readOGR("../!mapy_shp/powiaty.shp")
powiaty_mapa <- tidy(powiaty_mapa, region = "jpt_kod_je")

# zmniejszamy dokładność:
powiaty_mapa_small <- powiaty_mapa %>%
   select(-order) %>%
   mutate(long=round(long/10000, 1), lat=round(lat/10000, 1)) %>%
   unique() %>%
   mutate(order = row_number())

saveRDS(powiaty_mapa_small, file = "powiaty_mapa.RDS")
