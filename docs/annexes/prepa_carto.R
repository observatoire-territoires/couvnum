#devtools::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
library(sf)
library(tidyverse)

# récupération du fonds communal france métro GEOFLA COG 2016
COMMG_COG2016_MET <- loadMap(COG = 2016, nivsupra = "COM") %>%
  st_set_crs(2154) %>%
  st_transform(2154) %>%
  mutate(DEPCOM = ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                         ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
  group_by(DEPCOM) %>%
  summarise() %>%
  st_buffer(dist = 0) %>%
  rename(INSEE_COM = DEPCOM) 

# récupération du fonds communal france DROM CGET
COMMG_DROM <- st_read("./carto/COMMG_COG2016_DROM.geojson" , stringsAsFactors = F) %>%  st_set_crs(2154) %>%
  filter(substr(CODE_DEPT,1,2) %in% '97') 

# création du fonds toute France
COMMG_COG2016_METDOM <- COMMG_COG2016_MET %>%
  select(INSEE_COM) %>%
  filter(!substr(INSEE_COM,1,2) %in% '97') %>%
  rbind(COMMG_DROM %>% select(INSEE_COM) )


## mailles supracommunales
library(migR)

geo_DEP_poly <-
  creation_carto_supracomm(CARTO_COMM = COMMG_COG2016_METDOM,
                           CODE_COMMUNE = "INSEE_COM",
                           COG_IN = 2016,
                           COG_NIVGEO = 2018,
                           NIVGEO = "DEP",
                           FORMAT = "poly",
                           SG = 0.02)
