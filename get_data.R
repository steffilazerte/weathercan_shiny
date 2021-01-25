library(weathercan)
library(dplyr)
library(stringr)
library(sf)
library(janitor)
library(rnaturalearth)

# Get protected areas
# f <- "CPCAD-BDCAPC_Dec2019.gdb.zip"
# ff <- str_remove(f, ".zip")
# if(!dir.exists(ff)){
#   download.file(file.path("https://cws-scf.ca", f), destfile = f)
#   unzip(f)
#   unlink(f)
# }

# Filter Protected areas to MB
# st_layers(ff)
# pa <- st_read(ff, layer = "CPCAD_Dec2019") %>%
#   clean_names() %>%
#   filter(str_detect(loc_e, "Arctic|Manitoba"))
#
# mb_pa <- ne_states(country = "Canada", returnclass = "sf") %>%
#   filter(name_en == "Manitoba") %>%
#   st_transform(crs = st_crs(pa)) %>%
#   st_join(pa, ., left = FALSE)

# Get climate data
if(!file.exists("mb_2020.rds")){
  s <- filter(stations, prov == "MB", start <= 2020, end >= 2020, interval == "day")
  mb <- weather_dl(s$station_id, start = "2020-01-01", end = "2020-12-31", interval = "day")
  saveRDS(mb, "mb_2020.rds")
}

# Get Eco regions
if(!file.exists("env_ecological_areas.shp")) {
  download.file(file.path("http://mli2.gov.mb.ca/environment/shp_zip_files",
                          "env_ecological_areas_py_shp.zip"),
                destfile = "ecological_shp.zip")
  unzip("ecological_shp.zip")
  file.remove("ecological_shp.zip")
}

region <- st_read("env_ecological_areas.shp") %>%
  clean_names() %>%
  filter(manitoba == "yes") %>%
  select(region_nam, region_nom, prov_name, prov_nom, zone_name, zone_nom) %>%
  st_transform(crs = 3347)

saveRDS(region, "mb_regions.rds")



