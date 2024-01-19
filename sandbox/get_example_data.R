library(FIESTA)
library(tidyverse)

islands <- sf::st_read("/home/grayson/Downloads/Parcels/Parcels.shp")

plts <- FIESTAnalysis::anGetData_tsum(bnd_layer = islands,
                             # bnd.att = "Island",
                              estvarlst = "DRYBIO_AG",
                              eval='custom', 
                              eval_opts=eval_options(Cur=TRUE),
                              datsource = "datamart",
                              rastfolder = "/home/grayson/Documents/layers/",
                              rastlst.cont = c("usfs_2016_CONUS_tcc_analytical_90m.tif",
                                               "conus_dem_90m.tif"),
                              rastlst.cat = c("evt14_tnt_90m.tif"),
                              rastlst.cont.name = c("tcc", "elev"),
                              rastlst.cat.name = c("tnt"))

samp.x <- plts$pltassgn
samp.y <- plts$tsumdatp
samp.y <- samp.y %>%
  select(CN, MEASYEAR,  LON_PUBLIC, LAT_PUBLIC, DRYBIO_AG_TON_TPA_ADJ) %>%
  rename(PLT_CN = CN)

samp <- left_join(samp.x, samp.y, by = "PLT_CN") %>%
  select(-ONEUNIT, -COUNTYFIPS) %>%
  rename(biomass = DRYBIO_AG_TON_TPA_ADJ, measurement_year = MEASYEAR,
         latitude_public = LAT_PUBLIC, longitude_public = LON_PUBLIC,
         plt_cn = PLT_CN) %>%
  relocate(plt_cn, biomass)

samp.sf <- samp %>%
  st_as_sf(coords = c("longitude_public", "latitude_public"), crs = "EPSG:4269")

sf_use_s2(FALSE)
island.join <- st_intersection(islands %>% st_transform(crs = st_crs(samp.sf)), samp.sf) %>%
  select(plt_cn, Island)

samp <- left_join(samp, island.join, by = "plt_cn")
samp <- st_as_sf(samp)

pix <- read.csv("/home/grayson/Downloads/parcels/pixeldat.csv")
pix <- pix %>%
  select(pixelX, pixelY, Island, tcc16, elev, tnt)

refrast <- readRDS("/home/grayson/Downloads/refrast_crs.rds")

pix.sf <- st_as_sf(pix, coords = c("pixelX", "pixelY"), crs = refrast) %>%
  st_transform(crs = st_crs(samp))

ggplot() +
  geom_sf(data = pix.sf)

SJC_population <- pix.sf %>%
  rename(island = Island, tcc = tcc16)
SJC_sample <- samp %>%
  rename(island = Island) %>%
  select(-longitude_public, -latitude_public)

usethis::use_data(SJC_sample, overwrite = TRUE)
usethis::use_data(SJC_population, overwrite = TRUE)

islands.grp <- islands %>%
  group_by(Island) %>%
  summarize(n = n())

plot(st_geometry(islands.grp))

SJC_boundary <- islands.grp %>%
  st_transform(crs = st_crs(SJC_sample))

simplepolys <- rmapshaper::ms_simplify(input = SJC_boundary)
SJC_boundary <- simplepolys %>%
  select(-n) %>%
  rename(island = Island)
usethis::use_data(SJC_boundary, overwrite = TRUE)

