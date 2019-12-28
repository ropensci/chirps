library("chirps")
# Three points in the Tapajos National Forest, Brazil
# lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
#                      lat = c(-2.8094, -2.8756, -3.5279))

set.seed(12)
lonlat <- data.frame(lon = runif(2, -55, -54),
                      lat = runif(2, -3, -2))

dates <- c("2016-01-01","2016-12-31")

b <- Sys.time()
df <- get_chirps(lonlat, dates)
e <- Sys.time()
e - b

# take the indices for the entire period 
p1 <- precip_indices(df, timeseries = FALSE)

# take the indices for periods of 7 days
p2 <- precip_indices(df, timeseries = TRUE, span = 7)

library(tidyverse)

days <- c("MLDS","MLWS","R10mm","R20mm")

mm <- c("R95p","R99p","Rx5day","SDII")

gg1 <-
p2 %>% 
  filter(index %in% mm) %>% 
  ggplot() +
  geom_line(aes(x=bin, y = value, group = id)) + 
  geom_smooth(aes(x=bin, y = value)) +
  ggplot2::facet_wrap(. ~ index) +
  theme_classic() +
  labs(x = "Week", y = "Index (mm)")

ggsave("indices_mm.png", gg1)

gg2 <-
p2 %>% 
  filter(index %in% days) %>% 
  ggplot() +
  geom_line(aes(x=bin, y = value, group = id)) + 
  geom_smooth(aes(x=bin, y = value)) +
  ggplot2::facet_wrap(. ~ index) +
  theme_classic() +
  labs(x = "Week", y = "Index (days)")

ggsave("indices_days.png", gg2)


library(sf)
library(osmdata)

xy <- st_as_sf(lonlat, coords = c("lon", "lat"))

bb <- st_bbox(xy)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf", 
                      country = "Brazil")
class(world)

map1 <- 
ggplot(data = world) +
  geom_sf() +
  geom_point(data = lonlat, aes(x = lon, y = lat), size = 1, 
             shape = 23, fill = "darkred") + 
  labs(x="",y="")
ggsave("map.png", map1)
