#===================

library("chirps")

set.seed(12)
lonlat <- data.frame(lon = runif(2, -55, -54),
                     lat = runif(2, -3, -2))

dates <- c("2011-01-01","2016-12-31")

dat <- get_chirps(lonlat, dates)

head(dat)


# ===================

pi <- precip_indices(dat, timeseries = TRUE, span = 7)

head(pi)

# ===================

library("tidyverse")

mm <- c("R95p","R99p","Rx5day","SDII")
days <- c("MLDS","MLWS","R10mm","R20mm")

pi %>% 
  filter(index %in% mm) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = id)) + 
  geom_smooth(aes(x = date, y = value), method = "loess") +
  facet_wrap(. ~ index) +
  theme_classic() +
  labs(x = "Week", y = "Index (mm)") ->
  gg1

plot(gg1)

# ======================
  
pi %>% 
  filter(index %in% days) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = id)) + 
  geom_smooth(aes(x = date, y = value), method = "loess") +
  facet_wrap(. ~ index) +
  theme_classic() +
  labs(x = "Week", y = "Index (days)") ->
  gg2

plot(gg2)

# ======================
  
set.seed(12)
lonlat <- data.frame(lon = runif(2, -55, -54),
                     lat = runif(2, -3, -2))

dates <- c("2016-05-01","2016-12-31")

dat <- get_esi(lonlat, dates)

#=======================

set.seed(123)
lonlat <- data.frame(lon = runif(1, -55, -54),
                     lat = runif(1, -3, -2.7))

dates <- c("2017-12-01","2018-01-20")

# this return a error due to missing data
try(get_esi(lonlat, dates), silent = TRUE)

get_esi(lonlat, dates, dist = 0.1)


#===========================

library("sf")

lonlat <- data.frame(lon = c(-55.0281,-54.9857, -55.0714),
                     lat = c(-2.8094, -2.8756, -3.5279))

lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))

dates <- c("2017-12-15","2017-12-31")

get_chirps(lonlat, dates)

# as.sf TRUE returns an object of class 'sf'
get_chirps(lonlat, dates, as.sf = TRUE)


# library(sf)
# library(osmdata)
# 
# xy <- st_as_sf(lonlat, coords = c("lon", "lat"))
# 
# bb <- st_bbox(xy)
# 
# library("rnaturalearth")
# library("rnaturalearthdata")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf", 
#                       country = "Brazil")
# class(world)
# 
# map1 <- 
# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = lonlat, aes(x = lon, y = lat), size = 1, 
#              shape = 23, fill = "darkred") + 
#   labs(x="",y="")
# ggsave("map.png", map1)
