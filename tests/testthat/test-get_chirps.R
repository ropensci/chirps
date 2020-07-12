context("test-get_chirps")

# load("tests/test_data.rda")
load("../test_data.rda")

# Test if get_chirps fetch the correct values,
# for this we downloaded two points from
# https://climateserv.servirglobal.net/
# and will compare it with the values retrieved by get_chirps

# Test default method

test_that("default method", {
  vcr::use_cassette("default_method", {
    x <- get_chirps(lonlat, dates)
    
    x <- round(x$chirps, 2)
    
    equal <- all(x == chirps$chirps)
    
    expect_true(equal)
  })
})

library("sf")
# get_chirps for sf objects
coords <- st_as_sf(lonlat, coords = c("lon", "lat"))

test_that("sf method", {
  vcr::use_cassette("sf_method", {
    y <- get_chirps(coords, dates)
    
    y <- round(y$chirps, 2)
    
    equal <- all(y == chirps$chirps)
    
    expect_true(equal)
  })
})

# get chirps with geojson method
geojson <- as.geojson(lonlat)


test_that("geojson method", {
  vcr::use_cassette("geojson_method", {
    z <- suppressWarnings(get_chirps(geojson, dates))
    
    z <- round(z$chirps, 2)
    
    equal <- all(z == chirps$chirps)
    
    expect_true(equal)
  })
})
