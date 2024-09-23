
library(sf)
library(tmap)

# points and polygons: la basin ozone

la_o3 <- st_read("data/la_ozone/la_o3.shp")
ca_counties <- st_read("data/california/ca_counties.shp")

# map
plot(ca_counties$geometry)
plot(la_o3$geometry, col = "red")
# map together
plot(ca_counties$geometry)
plot(la_o3$geometry, col = "red", add = TRUE)

# check the CRS
st_crs(ca_counties)
st_crs(la_o3)
# compare the CRS
st_crs(la_o3) == st_crs(ca_counties)

# transform the CRS
la_o3 <- st_transform(la_o3, crs = st_crs(ca_counties))
# compare the CRS
st_crs(la_o3) == st_crs(ca_counties)

# map together
plot(ca_counties$geometry)
plot(la_o3$geometry, col = "red", add = TRUE)

# tmap_mode(mode = "view")
# tm_shape(la_o3) +
#   tm_dots(col = NA) + 
#   tm_shape(ca_counties) +
#   tm_polygons(col = "gray95", border.col = "black") +
#   tm_shape(la_o3) +
#   tm_dots(col = "MAXDAY", size = 0.4, alpha = 0.95, style = "cont", palette = "plasma")


# points and polygons: uk pubs and local authority district

# polygons (or multipolygons)
uk <- st_read("data/uk_pubs/uk.shp")

# check the contents
head(uk)

# check the contents visually
plot(uk$geometry)

plot(uk["LAD24NM"])
plot(uk["LAT"])
plot(uk["LONG"])

# points
pubs <- st_read("data/uk_pubs/pubs.shp")

st_crs(pubs) == st_crs(uk)

plot(uk$geometry)
plot(pubs$geometry, col = "gold", add = TRUE)


## choose a district (LAD24NM) to subset
sort(unique(uk$LAD24NM))
highland <- subset(uk, LAD24NM == "Highland")

highland_pubs <- st_filter(pubs, highland)

plot(highland$geometry)
plot(highland_pubs$geometry, col = "gold", add = TRUE)


york <- subset(uk, LAD24NM == "North Yorkshire")

york_pubs <- st_filter(pubs, york)

plot(uk$geometry)
plot(york$geometry, col = "seagreen", add = TRUE)
plot(york_pubs$geometry, col = "gold", add = TRUE)


# https://r-spatial.github.io/sf/

