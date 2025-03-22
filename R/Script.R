install.packages("oceanmap")
library(oceanmap)
library(terra)
library(sf)

install.packages("whitebox")
whitebox::install_whitebox()
library(whitebox)

#initialize
wbt_init()



# Convert to WGS84 (EPSG:4326)
landsat_wgs84 <- project(landsat_sr, "EPSG:4326")

plot(landsat_wgs84)
class(landsat_wgs84)

# True Color (R: B4, G: B3, B: B2)
plotRGB(landsat_wgs84, r = 4, g = 3, b = 2, stretch = "lin", main = "Landsat 8 True Color")

# Get new bounding box in Lat/Lon
bbox_wgs84 <- ext(landsat_wgs84)

# Extract min/max latitude and longitude
min_lon <- bbox_wgs84$xmin
max_lon <- bbox_wgs84$xmax
min_lat <- bbox_wgs84$ymin
max_lat <- bbox_wgs84$ymax

# Print results in Lat/Lon
cat("Min Longitude:", min_lon, "\n")
cat("Max Longitude:", max_lon, "\n")
cat("Min Latitude:", min_lat, "\n")
cat("Max Latitude:", max_lat, "\n")


lon = c(min_lon, max_lon)
lat = c(min_lat, max_lat)

bathy <- get.bathy(lon = lon,lat = lat, resolution = 1, visualize = F, grid =F)

plot(bathy)

deep <- bathy*-1
plot(deep)

class(bathy)

writeRaster(deep, filename = "deep.tif", overwrite = TRUE)

# transform to Terra Raster
bathy_DEM <- rast("deep.tif")
class(bathy_DEM)
plot(bathy_DEM)

bathy_scene_extend <- terra::crop(bathy_DEM,landsat_wgs84)

res(bathy_scene_extend)

writeRaster(landsat_wgs84, filename = "landsatwgs84.tif", overwrite = TRUE)
writeRaster(bathy_scene_extend, filename = "bathy_scene_extend.tif", overwrite = TRUE)

library(terra)
library(ggplot2)
library(plotly)

deep_values <- as.data.frame(bathy_scene_extend, xy=TRUE)

# View the first few rows of raster values
head(deep_values)

# Create 3D surface plot using plotly
p <- plot_ly(
  data = deep_values,
  x = ~x,
  y = ~y,
  z = ~deep,
  type = 'mesh3d'
)

# Show the 3D plot
p


# Create a static 2D plot using ggplot2 (if needed)
ggplot(deep_values, aes(x = x, y = y, fill = deep)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Raster Visualization", x = "Longitude", y = "Latitude")

class(deep_values)

# 2D plot with color mapping to 'z' to simulate 3D
ggplot(deep_values, aes(x = x, y = y, color = deep)) +
  geom_point() +
  labs(title = "2D Plot Simulating 3D with Color")

###WKT Geometry

coords <- matrix(c(min_lon, min_lat,
                   max_lon, min_lat,
                   max_lon, max_lat,
                   min_lon, max_lat,
                   min_lon, min_lat),  # Close the polygon
                 ncol = 2, byrow = TRUE)
library(sf)
# Create an sf POLYGON
polygon_sf <- st_sfc(st_polygon(list(coords)), crs = 4326)

# Convert to WKT
wkt_polygon <- st_as_text(polygon_sf)

# Print WKT
print(wkt_polygon)



install.packages("elevatr")
library(elevatr)

# Get the DEM (raster data)
terrain_dem <- get_elev_raster(locations = landsat_wgs84, z = 12)


### OBIS
install.packages("robis")
library(robis)

records <- occurrence(geometry = wkt_polygon, startdate = as.Date("2020-01-01"))

class(records)

map_ggplot(records)


View(records)


library()

diversity <- oceanDiversity(occurrences, species_name = "scientificName",
               lat_name = "decimalLatitude", long_name = "decimalLongitude",
               extent = "global", cell_size = 5, min_long = min_lon, max_long = max_lon,
               min_lat = min_lat, max_lat = max_lat, diversity_metric = "richness")




# White Box Slopes
#slope
slope <- "slope.tif"
wbt_slope(
  dem = bathy_DEM,
  output = slope,
  zfactor = 1,
  units = "degrees"
)

#tools like fill and depression not needed cause NO Data Areas are land areas

#d8 pointer
d8_pointer <- "d8_pointer.tif"
wbt_d8_pointer(
  dem = bathy_DEM,
  output = d8_pointer
)

#flow accumulation
flow_accum <- "flow_accum.tif"
wbt_d8_flow_accumulation(
  input = d8_pointer,
  output = flow_accum,
  pntr = TRUE
)

d8_cells <- rast(flow_accum)
plot(d8_cells)

#derive streams
streams <- "streams.tif"
wbt_extract_streams(
  flow_accum = flow_accum,
  output = streams,
  threshold = 10
)

#subbasins
subbasins <- "subbasins.tif"
wbt_subbasins(
  d8_pntr = d8_pointer,
  streams = streams,
  output = subbasins
)

subbasins_rast <- rast(subbasins)
plot(subbasins_rast)


install.packages("marmap")
library(marmap)

NOAA_dat <- getNOAA.bathy(lon1 = 9, lon2 = 31, lat1 = 53, lat2 = 64, resolution = 10)

class(NOAA_dat)
plot(NOAA_dat)
plot.bathy(NOAA_dat)

install.packages("oce")
library(oce)


install.packages("amt")
library(amt)

install.packages("move")
library(move)

whales <- move("Movements of Australia's east coast humpback whales-reference-data.csv"


# Retrieve the list of available datasets
datasets <- ed_datasets()

# View the first few entries
head(datasets)


# Search for datasets related to sea surface temperature
sst_datasets <- ed_search(query = "sea surface temperature")

# View the search results
sst_datasets$info


# Define the dataset ID and the desired time range and spatial extent
dataset_id <- "jplMURSST41"
time_range <- c("2022-01-01", "2022-01-31")
latitude_range <- c(30, 40)
longitude_range <- c(-130, -120)

# Retrieve the data
sst_data <- griddap(
  datasetx = dataset_id,
  time = time_range,
  latitude = latitude_range,
  longitude = longitude_range,
  fields = "analysed_sst"
)

# View the structure of the retrieved data
str(sst_data)

