## 05 classification land and water

## the rmarkdown is very unstable, so let's try this script.
## also, the raster functions are very slow, maybe GDAL could be used instead

rm(list=ls()) ; gc()

library(sf)
library(raster)
# library(tidyverse)
# library(data.table)
library(sp)
library(stringr)
library(rgdal)
# library(randomForest)
# library(spatialEco)
# library(party)
# library(velox)
# library(doParallel) # could be added at some point to speedup the process
# library(foreach) # could be added at some point to speedup the process

# raster options
rasterOptions(todisk=F) # forces on disk writing instead of in memory
rasterOptions(tmpdir="D:/Arthur/temp") # requires a lot of space for temporary file
# rasterOptions()
rasterOptions(maxmemory = 1e+9)

input_rasters = "D:/arthur/digitalglobe_archives/feature_extract/"
rasters = dir(input_rasters, pattern=".tif$", full.names=T)


i=6  
r = brick(rasters[i])
names(r) = c("C","B","G","Y","R","RE","N1","N","LS1","LS2","LS3","LS4","EE","HA1","HA2","HA3","HA4","HA5","HA6","HA7","HA8")

psp2 = read_sf("D:/arthur/digitalglobe_archives/water_mask/class_test_2017.gpkg")
submerged = psp2[psp2$class %in% c("water_blue","water_dark","water_brown","water_white","water_shallow","veg_submerged"),1] # specific for 2017
# submerged$dissolve = 1
# 
# st_write(submerged,dsn = "D:/arthur/digitalglobe_archives/water_mask/todissolve_2017.gpkg", layer = "todissolve_2017", driver="GPKG")
# 
# library(RSAGA)
# 
# work_env = rsaga.env(path = "C:/Users/degranda/Desktop/saga-7.6.2_x64")
# work_env
# 
# rsaga.get.libraries()
# rsaga.get.modules("shapes_polygons")
# rsaga.get.usage("shapes_polygons",5)
# 
# rsaga.geoprocessor("shapes_polygons", module = 5,
#                    env = work_env,
#                    param = list(POLYGONS = submerged,
#                                 FIELD_1  = dissolve,
#                                 DISSOLVED = "D:/arthur/digitalglobe_archives/water_mask/dissolve_test.shp",
#                                 STAT_NAMING = 3))
# 
# 
# rsaga.union.polygons(layer_a=psp2, layer_b=psp2, result = "D:/arthur/digitalglobe_archives/water_mask/saga_test.shp", env=work_env)


submerged_mask = st_union(submerged)
submerged_mask2 = st_cast(submerged_mask,"POLYGON")
st_write(submerged_mask2,dsn = "D:/arthur/digitalglobe_archives/water_mask/2017_submask.gpkg", layer = "2017_submask", driver="GPKG")

# plot(submerged_mask2)
spmask = as_Spatial(submerged_mask2)
water = mask(r[[1:8]], spmask)

output_rasters = "D:/arthur/digitalglobe_archives/water_mask/"
writeRaster(water, paste0(output_rasters,str_sub(rasters[i],-46)), overwrite=T)
