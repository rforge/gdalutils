library(sp)
library(rgdal)
library(rgeos)
library(gdalUtils)

################################################################################
# Setup test polygons

test_coords <- cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))
test_poly <- SpatialPolygons(list(Polygons(list(Polygon(test_coords)), 1)), 
                             as.integer(1), proj4string=CRS('+init=epsg:4326'))
test_poly <- SpatialPolygonsDataFrame(test_poly, data.frame('test'))
writeOGR(test_poly, '.', 'test_poly', driver='ESRI Shapefile', overwrite=TRUE)

clip_coords <- cbind(c(0, 0, .5, .5, 0), c(0, .5, .5, 0, 0))
test_clip <- SpatialPolygons(list(Polygons(list(Polygon(clip_coords)), 1)), 
                             as.integer(1), proj4string=CRS('+init=epsg:4326'))
test_clip <- SpatialPolygonsDataFrame(test_clip, data.frame('clip'))
writeOGR(test_clip, '.', 'clip_poly', driver='ESRI Shapefile', overwrite=TRUE)

plot(test_poly)
plot(test_clip, col='red', add=TRUE)

################################################################################
# Test without clipsrc - WORKS

# WORKS
ogr2ogr('test_poly.shp', 'test_unclipped.shp', overwrite=TRUE)

################################################################################
# Test specifying clipsrc as WKT - WORKS

ogr2ogr('test_poly.shp', 'test_clipped.shp', clipsrc=writeWKT(test_clip), overwrite=TRUE)

################################################################################
# Test specifying clipsrc as datasource - WORKS

ogr2ogr('test_poly.shp', 'test_clipped.shp', clipsrc='clip_poly.shp', overwrite=TRUE)

################################################################################
# Test specifying clipsrc as bounding box - FAILS

# Below fails as bounding box is not collapsed
ext <- as.numeric(bbox(test_clip))
print(ext)
ext_char <- paste(as.character(ext),collapse=" ")

ogr2ogr('test_poly.shp', 'test_clipped.shp', clipsrc=ext, overwrite=TRUE,verbose=T)
# above runs:
# "C:\OSGeo4W64\bin\ogr2ogr.exe" -clipsrc "0" -clipsrc "0" -clipsrc "0.5" -clipsrc "0.5" "test_clipped.shp" "test.shp"

# Below fails because there are quotes around the bounding box
ext <- paste(bbox(test_clip), collapse=' ')
print(ext)
ogr2ogr('test.shp', 'test_clipped.shp', clipsrc=ext, overwrite=TRUE)
# above runs:
# "C:\OSGeo4W64\bin\ogr2ogr.exe" -clipsrc "[0 0 0.5 0.5]" "test_clipped.shp" "test.shp"

# Below works (without quotes around bounding box)
system('"C:/OSGeo4W64/bin/ogr2ogr.exe" -clipsrc 0 0 0.5 0.5 "test_clipped.shp" "test.shp" -overwrite')

