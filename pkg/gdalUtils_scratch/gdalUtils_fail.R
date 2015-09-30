# TODO: Add comment
# 
# Author: jgrn307
###############################################################################


library(gdalUtils)
gdal_setInstallation()

getOption("gdalUtils_gdalPath")

gdalinfo(version=TRUE)

### setInstallation search

#' gdal_setInstallation is designed to invoke consecutively more 
#' rigorous searches in able to find a valid GDAL install.  Understanding
#' the search routine may help debug problems on your system.  The order
#' of the searches is as follows, noting that as soon as a valid install
#' is found (determined by running gdalinfo --version and getting the
#' correct output), gdal_setInstallation stops further searching:
#' 1) Checks a pre-determined location given by the search_path parameter.
#' 2) Checks for gdalinfo using Sys.which().  This is typically defined
#' 		in the system's PATH, so will override any other install.
#' 3) Checks for gdalinfo in common install locations (OS specific).  
#' 4) Finally, if it can't find a valid GDAL install anywhere else,
#' 		it will brute-force search the entire local system (which may
#' 		take a long time).  


#### removing rgdal tests
# Example from the original gdal_translate documentation:
src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Original gdal_translate call:
# gdal_translate -of GTiff -co "TILED=YES" tahoe_highrez.tif tahoe_highrez_tiled.tif
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",co="TILED=YES",verbose=TRUE)
# Pull out a chunk and return as a raster:
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",
		srcwin=c(1,1,100,100),output_Raster=TRUE,verbose=TRUE)
# Notice this is the equivalent, but follows gdal_translate's parameter format:
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",
		srcwin="1 1 100 100",output_Raster=TRUE,verbose=TRUE)
# Extract the first subdataset from an HDF4 file:
hdf4_dataset <- system.file("external/test_modis.hdf", package="gdalUtils")
gdal_translate(hdf4_dataset,"test_modis_sd1.tif",sd_index=1)