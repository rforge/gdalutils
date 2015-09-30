pkgname <- "gdalUtils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "gdalUtils-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('gdalUtils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("batch_gdal_translate")
### * batch_gdal_translate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: batch_gdal_translate
### Title: batch_gdal_translate
### Aliases: batch_gdal_translate

### ** Examples

## Not run: 
##D input_folder <- system.file("external",package="gdalUtils")
##D list.files(input_folder,pattern=".tif")
##D output_folder <- tempdir()
##D # library(spatial.tools)
##D # sfQuickInit() # from package spatial.tools to launch a parallel PSOCK cluster
##D batch_gdal_translate(infiles=input_folder,outdir=output_folder,
##D 	outsuffix="_converted.envi",of="ENVI",pattern=".tif$")
##D list.files(output_folder,pattern="_converted.envi$")
##D # sfQuickStop() # from package spatial.tools to stop a parallel PSOCK cluster
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("batch_gdal_translate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdal_chooseInstallation")
### * gdal_chooseInstallation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdal_chooseInstallation
### Title: gdal_chooseInstallation
### Aliases: gdal_chooseInstallation

### ** Examples

## Not run: 
##D # Choose the best installation that has both HDF4 and HDF5 drivers:
##D gdal_chooseInstallation(hasDrivers=c("HDF4","HDF5"))
##D # Get the version of this installation:
##D getOption("gdalUtils_gdalPath")[[
##D 	gdal_chooseInstallation(hasDrivers=c("HDF4","HDF5"))]]$version
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdal_chooseInstallation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdal_cmd_builder")
### * gdal_cmd_builder

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdal_cmd_builder
### Title: gdal_cmd_builder
### Aliases: gdal_cmd_builder

### ** Examples

## Not run: 
##D # This builds a gdal_translate command.
##D executable <- "gdal_translate"
##D 
##D parameter_variables <- list(
##D 			logical = list(
##D 					varnames <- c("strict","unscale","epo",
##D 					"eco","q","sds","stats")),
##D 			vector = list(
##D 					varnames <- c("outsize","scale","srcwin",
##D 					"projwin","a_ullr","gcp")),
##D 			scalar = list(
##D 					varnames <- c("a_nodata")),
##D 			character = list(
##D 					varnames <- c("ot","of","mask","expand","a_srs",
##D 					"src_dataset","dst_dataset")),
##D 			repeatable = list(
##D 					varnames <- c("b","mo","co")))
##D 
##D parameter_order <- c(
##D 			"strict","unscale","epo","eco","q","sds","stats",
##D 			"outsize","scale","srcwin","projwin","a_ullr","gcp",
##D 			"a_nodata",
##D 			"ot","of","mask","expand","a_srs",
##D 			"b","mo","co",
##D 			"src_dataset","dst_dataset")
##D 
##D parameter_noflags <- c("src_dataset","dst_dataset")
##D 
##D # Now assign some parameters:
##D parameter_values = list(
##D 	src_dataset = "input.tif",
##D 	dst_dataset = "output.envi",
##D 	of = "ENVI",
##D 	strict = TRUE
##D )
##D 
##D cmd <- gdal_cmd_builder(
##D 			executable=executable,
##D 			parameter_variables=parameter_variables,
##D 			parameter_values=parameter_values,
##D 			parameter_order=parameter_order,
##D 			parameter_noflags=parameter_noflags)
##D 
##D cmd
##D system(cmd,intern=TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdal_cmd_builder", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdal_rasterize")
### * gdal_rasterize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdal_rasterize
### Title: gdal_rasterize
### Aliases: gdal_rasterize

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
# Example from the original gdal_rasterize documentation:
# gdal_rasterize -b 1 -b 2 -b 3 -burn 255 -burn 0
# 	-burn 0 -l tahoe_highrez_training tahoe_highrez_training.shp tempfile.tif
dst_filename_original  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Back up the file, since we are going to burn stuff into it.
dst_filename <- paste(tempfile(),".tif",sep="")
file.copy(dst_filename_original,dst_filename,overwrite=TRUE)
#Before plot:
plotRGB(brick(dst_filename))
src_dataset <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
tahoe_burned <- gdal_rasterize(src_dataset,dst_filename,
	b=c(1,2,3),burn=c(0,255,0),l="tahoe_highrez_training",verbose=TRUE,output_Raster=TRUE)
#After plot:
plotRGB(brick(dst_filename))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdal_rasterize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdal_setInstallation")
### * gdal_setInstallation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdal_setInstallation
### Title: gdal_setInstallation
### Aliases: gdal_setInstallation

### ** Examples

## Not run: 
##D # Assumes you have GDAL installed on your local machine.
##D getOption("gdalUtils_gdalPath")
##D gdal_setInstallation()
##D getOption("gdalUtils_gdalPath")
##D # If there is more than one installation of GDAL, this is the
##D # most recent installation:
##D getOption("gdalUtils_gdalPath")[[1]]
##D # The version number:
##D getOption("gdalUtils_gdalPath")[[1]]$version
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdal_setInstallation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdal_translate")
### * gdal_translate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdal_translate
### Title: gdal_translate
### Aliases: gdal_translate

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
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
## Not run: 
##D # Extract the first subdataset from an HDF4 file:
##D hdf4_dataset <- system.file("external/test_modis.hdf", package="gdalUtils")
##D gdal_translate(hdf4_dataset,"test_modis_sd1.tif",sd_index=1)
## End(Not run)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdal_translate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdaladdo")
### * gdaladdo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdaladdo
### Title: gdaladdo
### Aliases: gdaladdo

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
filename  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
temp_filename <- paste(tempfile(),".tif",sep="")
file.copy(from=filename,to=temp_filename,overwrite=TRUE)
gdalinfo(filename)
gdaladdo(r="average",temp_filename,levels=c(2,4,8,16),verbose=TRUE)
gdalinfo(temp_filename)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdaladdo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdalbuildvrt")
### * gdalbuildvrt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdalbuildvrt
### Title: gdalbuildvrt
### Aliases: gdalbuildvrt

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
layer1 <- system.file("external/tahoe_lidar_bareearth.tif", package="gdalUtils")
layer2 <- system.file("external/tahoe_lidar_highesthit.tif", package="gdalUtils")
output.vrt <- paste(tempfile(),".vrt",sep="")
gdalbuildvrt(gdalfile=c(layer1,layer2),output.vrt=output.vrt,separate=TRUE)
gdalinfo(output.vrt)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdalbuildvrt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdaldem")
### * gdaldem

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdaldem
### Title: gdaldem
### Aliases: gdaldem

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
# We'll pre-check for a proper GDAL installation before running these examples:
gdal_setInstallation()
if(!is.null(getOption("gdalUtils_gdalPath")))
{
input_dem  <- system.file("external/tahoe_lidar_highesthit.tif", package="gdalUtils")
plot(raster(input_dem),col=gray.colors(256))

# Hillshading:
# Command-line gdaldem call:
# gdaldem hillshade tahoe_lidar_highesthit.tif output_hillshade.tif
output_hillshade <- gdaldem(mode="hillshade",input_dem=input_dem,
	output="output_hillshade.tif",output_Raster=TRUE)
plot(output_hillshade,col=gray.colors(256))

# Slope:
# Command-line gdaldem call:
# gdaldem slope tahoe_lidar_highesthit.tif output_slope.tif -p
output_slope <- gdaldem(mode="slope",input_dem=input_dem,
	output="output_slope.tif",p=TRUE,output_Raster=TRUE)
plot(output_slope,col=gray.colors(256))

# Aspect:
# Command-line gdaldem call:
# gdaldem aspect tahoe_lidar_highesthit.tif output_aspect.tif
output_aspect <- gdaldem(mode="aspect",input_dem=input_dem,
	output="output_aspect.tif",output_Raster=TRUE)
plot(output_aspect,col=gray.colors(256))
}
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdaldem", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdalinfo")
### * gdalinfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdalinfo
### Title: gdalinfo
### Aliases: gdalinfo

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Command-line gdalinfo call:
# gdalinfo tahoe_highrez.tif
gdalinfo(src_dataset)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdalinfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdalsrsinfo")
### * gdalsrsinfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdalsrsinfo
### Title: gdalsrsinfo
### Aliases: gdalsrsinfo

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Command-line gdalsrsinfo call:
# gdalsrsinfo -o proj4 tahoe_highrez.tif
gdalsrsinfo(src_dataset,o="proj4")
# Export as CRS:
gdalsrsinfo(src_dataset,as.CRS=TRUE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdalsrsinfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gdalwarp")
### * gdalwarp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gdalwarp
### Title: gdalwarp
### Aliases: gdalwarp

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
# Example from the original gdal_translate documentation:
src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Command-line gdalwarp call:
# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' raw_spot.tif utm11.tif
gdalwarp(src_dataset,dstfile="tahoe_highrez_utm11.tif",
		t_srs='+proj=utm +zone=11 +datum=WGS84',output_Raster=TRUE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gdalwarp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_subdatasets")
### * get_subdatasets

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_subdatasets
### Title: get_subdatasets
### Aliases: get_subdatasets

### ** Examples

## Not run: 
##D hdf4_dataset <- system.file("external/test_modis.hdf", package="gdalUtils")
##D get_subdatasets(hdf4_dataset)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_subdatasets", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mosaic_rasters")
### * mosaic_rasters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mosaic_rasters
### Title: Mosaic raster files using GDAL Utilities
### Aliases: mosaic_rasters

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
layer1 <- system.file("external/tahoe_lidar_bareearth.tif", package="gdalUtils")
layer2 <- system.file("external/tahoe_lidar_highesthit.tif", package="gdalUtils")
mosaic_rasters(gdalfile=c(layer1,layer2),dst_dataset="test_mosaic.envi",separate=TRUE,of="ENVI",
		verbose=TRUE)
gdalinfo("test_mosaic.envi")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mosaic_rasters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ogr2ogr")
### * ogr2ogr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ogr2ogr
### Title: ogr2ogr
### Aliases: ogr2ogr

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
src_datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
dst_datasource_name <- paste(tempfile(),".shp",sep="")
ogrinfo(src_datasource_name,"tahoe_highrez_training")
# reproject the input to mercator
ogr2ogr(src_datasource_name,dst_datasource_name,t_srs="EPSG:3395",verbose=TRUE)
ogrinfo(dirname(dst_datasource_name),layer=remove_file_extension(basename(dst_datasource_name)))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ogr2ogr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ogrinfo")
### * ogrinfo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ogrinfo
### Title: ogrinfo
### Aliases: ogrinfo

### ** Examples

# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
# Display all available formats:
# Command-line ogrinfo call:
# ogrinfo --formats
ogrinfo(formats=TRUE)

# Get info on an entire shapefile:
# ogrinfo tahoe_highrez_training.shp
ogrinfo(datasource_name)

# Get info on the layer of the shapefile:
# ogrinfo tahoe_highrez_training.shp tahoe_highrez_training
ogrinfo(datasource_name,"tahoe_highrez_training")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ogrinfo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("qm")
### * qm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: qm
### Title: qm
### Aliases: qm

### ** Examples

{
qm("Hi!")
qm(42)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("qm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("remove_file_extension")
### * remove_file_extension

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: remove_file_extension
### Title: remove_file_extension
### Aliases: remove_file_extension

### ** Examples

myfilename="my.file.gri"
remove_file_extension(myfilename,".")
remove_file_extension(myfilename,".file.gri")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("remove_file_extension", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tahoe_highrez_training")
### * tahoe_highrez_training

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tahoe_highrez_training
### Title: Point and polygon files for use with gdalUtils
### Aliases: tahoe_highrez_training
### Keywords: data

### ** Examples

## Not run: 
##D tahoe_highrez_training_polygons <- readOGR(
##D 	dsn=system.file("external", package="gdalUtils"),layer="tahoe_highrez_training")
##D spplot(tahoe_highrez_training_polygons,zcol="Class")
##D tahoe_highrez_training_points <- readOGR(
##D 	dsn=system.file("external", package="gdalUtils"),layer="tahoe_highrez_training_points")
##D spplot(tahoe_highrez_training_points,zcol="SPECIES")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tahoe_highrez_training", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tahoe_lidar_bareearth.tif")
### * tahoe_lidar_bareearth.tif

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tahoe_lidar_bareearth.tif
### Title: Lidar-derived bare earth digital elevation model from the Lake
###   Tahoe Basin.
### Aliases: tahoe_lidar_bareearth.tif
### Keywords: data

### ** Examples

## Not run: 
##D tahoe_lidar_bareearth <-
##D raster(system.file("external/tahoe_lidar_bareearth.tif", package="gdalUtils"))
##D plot(tahoe_lidar_bareearth)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tahoe_lidar_bareearth.tif", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tahoe_lidar_highesthit.tif")
### * tahoe_lidar_highesthit.tif

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tahoe_lidar_highesthit.tif
### Title: Lidar-derived highest hit (aka canopy) digital elevation model
###   from the Lake Tahoe Basin.
### Aliases: tahoe_lidar_highesthit.tif
### Keywords: data

### ** Examples

## Not run: 
##D tahoe_lidar_highesthit <-
##D raster(system.file("external/tahoe_lidar_highesthit.tif", package="gdalUtils"))
##D plot(tahoe_lidar_highesthit)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tahoe_lidar_highesthit.tif", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test_modis.hdf")
### * test_modis.hdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test_modis.hdf
### Title: MODIS HDF4 file
### Aliases: test_modis.hdf
### Keywords: data

### ** Examples

## Not run: 
##D gdalinfo(system.file("external/test_modis.hdf", package="gdalUtils"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test_modis.hdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
