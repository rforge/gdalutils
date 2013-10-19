

dst_filename_original  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Back up the file, since we are going to burn stuff into it.
dst_filename <- paste(tempfile(),".tif",sep="")
file.copy(dst_filename_original,dst_filename,overwrite=TRUE)

# Before plot:
plotRGB(brick(dst_filename))

src_dataset <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")

# gdal_rasterize -b 1 -b 2 -b 3 -burn 255 -burn 0 -burn 0 -l tahoe_highrez_training tahoe_highrez_training.shp tempfile.tif
tahoe_burned <- gdal_rasterize(src_dataset,dst_filename,
	b=c(1,2,3),burn=c(0,255,0),l="tahoe_highrez_training",verbose=TRUE,output_Raster=TRUE)

plotRGB(brick(dst_filename))