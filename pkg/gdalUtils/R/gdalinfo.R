#' Rgdal_translate
#' 
#' R wrapper for gdal_translate
#' 
#' @param src_dataset Character. The source dataset name. It can be either file name, URL of data source or subdataset name for multi-dataset files.
#' @param dst_dataset Character. The destination file name.
#' @param ot Character. ("Byte"/"Int16"/"UInt16"/"UInt32"/"Int32"/"Float32"/"Float64"/"CInt16"/"CInt32"/"CFloat32"/"CFloat64"). For the output bands to be of the indicated data type.
#' @param strict Logical. Don't be forgiving of mismatches and lost data when translating to the output format.
#' @param of Character. Select the output format. The default is GeoTIFF (GTiff). Use the short format name.
#' @param b Numeric or Character. Select an input band band for output. Bands are numbered from 1. Multiple bands may be used to select a set of input bands to write to the output file, or to reorder bands. Starting with GDAL 1.8.0, band can also be set to "mask,1" (or just "mask") to mean the mask band of the first band of the input dataset.
#' @param mask Numeric. (GDAL >= 1.8.0) Select an input band band to create output dataset mask band. Bands are numbered from 1. band can be set to "none" to avoid copying the global mask of the input dataset if it exists. Otherwise it is copied by default ("auto"), unless the mask is an alpha channel, or if it is explicitly used to be a regular band of the output dataset ("-b mask"). band can also be set to "mask,1" (or just "mask") to mean the mask band of the 1st band of the input dataset.
#' @param expand Character. ("gray"|"rgb"|"rgba").  (From GDAL 1.6.0) To expose a dataset with 1 band with a color table as a dataset with 3 (RGB) or 4 (RGBA) bands. Useful for output drivers such as JPEG, JPEG2000, MrSID, ECW that don't support color indexed datasets. The 'gray' value (from GDAL 1.7.0) enables to expand a dataset with a color table that only contains gray levels to a gray indexed dataset.
#' @param outsize Numeric. (c(xsize[%],ysize[%])). Set the size of the output file. Outsize is in pixels and lines unless '%' is attached in which case it is as a fraction of the input image size.
#' @param scale Numeric. (c(src_min,src_max,dst_min,dst_max)). Rescale the input pixels values from the range src_min to src_max to the range dst_min to dst_max. If omitted the output range is 0 to 255. If omitted the input range is automatically computed from the source data.
#' @param unscale Logical. Apply the scale/offset metadata for the bands to convert scaled values to unscaled values. It is also often necessary to reset the output datatype with the -ot switch.
#' @param srcwin Numeric. (c(xoff,yoff,xsize,ysize)).  Selects a subwindow from the source image for copying based on pixel/line location.
#' @param projwin Numeric. (c(ulx,uly,lrx,lry)).  Selects a subwindow from the source image for copying (like -srcwin) but with the corners given in georeferenced coordinates.
#' @param epo Logical. (Error when Partially Outside)  (GDAL >= 1.10) If this option is set, -srcwin or -projwin values that falls partially outside the source raster extent will be considered as an error. The default behaviour starting with GDAL 1.10 is to accept such requests, when they were considered as an error before.
#' @param eco Logical. (Error when Completely Outside) (GDAL >= 1.10) Same as -epo, except that the criterion for erroring out is when the request falls completely outside the source raster extent.
#' @param a_srs Character.  Override the projection for the output file. The srs_def may be any of the usual GDAL/OGR forms, complete WKT, PROJ.4, EPSG:n or a file containing the WKT.
#' @param a_ullr Numeric. (c(ulx,uly,lrx,lry)). Assign/override the georeferenced bounds of the output file. This assigns georeferenced bounds to the output file, ignoring what would have been derived from the source file.
#' @param a_nodata Numeric. Assign a specified nodata value to output bands. Starting with GDAL 1.8.0, can be set to none to avoid setting a nodata value to the output file if one exists for the source file
#' @param mo Character. ("META-TAG=VALUE").  Passes a metadata key and value to set on the output dataset if possible.
#' @param co Character. ("NAME=VALUE"). Passes a creation option to the output format driver. Multiple -co options may be listed. See format specific documentation for legal creation options for each format.
#' @param gcp Numeric. (c(pixel,line,easting,northing(,elevation))). Add the indicated ground control point to the output dataset. This option may be provided multiple times to provide a set of GCPs.
#' @param q Logical. Suppress progress monitor and other non-error output.
#' @param sds Logical. Copy all subdatasets of this file to individual output files. Use with formats like HDF or OGDI that have subdatasets.
#' @param stats Logical. (GDAL >= 1.8.0) Force (re)computation of statistics.
#' @param additional_commands Character. Additional commands to pass directly to gdal_translate.
#' @param modis_sds_index Numeric. If the file is a MODIS HDF4 file, which subdataset should be returned (1 to the number of subdatasets)?  If this flag is used, src_dataset should be the filename of the HDF4 file.
#' @param output_Raster Logical. Return output dst_dataset as a RasterBrick?
#' @param verbose Logical.
#' @return NULL or if(output_Raster), a RasterBrick.
#' @author Jonathan A. Greenberg (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper to the gdal_translate function that is part of the 
#' Geospatial Data Abstraction Library (GDAL) library.  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdal_translate format (\url{http://www.gdal.org/gdal_translate.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "spatial.tools.gdalInstallation" option has been set (usually by get_gdal_installation),
#' the GDAL found in that path will be used.  If nothing is found, get_gdal_installation
#' will attempt to find a working GDAL that has the right drivers as specified with the
#' "of" (output format) parameter.
#' 
#' The user can choose to (optionally) return a RasterBrick of the output file (assuming
#' raster/rgdal supports the particular output format).
#'
#' @references \url{http://www.gdal.org/gdal_translate.html}
#' @examples \dontrun{ 
#' # Example from the original gdal_translate documentation:
#' src_dataset <- system.file("external/tahoe_highrez.tif", package="spatial.tools")
#' # Original gdal_translate call:
#' # gdal_translate -of GTiff -co "TILED=YES" tahoe_highrez.tif tahoe_highrez_tiled.tif
#' # Rgdal_translate:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",co="TILED=YES",verbose=TRUE)
#' # Pull out a chunk and return as a raster:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",
#' srcwin=c(1,1,100,100),output_Raster=TRUE,verbose=TRUE)
#' # Notice this is the equivalent, but follows gdal_translate's parameter format:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",
#' srcwin="1 1 100 100",output_Raster=TRUE,verbose=TRUE)
#' }
#' @export

gdalinfo <- function(datasetname,mm,stats,
	hist,nogcp,nomd,norat,noct,nofl,checksum,
	proj4,mdd,sd,additional_commands,
	raw_output=FALSE,verbose=FALSE)
{
	parameter_values <- as.list(environment())

	if(verbose) message("Checking gdal_installation...")
	gdal_setInstallation()
	
	# Start gdalinfo setup
	parameter_variables <- list(
			logical = list(
					varnames <- c("mm","stats","approx_stats","hist","nogcp","nomd",
							"nrat","noct","checksum","nofl","proj4")),
			vector = list(
					varnames <- NULL),
			scalar = list(
					varnames <- c("sd")),
			character = list(
					varnames <- c("mdd","datasetname")),
			repeatable = list(
					varnames <- NULL)
	)
	
	parameter_order <- c(
			"mm","stats","hist","nogcp","nomd","norat","noct","nofl","checksum",
			"proj4","mdd","sd","datasetname")
	
	parameter_noflags <- c("datasetname")
	
	executable <- "gdalinfo"
	# End gdalinfo setup
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags)
	
	if(verbose) message(paste("GDAL command being used:",cmd))
	
	cmd_output <- system(cmd,intern=TRUE) 
	
	if(verbose) { message(cmd_output) } 
	
	# (Optional) return Raster
	if(raw_output)
	{
		return(cmd_output)	
	} else
	{
		result <- list()
		
		# Raster size (in pixels and lines).
		dims          <- strsplit(gsub(grep(cmd_output,pattern="Size is ",value=TRUE), pattern="Size is ",replacement=""),",")[[1]]
		result$rows   <- as.numeric(dims[2])
		result$columns<- as.numeric(dims[1])
		
		orig          <- as.numeric(strsplit(gsub(strsplit(grep(cmd_output,pattern="Lower Left  \\(",value=TRUE), "\\) \\(")[[1]][1],pattern="Lower Left  \\(",replacement=""),",")[[1]])
		result$ll.x   <- orig[1]
		result$ll.y   <- orig[2]
		
		res           <- as.numeric(strsplit(gsub(gsub(grep(cmd_output,pattern="Pixel Size = \\(",value=TRUE),pattern="Pixel Size = \\(",replacement=""),pattern="\\)",replacement=""),",")[[1]])
		result$res.x  <- res[1]
		result$res.y  <- res[2]
		
		result$file <- gsub(grep(cmd_output,pattern="Files: ",value=TRUE),pattern="Files: ",replacement="")
		
		
#		result$oblique.x <- NA
#		result$oblique.y <- NA
		
		# The format driver used to access the file.
		result$driver <- strsplit(gsub(grep(cmd_output,pattern="Driver: ",value=TRUE), pattern="Driver: ",replacement=""),"/")[[1]][1]
		
		# The coordinate system for the file (in OGC WKT).
		# TODO
	
		# The geotransform associated with the file (rotational coefficients are currently not reported).
		# TODO
	
		# Corner coordinates in georeferenced, and if possible lat/long based on the full geotransform (but not GCPs).
		# TODO
	
		# Ground control points.
		# TODO
		
		# File wide (including subdatasets) metadata.
		# TODO
	
		# Band data types.
		# TODO
	
		# Band color interpretations.
		# TODO	
		
		# Band block size.
		# TODO
		
		# Band descriptions.
		# TODO
		
		# Band min/max values (internally known and possibly computed).
		# TODO
		
		# Band checksum (if computation asked).
		# TODO

		# Band NODATA value.
		# TODO

		# Band overview resolutions available.
		# TODO

		# Band unit type (i.e.. "meters" or "feet" for elevation bands).
		# TODO

		# Band pseudo-color tables.
		# TODO	
	
		return(result)
	
	}	
}
