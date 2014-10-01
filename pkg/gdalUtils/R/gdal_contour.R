#' gdal_contour
#' 
#' R wrapper for gdal_contour: builds vector contour lines from a raster elevation model
#' 
#' @param src_filename Character. Any OGR supported readable datasource.
#' @param dst_filename Character. The OGR supported output file. 
#' @param b Numeric. Picks a particular band to get the DEM from. Defaults to band 1.
#' @param a Character. Provides a name for the attribute in which to put the elevation. If not provided no elevation attribute is attached.
#' @param threeD Logical. (GDAL parameter '3d') Force production of 3D vectors instead of 2D. Includes elevation at every vertex.
#' @param inodata Logical. Ignore any nodata value implied in the dataset - treat all values as valid.
#' @param snodata Numeric. Input pixel value to treat as "nodata".
#' @param f Character. Create output in a particular format, default is shapefiles.
#' @param dsco Character. Dataset creation option (format specific).  Follows "NAME=VALUE" format.
#' @param lco Character. Layer creation option (format specific).  Follows "NAME=VALUE" format.
#' @param i Numeric. Elevation interval between contours.
#' @param off Numeric. Offset from zero relative to which to interpret intervals.
#' @param fl Character. Name one or more "fixed levels" to extract.
#' @param nln Character. Provide a name for the output vector layer. Defaults to "contour". 

 #' @return output vector file and NULL.
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net}) (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper for the 'gdal_contour' function that is part of the 
#' Geospatial Data Abstraction Library (GDAL).  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdal_contour format (\url{http://www.gdal.org/gdal_contour.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "gdalUtils_gdalPath" option has been set (usually by gdal_setInstallation),
#' the GDAL found in that path will be used.  If nothing is found, gdal_setInstallation
#' will be executed to attempt to find a working GDAL that has the right drivers 
#' as specified with the "of" (output format) parameter.
#' 
#' The user can choose to (optionally) return a RasterBrick of the output file (assuming
#' raster/rgdal supports the particular output format).
#'
#' @references \url{http://www.gdal.org/gdal_contour.html}
#' @examples 
#' # We'll pre-check to make sure there is a valid GDAL install
#' # and that raster and rgdal are also installed.
#' # Note this isn't strictly neccessary, as executing the function will
#' # force a search for a valid GDAL install.
#' gdal_setInstallation()
#' valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
#' if(require(raster) && require(rgdal) && valid_install)
#' {
#' # Example from the original gdal_rasterize documentation:
#' # gdal_rasterize -b 1 -b 2 -b 3 -burn 255 -burn 0 
#' # 	-burn 0 -l tahoe_highrez_training tahoe_highrez_training.shp tempfile.tif
#' dst_filename_original  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
#' # Back up the file, since we are going to burn stuff into it.
#' dst_filename <- paste(tempfile(),".tif",sep="")
#' file.copy(dst_filename_original,dst_filename,overwrite=TRUE)
#' #Before plot:
#' plotRGB(brick(dst_filename))
#' src_dataset <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
#' tahoe_burned <- gdal_rasterize(src_dataset,dst_filename,
#' 	b=c(1,2,3),burn=c(0,255,0),l="tahoe_highrez_training",verbose=TRUE,output_Raster=TRUE)
#' #After plot:
#' plotRGB(brick(dst_filename))
#' }
#' @export

gdal_contour <- function(
		src_filename,dst_filename,
		b,a,threeD,inodata,snodata,i,f,
		dsco,lco,off,fl,nln,
		additional_commands,
		ignore.full_scan=TRUE,
		verbose=FALSE)
{
	if(output_Raster && (!require(raster) || !require(rgdal)))
	{
		warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
		return(NULL)
	}
	
	parameter_values <- as.list(environment())
	
	if(verbose) message("Checking gdal_installation...")
	gdal_setInstallation(ignore.full_scan=ignore.full_scan)
	if(is.null(getOption("gdalUtils_gdalPath"))) return()
	
	# Place all gdal function variables into these groupings:
	parameter_variables <- list(
			logical = list(
					varnames <- c(
							"threeD","inodata"
					)),
			vector = list(
					varnames <- c(
								
					)),
			scalar = list(
					varnames <- c(
							"b","snodata","i","off"
					)),
			character = list(
					varnames <- c(
							"a","f","fl","nln","src_filename","dst_filename"
					)),
			repeatable = list(
					varnames <- c(
							"dsco","lco"
					))
	)
	
	parameter_order <- c(
			"b","a","threeD","inodata","snodata",
			"f","dsco","lco","i","off","fl","nln",
			"src_filename","dst_filename"
	)
	
	parameter_noflags <- c("src_filename","dst_filename")
	
	parameter_noquotes <- unlist(parameter_variables$vector)
	
	executable <- "gdal_contour"
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags,
			parameter_noquotes=parameter_noquotes,
			gdal_installation_id=gdal_chooseInstallation(hasDrivers=of))
	
	if(verbose) message(paste("GDAL command being used:",cmd))
	
	cmd_output <- system(cmd,intern=TRUE) 
	
	if(output_Raster)
	{
		return(brick(dst_filename))	
	} else
	{
		return(NULL)
	}		
}