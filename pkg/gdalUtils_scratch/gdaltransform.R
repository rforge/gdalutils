#' gdaladdo
#' 
#' R wrapper for gdaladdo: builds or rebuilds overview images
#' 
#' @param filename Character. The file to build overviews for (or whose overviews must be removed).
#' @param levels Numeric. A list of integral overview levels to build. Ignored with clean=TRUE option.
#' @param r Character. ("nearest"|"average"|"gauss"|"cubic"|"average_mp"|"average_magphase"|"mode") Select a resampling algorithm.  Default is "nearest".
#' @param b Numeric. (available from GDAL 1.10) Select an input band band for overview generation. Band numbering starts from 1. Multiple -b switches may be used to select a set of input bands to generate overviews.
#' @param ro Logical. (available from GDAL 1.6.0) open the dataset in read-only mode, in order to generate external overview (for GeoTIFF especially).
#' @param clean Logical. (available from GDAL 1.7.0) remove all overviews.
#' @param additional_commands Character. Additional commands to pass directly to gdaladdo.
#' @param verbose Logical. Verbose execution?
#' @param ... Other parameters to pass to gdaladdo.
#' 
#' @return NULL
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net}) (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper for the 'gdaladdo' function that is part of the 
#' Geospatial Data Abstraction Library (GDAL).  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdalinfo format (\url{http://gdal.org/gdaladdo.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "gdalUtils_gdalPath" option has been set (usually by gdal_setInstallation),
#' the GDAL found in that path will be used.  If nothing is found, gdal_setInstallation
#' will be executed to attempt to find a working GDAL.
#'
#' @references \url{http://www.gdal.org/gdaladdo.html}
#' 
#' @examples \dontrun{ 
#' filename  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
#' temp_filename <- paste(tempfile(),".tif",sep="")
#' file.copy(from=filename,to=temp_filename,overwrite=TRUE)
#' gdalinfo(filename)
#' gdaladdo(r="average",temp_filename,levels=c(2,4,8,16),verbose=TRUE)
#' gdalinfo(temp_filename)
#' }
#' @export

gdaltransform <- function(srcfile,dstfile,
		i,s_srs,t_srs,to,order,tps,rpc,geoloc,gcp,
		additional_commands,
		verbose=FALSE,
		...)
{
	
	parameter_values <- as.list(environment())
	
	if(verbose) message("Checking gdal_installation...")
	gdal_setInstallation()
	
	# Start gdalinfo setup
	parameter_variables <- list(
			logical = list(
					varnames <- c("tps","rpc","geoloc","i")),
			vector = list(
					varnames <- c("")),
			scalar = list(
					varnames <- c("order")),
			character = list(
					varnames <- c("s_srs","t_srs","to","gcp","srcfile","dstfile")),
			repeatable = list(
					varnames <- c(""))
	)
	
	parameter_order <- c(
			"i","s_srs","t_srs","to","order","tps","rpc","geoloc","gcp",
			"srcfile","dstfile")
	
	parameter_noflags <- c("srcfile","dstfile")
	
	parameter_doubledash <- NULL
	
	parameter_noquotes <- NULL
	
	executable <- "gdaltransform"
	# End gdalinfo setup
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags,
			parameter_doubledash=parameter_doubledash,
			parameter_noquotes=parameter_noquotes)
	
	if(verbose) message(paste("GDAL command being used:",cmd))
	
	cmd_output <- system(cmd,intern=TRUE) 
	
	return(cmd_output)
}
