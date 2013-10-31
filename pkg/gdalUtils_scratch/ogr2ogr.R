#' ogr2ogr
#' 
#' R wrapper for ogr2ogr: converts simple features data between file formats
#' 
#' @param src_datasource_name
#' @param dst_datasource_name
#' @param layer
#' @param skipfailures
#' @param append
#' @param update
#' @param select
#' @param where
#' @param progress
#' @param sql
#' @param dialect
#' @param preserve_fid
#' @param fid
#' @param spat
#' @param geomfield
#' @param a_srs
#' @param t_srs
#' @param s_srs
#' @param f
#' @param overwrite
#' @param dsco
#' @param lco
#' @param nln
#' @param nlt
#' @param dim
#' @param gt
#' @param clipsrc
#' @param clipsrcsql
#' @param clipsrclayer
#' @param clipsrcwhere
#' @param clipdst
#' @param clipdstsql
#' @param clipdstlayer
#' @param clipdstwhere
#' @param wrapdateline
#' @param datelineoffset
#' @param simplify
#' @param segmentize
#' @param addfields
#' @param fieldTypeToString
#' @param unsetFieldWidth		
#' @param fieldmap
#' @param splitlistfields
#' @param maxsubfields
#' @param explodecollections
#' @param zfield
#' @param gcp
#' @param order
#' @param additional_commands Character. Additional commands to pass directly to ogrinfo.
#' @param verbose Logical.
#'  
#' @return character
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net}) (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper for the 'ogr2ogr' function that is part of the 
#' Geospatial Data Abstraction Library (GDAL).  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdalinfo format (\url{http://gdal.org/ogrinfo.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "gdalUtils_gdalPath" option has been set (usually by gdal_setInstallation),
#' the GDAL found in that path will be used.  If nothing is found, gdal_setInstallation
#' will be executed to attempt to find a working GDAL.
#'
#' @references \url{http://www.gdal.org/ogr2ogr.html}
#' 
#' @examples \dontrun{ 
#' src_datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
#' dst_datasource_name <- paste(tempfile(),".shp",sep="")
#' ogrInfo(src_datasource_name,"tahoe_highrez_training")
#' # reproject the input to mercator
#' ogr2ogr(src_datasource_name,dst_datasource_name,t_srs="EPSG:3395",verbose=TRUE)
#' ogrInfo(dirname(dst_datasource_name),layer=remove_file_extension(basename(dst_datasource_name)))
#' }
#' @export

# TODO: clipdst can be vector or character, fix.

ogr2ogr <- function(src_datasource_name,dst_datasource_name,
		layer,
		skipfailures,append,update,select,where,
		progress,sql,dialect,preserve_fid,fid,
		spat,geomfield,a_srs,t_srs,s_srs,
		f,overwrite,dsco,lco,nln,nlt,dim,
		gt,clipsrc,clipsrcsql,clipsrclayer,
		clipsrcwhere,clipdst,clipdstsql,clipdstlayer,
		clipdstwhere,wrapdateline,datelineoffset,
		simplify,segmentize,addfields,
		fieldTypeToString,unsetFieldWidth,		
		fieldmap,splitlistfields,maxsubfields,
		explodecollections,zfield,gcp,order,
		additional_commands,
		verbose=FALSE)
{
	
	parameter_values <- as.list(environment())
	
	if(verbose) message("Checking gdal_installation...")
	gdal_setInstallation()
	
	# Start gdalinfo setup
	parameter_variables <- list(
			logical = list(
					varnames <- c("append","overwrite","update",
							"progress","skipfailures",
							"preserve_fid","wrapdateline",
							"datelineoffset","unsetFieldWidth",
							"splitlistfields","explodecollections",
							"tps","addfields")),
			vector = list(
					varnames <- c("spat","clipdst")),
			scalar = list(
					varnames <- c("dim","gt","simplify","segmentize",
							"maxsubfields","order")),
			character = list(
					varnames <- c("format_name","select","sql","dialect",
							"where","geomfield","dsco","lco","nln","nlt",
							"a_srs","t_srs","s_srs",
							"fid","clipsrc","clipsrcsql","clipsrclayer",
							"clipsrcwhere","clipdst","clipdstsql",
							"clipdstlayer","clipdstwhere","fieldTypeToString",
							"zfield","fieldmap",
							"dst_datasource_name","src_datasource_name",
							"layer")),
			repeatable = list(
					varnames <- c("gcp"))
	)
	
	parameter_order <- c(
			"skipfailures","append","update","select","where",
			"progress","sql","dialect","preserve_fid","fid",
			"spat","geomfield","a_srs","t_srs","s_srs",			
			"f","overwrite","dsco","lco","nln","nlt","dim",
			"gt","clipsrc","clipsrcsql","clipsrclayer",
			"clipsrcwhere","clipdst","clipdstsql","clipdstlayer",
			"clipdstwhere","wrapdateline","datelineoffset",
			"simplify","segmentize","addfields",
			"fieldTypeToString","unsetFieldWidth",		
			"fieldmap","splitlistfields","maxsubfields",
			"explodecollections","zfield","gcp","order",
			"dst_datasource_name","src_datasource_name",
			"layer"
			)
	
	parameter_noflags <- c("dst_datasource_name","src_datasource_name","layer")
	
	parameter_doubledash <- NULL
	
	executable <- "ogr2ogr"
	# End ogr2ogr setup
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags,
			parameter_doubledash=parameter_doubledash)
	
	if(verbose) message(paste("GDAL command being used:",cmd))
	
	cmd_output <- system(cmd,intern=TRUE) 

		return(cmd_output)
}
