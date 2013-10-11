
gdal_get_executable <- function(executable)
{
	executable <- normalizePath(list.files(getOption("gdalUtils_gdalPath")[[1]]$path,
					executable,full.names=TRUE))# [1] is prov!
	
	return(executable)
	
}