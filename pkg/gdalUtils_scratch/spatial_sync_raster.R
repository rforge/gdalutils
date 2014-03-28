spatial_sync_raster <- function(unsynced,reference,dstfile,output_Raster=FALSE,...)
{
	# Get projection from reference
	reference_info <- gdalinfo(reference,verbose=TRUE,proj4=TRUE,raw_output=FALSE)
	
#	cmd_output <- gdalinfo(reference,verbose=TRUE,proj4=TRUE,raw_output=FALSE)
	
	proj4_string <- reference_info$proj4
	
	bbox <- reference_info$bbox
	te <- c(reference_info$bbox[1,1],reference_info$bbox[2,1],reference_info$bbox[1,2],reference_info$bbox[2,2])
	ts <- c(reference_info$columns,reference_info$rows)
	
	if(missing(dstfile))
		dstfile <- tempfile()
				
	synced <- gdalwarp(srcfile=unsynced,dstfile=dstfile,te=te,t_srs=proj4_string,ts=ts,output_Raster=output_Raster,...)
	return(synced)
	
}