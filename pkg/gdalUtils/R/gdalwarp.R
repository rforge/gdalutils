#gdalwarp [--help-general] [--formats]
#[-s_srs srs_def] [-t_srs srs_def] [-to "NAME=VALUE"]
#[-order n | -tps | -rpc | -geoloc] [-et err_threshold]
#[-refine_gcps tolerance [minimum_gcps]]
#[-te xmin ymin xmax ymax] [-tr xres yres] [-tap] [-ts width height]
#[-wo "NAME=VALUE"] [-ot Byte/Int16/...] [-wt Byte/Int16]
#[-srcnodata "value [value...]"] [-dstnodata "value [value...]"] -dstalpha
#[-r resampling_method] [-wm memory_in_mb] [-multi] [-q]
#[-cutline datasource] [-cl layer] [-cwhere expression]
#[-csql statement] [-cblend dist_in_pixels] [-crop_to_cutline]
#[-of format] [-co "NAME=VALUE"]* [-overwrite]
#[-nomd] [-cvmd meta_conflict_value] [-setci]
#srcfile* dstfile

#' @export

gdalwarp <- function(
		#help_general,formats, # Need to fix these
		s_srs,t_srs,to,
		order,et,refine_gcps,te,tr,tap,ts,wo,ot,wt,srcnodata,dstnodata,
		dstalpha,r,wm,multi,q,cutline,cl,cwhere,csql,cblend,crop_to_cutline,
		of,co,overwrite,nomd,cvmd,setci,srcfile,dstfile,
		output_Raster=FALSE,verbose=FALSE)
{
	parameter_values <- as.list(environment())
	# Place all gdal function variables into these groupings:
	parameter_variables <- list(
			logical = list(
					varnames <- c(
							"tps","rpc","geoloc","tap","dstalpha",
							"multi","q","crop_to_cutline","overwrite","nomd",
							"setci"
					)),
			vector = list(
					varnames <- c(
							"te","tr","ts"
					)),
			scalar = list(
					varnames <- c(
							"order","et","refine_gcps","wm",
							"cblend"
					)),
			character = list(
					varnames <- c(
							"s_srs","t_srs","to","ot","wt","r",
							"srcnodata","dstnodata","of","cutline","cl",
							"cwhere","csql","cvmd","dstfile"
					)),
			repeatable = list(
					varnames <- c(
							"wo","co","srcfile"
					))
	)
	
	parameter_order <- c(
			"tps","rpc","geoloc","tap","dstalpha",
			"multi","q","crop_to_cutline","overwrite","nomd",
			"setci",
			"te","tr","ts",
			"order","et","refine_gcps","wm",
			"cblend",
			"s_srs","t_srs","to","ot","wt","r",
			"srcnodata","dstnodata","of","cutline","cl",
			"cwhere","csql","cvmd",
			"wo","co",
			"srcfile","dstfile"
	)
					
	parameter_noflags <- c("srcfile","dstfile")
	
	executable <- file.path(gdal_path()[1],"gdalwarp")
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags)
	
#	print(cmd)
	
	gdal_translate_output <- system(cmd,intern=TRUE) 

	if(output_Raster)
	{
		return(brick(dstfile))	
	} else
	{
		return(NULL)
	}	
	
}


# c:/PROGRA~2/FWTOOL~1.7/bin/gdalwarp "MOD13Q1.A2001001.h08v04.005.2008269154350.tif" "MOD13Q1.A2001001.h08v05.005.2008269153135.tif" "testgw.tif"
