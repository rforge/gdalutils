gdalsrsinfo <- function(srs_def,p,V,o,
	additional_commands,
	verbose=FALSE)
{
	parameter_values <- as.list(environment())
	
	if(verbose) message("Checking gdal_installation...")
	gdal_setInstallation()
	
	# Start gdalinfo setup
	parameter_variables <- list(
			logical = list(
					varnames <- c("p","V")),
			vector = list(
					varnames <- NULL),
			scalar = list(
					varnames <- c("sd")),
			character = list(
					varnames <- c("o","srs_def")),
			repeatable = list(
					varnames <- NULL)
	)
	
	parameter_order <- c(
			"p","V","o","srs_def")
	
	parameter_noflags <- c("srs_def")
	
	executable <- "gdalsrsinfo"
	# End gdalinfo setup
	
	cmd <- gdal_cmd_builder(
			executable=executable,
			parameter_variables=parameter_variables,
			parameter_values=parameter_values,
			parameter_order=parameter_order,
			parameter_noflags=parameter_noflags)
	
	if(verbose) message(paste("GDAL command being used:",cmd))
	
	cmd_output <- system(cmd,intern=TRUE) 
	
	if(verbose) { message(gdal_translate_output) } 
	return(cmd_output)
}
