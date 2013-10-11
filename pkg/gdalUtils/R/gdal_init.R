#' Find GDAL installation
#' 
#' Tools for properly locating and determing the state of the Geospatial Data Abstraction Library
#' 
#' @param return_drivers Logical. Return a table of GDAL drivers? Default is TRUE. 
#' @param return_python_utilities Logical. Return a vector of available python utilities? Default is TRUE. 
#' @param return_most_current Logical. Return only the most current version of GDAL (if multiple installs are present)? Default is TRUE.
#' @param required_drivers Character. What driver is required?  Default is no required drivers.
#' @param setOptions Logical. Set the "spatial.tools.gdalInstallation" option for use with other functions?  Default is TRUE. 
#' @param verbose logical. Enable verbose execution? Default is FALSE. 
#' @param scan logical. Re-Scan your drive to detect GDAL installations. 
#' @return A list with one element per GDAL installation.  See Description for parameter names.
#' @author Jonathan A. Greenberg and Matteo Mattiuzzi
#' @keywords format
#' @details gdal_installation is designed to help determine the correct path to the
#' Geospatial Data Abstraction Library, even if the PATH is not set on the local computer.  It
#' accomplishes this by brute-force searching for the gdalinfo(.exe) executable on the user's
#' local system, starting at the root level ("/" on Unix-alikes, "C:" on Windows).  It can
#' also (optionally) return information on the available drivers, python utilities, and can
#' determine the "best" (most current version) of GDAL if there are multiple installs (more
#' common on Windows boxes than Unix-alikes).
#'
#' Each user's installation will be different, but in the author's experience, for each OS, we 
#' recommend the following installations: 
#' 
#' Unix: See \url{http://gdal.org} for building/installing GDAL on your system. 
#' 
#' Mac: Install the William Kyngesburye's GDAL Complete Framework: \url{http://www.kyngchaos.com/software:frameworks}
#' 
#' Windows: Several choices (in order of the author's preference):
#' \itemize{
#' \item Standalone QGIS Installer: \url{http://hub.qgis.org/projects/quantum-gis/wiki/Download#11-Standalone-Installer-recommended-for-new-users}
#' \item OSGeo4W (follow the "Quick Start for OSGeo4W Users"): \url{http://trac.osgeo.org/osgeo4w/}
#' \item FWTools, 32-bit only: \url{http://fwtools.maptools.org/}
#' }
#' 
#' @references \url{http://gdal.org}
#' @examples \dontrun{ 
#' # Determine the most current GDAL installations:
#' mygdals <- gdal_installation()
#' mygdals[[1]]$gdal_path
#' mygdals[[1]]$version
#' mygdals[[1]]$drivers
#' mygdals[[1]]$python_utilities
#' # Determine all available GDAL installations:
#' mygdals <- gdal_installation(return_most_current=FALSE)
#' sapply(mygdals,function(X) X$gdal_path)
#' # Only return GDAL installs that support a given driver: 
#' mygdals <- gdal_installation(required_drivers="HDF4")
#' }
#' @export

# TODO: multiple steps of searching (stopping if found):
# 1) Search in PATH
# 2) Search in common install locations
# 3) Brute force search
# add a "force_search" parameter that automatically
# brute force searches.

gdal_init <- function()
{
	gdal_paths <- gdal_path()
	options(gdalUtils_gdalPath=data.frame(path=gdal_paths,stringsAsFactors=FALSE))
	gdal_versions <- gdal_version()
	gdal_installations <- gdal_installation(sort_most_current=FALSE)
	
	
}


gdal_path <- function(
		search_path,
		ignore.options=FALSE,
		ignore.which=FALSE,
		ignore.common=FALSE,
		force_full_scan = FALSE, 
		checkValidity, 
		search_path_recursive=FALSE,
		verbose = FALSE)
{
	owarn <- getOption("warn")
	options(warn=-2)
	on.exit(options(warn=owarn))
	
	if(missing(checkValidity))
	{
		if(is.null(getOption("gdalUtils_gdalPath"))) checkValidity=TRUE else checkValidity=FALSE
	}
	
	path <- NULL
	# Rescan will override everything.
	if(!force_full_scan)
	{
		# Check options first.
		if(!ignore.options)
		{
			if(verbose) message("Checking the gdalUtils_gdalPath option...")
			option_paths <- unlist(
					sapply(getOption("gdalUtils_gdalPath"),function(x) return(x$path)))
			if(!is.null(option_paths) && checkValidity)
			{
				option_paths_check <- gdal_check_validity(option_paths)
				option_paths <- option_paths[option_paths_check]
			}
			path <- c(path,option_paths)
		}
		
		# Next try Sys.which unless ignored:
		if(!ignore.options && length(path)==0)
		{
			if(verbose) message("Checking Sys.which...")
			Sys.which_path <- dirname(Sys.which("gdalinfo"))
			if(Sys.which_path=="") Sys.which_path <- NULL
			if(!is.null(Sys.which_path) && checkValidity)
			{
				Sys.which_path_check <- gdal_check_validity(Sys.which_path)
				Sys.which_path <- Sys.which_path[Sys.which_path_check]
			}
			path <- c(path,Sys.which_path)
		}
		
		# Next, try scanning the search path
		if(!missing(search_path) && length(path)==0)
		{
			if(verbose) message("Checking the search path...")
			search_paths <- normalizePath(dirname(
							list.files(path=search_path,pattern="gdalinfo",
									recursive=search_path_recursive,full.names=TRUE)))
			if(length(search_paths)==0) search_paths <- NULL
			if(!is.null(search_paths) && checkValidity)
			{
				search_paths_check <- gdal_check_validity(search_paths)
				search_paths <- search_paths[search_paths_check]
			}
			path <- c(path,search_paths)
			
		}
		
		# If nothing is still found, look in common locations
		if(!ignore.common && length(path)==0)
		{
			if(verbose) message("Checking common locations...")
			if (.Platform$OS=="unix")
			{
				common_locations <- c(
						# UNIX systems
						"/usr/bin",
						"/usr/local/bin",
						# Mac
						# Kyngchaos frameworks:
						"/Library/Frameworks/GDAL.framework/Programs",
						# MacPorts:
						"/opt/local/bin"
				)
			}
			
			if (.Platform$OS=="windows")
			{
				common_locations <- c(
						"C:\\Program Files",
						"C:\\Program Files (x86)",
						"C:\\OSGeo4W"
				)
			}
			
			if(length(common_locations != 0))
			{
				common_paths <- unlist(sapply(common_locations,
								function(x)
								{
									search_common_paths <- normalizePath(dirname(
													list.files(path=x,pattern="gdalinfo",recursive=TRUE,full.names=TRUE)))
									return(search_common_paths)
								}))
				if(length(common_paths)==0) common_paths <- NULL
				if(!is.null(common_paths) && checkValidity)
				{
					common_paths_check <- gdal_check_validity(common_paths)
					common_paths <- common_paths[common_paths_check]
				}
				path <- c(path,common_paths)
			}
		}
		if(length(path)==0)
		{
			force_full_scan=TRUE
		}
	}
	
	if(force_full_scan)
	{
		if(verbose) message("Scanning your root-dir for available GDAL installations,... This could take some time...")
		if (.Platform$OS=="unix")
		{
			root_dir <- "/"	
		}
		
		if (.Platform$OS=="windows")
		{
			root_dir <- "C:\\"
		}
		
		search_full_path <- normalizePath(dirname(
						list.files(path=root_dir,pattern="gdalinfo",
								recursive=TRUE,full.names=TRUE)))
		if(length(search_full_path)==0) search_full_path <- NULL
		if(!is.null(search_full_path) && checkValidity)
		{
			search_full_path_check <- gdal_check_validity(search_full_path)
			search_full_path <- search_full_path[search_full_path_check]
		}
		path <- c(path,search_paths)
	}
	
	if(length(path)==0)
	{
		#add QGIS?
		stop("No GDAL installation found. Please install 'gdal' before continuing:\n\t- www.gdal.org (no HDF4 support!)\n\t- www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMENDED)\n\t- www.fwtools.maptools.org (with HDF4 support)\n") # why not stop?
	}
	
	return(correctPath(path))
}

gdal_setPath <- function(path, makePermanent=FALSE)
{
#	path <- gdal_path(ignore.options=TRUE)
	options(gdalUtils_gdalPath=gdal_installation())
	
	# try to write options to 'Rprofile.site' (in file.path(R.home(),"etc")), if no write permission is given, write (create if not existing) a .Rprofile in isers home path.expand("~"). 
	if (makePermanent)
	{
		f <- paste0(file.path(R.home(),"etc"),"/Rprofile.site")
		
		owarn <- options()$warn
		options(warn=-2)
		on.exit(options(warn=owarn))
		
		con  <- try( file(f,"r+b"),silen=TRUE)
		
		if(inherits(con,"try-error"))
		{
			if(!file.exists("~/.Rprofile"))
			{
				doit <- toupper(readline(paste0("No write access to: '", paste0(file.path(R.home(),"etc"),"/Rprofile.site") ,"', should '",path.expand("~/.Rprofile") ,"' be created? [y/n]: ")))
				if (doit %in% c("Y","YES","J","JA","SI","S"))
				{
					file.create("~/.Rprofile")              
				} else 
				{
					return()
				}
			}
			con  <- file("~/.Rprofile","r+b")       
		}
		
		sets  <- readLines(con)
		linec <- grep(sets,pattern="options\\(gdalUtils_gdalPath=")
		
		if (length(linec)!=0) # if already existing update content
		{
			sets[linec] <- paste0("options(gdalUtils_gdalPath=c('",paste0(path,collapse="', '"),"'))") 
		} else
		{
			linet <- grep(sets,pattern="# Options for the 'gdalUtils' package")
			if(length(linet)!=0)
			{
				sets <- append(sets,c(paste0("options(gdalUtils_gdalPath=c('",paste0(path,collapse="', '"),"'))")," "),after=linet)
			} else
			{
				sets <- c(sets, " ","# Options for the 'gdalUtils' package",paste0("options(gdalUtils_gdalPath=c('",paste0(path,collapse="', '"),"'))")," ")
			}            
		}
		writeLines(text=sets,con=con)
		close(con)
	}
} 

gdal_version <- function(path, newerThan=NULL, verbose=FALSE)
{
	if(missing(path)) { path <- gdal_path() }
	
	cmd <- normalizePath(list.files(path, "gdalinfo",full.names=TRUE))
	cmd <- paste0('"',cmd,'"'," --version")
	
	result <- lapply(cmd,system,intern=TRUE)
	
	# It seams that system works also in Windows (MAC?), do you confirm?
	# Does shell work here?
	# if (.Platform$OS=="unix") 
	#{
	# gdal_version <- system(cmd,intern=TRUE) 
	# else 
	#{
	#gdal_version <- shell(cmd,intern=TRUE)
	#}
	
	res <- sapply(result,length)
	
	if(sum(res)!=length(result))
	{
		message("Probably broken install of gdal at '",paste0(path[which(res!=1)],collapse= "' and '"),"'")
	}
	result <- result[res==1]
	
	date <- version <- vector(mode = "list", length = length(result))
	
	for(i in seq_along(result))
	{
		ingd         <- strsplit(result[[i]],",")[[1]]
		version[[i]] <- gsub(ingd[1],pattern="GDAL ",replacement="")
		ind          <- grep(ingd,pattern="releas") # should this be: glob2rx("GDAL*")?
		date[[i]]    <- as.character(as.Date(gsub(ingd[ind],pattern=" released ",replacement=""),format="%Y/%m/%d"))
	}
	
	if(!is.null(newerThan))
	{
		test <- try(as.Date(newerThan),silent=TRUE)
		if(!inherits(test,"try-error"))
		{
			datein <- lapply(date,as.Date)
			res    <- sapply(datein,">=",as.Date(newerThan)) 
		} else
		{
			version   <- gsub(tolower(version),pattern="[a-z]",replacement="")
			res       <- sapply(version,strsplit,"\\.") 
			newerThan <- strsplit(newerThan,"\\.")[[1]]
			
			for(i in seq_along(res))
			{
				difs <- as.numeric(res[[i]]) - as.numeric(newerThan)
				difs <- sign(difs)
				
				if(sum(difs==-1)==0)
				{
					res[[i]] <- TRUE
				} else
				{
					if(difs[1]<0)
					{
						res[[i]] <- FALSE
					} else if(difs[1]>0)
					{
						res[[i]] <- TRUE
					} else if(difs[1]==0)
					{
						if(difs[2]<0)
						{
							res[[i]] <- FALSE
						} else if(difs[2]>0)
						{
							res[[i]] <- FALSE
						} else
						{  
							if(difs[3]>=0)
							{
								res[[i]] <- TRUE                  
							} else if (difs[3]<0)
							{
								res[[i]] <- FALSE
							}
						}
					}
				}
			}
		}
		names(res) <- path
		return(res)
	}
	result <- as.data.frame(cbind(path=path[res==1],version=version,date=date), stringsAsFactors=FALSE)
	return(result)
}

# browser()
# http://www.gdal.org/gdal_utilities.html --formats
# The format support is indicated as follows: 
# 'ro' is read-only driver; 'rw' is read or write (ie. supports CreateCopy);
# 'rw+' is read, write and update (ie. supports Create).
# 'v' is appended for formats supporting virtual IO (/vsimem, /vsigzip, /vsizip, etc).
# 's' is appended for formats supporting subdatasets. 
# Note: The valid formats for the output of gdalwarp are formats that support the Create() method (marked as rw+), not just the CreateCopy() method.

gdal_drivers <- function(path, verbose=FALSE)   
{
	if(missing(path)) path <- gdal_path(checkValidity=TRUE)
	
	cmd <- file.path(path, "gdalinfo")
	cmd <- paste0('"',cmd,'"'," --formats")
	
	drivers_raw <- lapply(cmd,system,intern=TRUE)
	
	result <- vector(mode='list',length(path))
	names(result) <- path
	for(i in seq_along(drivers_raw))
	{
		drivers_raw[[i]] <- drivers_raw[[i]][-1]
		drivers=strsplit(drivers_raw[[i]],":")
		driver_names=gsub("^ ","",sapply(drivers,function(x) { x[2] })) # Need to remove spaces
		driver_codes_perm=strsplit(sapply(drivers,function(x) { x[1] }),"\\(")
		driver_codes=gsub(" ","",sapply(driver_codes_perm,function(x) { x[1] }),fixed=TRUE)
		driver_perm=gsub("\\)","",sapply(driver_codes_perm,function(x) { x[2] }))
		
		r <- w <- u <- v <- s <- rep(FALSE,length(driver_perm))
		r[grep(driver_perm, pattern="r")]   <- TRUE
		w[grep(driver_perm, pattern="w")]   <- TRUE
		u[grep(driver_perm, pattern="\\+")] <- TRUE
		v[grep(driver_perm, pattern="v")]   <- TRUE
		s[grep(driver_perm, pattern="s")]   <- TRUE	
		
		result[[i]] <- data.frame(format_code=driver_codes,read=r,write=w,update=u,virtualIO=v,subdatasets=s,format_name=driver_names)
	}
	return(result)
}

gdal_supports <- function(driver, read=NULL, write=NULL, update=NULL, virtualIO=NULL, subdatasets=NULL, path, stopOnFirst=FALSE)
{
	path <- gdal_sortInstallation(path)
	# debug
	# driver="HDA"; read=NULL; write=NULL; update=NULL; virtualIO=NULL; subdatasets=NULL; stopOnFirst=FALSE;path <- gdal_sortInstallation()
	result <- vector(mode="list",length(path))
	names(result) <- path
	
	if(!isTRUE(read)) {read <- NULL}
	if(!isTRUE(write)) {write <- NULL}
	if(!isTRUE(update)) {update <- NULL}
	if(!isTRUE(virtualIO)) {virtualIO <- NULL}
	if(!isTRUE(subdatasets)) {subdatasets <- NULL}
	
	justPrint <- sum(c(read,write,update,virtualIO,subdatasets),na.rm=TRUE)==0
	
	check <- 0
	for(i in seq_along(path))
	{
		drivers <- gdal_drivers(path[i])[[1]]
		drivers <- drivers[grep(drivers$format_code,pattern=paste0("^",driver,"$"),ignore.case=TRUE),] # regex is fixed for exact match, we could also let search by regex...but it could be dangerous
		
		if(nrow(drivers)>0)
		{
			if(justPrint)
			{
				result[[i]] <- drivers
			} else
			{
				test <- all(c(drivers$read==read, drivers$write==write, drivers$update==update, drivers$virtualIO==virtualIO, drivers$subdatasets==subdatasets))
				
				result[[i]] <- test
				
				if(stopOnFirst & test)
				{
					return(result[i])
				}
			}
		} else
		{
			result[[i]] <- FALSE
			check <- check + 1
		}
	}
	if (check==length(result))
	{
		warning("Driver '", driver,"' not found, run 'gdal_drivers()' column 'format_code' for available drivers.")
	}
	return(result)
}

gdal_python_utilities <- function(path)
{
	if(missing(path)) { path <- gdal_path() }
	sapply(path,list.files,pattern="\\.py")
}

gdal_getExtension <- function(dataFormat)
{
	path <- gdal_path()
	path <- gdal_sortInstallation(path)
	
	check <- 0
	result <- list()
	
	for(i in seq_along(path))
	{
		cmd <- file.path(path[i],'gdalinfo --format ')
		cmd <- paste0(cmd,dataFormat) 
		
		owarn <- options()$warn
		options(warn=-2)
		
		ext <- system(cmd,intern=TRUE)
		
		options(warn=owarn)
		if(length(ext)>0)
		{
			ext <- grep(ext,pattern="Extension:",value=TRUE)
			
			if (length(ext)>0)
			{
				ext <- gsub(strsplit(ext,":")[[1]][2], pattern=" ", replacement="")
				
				if (ext!="")
				{
					ext <- paste0(".",ext)
				}
				names(ext) <- path[i]
				return(ext)
			}
		}
	}
	warning("No extension found for '",dataFormat ,"', run: 'gdal_drivers()' and see in column 'format_code' for available drivers!")
	return("")
}

# sort GDALs by release date
gdal_sortInstallation <- function(path)
{
	if(missing(path)) path <- gdal_path()
	path_versions <- gdal_version(path)
	path <- path_versions[order(as.Date(unlist(path_versions$date)),decreasing=TRUE),]
	return(path)
}

gdal_installation <- function(
		return_versions=TRUE,
		return_drivers=TRUE,
		return_python_utilities=TRUE,
		sort_most_current=TRUE
)
{
	path <- gdal_path(checkValidity=TRUE)
	
	gdal_installation_results <- lapply(path,
			function(x,return_drivers,return_python_utilities,return_versions)
			{
				result <- list(path=x)
				
				if(return_versions)
				{
					version <- gdal_version(x)
					result$version <- version$version
					result$date <- version$date
				}
				
				if(return_drivers)
				{
					result$drivers <- gdal_drivers(x)    
				}
				
				if(return_python_utilities)
				{
					result$python_utilities <- gdal_python_utilities(x)    
				}
				return(result)
			},return_drivers=return_drivers,
			return_python_utilities=return_python_utilities,return_versions=return_versions)
	if(sort_most_current)
	{
		versions <- unlist(sapply(gdal_installation_results,function(x) return(x$date)))
		gdal_installation_results <- gdal_installation_results[
			order(as.Date(unlist(versions)),decreasing=TRUE)]
	}
	return(gdal_installation_results)    
}

# sligly adapted from MODIS package, this function cares about the final "/" and for blanks and backslashes on Windows
correctPath <- function(x)
{
	if(!is.null(x))
	{
		if (.Platform$OS.type=="windows")
		{
			x <- shortPathName(x)
		} else
		{
			x <- path.expand(x)
		}
		x      <- gsub(x,pattern="\\\\",replacement="/") # switch "\\" to "/
		ind    <- substr(x,nchar(x),nchar(x))!="/"       # some x without "/" at the end?
		x[ind] <- paste0(x[ind],"/")                     # add "/" at the end
	}
	return(x)
}

gdal_check_validity <- function(path)
{
	checkValidity <- sapply(path,
			function(x)
			{
				cmd <- normalizePath(
						list.files(path=x,pattern="gdalinfo",full.names=TRUE))
				
				if(length(cmd)==0)
				{
					return(FALSE)
				} else
				{
					cmd <- paste0('"',cmd,'"'," --version")
					validity = length(try(gdal <- system(cmd,intern=TRUE),silent=TRUE))
					
					return(as.logical(validity))
				}
			}
	)
}

