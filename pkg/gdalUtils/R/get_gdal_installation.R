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
#' @details get_gdal_installation is designed to help determine the correct path to the
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
#' mygdals <- get_gdal_installation()
#' mygdals[[1]]$gdal_path
#' mygdals[[1]]$version
#' mygdals[[1]]$drivers
#' mygdals[[1]]$python_utilities
#' # Determine all available GDAL installations:
#' mygdals <- get_gdal_installation(return_most_current=FALSE)
#' sapply(mygdals,function(X) X$gdal_path)
#' # Only return GDAL installs that support a given driver: 
#' mygdals <- get_gdal_installation(required_drivers="HDF4")
#' }
#' @export

# TODO: multiple steps of searching (stopping if found):
# 1) Search in PATH
# 2) Search in common install locations
# 3) Brute force search
# add a "force_search" parameter that automatically
#	brute force searches.

# check if path is already set (is the used GDAL!)

gdal_path <- function(path, verbose = FALSE, rescan = FALSE)
{
    owarn <- options()$warn
    options(warn=-2)
    on.exit(options(warn=owarn))
        
    if (missing(path))
    {
        path <- options()$gdalUtils_gdalPath
	}
	if (!is.null(path) & !rescan)
	{
	    inPath <- function(path, verbose)
	    {    
	        cmd <- file.path(path, "gdalinfo")
	        cmd <- paste('"',cmd,'"'," --version",sep="")
	    
	        try(gdal <- system(cmd,intern=TRUE),silent=TRUE)
	    
	        if (length(grep(gdal, pattern="^GDAL "))!=1)
	        {
	            path <- NULL 
	        } else
	        {
	            if(verbose) message("GDAL found in: ",path.expand(path))
	            path
	        }
	    }
	    gdal_paths <- unlist(lapply(path,inPath,verbose)) # unlist removes NULL
    
	}
	
	if(is.null(path)|rescan)
	{
	    if (.Platform$OS=="unix")
    	{    
	    	# Unix-likes
	    	try(gdal <- system("gdalinfo --version",intern=TRUE),silent=TRUE)

	    	if (length(grep(gdal, pattern="^GDAL "))!=1 | rescan)
	    	{
	    		if(verbose & !rescan) message("GDAL not found in PATH, trying a search... This could take some time...")
	    		if(verbose & rescan) message("Scanning your root-dir for available GDAL installations,... This could take some time...")

	    		gdalinfo_paths <- dirname(system("find / -name gdalinfo",intern=TRUE))# on linux this can triggers a very long search! some restiction is required.

	    		if(length(gdal_paths)==0)
	    		{
	    			#if(verbose) message("No GDAL was found.")
				    #return(NULL)
				    stop("No GDAL was found. Please install 'gdal-bin' before continuing") # why not stop?

	    		}
	    	} else
	    	{
	    		gdal_paths <- dirname(system("which gdalinfo",intern=TRUE))
	    	}
	    } else 
	    {	
		    # Windows
	        try(gdal <- shell("gdalinfo --version",intern=TRUE),silent=TRUE)
            
	        if (length(grep(gdal, pattern="^GDAL "))!=1 | rescan)
	    	{
	    		if(verbose & !rescan) message("GDAL not found in PATH, trying a search... This could take some time...")
	    		if(verbose & rescan) message("Scanning your c-drive for available GDAL installations,... This could take some time...")
                
                # focussed search (in c:/OSGeo4W/ and in c:/Progs*)	        
		        poss <- dir("c:/",full.names=TRUE)

                osgeos <- grep(poss, pattern="OSGeo", ignore.case=TRUE, value=TRUE)                
                if(length(osgeos)!=0)                
                {
                    osgeos <- list.files(file.path(osgeos,"bin"), pattern="^gdalinfo.exe$", full.names=TRUE,include.dirs=TRUE)
                }

                progs <- grep(poss, pattern="Progr*", ignore.case=TRUE, value=TRUE)
                
                gdalinfo_paths <- list()
                for (i in seq_along(progs))
                {
                    gdalinfo_paths[[i]] <- list.files(path=progs[i],pattern="^gdalinfo.exe$", full.names=TRUE, recursive=TRUE, include.dirs=TRUE)
		        }
	    		gdal_paths <- dirname(shortPathName(c(unlist(gdalinfo_paths),osgeos)))
            }
		    if(length(gdal_paths)==0) 
		    {
		        #if(verbose) message(paste("No GDAL was found."))
		    	#return(NULL)
                # add QGIS?
		        stop("No GDAL installation found. Please install 'gdal Utilities' before continuing:\n\t- www.gdal.org (no HDF4 support!)\n\t- www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMANDED)\n\t- www.fwtools.maptools.org (with HDF4 support)\n") # why not stop?
            }
	    }
	}
    return(gdal_paths)
}

gdal_setPath <- function(path,makePermanent=FALSE)
{
    options(gdalUtils_gdalPath=path)
    
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
                doit <- toupper(readline(paste("No write access to: '", paste0(file.path(R.home(),"etc"),"/Rprofile.site") ,"', should '",path.expand("~/.Rprofile") ,"' be created? [y/n]: ",sep="")))
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

        if (length(linec)!=0)
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

gdal_version <- function(path,verbose=FALSE)
{	
    if (missing(path))
    {
        path <- gdal_path()
    }
    
    cmd <- file.path(path, "gdalinfo")
    cmd <- paste('"',cmd,'"'," --version",sep="")
	
	version <- system(cmd,intern=TRUE) 
    
# It seams that system works also in Windows (MAC?), do you confirm?
    # Does shell work here?
	#	if (.Platform$OS=="unix") 
	#	{
	#		gdal_version <- system(cmd,intern=TRUE) 
	#	} else 
	#	{
	#		gdal_version <- shell(cmd,intern=TRUE)
	#	}
		
	if(length(grep(glob2rx("GDAL*"),version)) != 0)
	{
	    version=strsplit(strsplit(version,",")[[1]][1]," ")[[1]][2]
	} else
	{
	    # Broken install
		if(verbose)
		{
		    message("Probably broken install of gdal at '",path,"'")
		}
		version=NA
	}
return(version)	    
}
	
# browser()
# http://www.gdal.org/gdal_utilities.html --formats
# The format support is indicated as follows: 
# 'ro' is read-only driver; 'rw' is read or write (ie. supports CreateCopy);
# 'rw+' is read, write and update (ie. supports Create).
# 'v' is appended for formats supporting virtual IO (/vsimem, /vsigzip, /vsizip, etc).
# 's' is appended for formats supporting subdatasets. 
# Note: The valid formats for the output of gdalwarp are formats that support the Create() method (marked as rw+), not just the CreateCopy() method.
    
gdal_drivers <- function(path,verbose=FALSE)   
{
    if (missing(path))
    {
        path <- gdal_path()
    }
    
    cmd <- file.path(path, "gdalinfo")
    cmd <- paste('"',cmd,'"'," --formats",sep="")
    
	#if (.Platform$OS=="unix") 
	#{
    	drivers_raw <- system(cmd,intern=TRUE) 
	#} else 
	#{
	#	drivers_raw <- shell(cmd,intern=TRUE)
	#}
    drivers_raw <- drivers_raw[-1]
	drivers=strsplit(drivers_raw,":")
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
                
	return(data.frame(format_code=driver_codes,read=r,write=w,update=u,virtualIO=v,subdatasets=s,format_name=driver_names))
	#drivers=drivers[2:dim(drivers)[1],]
}

gdal_python_utilities <- function(path,verbose=FALSE)
{
    if (missing(path))
    {
        path <- gdal_path()
    }
    sapply(path,list.files,pattern="\\.py") 
}


gdal_getExtension <- function(dataFormat)
{
    if(toupper(dataFormat) == "HDF4IMAGE") # MRT + GDAL
    {
        return(".hdf")
    } else if (toupper(dataFormat) %in% c("GTIFF","GEOTIFF"))  # MRT + GDAL
    {
        return(".tif")
    } else if (toupper(dataFormat)=="ENVI") 
    {
        return("") # should generate a '.hdr' file + a file without extension
    } else if (dataFormat=="FIT") 
    {
        return(NA)    
    } else if (toupper(dataFormat)=="ILWIS")
    {
        return(".mpr") # is this ok?
    } else 
    {
        path <- gdal_path()
        
        cmd <- sapply(path,file.path,'gdalinfo --format ')
        cmd <- sapply(cmd,paste0,dataFormat)
        # cmd <- paste0(c(path,'gdalinfo --format '),collapse="/")
        
        #if(.Platform$OS.type=="unix")
        #{
        ext <- sapply(cmd, system(paste0(cmd, dataFormat),intern=TRUE)   
        #} else
        #{
        #    ext <- shell(paste0(cmd, dataFormat),intern=TRUE)   
        #}
        
        ext <- grep(ext,pattern="Extension:",value=TRUE)
        
        if(length(ext)==0)
        {
            return(NA)
        } else
        {
            ext <- gsub(strsplit(ext,":")[[1]][2],pattern=" ",replacement="")
            
            if (ext!="")
            {
                ext <- paste0(".",ext)
            }
            return(ext)
        }
    }
}

####
gdal_installation=function(
        return_drivers=TRUE,
		return_python_utilities=TRUE,
		return_most_current=TRUE,
		setOptions=TRUE,
		verbose=FALSE,
        )
{
gdal_paths <- gdal_path()	

if(return_drivers)
{
    
}
if(return_python_utilities)
{
    
}
if(most_current)
{
    
}

