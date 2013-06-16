gdalinfo <- function(x, tryRgdal=TRUE, verbose=FALSE)
{
  if(inherits(x,"Raster"))
  {  
    if(!hasValues(x))
    {
      stop("The provided object has no values!")
    } else if(raster:::.driver(x)!="gdal")
    {
#      if (verbose)
#      {
        message("File in a non readable form for GDAL, converting...(you may change the default raster format in '?rasterOptions' to 'GTiff' or another GDAL supported format see 'gdal_drivers()')")
#      }
      fname <- gdalTmpFile()
      writeRaster(x,filename=fname)
    } else
    {
      fname <- filename(x)
    }
  } else
  {
    fname <- as.character(x)
  }
  done <- FALSE
  if(tryRgdal & require(rgdal))
  {
    result <- GDALinfo(fname) # rgdal:::GDALinfo() is the better choise!
    done   <- inherits(result,"GDALobj")    
  }
    
  if (done)
  { 
    result
  } else
  {
    path <- gdal_path()
    path <- gdal_sortInstallation(path)
    # conversion from Filename to Driver! (maybe add it into the gdal_suppotrs() function, but I think it is better to have a separated fun that does the inversion of gdal_getExtension(), a kind of get_driverFromFile())
    #path <- gdal_supports(x,stopOnFirst=TRUE)
  
    for (i in seq_along(path))
    {
      owarn <- getOption("warn")
      options(warn=-2)
      
      cmd <- file.path(path[i],"gdalinfo ")
      try(meta <- system(paste0(cmd,fname),intern=TRUE),silent=TRUE)
      
      options(warn=owarn)
      
      if(length(meta)!=0)
      {
        result        <- list()
        dims          <- strsplit(gsub(grep(meta,pattern="Size is ",value=TRUE), pattern="Size is ",replacement=""),",")[[1]]
        result$rows   <- as.numeric(dims[2])
        result$columns<- as.numeric(dims[1])
        result$bands  <- length(grep(meta,pattern="^Band [1-9]"))
        result$driver <- strsplit(gsub(grep(meta,pattern="Driver: ",value=TRUE), pattern="Driver: ",replacement=""),"/")[[1]][1]
        result$file   <- gsub(grep(meta,pattern="Files: ",value=TRUE),pattern="Files: ",replacement="")
            
        # result$...
                      
        return(result)
      } 
        
      if(i==length(path))
      {
        stop("Could not get metadata from this file:", x) 
      }
    }
  }
}        
