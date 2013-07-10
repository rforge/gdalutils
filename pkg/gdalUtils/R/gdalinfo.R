gdalinfo <- function(x, tryRgdal=TRUE, verbose=FALSE)
{
  if(inherits(x,"Raster"))
  {
    fname <- gdal_rasterToGdal(x,verbose=verbose)
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
        
        orig          <- as.numeric(strsplit(gsub(strsplit(grep(meta,pattern="Lower Left  \\(",value=TRUE), "\\) \\(")[[1]][1],pattern="Lower Left  \\(",replacement=""),",")[[1]])
        result$ll.x   <- orig[1]
        result$ll.y   <- orig[2]
        
        res           <- as.numeric(strsplit(gsub(gsub(grep(meta,pattern="Pixel Size = \\(",value=TRUE),pattern="Pixel Size = \\(",replacement=""),pattern="\\)",replacement=""),",")[[1]])
        result$res.x  <- res[1]
        result$res.y  <- res[2]
        
        result$oblique.x <- NA
        result$oblique.y <- NA
        

        result$driver <- strsplit(gsub(grep(meta,pattern="Driver: ",value=TRUE), pattern="Driver: ",replacement=""),"/")[[1]][1]
        
        if(as.logical(gdal_version(path[i],newerThan="1.9.0")))
        {
          cmd <- file.path(path[i],"gdalsrsinfo -o proj4 ")
          result$projection <- system(paste0(cmd,fname),intern=TRUE)
        } else
        {
            result$projection <- "not extracted"
        }
          result$file <- gsub(grep(meta,pattern="Files: ",value=TRUE),pattern="Files: ",replacement="")
        
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
