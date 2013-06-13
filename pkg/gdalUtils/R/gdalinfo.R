gdalinfo <- function(x,verbose=FALSE)
{
    if(inherits(x,"Raster"))
    {
        if(inMemory(x))
        {
            if (verbose)
            {
                message("File for GDAL in a non readable form, converting...")
            }
            x <- gdalTmpFile()
            writeRaster(x,filename=x)
        } else
        {
            x <- filename(x)
        }
    }
    path <- gdal_sortInstallation(path)
    
    for (i in seq_along(path))
    {
        cmd      <- file.path(path[i],"gdalinfo ")
        try(meta <- system(paste0(cmd,x),intern=TRUE),silent=TRUE)
        
        if(length(meta)!=0)
        {
            result <- list()
            result$filename <- gsub(grep(meta,pattern="Files: ",value=TRUE),pattern="Files: ",replacement="")
            result$driver   <- strsplit(gsub(grep(meta,pattern="Driver: ",value=TRUE), pattern="Driver: ",replacement=""),"/")[[1]][1]
            result$nbands   <- length(grep(meta,pattern="^Band [1-9]"))
            # result$...
                        
            return(result)
        } else if(i==length(path))
        {
            stop("Could not get metadata from this file:", x) 
        }
    }
}    
        
