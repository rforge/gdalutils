# interface to raster package

# x <-raster("/home/matteo/PROCESSED/testJob2/MOD13Q1.A2002001.250m_16_days_EVI.tif")

# slightly adapted from raster:::rasterTmpFile() 
gdalTmpFile <- function(prefix='raster_tmp_', driver="GTiff",verbose=FALSE)  
{
  extension <- gdal_getExtension(driver)
  
  if(is.na(extension)| extension=="")
  {
    extension <- ".tif"
    if (verbose) 
    { 
    	cat("Could not find extension for: ",driver, " using '.tif'") 
    }
  }
  
  d <- raster:::.tmpdir()
  
  while(TRUE)
  {
    f    <- paste(gsub(' ', '_', gsub(':', '', as.character(Sys.time()))), '_', paste(round(runif(5)*10), collapse=""), sep='')
    tmpf <- paste(d, prefix, f, extension, sep="")
    if (!file.exists(tmpf)) break
  }
  if (verbose)
  { 
    cat('Writing raster to:', tmpf)
  }
  return(tmpf)
}

# bring any raster* object to a file supported by gdal, and give out the filename
gdal_rasterToGdal <- function(x,verbose=TRUE)
{
  if (is.character(x))
  {
    fname <- path.expand(x)
  } else
  {
    datFor <- raster:::.driver(x,warn=FALSE)
    
    if(datFor!="gdal")
    {
      if (verbose) 
      {
        message("Data in a non GDAL format and must be converted...")
      }
      fname <- gdalTmpFile()
      x <- writeRaster(x,filename=fname)
    } else
    {
     fname <- path.expand(filename(x))
    }
  }
  return(fname)
}


gdal_gdalToRaster <- function(x)
{
  #if (!is.character(x))
  #{
  # x <- filename(x)
  #}
  if(!file.exists(x))
  {
    stop("Could not find file:", x)     
  }
  
  nbands <- gdalinfo(x)[3]
  
  if(nbands==1)
  {
    return(raster(x))
  } else
  {
    return(brick(x))
  }
}
