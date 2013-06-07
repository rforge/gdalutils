
# The build notes will go here, probably should be outside the main path
setwd("/Users/jgrn307/Documents/") 

require("roxygen2")
roxygenize(package.dir="/Users/jgrn307/Documents/workspace/gdalutils/pkg/gdalUtils",
		roxygen.dir="/Users/jgrn307/Documents/workspace/gdalutils/pkg/gdalUtils",
		copy.package=FALSE,unlink.target=FALSE)
