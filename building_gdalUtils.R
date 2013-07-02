# Jonathan's Mac
# The build notes will go here, probably should be outside the main path
setwd("/Users/jgrn307/Documents/") 
require("roxygen2")
roxygenize(package.dir="/Users/jgrn307/Documents/workspace/gdalutils/pkg/gdalUtils",
		roxygen.dir="/Users/jgrn307/Documents/workspace/gdalutils/pkg/gdalUtils",
		copy.package=FALSE,unlink.target=FALSE)

# Jonathan's PC:
setwd("D:\\Users\\jgrn\\Documents\\code\\workspace\\gdalutils\\pkg\\gdalUtils")
require("roxygen2")
roxygenize(package.dir="D:\\Users\\jgrn\\Documents\\code\\workspace\\gdalutils\\pkg\\gdalUtils",
		roxygen.dir="D:\\Users\\jgrn\\Documents\\code\\workspace\\gdalutils\\pkg\\gdalUtils",
		copy.package=FALSE,unlink.target=FALSE)
