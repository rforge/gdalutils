# For jgrn/tauboa:
pathtopackage <- "D:\\Users\\jgrn\\Documents\\code\\workspace\\gdalutils\\pkg\\gdalUtils"

# For jgrn/krypton:
pathtopackage <- "/Users/jgrn307/Documents/workspace/gdalutils/pkg/gdalUtils"


setwd(pathtopackage)

require("roxygen2")
roxygenize(package.dir=pathtopackage,
		roxygen.dir=pathtopackage,
		copy.package=FALSE,unlink.target=FALSE)

