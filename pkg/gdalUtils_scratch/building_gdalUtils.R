# For jgrn/tauboa:
pathtopackage <- "D:\\Users\\jgrn\\workspace\\gdalutils\\pkg\\gdalUtils"

# For jgrn/krypton:
pathtopackage <- "/Users/jgrn307/git/gdalUtils"

pathtopackage <- "/Users/jgrn307/git/gdalUtils"

setwd(pathtopackage)

# This builds the man pages and updates the NAMESPACE.
# When in doubt, you can delete all your local man files
# and this will re-create them.
require("roxygen2")
roxygenize(package.dir=pathtopackage)
