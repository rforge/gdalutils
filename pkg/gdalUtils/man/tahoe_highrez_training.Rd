\docType{data}
\name{tahoe_highrez_training}
\alias{tahoe_highrez_training}
\title{Point and polygon files for use with spatial.tools}
\description{
  Point and polygon files for use with spatial.tools
}
\examples{
tahoe_highrez_training_polygons <- readOGR(
	dsn=system.file("external", package="gdalUtils"),layer="tahoe_highrez_training")
spplot(tahoe_highrez_training_polygons,zcol="Class")
tahoe_highrez_training_points <- readOGR(
	dsn=system.file("external", package="gdalUtils"),layer="tahoe_highrez_training_points")
spplot(tahoe_highrez_training_points,zcol="SPECIES")
}
\author{
  Jonathan A. Greenberg \email{spatial.tools@estarcion.net}
}
\keyword{data}
\keyword{datasets}

