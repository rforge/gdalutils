\name{align_rasters}
\alias{align_rasters}
\title{Aligns raster files}
\usage{
align_rasters(unaligned, reference, dstfile, output_Raster = FALSE, ...)
}
\arguments{
  \item{unaligned}{Character.  The filename of a raster to
  be aligned to the reference raster.}

  \item{reference}{Character. The filename of a raster to
  be used as the reference for the alignment. Syncing will
  use the reference's projection, resolution, and extent.}

  \item{dstfile}{Character. The filename of the
  synchronized output file.}

  \item{output_Raster}{Logical. Return output dst_dataset
  as a RasterBrick?}

  \item{...}{parameters to be passed to {\link{gdalwarp}}
  (e.g. resampling approach).}
}
\value{
Either NULL or a RasterBricks depending on whether
output_Raster is set to TRUE.
}
\description{
Aligns a raster to a reference raster.
}
\details{
Aligns a raster to the extent and projection of a reference
raster and matches the resolution of the reference raster.
This is helpful in preparing multiple files of different
projections, resolutions, extents, and rotations for
performing map algebra or change detection.
}
\author{
Jonathan A. Greenberg (\email{gdalUtils@estarcion.net})
}
\seealso{
\code{\link{gdalwarp}}
}

