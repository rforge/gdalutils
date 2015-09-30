library(gdalUtils)

unaligned="P:/cali_disturbance/scratch/raw/landsat/p38r36/l4/LT40380361990041XXX02_sr_band1.tif"
reference="P:/cali_disturbance/scratch/raw/landsat/p38r36/l7/LE70380361999194EDC00_sr_band1.tif"
dstfile = "test_aligned.tif"

test_align <- align_rasters(unaligned=unaligned,reference=reference,dstfile=dstfile,output_Raster=FALSE,nThreads=1,verbose=TRUE)