---
title: "04_feature_extraction_and_segmentation"
author: "Arthur de Grandpré"
date: "3 mars 2020"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: yes
---

This fourth script is used to extract additionnal textural and statistical information from the images, using local statistical moments, haralick textures and edge extraction from OrfeoToolBox. OrfeoToolBox (OTB) is an open-source high resolution image analysis software code in C++ that can run within it's own GUI, or can be called from command line, allowing its use within R or Python. The feature extraction will be applied onto bottom of atmosphere mosaic images as produced in the previous scripts, 03_atmospheric_correction.

**IMPORTANT :** running this script will required a functionnal installation of Orfeo Toolbox 7.0.0 or higher. Please, install OTB directly from :   https://www.orfeo-toolbox.org/download/

and specify the installation path here, aswell as allocated RAM (does not need to be a lot) : 

```r
rm(list=ls())

otb.path  = "C:\\OTB-7.0.0-Win64\\bin"
otb.ramlimit = 2048 # does not seem to work without changing windows environment variable (not an issue)
```

# R setup

All libraries must be installed prior to loading them. Installing them individually is the best course of action, using the install.packages("nameofthepackage") function.

**important : ** Do not forget to change the tmpdir path in the setup code chunk to a disk with a lot of free space. The raster options prevent the data from being all loaded into the RAM and crashing the process due to lack of memory, thus requiring to write large amounts of intermediate products.


```r
library(sf)
library(raster)
library(tidyverse)
library(data.table)
library(sp)
library(stringr)
library(rgdal)
library(landscapetools)
# library(doParallel) # could be added at some point to speedup the process
# library(foreach) # could be added at some point to speedup the process

# raster options
rasterOptions(todisk=T) # forces on disk writing instead of in memory
rasterOptions(tmpdir="D:/Arthur/temp") # requires a lot of space for temporary file
```


# Feature extraction

## defining OTB function calls

Since OTB is not natively integrated in R, we need to write our own call functions. They will include a text configuration file with every product in order to know which parameters were used to create them.  
  
### LocalStatisticsExtraction

```r
feature.LocalStatisticExtraction <- function(
                           raster.in = "",
                           out.path  = "",
                           name      = "",
                           channel = "1",   #default 1, selected channel index in input image
                           radius   = "3"  #default 3, computational window radius
                           ){
  
# Set configuration      
conf <- paste("-RAM ",otb.ramlimit,
              "-in",raster.in,
              "-channel",channel,
              "-radius",radius,
              "-out",paste0(out.path,"/",name)
              )
  
  shell(paste("pushd ",otb.path,"&& otbcli_LocalStatisticExtraction ",conf))

write.table(x = conf,file = paste(out.path,"/",name,"_conf.txt",sep=""),row.names = F, col.names = F)
}
```

### EdgeExtraction

```r
feature.EdgeExtraction <- function(
                           raster.in = "",
                           out.path  = "",
                           name      = "",
                           channel = "1",   #default 1, selected channel index in input image
                           filter   = "gradient"  #default gradient, but can be changed for sobel or touzi
                           ){
  
# Set configuration      
conf <- paste("-RAM ",otb.ramlimit,
              "-in",raster.in,
              "-channel",channel,
              "-filter",filter,
              "-out",paste0(out.path,"/",name)
              )
  
  shell(paste("pushd ",otb.path,"&& otbcli_EdgeExtraction ",conf))

write.table(x = conf,file = paste(out.path,"/",name,"_conf.txt",sep=""),row.names = F, col.names = F)
}
```

### HaralickTextures

```r
feature.HaralickTextureExtraction <- function(
                           raster.in = "",
                           out.path  = "",
                           name      = "",
                           channel = "1",   #default 1, selected channel index in input image
                           texture   = "simple",  #default simple, also available "advanced" or "higher"
                           parameters.min = "0", #input image minimum value
                           parameters.max = "1", #input image maximum value
                           parameters.xrad = "2", #xradius in pixels
                           parameters.yrad = "2", #yradisu in pixels
                           parameters.xoff = "1", #xoffset in pixels
                           parameters.yoff = "1", #yoffset in pixels
                           parameters.nbbin = "8" # bins per axis of histogram
                           
                           ){
  
# Set configuration      
conf <- paste("-RAM ",otb.ramlimit,
              "-in",raster.in,
              "-channel",channel,
              "-texture",texture,
              "-parameters.min",parameters.min,
              "-parameters.max",parameters.max,
              "-parameters.xrad",parameters.xrad,
              "-parameters.yrad",parameters.yrad,
              "-parameters.xoff",parameters.xoff,
              "-parameters.yoff",parameters.yoff,
              "-parameters.nbbin",parameters.nbbin,
              "-out",paste0(out.path,"/",name)
              )
  
  shell(paste("pushd ",otb.path,"&& otbcli_HaralickTextureExtraction ",conf))

write.table(x = conf,file = paste(out.path,"/",name,"_conf.txt",sep=""),row.names = F, col.names = F, overwrite=T)
}
```

## OTB NIR based feature extraction

For the extraction, input and output directories need to be defined.


```r
input_dir = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/doscost/"
output_dir = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/feature_extract/"
imgs = dir(input_dir,full.names=T, pattern=".tif$")
```

Sligthly different calls will be made based on wether the image has 4 or 8 bands in order to use the right NIR band.


```r
for(i in seq_along(imgs)){

  r = brick(imgs[i])
  
  if(nlayers(r)==8){
feature.LocalStatisticExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"localstats"),
                           name      = paste0("ls_",str_sub(imgs[i],-46)),
                           channel = "8",   #default 1, selected channel index in input image
                           radius   = "3"  #default 3, computational window radius
                           )
  
feature.EdgeExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"edge"),
                           name      = paste0("ee_",str_sub(imgs[i],-46)),
                           channel = "8",   #default 1, selected channel index in input image
                           filter   = "gradient"  #default 3, computational window radius
                           )

feature.HaralickTextureExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"haralick"),
                           name      = paste0("ht_",str_sub(imgs[i],-46)),
                           channel = "8",   #default 1, selected channel index in input image
                           texture   = "simple",  #default simple, also available "advanced" or "higher"
                           parameters.min = "0", #input image minimum value
                           parameters.max = "1", #input image maximum value
                           parameters.xrad = "2", #xradius in pixels
                           parameters.yrad = "2", #yradisu in pixels
                           parameters.xoff = "1", #xoffset in pixels
                           parameters.yoff = "1", #yoffset in pixels
                           parameters.nbbin = "8" # bins per axis of histogram
                           )
  } else {
feature.LocalStatisticExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"localstats"),
                           name      = paste0("ls_",str_sub(imgs[i],-46)),
                           channel = "4",   #default 1, selected channel index in input image
                           radius   = "3"  #default 3, computational window radius
                           )
  
feature.EdgeExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"edge"),
                           name      = paste0("ee_",str_sub(imgs[i],-46)),
                           channel = "4",   #default 1, selected channel index in input image
                           filter   = "gradient"  #default 3, computational window radius
                           )

feature.HaralickTextureExtraction(
                           raster.in = gsub("/","\\\\",imgs[i]),
                           out.path  = paste0(gsub("/","\\\\",output_dir),"haralick"),
                           name      = paste0("ht_",str_sub(imgs[i],-46)),
                           channel = "4",   #default 1, selected channel index in input image
                           texture   = "simple",  #default simple, also available "advanced" or "higher"
                           parameters.min = "0", #input image minimum value
                           parameters.max = "1", #input image maximum value
                           parameters.xrad = "2", #xradius in pixels
                           parameters.yrad = "2", #yradisu in pixels
                           parameters.xoff = "1", #xoffset in pixels
                           parameters.yoff = "1", #yoffset in pixels
                           parameters.nbbin = "8" # bins per axis of histogram
                           )
  }
  }
```

Finally,  
Let's add those features to the BOA image for segmentation.


```r
for(i in seq_along(imgs)){
  r = brick(imgs[i])
  
  features = c("localstats","edge","haralick")
  
  for(j in seq_along(features)){
  r = addLayer(r, util_rescale(brick(dir(paste0(output_dir,features[j]),full.names=TRUE, pattern=".tif$")[i])))
  }
  
  writeRaster(r, paste0(output_dir,str_sub(imgs[i],-46)), overwrite=T)
}
```

### visual validation


```r
initial = dir(input_dir, full.names = T, pattern=".tif")
final = dir(output_dir, full.names = T, pattern=".tif")

plot(brick(initial[1]), main = "BOA image"); plot(brick(final[1]), main = "BOA + NIR features image")
```

![](04_feature_extraction_and_segmentation_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](04_feature_extraction_and_segmentation_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

# Image segmentation

Image segmentation is used to create polygons based on a raster. In this case, we want to use an algorythm able to process large images with a large amount of layers. For this application, we will use OTB's MeanShift segmentation, which can process rapidly very large amounts of multiband data.  
  
  Those resulting polygons will then be used to classify the image based on object properties instead of pixel values.
  
## defining segmentation function


```r
meanshift.segm <- function(raster.in = "",
                           out.path  = "",
                           name      = "",
                           filter.meanshift.spatialr = "5",   #default 5
                           filter.meanshift.ranger   = "0.003",  #default 15
                           filter.meanshift.thres    = "0.001", #default 0.1
                           filter.meanshift.maxiter  = "100", #default 100
                           filter.meanshift.minsize  = "10"  #default 100
                           
                           ){
  
# Set configuration      
conf <- paste("-in",raster.in,"-filter meanshift","-filter.meanshift.spatialr",filter.meanshift.spatialr,
                "-filter.meanshift.ranger",filter.meanshift.ranger,"-filter.meanshift.thres",filter.meanshift.thres,
                "-filter.meanshift.maxiter",filter.meanshift.maxiter,"-filter.meanshift.minsize",filter.meanshift.minsize,
                "-mode vector","-mode.vector.out",paste0(out.path,"/",name,".shp"))
  
  shell(paste("pushd ",otb.path,"&& otbcli_Segmentation ",conf))

write.table(x = conf,file = paste(out.path,"/",name,"_conf.txt",sep=""),row.names = F, col.names = F)
}
```

## apply segmentation

Segmentation parameters will greatly affect the segmentation quality. If segmentation quality seems bad after visual validation, it is possible to change the parameters in the following chunk. The current values are based on simple tweaking and visual cues.

The output from this section is a simple shapefile


```r
input_dir = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/feature_extract/"
output_dir = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/segmentation_water"
```


```r
imgs = dir(input_dir,full.names=T, pattern=".tif$")

 for(i in seq_along(imgs)){
  
meanshift.segm(filter.meanshift.spatialr = "5",      # default 5
               filter.meanshift.ranger   = "0.003",  # default 15
               filter.meanshift.thres    = "0.001",  # default 0.1
               filter.meanshift.maxiter  = "100",    # default 100
               filter.meanshift.minsize  = "10",     # default 100
               raster.in = gsub("/","\\\\",imgs[i]),
               out.path  = gsub("/","\\\\",output_dir),
               name      = str_sub(imgs[i],-46))

  }
```

### visual validation


```r
initial = dir(input_dir, full.names = T, pattern=".tif")
final = dir(output_dir, full.names = T, pattern=".shp")

plot(brick(initial[1]), main = "BOA image"); plot(readOGR(final[1]), main = "segments")
```

![](04_feature_extraction_and_segmentation_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```
## OGR data source with driver: ESRI Shapefile 
## Source: "\\Glaciolab\homes\degranda\MFFP\digitalglobe_archives\segmentation_water\2009-09-05_010623053080_01_P001_BOA_MOSAIC.tif.shp", layer: "2009-09-05_010623053080_01_P001_BOA_MOSAIC.tif"
## with 273276 features
## It has 1 fields
```

![](04_feature_extraction_and_segmentation_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
plot(brick(initial[1])[[2]], main = "BOA image, zoom B2 + segments", xlim=c(664500,665000),ylim=c(5111500,5112000)); plot(readOGR(final[1]), main = "segments",add=T, xlim=c(664500,665000),ylim=c(5111500,5112000))
```

![](04_feature_extraction_and_segmentation_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```
## OGR data source with driver: ESRI Shapefile 
## Source: "\\Glaciolab\homes\degranda\MFFP\digitalglobe_archives\segmentation_water\2009-09-05_010623053080_01_P001_BOA_MOSAIC.tif.shp", layer: "2009-09-05_010623053080_01_P001_BOA_MOSAIC.tif"
## with 273276 features
## It has 1 fields
```
