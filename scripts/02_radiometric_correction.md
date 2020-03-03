---
title: "02_radiometric_correction"
author: "Arthur de Grandpré, UQTR, @ arthur.de.grandpre@uqtr.ca"
date: "1 mars 2020"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float: yes
---

This second script is used to go from digital number pixel values to bottom of atmosphere spectral radiance based on the image metadata extracted in the first script and calibration data from DigitalGlobe. It is adapted to the datastructure of DigitalGlobe products, including Quickbird and Worlview-02, 03 & 04.

# R setup

All libraries must be installed prior to loading them. Installing them individually is the best course of action, using the install.packages("nameofthepackage") function.

**important : ** Do not forget to change the tmpdir path in the setup code chunk to a disk with a lot of free space. The raster options prevent the data from being all loaded into the RAM and crashing the process due to lack of memory, thus requiring to write large amounts of intermediate products.


```r
rm(list=ls())
library(sf)
library(raster)
library(tidyverse)
library(data.table)
library(satellite)
library(GeoLight)
library(sp)
library(stringr)
# library(doParallel) # could be added at some point to speedup the process
# library(foreach) # could be added at some point to speedup the process

# raster options
rasterOptions(todisk=T) # forces on disk writing instead of in memory
rasterOptions(tmpdir="D:/Arthur/temp") # requires a lot of space for temporary file
```

# Absolute radiometric calibration & TOA transformation

This step is detailed in DigitalGlobe's technical notes for radiometric use of worldview-02 and worldview-03 products, available in the documentation subfolder of this repository.
  
All parameters required for radiometric calibration have been compiled in a file and stored into the data subfolder (gain_offset_table_exo.csv), or extracted from the metadata in step 01.  
  
The given equation to do so is $$L =  GAIN * DN * (\frac{abscalfactor}{effectivebandwith})+OFFSET$$ 
  
Where $$L$$ is the at sensor radiance in units of W·sr−1·m−2·nm−1 (watt per steradian per square metre per nanometre)  
  
GAIN is the gain parameter specific to the band and satellite, as obtained from DigitalGlobe  
  
DN is the raw pixel value for a given band in the original product  
  
OFFSET is the offset parameter specific to the band and satellite, as obtained from DigitalGlobe  
  
abscalfactor is the absolute calibration factor parameter specific to the band, satellite and scene, as retrieved from each image's metadata.  
  
effectivebandwith is the actual effective wavelength sensibility range of every band, satellite and scene, as retrieved from each image's metadata.
  
  
  Once at sensor radiance is obtained, it is possible to transform it into top-of-atmosphere radiance, by the equation given within the technical notes of DigitalGlobe that goes as follow : 
  
  $$\rho(TOA)_\lambda = \frac{L_\lambda*d^2*\pi}{E_\lambda*cos(\theta_S)} $$

Where $\rho(TOA)_\lambda$ is the top of atmosphere reflectance,  
  
  $L_\lambda$ is the at sensor radiance (previously calculated),  
  
  $d^2$ is the squared distance between the earth and the sun in astronomical units
  
  $E_\lambda$ is the exoatmospheric radiance (obtained from digitalglobe in 3 different values, but here the mean will be used)
  
  $cos(\theta_S)$ is the cosine of the solar zenith angle
  
## data preparation for calibration

Let's read the data necessary for calibration


```r
DG_param = read.csv("../data/gain_offset_table_exo.csv", sep=";",header=T)
rcp = read.csv("../data/radiometric_calibration_parameters.csv")
mul = read.csv("../data/multispectral_summary.csv")
pan = read.csv("../data/panchromatic_summary.csv")
```

Let's merge digitalglobe's parameters with our metadata information. Mean orbit altitude will also be added since it was not included in previous references.


```r
DG_param$bands = gsub(" ","",DG_param$bands) # makes sure the are no useless spaces in strings
DG_param$sats = gsub(" ","",DG_param$sats) # makes sure the are no useless spaces in strings

rcp$sats = gsub(" ","",rcp$sats) # makes sure the are no useless spaces in strings
rcp$bands = gsub(" ","",rcp$bands) # makes sure the are no useless spaces in strings

rct = merge(rcp,DG_param,all=T) # radiometric correction table

# add satellite altitude

sats = c("QB02","WV02","WV03","WV04")
alt_m = c(450000, 770000, 617000, 617000)
esd = data.frame(sats,alt_m)

rct = merge(rct, esd)

write.csv(rct, "../data/radiometric_correction_table.csv")
```

## radiometric calibration and top of atmosphere reflectance transformation

### PAN images


```r
#pan_arc_output = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/radiometric_correction/PAN/" # PATH TO ARC OUTPUT, optionnal
pan_toa_output = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/toa/PAN/" # PATH TO TOA OUTPUT
path_to_images = "//Glaciolab/homes/degranda/MFFP/" # missing part to the path variable in image summaries

for(i in seq_along(pan$X)){

print(paste0(i,"/",length(pan$X))) # to track progress
  
img = pan[i,] # subset to one image
r = brick(paste0(path_to_images,img$path)) # load image into R raster
r[r<1]=NA # make 0s into NA

rctx = subset(rct, rct$id_part==img$id_part)

#### absolute radiometric correction ####
r2 = rctx$gain*r*(rctx$abscalfactor/rctx$effectivebandwidth)+rctx$offset # absolute radiometric calibration

#### NOTE : the following line can be used to write the absolute radiometric calibration product, if needed
 # writeRaster(r2,paste0(pan_arc_output, as.Date(rctx$date[1]),"_PAN_",rctx$sats[1],"_",str_sub(img,-29)[1]), overwrite=T)

#### TOA ####

r3 = (r2*(calcEarthSunDist(rctx$date)^2)*pi) /
  ((rctx$exo_thuilier+rctx$exo_chkur+rctx$exo_wrc)/3 * 
          cos(rctx$solar_zenith*pi/180)) # using metadata

writeRaster(r3,paste0(pan_toa_output, as.Date(rctx$date[1]),"_PAN_",rctx$sats[1],"_",str_sub(img,-29)[1]), overwrite=T)
 }
```

```
## [1] "1/37"
## [1] "2/37"
## [1] "3/37"
## [1] "4/37"
## [1] "5/37"
## [1] "6/37"
## [1] "7/37"
## [1] "8/37"
## [1] "9/37"
## [1] "10/37"
## [1] "11/37"
## [1] "12/37"
## [1] "13/37"
## [1] "14/37"
## [1] "15/37"
## [1] "16/37"
## [1] "17/37"
## [1] "18/37"
## [1] "19/37"
## [1] "20/37"
## [1] "21/37"
## [1] "22/37"
## [1] "23/37"
## [1] "24/37"
## [1] "25/37"
## [1] "26/37"
## [1] "27/37"
## [1] "28/37"
## [1] "29/37"
## [1] "30/37"
## [1] "31/37"
## [1] "32/37"
## [1] "33/37"
## [1] "34/37"
## [1] "35/37"
## [1] "36/37"
## [1] "37/37"
```


Let's visualize the first layer of every product to understand the difference between them. At this stage, scenes from different satellites should be comparable in their value range.


```r
 plot(r[[1]], main="DN"); plot(r2[[1]], main="ARC"); plot(r3[[1]],main="TOA")
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-1.png)<!-- -->![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-2.png)<!-- -->![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
 hist(r[[1]], main="DN"); hist(r2[[1]], main="ARC"); hist(r3[[1]],main="TOA")
```

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-1-6.png)<!-- -->


### MUL images


```r
#mul_arc_output = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/radiometric_correction/MUL/" # PATH TO ARC OUTPUT, optionnal
mul_toa_output = "//Glaciolab/homes/degranda/MFFP/digitalglobe_archives/toa/MUL/" # PATH TO TOA OUTPUT
path_to_images = "//Glaciolab/homes/degranda/MFFP/" # missing part to the path variable in image summaries

for(i in seq_along(mul$X)){
  print(paste0("image ",i,"/",length(mul$X)))

   img = mul[i,]
   r = brick(paste0(path_to_images,img$path))
   r[r<1]=NA
   
   rctx1 = unique(subset(rct, rct$id_part==img$id_part[1]))
   rctx1 = subset(rctx1, rctx1$bands!="P")

   bands_order = c("C","B","G","Y","R","RE","N1","N")
   rctx = rctx1 %>% arrange(factor(bands, levels = bands_order))

   r2=r

for(b in 1:length(rctx$bands)){
    print(paste0("band ",b,"/",length(rctx$bands)))

#### Absolute radiometric correction ####

r2[[b]] = rctx$gain[b]*r[[b]]*(rctx$abscalfactor[b]/rctx$effectivebandwidth[b])+rctx$offset[b]
print("ARC")
# TOA ######################

r3 = (r2*(calcEarthSunDist(rctx$date[b])^2)*pi) / # this gives TOA
  (((rctx$exo_thuilier[b]+rctx$exo_chkur[b]+rctx$exo_wrc[b])/3)* #cos(zenith(solar(rctx$date[b]),centre_wgs@coords[,1],centre_wgs@coords[,2])*pi/180))
     cos(rctx$solar_zenith[b]*pi/180))
print("TOA")
}
   
#### NOTE : the following line can be used to write the absolute radiometric calibration product, if needed
# writeRaster(r2,paste0(mul_arc_output, as.Date(rctx$date[1]),"_MUL_",rctx$sats[1],"_",str_sub(img,-29)[1]), overwrite=T)

writeRaster(r3,paste0(mul_toa_output, as.Date(rctx$date[1]),"_MUL_",rctx$sats[1],"_",str_sub(img,-29)[1]), overwrite=T)
}
```

```
## [1] "image 1/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 2/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 3/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 4/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 5/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 6/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 7/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 8/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 9/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 10/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 11/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 12/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 13/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 14/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 15/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 16/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 17/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 18/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 19/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 20/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 21/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 22/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 23/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 24/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 25/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 26/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 27/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 28/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 29/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 30/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 31/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 32/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 33/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 34/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 35/37"
## [1] "band 1/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/4"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/4"
## [1] "ARC"
## [1] "TOA"
## [1] "image 36/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
## [1] "image 37/37"
## [1] "band 1/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 2/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 3/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 4/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 5/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 6/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 7/8"
## [1] "ARC"
## [1] "TOA"
## [1] "band 8/8"
## [1] "ARC"
## [1] "TOA"
```


Let's visualize the first layer of every product to understand the difference between them. At this stage, scenes from different satellites should be comparable in their value range.


```r
 plot(r[[1]], main="DN"); plot(r2[[1]], main="ARC"); plot(r3[[1]],main="TOA")
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-2.png)<!-- -->![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
 hist(r[[1]], main="DN"); hist(r2[[1]], main="ARC"); hist(r3[[1]],main="TOA")
```

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-5.png)<!-- -->

```
## Warning in .hist1(x, maxpixels = maxpixels, main = main, plot = plot, ...):
## 0% of the raster cells were used. 100000 values used.
```

![](02_radiometric_correction_files/figure-html/unnamed-chunk-2-6.png)<!-- -->
