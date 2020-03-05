---
title: "README"
author: "Arthur de Grandpré, UQTR"
date: "28 février 2020"
output: 
  html_document: 
    keep_md: yes
---

# Open_HRRS_W2 - README
This repository aims to provide an open-source based high resolution remote sensing workflow for vegetation mapping in optically shallow (type-2) waters.  
It relies heavily on object based image analysis methods, such as feature extraction and image segmentation, which are provided by **Orfeo-Toolbox**.  

## Requirements
High resolution multiband raster images are quite heavy and can require large volumes of storage, aswell as high amounts of RAM.  
While storage use is dependant on the amount and size of the images, being able to work with about **500Gb to 1Tb free space** can reduce issues associated with temporary files.  
**Recommended RAM is 32Gb**. We *have* had issues with systems using 16Gb.  
A functionnal installation of **Orfeo-Toolbox** is required to call the Segmentation and feature extraction functions. We recommend version 7.0.0 or over.  
Finally, the workflow is coded in R language, using Rmarkdown notebooks for annotation and visualisation. It is thus required to have an up-to-date version of **R and RStudio** in order to run the script efficiently. 

## Instructions
The workflow includes many steps, some of which are not yet functionnal.  
All steps will be contained within their own Rmarkdown script (.Rmd) located within the "scripts" subfolder of this repository, and accompanied by a standalone html file for easier reading, and another .md file for displaying the code on the online github repository.  
  
Steps are numbered in the order they have to be executed.  
In order **to explore the workflow** and see each step's results, download and read the html version of the scripts.  
In order **to use the workflow**, first download, clone or fork this repository.  
Then, open the R project located in the main directory, then the scripts for each step via their .Rmd file.  
First code chunks for each step include a clean-up of the R environment and the loading of all required libraries.  
All libraries MUST be installed before loading them. To prevent involountary updates, please install them **only** if necessary using the **install.packages("package_name_here")** function.  

Be careful of correctly adjusting the paths defining the input and output directories troughout the process.
  
In order for each step to run smoothly, many intermediate files are created across the workflow. Posteriori clean-up might be beneficial.  

If you encounter major issues, please add an issue to the online github repository page in the issues section and/or contact Arthur de Grandpre at arthur.de.grandpre@uqtr.ca.

## State of the workflow
At this moment, functionnal steps include :  
- metadata extraction (step 01)  
- absolute radiometric calibration (step 02)  
- top of atmosphere reflectance transformation (step 02)  
- mosaic building and atmospheric correction (step 03)  
- feature extraction and image segmentation (step 04) 

## Roadmap
Steps that are not yet fully functionnal or incorporated include :  
 
- wet vs dry objects classification (status: currently testing supervised object based methods, ETA March)  
- sun glint correction (status: functionnal, but requires better wet vs dry classification for better results, ETA soon after classification)  
- vegetation classification (status: not yet coded, very similar method to wet vs dry classification, ETA March-April)  
- better georeferencing and reprojection (data specific objective for 2019 CEGRIM data, ETA Q2 2020)
- pansharpening and high resolution classification (status: optionnal, but can greatly improve mapping resolution. ETA Q2 2020)  
- parrallel processing (status: optionnal, but can greatly improve processing speed. ETA Q2 2020)
- pack code into functions and into a library (status: optionnal, ETA ???)
- add a *shiny* application interface (status: optionnal, ETA ???)
