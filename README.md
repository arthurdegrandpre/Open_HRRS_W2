---
title: "README"
author: "Arthur de Grandpré, UQTR"
date: "28 février 2020"
output: 
  html_document: 
    keep_md: yes
---

# Open_HRRS_W2
This repository aims to provide an open-source based high resolution remote sensing workflow for vegetation mapping in optically shallow waters.  
It relies heavily on object based image analysis methods, such as feature extraction and image segmentation, which are provided by **Orfeo-Toolbox**.  

## Requirements
High resolution multiband raster images are quite heavy and can require large volumes of storage, aswell as high amounts of RAM.  
While storage use is dependant on the amount and size of the images, being able to work with about 500Gb to 1Tb free space can reduce issues associated with temporary files.  
Recommended RAM is 32Gb. We *have* had issues with systems using 16Gb.  
A functionnal installation of **Orfeo-Toolbox** is required to call the Segmentation and feature extraction functions. We recommend version 7.0.0 or over.  
Finally, the workflow is coded in R language, using Rmarkdown notebooks for annotation and visualisation. It is thus required to have an up-to-date version of R and RStudio in order to **run** the script efficiently. 

## Instructions
The workflow includes many steps, some of which are not yet functionnal.  
All steps will be contained within their own Rmarkdown script (.Rmd), and accompanied by a standalone html file for easier reading, and another .md file for displaying the code on the online github repository.  
 
At this moment, functionnal steps include :


## Roadmap
