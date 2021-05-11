---
title: "README"
author: "Arthur de Grandpré, UQTR"
date: "11 mai 2021"
output: 
  html_document: 
    keep_md: yes
---
This project is licensed under the terms of the cc-by-nc-sa license.  

# Open_HRRS_W2 - V0.01 - README

This repository aims to provide an open-sourced high resolution remote sensing workflow for submerged vegetation cover mapping in optically complex (type-2) waters in a context of scarce validation data.  
It relies heavily on object based image analysis methods, such as feature extraction and image segmentation, which are provided by **Orfeo-Toolbox**.  

## Requirements
High resolution multiband raster images are quite heavy and can require large volumes of storage, aswell as high amounts of RAM.  
While storage use is dependant on the amount and size of the images, being able to work with about **500Gb to 1Tb free space** can reduce issues associated with temporary files.  
**Recommended RAM is 32Gb**. We *have* had issues with systems using 16Gb.  
A functionnal installation of **Orfeo-Toolbox** is required to call the Segmentation and feature extraction functions. We recommend version 7.0.0 or above.  
Finally, the workflow is coded in R language, using Rmarkdown notebooks for annotation and visualisation. It is thus required to have an up-to-date version of **R and RStudio** in order to run the script efficiently. 

## Instructions
The workflow includes many steps, some of which require very little interactions, others more so.  
All steps will be contained within their own Rmarkdown script (.Rmd) located within the "scripts" subfolder of this repository, and sometimes accompanied by a standalone html file for easier reading, and another .md file for displaying the code on the online github repository.  
  
Steps are numbered in the order they have to be executed.  
In order **to use the workflow**, first download, clone or fork this repository.  
Then, open the R project located in the main directory, then the scripts for each step via their .Rmd file.  
First code chunks for each step include a set-up of the R environment and the loading of all required libraries.  
All libraries MUST be installed before loading them. To prevent involountary updates, please install them **only** if necessary using the **install.packages("package_name_here", dependencies=T)** function.  

Be careful of correctly adjusting the paths defining the input and output directories troughout the process, especially in the later scripts, where multiple intermediate files are created.
  
In order for each step to run smoothly, many intermediate files are created across the workflow. Posteriori clean-up might be beneficial.  

If you encounter major issues, please add an issue to the online github repository page in the issues section and/or contact Arthur de Grandpre at arthur.de.grandpre@uqtr.ca.

## State of the workflow
At this moment, functionnal steps include :  
- Metadata extraction (step 01)  
- absolute radiometric calibration, mosaic building, top and bottom of atmosphere empirical corrections (step 02)  
- Empirical destriping of pushbroom artifacts (step 03)  
- Image segmentation and feature extraction based on OrfeoToolBox algorithms (step 04)  
- Aquatic and terrestrial object based masking, based on random forest classification (step 05)
- Empirical image deglinting (step 06, not yet implemented in step 07)
- Classification of aquatic objects, based on iterative random forest classifications (step 07)

## Roadmap
What is yet to come :  

- Better detailed user guidelines and tips  
- step 08 : training set sensitivity and accuracy estimations by bootstrapping  
- Peer review journal submission
- pack code into functions and into a library (status: optionnal)
- add a *shiny* application interface (status: optionnal)
