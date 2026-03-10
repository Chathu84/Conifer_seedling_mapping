# Conifer Seedling Mapping in Postfire Mountaneous Landscapes Using UAS Data
![Postfire confier seedlings mixed up with dense shrubs](https://github.com/Chathu84/Conifer_seedling_mapping/blob/main/images/figure1.jpg)

This project uses UAS based RGB images and followed by structure from mortion (sfm) based point clouds and orthomosaics to map conifer seedlings in postfire mountainous landscapes. 

## Data
The data includes;
  1. RGB image based indices
    - List of indices used in this study can be foud [here](https://github.com/Chathu84/Conifer_seedling_mapping/blob/main/reports/Supplementary%20Material.pdf)
  2. Point cloud based variables
    - Canopy height and rumple index
  3. Texture base variables
    - List of Texture based variables used in this study can be foud [here](https://github.com/Chathu84/Conifer_seedling_mapping/blob/main/reports/Supplementary%20Material.pdf)


## Methods

This repository contains data and codes to implement simple machine learning algorithms (e.g. neural network(nnet), random forest (fr), Support vector machine (SVM)) to detect seedlings in coniferous forests using UAS based data.


## Findings
Random forest can detect seedlings and other vegetations including shrub, standing dead, mature evergreen trees, and deceduous trees in a co-occuring system at 89% overall accuracy. Vegetetaion indices and canoy height variables were the most importnat variable in identifying each class. The texture variables help to decrease the uncertainty but did not help increase the overall accuracy.
The manuscript is in review and will be updated here after publishing. The preprint can be found [here](https://github.com/Chathu84/Conifer_seedling_mapping/blob/main/reports/ssrn-6042292.pdf).


 

