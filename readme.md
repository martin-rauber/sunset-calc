# Readme

### Overview
The sunset-calc program is a [R shiny app](https://shiny.rstudio.com) to calculate the amount of carbon for TC and Swiss_3S protocols. Be aware that this app only works with the designated Sunset protocols. You can upload one or multiple files, however, **each file must contain only one Sunset run**. Expected filesize for TC files are ~48 KB and ~140 KB for OC (Swiss_3S) files.

### TC calc

Program to calculate TC for TC files. You get a .zip file containg a .csv file with the amount of carbon (µg C) for each step and the total carbon. 
If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

### OC calc

Program to calculate S1, S2, and S3 OC and total for Sunset OC-removal (Swiss_3S) files. See [Zhang et al., 2012](https://doi.org/10.5194/acp-12-10841-2012) for details. You get a .zip file containg a .csv file with the amount of carbon (µg C) for each step and the total carbon. 
If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

#### Repository

The code for this app is currently in a private repository on
[Github](https://github.com/martin-rauber/sunset-calc).

#### About

This app was created by [Martin Rauber](https://martin-rauber.com) for LARA, the Laboratory for the Analysis of Radiocarbon with AMS at the University of Bern.

