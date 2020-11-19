# Readme

### Overview
The sunset-calc program is a [R shiny app](https://shiny.rstudio.com) to calculate the amount of carbon for TC and Swiss_3S protocols from raw files recorded using a commercial thermo-optical OC/EC analyzer (Model 5L, [Sunset Laboratory Inc.](https://www.sunlab.com), OR, United States). Be aware that this app only works with the designated Sunset protocols. You can upload one or multiple files, however, **each file must contain only one Sunset run**. Expected file size for TC files are ~48 KB and ~140 KB for OC (Swiss_3S) files.

### TC calc

Program to calculate TC for TC files. You get a .zip file containing a .csv file with the amount of carbon (µg C) for each step and the total carbon. 
If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

### OC calc

Program to calculate S1, S2, and S3 OC and total for Sunset OC-removal (Swiss_3S) files. See [Zhang et al., 2012](https://doi.org/10.5194/acp-12-10841-2012) for details. You get a .zip file containing a .csv file with the amount of carbon (µg C) for each step and the total carbon. 
If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

#### Known issues

OC calc can only handle unmodified Swiss_3S protocols. This means with a time shortened version (e.g. in S3) the calculation will fail.

### How does it work

The *Sunset calc* app is made with [shinydashboard](https://rstudio.github.io/shinydashboard/), which contains the two very similar fully self functioning apps *TC calc* and *OC calc* linked in the sidebar. Additionally, there is this *readme.md* markdown file you are reading right now for information. The plots immediately shown after file upload are generated independently from the calculation in the app, the calculation takes place in a linked R script and is triggered by pressing the 'Calculate & Download' button. After calculation, the result data frame is handled back to the shiny app, which creates a csv file and wraps this into a zip file for download.<br>

here is some more text

### About Sunset calc

#### Feature wish list

- test whether a pressure dependent calculation needs to be implemented, especially for online files
- show a table with results in the browser window
- show the pressure and CO2 plot (and maybe more parameters such as temperature) in the browser window
- handle modified (shortened) Swiss_3S protocols

#### Repository

The code for this app is currently in a private repository on
[Github](https://github.com/martin-rauber/sunset-calc).

#### Info

This app was created by [Martin Rauber](https://martin-rauber.com) for LARA, the Laboratory for the Analysis of Radiocarbon with AMS at the University of Bern.

#### Markdown tests below

some `code` here!

SVG image below!

![image info](./images/pressure-plot.svg "pressure plot")


