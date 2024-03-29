# GPRsuite

**in construction!**

Shiny apps for the R-package [RGPR](https://github.com/emanuelhuber/RGPR)

## Generic: interactively set and delete points

`app_interactivclicks.R`: App to add and remove points on an empty plot 
(points can be removed by clicking on them or by selecting them with an
interactive window). This app is used for the app `app_GPRdelineation.R`.

## Visualizer for GPR data

`app_GPRvisualizer.R`: Reads and plots GPR data. Note that you have to import all the files belonging
to the GPR data. For example, I import (Mala data):
* DAT_0215.cor
* DAT_0215.mrk
* DAT_0215.rad
* DAT_0215.rd3
* DAT_0215.rd7
* DAT_0215.srd

## Interactively delineate GPR data

`app_GPRdelineation.R`: A combination of the two previous app. Read your GPR data and draw a line on it. You can export the line coordinates.
