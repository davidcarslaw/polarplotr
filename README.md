# **polarplotr**

Functions to plot bivariate polar plots. **polarplotr** uses [openair](https://github.com/davidcarslaw/openair) functions but they have been enhanced to do consider pair-wise statistics to compare two pollutants through correlation and regression.

A publication is to come and more information can be found [here](http://davidcarslaw.github.io/polarplotr/docs/). 

## Installation

The development version: 

```
# Install
devtools::install_github("davidcarslaw/polarplotr")
```

## Use

At some point in the near future the existing `polarPlot` function in **openair** will be removed and the `polarplotr` version, which has more capabilities used instead. In teh meantime it is best to load `polarplotr` *after* `openair` i.e.

```
library(openair)
library(polarplotr)
```
