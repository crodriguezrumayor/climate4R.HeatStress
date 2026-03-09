# climate4R.HeatStress

## What is `climate4R.HeatStress`?

A R package for computing heat stress indices directly from climate data within the `climate4R` framework.

`climate4R.HeatStress` is a wrapper of the [`HeatStress`](https://github.com/anacv/HeatStress) R package, which implements a collection of heat stress indices as atomic functions operating on numeric vectors. This wrapper adapts those functions for a seamless integration with the **climate4R** data structures, providing support for parallel computing.

## Installation

The recommended procedure for installing the package is using the remotes package. 

```R
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("crodriguezrumayor/climate4R.HeatStress")
```
Note that the following dependencies need to be installed beforehand:

```R
remotes::install_github("SantanderMetGroup/transformeR")
remotes::install_github("anacv/HeatStress")
remotes::install_github("SantanderMetGroup/convertR")
```

A list of all available indices and the atomic functions calculating them is printed on screen with:

```R
library(climate4R.HeatStress)
indexShow() 
```