# climate4R.HeatStress

## What is `climate4R.HeatStress`?

A R package for computing heat stress indices directly from climate data within the `climate4R` framework.

`climate4R.HeatStress` is a wrapper of the [`HeatStress`](https://github.com/anacv/HeatStress) R package, which implements a collection of heat stress indices as atomic functions operating on numeric vectors. This wrapper adapts those functions for a seamless integration with the **climate4R** data structures, providing support for parallel computing.

## Installation

The recommended procedure for installing the package is using the devtools package. 

```R
devtools::install_github(c("SantanderMetGroup/transformeR", "anacv/HeatStress", "crodriguezrumayor/climate4R.HeatStress"))
```

A list of all available indices and the atomic functions calculating them is printed on screen with:

```R
library(climate4R.HeatStress)
indexShow() 
```