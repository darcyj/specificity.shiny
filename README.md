# specificity.shiny
Companion R package for `specificity`; enables interactive visualization and exploration of analysis results


by John L. Darcy
Updated 06 AUG 2021


## Installation
This package is extremely lightweight, and can be installed quite easily using `remotes::install_github()` in R. Install `remotes` using `install.packages("remotes")` if you don't have it already. Then run `remotes::install_github("darcyj/specificity.shiny")`. All done!

## Requirements
`specificity.shiny` has a couple dependencies (and *their* dependencies...), which should be installed automatically using `install_github()` above. The dependencies are:
* `shiny` (obviously)
* `ggplot2` (because its easier to get `ggplot2` to work with shiny compared to base R plots)
* `DT` (to make data table visualization nice)
* `colourpicker` (for friendly color picking)
* THIS PACKAGE DOES NOT REQUIRE `specificity`! This was intentional, so that `specificity` results can be shared interactively with collaborators, without the need for them to install the main package.

## How to use
See the example in `?plot_specs_shiny`. It's really easy. The [tutorial vignette for `specificity`](https://raw.githubusercontent.com/darcyj/specificity/master/vignette/vignette.pdf) also contains a full walkthrough of this package.

## Documentation
Documentation for individual functions can be obtained within R using `?fun` where fun is the function you're interested in. There is also a full .pdf documentation file in this repository named `specificity.pdf`, which is a standard glossary of all functions in the package.
