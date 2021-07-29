#!/usr/bin/env Rscript


# to do before running this script:
# 1. increase version number in ../DESCRIPTION
# 2. describe changes in changelog.txt

# RUN FROM MAIN PACKAGE FOLDER
packwd <- getwd()
if(!file.exists("DESCRIPTION")){
	stop("Wrong directory!")
}


# add files to .Rbuildignore (files ignored during package building)
write("^other$",                   file=".Rbuildignore",append=FALSE)
# write("^specificity\\.Rproj$",     file=".Rbuildignore",append=TRUE)
# write("^\\.Rproj\\.user$",         file=".Rbuildignore",append=TRUE)
# write("^vignette$",                file=".Rbuildignore",append=TRUE)
# write("^\\.travis.yml$",           file=".Rbuildignore",append=TRUE)
# write("^other$",                   file=".Rbuildignore",append=TRUE)


# update text versions of most recent functions to data
# done so that functions can be exported as text for better
# shiny online portability
# i know it's hacky, but it's WAY better than hoping the user
# has the right flags to source github packages
specShinyFuns <- list(
	plot_specs_shiny=readLines("R/plot_specs_shiny.r"),
	aggregate_specs_list=readLines("R/aggregate_specs_list.r")
)
save(list="specShinyFuns", file="data/specShinyFuns.rdata")


library(devtools)
library(roxygen2)

usethis::use_package("shiny")
usethis::use_package("ggplot2")
usethis::use_package("colourpicker")

document()


# add stuff to namespace that's not in BASE R but sure is in VANILLA R
# generated by (in shell):
#   cd ../
#   R CMD build specificity.shiny
#   R CMD check specificity.shiny_XYZ.tar.gz 
# look at specificity.shiny.Rcheck/00check.log
lines2add <- readLines("other/additional_namespace/vanillaR_namespace.txt")
write(lines2add, file="NAMESPACE", append=TRUE)

# install
install("../specificity.shiny")

# document packageVersions used for this test and build:
pkgvers_fp <- "other/package_versions.txt"
write("Pagage versions used in this build:", file=pkgvers_fp, append=FALSE)
writepkgvers <- function(pkgname, fp=pkgvers_fp){
	write(paste0(pkgname, "=", packageVersion(pkgname)), file=fp, append=TRUE)
}
writepkgvers("shiny")
writepkgvers("ggplot2")
writepkgvers("DT")
writepkgvers("colourpicker")
writepkgvers("specificity.shiny")

# make pdf manual
# texinfo required: sudo apt-get install texinfo
pack <- "specificity.shiny"
path <- find.package(pack)
system("rm specificity.shiny.pdf")
# commmand below requires: texlive-fonts-extra, texinfo (install both with apt)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))

