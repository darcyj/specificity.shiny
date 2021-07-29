#' Specificity results from Antarctic cryoconite holes
#'
#' A dataset containing a specs list (a named list of outputs from 
#' specificity::phy_or_env_spec), and taxonomic information for each
#' feature (bacterial OTU) within the specs list.
#'
#' @format A list containing 2 objects:
#' \describe{
#'   \item{antarctica_specs_list:}{
#'     A list of outputs from specificity::phy_or_env_spec(), with entries for
#'     Nitrogen ("N"), Phosphorus ("P"), geographic distance ("Geo"), 
#'     fungal beta-diversity ("Fungi"), and algal beta-diversity ("Algae").
#'   }
#'   \item{antarctica_taxonomy:}{
#'     data.frame object containing taxonomic information for the species in
#'     antarctica_specs_list. Columns are as follows:
#'   }
#' }
#' Columns in antarctica_taxonomy:
#' \describe{
#'   \item{id:}{species identity. corresponds to rownames in antarctica_specs_list.}
#'   \item{eval:}{e-value from BLAST, which was used to assign taxonomy from the 
#'     greengenes database.}
#'   \item{hit:}{identity of the closest match within the greengenes database.}
#'   \item{k:}{kingdom-level taxonomy}
#'   \item{p:}{phylum-level taxonomy}
#'   \item{c:}{class-level taxonomy}
#'   \item{o:}{order-level taxonomy}
#'   \item{f:}{family-level taxonomy}
#'   \item{g:}{genus-level taxonomy}
#'   \item{s:}{species-level taxonomy}
#' }
#' @source Sommers et al. (2019) Comparison of microbial communities in the sediments 
#'   and water columns of frozen cryoconite holes in the McMurdo Dry Valleys, Antarctica.
#'   Front Microbiol 10(65) https://doi.org/10.3389/fmicb.2019.00065
#' 
"antarctica"



#' Portable source code for specificity.shiny functions
#'
#' This source code is included so that functions can be written out as .r files, 
#' so that a server running the shiny visualization does not need to have the 
#' specificity.shiny R package installed. 
#'
#' @format A list containing 2 objects:
#' \describe{
#'   \item{plot_specs_shiny:}{
#'     Source code for plot_specs_shiny(). Can be written to file using writeLines().
#'   }
#'   \item{aggregate_specs_list:}{
#'     Source code for aggregate_specs_list(). Can be written to file using writeLines().
#'   }
#' }
#' 
"specShinyFuns"