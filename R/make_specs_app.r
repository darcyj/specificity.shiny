#' make_specs_app
#' 
#' Makes a portable version of the interactive visualization from plot_specs_shiny()
#' that can be uploaded to shinyapps.io (or similar?). Note that this function takes
#' the same arguments as plot_specs_shiny(). It is advised to make sure your
#' visualization works properly using that function, before making it portable 
#' with this function. An additional argument for the name of your output app directory
#' is also required; this name will become the last part of your URL if you upload to
#' shinyapps.io, so use care when choosing. Note that this function automatically
#' includes a portable version of the specificity.shiny R package inside your app,
#' so you don't need to deal with installing it on the server; although the server
#' will need specificity.shiny's dependencies installed (shinyapps.io already does,
#' you don't need to do anything).
#' 
#' @author John L. Darcy
#'
#' @param sl specs_list. A named list of outputs from phy_or_env_spec. See examples.
#' @param fd data.frame. Optional feature data - a data.frame object with one row per 
#'   feature, including some column with feature IDs that includes feature IDs in sl as
#'   rownames. If NULL, no feature data will be used (default:NULL).
#' @param fd_id_col integer or string. Either the column number of fd containing feature
#'   IDs (see above), or the column name of that column (default: 1).
#' @param app_fp filepath to a folder where the app should be stored. Only forward
#'   slashes ("/") are supported! The folder should *not* exist yet, and this function will 
#'   throw an error if it does exist. This is to avoid overwriting. As mentioned above,
#'   the name of this folder will become the last part of your URL if you upload to
#'   shinyapps.io. For example, if you set app_fp="~/Desktop/mySpecApp", this function
#'   will first check if directory "mySpecApp" exists on your desktop. Then, if it
#'   doesn't, it will create that directory and put all the necessary files inside of it
#'   (default:"mySpecApp").
#' @return Returns nothing, but creates a folder containing a shiny app.
#'
#' @examples
#' # attach(antarctica)
#' # # test that it works:
#' # plot_specs_shiny(antarctica_specs_list, antarctica_taxonomy, 1)
#' # # make the app:
#' # make_specs_app(antarctica_specs_list, antarctica_taxonomy, 1, "antarctica_specs_shiny_app")
#' # # upload to shinyapps.io (will automatically open in browser when done)
#' # # package "rsconnect" is required for this next step, and requires an account with
#' # # shinyapps.io and configuration of the rsconnect package; see their VERY easy guide
#' # # here: https://docs.rstudio.com/shinyapps.io/getting-started.html
#' # # Just make an account and configure rsconnect, no need to do anything after that.
#' # rsconnect::deployApp("antarctica_specs_shiny_app")
#'
#' @export
make_specs_app <- function(sl, fd=NULL, fd_id_col=1, app_fp="mySpecApp"){
  if(app_fp == "mySpecApp"){
    warning("app_fp is set to default 'mySpecApp'... did you mean to do this?")
  }
  # trim tailing slashes from app_fp
  while(endsWith(app_fp, "/")){
    app_fp <- substr(app_fp, start=1, stop=nchar(app_fp)-1)
  } 

  # check if app_fp exists already. if it does, throw error
  if(dir.exists(app_fp) || file.exists(app_fp)){
    stop(paste0("a file or directory already exists at ", app_fp))
  }

  # make app directory. 
  tryCatch({
    dir.create(app_fp)
  }, warning=function(w){
    stop(paste0("Warning when trying to make directory ", app_fp, 
      ". Aborting out of an excess of caution. Warning text: ", w))
  }, error=function(e){
    stop(paste0("Error when trying to make directory ", app_fp, 
      ". Aborting. Error text: ", e))
  })

  # fetch function files and write to text
  write(
    x=specificity.shiny::specShinyFuns$aggregate_specs_list, 
    file=paste0(app_fp, "/funs.r"),
    append=FALSE
  )
  write(
    x=specificity.shiny::specShinyFuns$plot_specs_shiny, 
    file=paste0(app_fp, "/funs.r"),
    append=TRUE
  )

  # export necessary data for the portable app
  save(
    file=paste0(app_fp,"/appdata.rdata"),
    list=c(
      "sl", 
      "fd",
      "fd_id_col"
    )
  )

  # put standard "app.R" in there
  writeLines(
    con=paste0(app_fp, "/app.R"),
    text=c(
      "# get packages required by specificity.shiny",
      "library(shiny)",
      "library(ggplot2)",
      "library(DT)",
      "library(colourpicker)",
      "",
      "# source specificity.shiny portable functions",
      "source('funs.r')",
      "",
      "# load data for the app",
      "load('appdata.rdata')",
      "# run visualization",
      "plot_specs_shiny(sl, fd, fd_id_col)"
    )
  )

}
