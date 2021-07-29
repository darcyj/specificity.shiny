# Tests have to be done manually, because it's not like testthat can check if
# a shiny visualization looks like its supposed to. Plus, if there's an error
# you don't get an error in your local R instance. 

library(specificity.shiny)

# load data set
attach(antarctica)


# WITHOUT feature data:
# test that primary visualization works
plot_specs_shiny(antarctica_specs_list)
# make app
make_specs_app(antarctica_specs_list, app_fp="test")
# check if it can be run with shiny::runApp
shiny::runApp("./test")
# see if it works online
rsconnect::deployApp("test")
# check if it works...
rsconnect::terminateApp("test")
system("rm -r test/")



# WITH feature data:
# test that primary visualization works
plot_specs_shiny(antarctica_specs_list, antarctica_taxonomy, 1)
# make app
make_specs_app(antarctica_specs_list, antarctica_taxonomy, 1, app_fp="test")
# check if it can be run with shiny::runApp
shiny::runApp("./test")
# see if it works online
rsconnect::deployApp("test")
# check if it works...
rsconnect::terminateApp("test")
system("rm -r test/")
