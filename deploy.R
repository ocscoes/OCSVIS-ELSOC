library(pacman)
p_load(rsconnect,
       quarto)


rsconnect::setAccountInfo(name='ocs-coes',
                          token='966FC8FD0EF43CB5B3EFB982C32530A7',
                          secret='gGXd5aw/yza5GHnyukmmeoMjicFUfeqWLgyP056c')


quarto_publish_app(input = "viselsoc.qmd",
                   server="shinyapps.io",
                   name = "ocsvis-elsoc",account = "ocs-coes")