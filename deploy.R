library(pacman)
p_load(rsconnect,
       quarto)


rsconnect::setAccountInfo(name='ocs-coes',
                          token='966FC8FD0EF43CB5B3EFB982C32530A7',
                          secret='gGXd5aw/yza5GHnyukmmeoMjicFUfeqWLgyP056c')


# 1) Ignorar la carpeta 'proc/' en el despliegue
writeLines("^proc/\n", ".rscignore")
writeLines("proc/\n", ".quartoignore")

quarto_publish_app(input = "ocs-viselsoc.qmd",
                   server="shinyapps.io",
                   name = "ocs-viselsoc",
                   account = "ocs-coes")


