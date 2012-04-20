document(pkg="~/R/Devel/rmisc", clean=TRUE)
build(pkg="~/R/Devel/rmisc")
install(pkg="~/R/Devel/rmisc")

load_all("/home/gerhard/R/Devel/rmisc/")
installPackages("httr", "github", "hadley")
installPackages("evaluate")


