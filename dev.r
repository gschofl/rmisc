
require(devtools)
require(roxygen2)
rmisc <- as.package("~/R/Devel/rmisc")

document(rmisc, clean=TRUE, reload=TRUE)
build(rmisc)
install(rmisc, reload=TRUE)

undebug(rmisc:::generate_tag_files)
install_packages("roxygen3", "github", "hadley")