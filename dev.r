document(pkg="~/R/Devel/rmisc", clean=TRUE)
build(pkg="~/R/Devel/rmisc")
install(pkg="~/R/Devel/rmisc")

load_all("/home/gerhard/R/Devel/rmisc/")
installPackages(c("rmisc","rentrez"), "github", "gschofl")
installPackages(c("devtools","scales","sinartra","evaluate","ggplot2"),
                "github", "hadley")
installPackages(c("plyr","test_that","staticdocs","httr","stringr"),
                "github", "hadley")
installPackages(c("reshape","lubridate","helpr"),
                "github", "hadley")
installPackages(c("roxygen"), "github", "klutometis")
installPackages(c("knitr"), "github", "yihui")

installPackages(c("igraph","multtest","vegan","foreach","iterators"), 
                repos="CRAN", dependencies=TRUE)

installPackages(c("XML","RCurl","RMySQL","itertools","lme4","ape"), 
                repos="CRAN", dependencies=TRUE)

installPackages(c("Biostrings","genomes","biomaRt"), dependencies=TRUE)
