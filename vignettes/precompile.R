#Vignettes that depend on internet access have been precompiled:
old_wd <- getwd()

setwd("vignettes/")

library(knitr)
knit("phanerozoic-reefs.Rmd.orig",
     "phanerozoic-reefs.Rmd")
knit("tetrapod-biodiversity.Rmd.orig",
     "tetrapod-biodiversity.Rmd")

library(devtools)
build_vignettes()

setwd(old_wd)

