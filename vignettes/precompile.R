# vignettes that depend on internet access need to be precompiled and take a
# while to run
library("knitr")
knit("vignettes/Overview.Rmd.orig", "vignettes/Overview.Rmd")

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/Overview.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
fileConn <- file("vignettes/Overview.Rmd")
writeLines(replace, fileConn)
close(fileConn)


library("devtools")
build_vignettes()
 
# move resource files to /doc
resources <-
  list.files("vignettes/", pattern = ".png$", full.names = TRUE)
file.copy(from = resources,
          to = "docs",
          overwrite =  TRUE)
