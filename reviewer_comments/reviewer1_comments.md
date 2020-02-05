# general comments

>README, it seems the code in your README file is only visualised but not executed. In this case, you could keep the README.md and remove README.Rmd (as this is basically redundant). 

The file README.Rmd was removed

> Your R folder contains a file called sysdata.rda. Is there a reason to keep these data there? Usually data should be placed under the root folder in a subfolder called data. I would suggest to move sysdata under the chirps/data folder, document the datasets (documentation is currently missing for both tapajos and tapajos_geom) and load the data using data("sysdata") or chirps::tapajos. If you make this change, please change the call to chirps:::tapajo in get_esi() accordingly. 

The sf polygon is exported as 'tapajos', the sf POINT object is not necessary and can be generated in the examples with sf. Also, functions dataframe_to_geojson and sf_to_geojson are exported since they had the same issue using ::: in the examples and could be useful for the users

> the man folder contains a figure folder. Is this good practice? I thought the man folder should be used only for documentation. Would it make sense to move the figure folder under inst, for instance? 

The /man structure is part of using the pre-built vignettes. We not aware of any issues with it. CRAN hasn't baulked at it and Adam (one of the co-authors) just submitted two package updates in the last month that use this method

#	tests folder:

> each of your test files contain a call to library(chirps). This is superflous because you load the package in testthat.R and that should suffice. My suggestion is to load all the packages needed by the tests in testthat.R and remove the commands library(package_name) in the individual test files.

DONE

> When you call library() sometimes you use library(package_name), other times library("package_name"). I would suggest to consistently use the latter. 

DONE

> test-get_chirps.R: you only test that you get the right object class but do not test the returned values, is there a reason for that? In my experience, it is very important to test the correctness of the actual data and I would suggest you to develop tests for that. 

We have updated the tests so it checks if the functions return the correct values. For this we downloaded the data from ClimateSERV and compared it with the ones retrieved by get_chirps, get_esi and precip_indices to validate it. The tests still have a skip_on_cran option as a CRAN policy. But we have opened an issue in the package repo and will keep it there until we figure out how to make 'vcr' works with 'chirps' https://github.com/agrobioinfoservices/chirps/issues/7

>test-get_esi.R: as above, you only test that you get the right object class but do not test the returned values. I would suggest to also test the data values. 

Same as above

> test-precip_indicesl.R: the file name seems to contain an extra "l", should that not be test-precip_indices.R? you only test the dimensions of the output data frame but not the values themselves. As, above, I would suggest to also test the data values. 

DONE

# vignettes:
>The file Overview.Rmd.orig seems redundant and can be removed. 

We use this file to speed up the vignette creation, here Jeroen Ooms shows how it works https://ropensci.org/technotes/2019/12/08/precompute-vignettes/

>It's not necessary to run twice the command precip_indices(dat, timeseries = TRUE, intervals = 15), the second one can be removed.

DONE

>When running the command get_chirps, I get the following warning message: In st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle = endCapStyle,: st_buffer does not correctly buffer longitude/latitude data. Can this warning be eliminated? Maybe adding a note in the documentation? 

These warning messages comes from 'sf', but we don't know if is a good practice to suppress that. We added a note to the documentation

>When running the command get_chirps, I get the following warning message: Warning messages: 1: In st_centroid.sfc(x$geometry) : st_centroid does not give correct centroids for longitude/latitude data. 2: In st_centroid.sfc(x$geometry) : st_centroid does not give correct centroids for longitude/latitude data. Can this warning be eliminated? Maybe adding a note in the documentation? 

Same as above

>more in-depth discussion of the functionalities included in the package will make it easier for the reader to understand if the chirps dataset is suitable for a given purpose. I would also mention that requests may take a long time to be executed. Is it feasible to use these functions to download large amount of data (for instance to perform a global scale analysis)? In general, a mention of the limitations of this package would be valuable. 

We added a section for the package limitations. And a better explanation about CHIRPS application into the paper.

# LICENSE = GPL-3
>when you use a widely known license you should not need to add a copy of the license to your repo. The files LICENSE and LICENSE.md are redundant and can be removed. 

DONE

>When I got my packages reviewed I was made aware that GPL-3 is a strongly protective license and, if you want your package to be used widely (also commercially), MIT or Apache licenses are more suitable. I just wanted to pass on this very valuable suggestion I received. 

Thank you, we changed to MIT as suggested

# inst/paper folder:

>it seems the code in your paper is only visualised but not executed. In this case, you could keep the paper.md and remove paper.Rmd. Also paper.pdf could be removed. 

DONE

>Fig1.svg is redundant (Fig1.png is used for rendering the paper). 

DONE

>in the paper, I would move the introduction to the CHIRPS data at the beginning as readers might not be familiar with these data. 

DONE

>in the paper you use the command chirps:::tapajos to load data in your sysdata.rda. This is not good practice. The ::: operator should not be used as it exposes non-exported functionalities. If you move sysdata under the chrips/data folder (as suggested above), the dataset can be loaded using data("sysdata") or chirps::tapajos. 

DONE

>Towards the end of your paper you state: Overall, these indices proved to be an excellent proxy to evaluate the climate variability using precipitation data [@DeSousa2018], the effects of climate change [@Aguilar2005], crop modelling [@Kehel2016] and to define strategies for climate adaptation [@vanEtten2019]. Maybe you could expand a bit, perhaps on the link with crop modelling? 

We updated this section with more examples, and hopefully a better explanation on CHIRPS applications and how *chirps* can help

# goodpractice::goodpractice():

>write unit tests for all functions, and all package code in general. 34% of code lines are covered by test cases. This differs from what is stated on GitHub (codecv badge = ~73% code coverage). The reason might be due to the fact you skip most of your tests on cran, is this because tests take too long to run? If so, is there a way you could modify the tests so that they take less time?

Same as above in the tests section

>fix this R CMD check WARNING: Missing link or links in documentation object 'precip_indices.Rd': ‘[tidyr]{pivot_wider}’ See section 'Cross-references' in the 'Writing R Extensions' manual. Maybe you could substitute \code{\link[tidyr]{pivot_wider}} with \code{tidyr::pivot_wider()}. 

The code was removed from @seealso

