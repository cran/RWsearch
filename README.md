
# RWsearch <img src="man/figures/logo.png" align="right" alt="RWsearch logo" height="130">


## Installation

**RWsearch** stands for « Search in R packages, task views, CRAN and in the Web ». The package is currently available only on CRAN (CRAN/package=RWsearch) and can be installed via:

```r
install.packages("RWsearch")
```

If you find _RWsearch_ useful and wish to use it everyday, then modify your _R/etc/Rprofile.site_ file to launch _RWsearch_ at every R start-up. Install also [pacman](https://CRAN.R-project.org/package=pacman) as the two packages share the same syntax and complement each other. For instance:

```r
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
      options(repos=r)
      old <- getOption("defaultPackages")
      options(defaultPackages = c(old, "pacman", "RWsearch"), repos = r)
      })
```


## Introduction

**RWsearch** is a management tool that serves several purposes:

1. Provide a simple instruction to read and evaluate non-standard content (**non-standard evaluation**).
2. Download the **list of all packages** and **task views** available on CRAN at a given date and rearrange them in a convenient format.
3. List the packages that has been added, updated and removed between two dates. 
4. **Search packages that match one or several keywords** (with mode _or_, _and_, _relax_) in any column of the data.frame and by default in columns Package name, Title, Description, Author and Maintainer (can be selected individually).
5. Arrange the results in different format (list, tables with different columns) and display them in console or in txt, md, html, pdf files.
6. In one instruction, download on your disk the whole documentation related to one or several packages. This is the perfect tool to read the documentation off-line. 
7. Provide **tools for task view maintenance**: Detect the packages recently added to or updated in CRAN, check if they match some keywords, check if they are already recorded in a given task views.
8. Provide **quick links to launch web search engines** and search for keywords from the R console. This list of web search engines is expected to grow (with your help).

Compare to other packages ([packagefinder](https://CRAN.R-project.org/package=packagefinder), [websearchr](https://CRAN.R-project.org/package=websearchr)) or web services ([RDocumentation](https://www.rdocumentation.org/search?q=search&latest=1), [rdrr](https://rdrr.io/search?q=search)) with similar objectives, the search options of _RWsearch_ are more sophisticated and allow for a finer search. RWsearch also addresses a much larger number of web search engines. Non-standard evaluation (or evaluation of non-standard content) makes it user friendly.

Read the vignettes. 



## Talk About `RWsearch` at useR!2019 in Toulouse, France

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/YYI7yYquJOc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


