
# RWsearch <img src="man/figures/logo.png" align="right" alt="RWsearch logo" height="130">


## Installation

**RWsearch** stands for « Search in R packages, task views, CRAN and in the Web ». The package is available on CRAN (CRAN/package=RWsearch) and can be installed via:

```r
install.packages("RWsearch")
```

If you find **RWsearch** useful and wish to use it everyday (as I do), then modify your _R/etc/Rprofile.site_ file to launch **RWsearch** at every R start-up. Install also [pacman](https://CRAN.R-project.org/package=pacman) as the two packages share the same syntax and complement each other. For instance:

```r
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
       options(repos=r)
       old <- getOption("defaultPackages")
       options(defaultPackages = c(old, "pacman", "RWsearch"))
      })
```


## Introduction

**RWsearch** is a management tool that serves several purposes:

1. Provide a simple **non-standard evaluation** instruction to read and evaluate non-standard content, mainly character vectors.
2. Download the files that list all **available packages**, **archived packages**, **check_results** and **task views** available on CRAN at a given date and rearrange them in convenient formats. The downloaded files are:
    - crandb_down() => *crandb.rda* (4.9 Mo),
    - archivedb_down() => *CRAN-archive.html* (3.3 Mo),
    - checkdb_down() => *check_results.rds* (7.6 Mo),
    - tvdb_down() => *tvdb.rda* (31 ko).
3. **List the packages** that has been added, updated and removed from CRAN between two downloads or two dates.
4. **Search for packages that match one or several keywords** in any column of the *crandb* data.frame and by default (with mode _or_, _and_, _relax_) in the *Package name*, *Title*, *Description*, *Author* and *Maintainer* columns (can be selected individually).
5. **Display the results as a list or as a table** in the console or in the browser and save them as txt, md, html, tex or pdf files.
6. In one instruction, **download in one directory the whole documentation** and the tar.gz files related to one or several packages. This is the perfect tool to read the documentation off-line and study the source code of a package.
7. **Plot the graph of the package dependencies** in an html page in your browser (one example is the image behind me in the video below).
8. List the **parent and children dependencies** of one or several packages, either the first level or the full list (termed *recursive*). 
9. Use this information to reinstall or **install packages and their dependencies in the right order**, from the lowest number to the largest number of dependencies, and avoid mic-mac (very useful).
10. Check the **CRAN check_results** for one or several package and their (first level or recursive) dependencies. 
11. Explore the packages that have been **sent to the archives**. Download the source code of the latest version or a specific one. Get their last *CRAN check_result* and the reason why they have been archived.
12. Provide **tools for task view maintenance**: Detect the packages recently added to or updated in CRAN, check if they match some keywords, check if they are already recorded in a given task views.
13. Use some **bookmarks to various websites and search engines**. Keywords can be typed directly in the R console and are sent to the search engine, then displayed in your browser. The list of the search engines is expected to grow (with your help).

Compare to other packages ([packagefinder](https://CRAN.R-project.org/package=packagefinder), [websearchr](https://CRAN.R-project.org/package=websearchr)) or web services ([RDocumentation](https://www.rdocumentation.org/search?q=search&latest=1), [rdrr](https://rdrr.io/search?q=search)) with similar objectives, the search options of **RWsearch** are more sophisticated and allow for a finer search. RWsearch also addresses a much larger number of web search engines. Non-standard evaluation (or evaluation of non-standard content) makes it user friendly.


## Read the vignettes

5 vignettes are currently available (for the above topics):

    1. Introduction (1, 2, 3, 4)
    2. Print and download the documentation (5, 6)
    3. Packages versions and dependencies + install packages (7, 8, 9)
    4. Manage the task views (12, 5)
    5. Bookmarks and web search engines (13)


## `RWsearch` was presented at useR! 2019 in Toulouse, France

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/YYI7yYquJOc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


