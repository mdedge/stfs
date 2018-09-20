# stfs
Code and supplemental material for Statistical Thinking from Scratch: A Primer for Scientists, by M.D. Edge.

Each chapter's folder contains supplemental material for that chapter, including, if applicable, a pdf of exercise solutions, an R script to execude the R-based exercises, and an R script containing any code from the main text. All the exercise solutions and exercise code is also available in the exercise-solutions directory.

Additionally, the R functions used in the book are available in the R functions directory. These functions can also be installed via the book's companion R package, stfspack. To install and load stfspack, use

```
install.packages("devtools")
library(devtools) 
install_github("mdedge/stfspack")
library(stfspack)
```

You only need to install the package once. After restarting R, re-load the package with library(stfspack).

A script that generates all the book's figures is available in the figures directory.
