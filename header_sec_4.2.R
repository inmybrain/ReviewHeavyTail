## Last updated: 
## 22.01.14: created
rm(list = ls())
options(repos = c(CRAN = "http://cran.rstudio.com"))


## Global parameters -------------------------------------------------------
global_para <- list()
global_para$transform_funs <- c(function(x) x^3)


## library ----------------------------------------------------------
name_pkg <- c(
  # Rcpp
  "RcppArmadillo",
  "Rcpp",
  
  # Visualization
  "ggplot2",
  "gridExtra",
  "lemon",
  "ggpubr",
  
  # tidy data
  "tidyverse", "reshape2"
)
name_pkg <- unique(name_pkg)

bool_nopkg <- !name_pkg %in% rownames(installed.packages())
if (sum(bool_nopkg) > 0) {
  install.packages(name_pkg[bool_nopkg])
}
invisible(lapply(name_pkg, library, character.only = T)) # load multiple packages



## load functions ----------------------------------------------------------
sourceCpp("./Fun_cov_est.cpp")

CheckFile <-
  function(filename,
           fileext,
           filepath,
           cnt = 1,
           today = format(Sys.Date(), "%y%m%d")) {
    if (nchar(fileext) > 0) {
      fullname <- paste0(today, "_", filename, "_", cnt, ".", fileext)
      while (any(fullname %in% list.files(path = filepath))) {
        cnt <- cnt + 1
        fullname <-
          paste0(today, "_", filename, "_", cnt, ".", fileext)
      }
    } else{
      # file should be a folder
      fullname <- paste0(today, "_", filename, "_", cnt)
      while (any(fullname %in% list.files(path = filepath))) {
        cnt <- cnt + 1
        fullname <- paste0(today, "_", filename, "_", cnt)
      }
    }
    return(paste0(filepath, fullname))
  }

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


## Setting for plot --------------------------------------------------------
list_pdf_option <- list()
list_pdf_option$width <- 10
list_pdf_option$height <- 7
user_theme <- theme_bw() + theme(plot.title = element_text(size = 30),
                                 text = element_text(size = 25), 
                                 axis.text = element_text(size = 15),
                                 axis.title = element_text(size = 20), 
                                 legend.position = "top"
)