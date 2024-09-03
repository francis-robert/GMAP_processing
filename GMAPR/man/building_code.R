# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(devtools)
library(usethis)
library(plyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(purrr)
library(reshape2)
library(scales)
library(stringr)
library(tidyr)
library(tidyverse)
library(lintr)
library(roxygen2)

use_package("dplyr", type="Imports", min_version = "1.1.2")
use_package("ggplot2", type="Imports", min_version = "3.4.4")
use_package("janitor", type="Imports", min_version = "2.2.0")
use_package("plyr", type = "Imports", min_version = "1.8.8")
use_package("purrr", type = "Imports",min_version = "1.0.1")
use_package("reshape2", type = "Imports", min_version = "1.4.4")
use_package("scales", type = "Imports", min_version = "1.2.1")
use_package("stringr", type = "Imports", min_version = "1.5.0")
use_package("tidyr", type = "Imports", min_version = "1.3.0")
use_package("openair",type = "Suggests",min_version = "2.17.0")
#use_package("tidyverse", type = "Imports", min_version = "2.0.0")

use_description(fields = list(
  Title = "Processing & Visualization of Geospatial Mobile Air-Monitoring Data",
  Version = "0.0.1.0000",
  `Authors@R` = person(given="Robert",family ="Francis",middle= "Tony",
                      email = "francis.robert@epa.gov", role = c("aut","cre")),
  Description = "GMAPR allows users to process raw data from multiple mobile
  air monitoring sampling methods in preperation for validation by quality
  assurance officers. Invalid data can be identified. Quality flags can be
  applied based on values of specific analytes and/or specific temporal sampling
  periods. Specific flags are defined by approved Quality Assurance Assessment
  Programs. This package also provides an `in field` tool for visualizing data
  before the validation process for both mobile and stationary air monitoring
  transects. GMAPR also provides function to analyze and visualize canister
  collected samples.",
  License = use_agpl3_license()

))
