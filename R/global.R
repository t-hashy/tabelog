# Loading -----------------------------------

# Create loading function
# Create loading function
load.pkg <- function(pkg, install.only = FALSE, install.available = TRUE) {

  # Install the package
  if(!pkg %in% library()$results[,1]){

    if(install.available){
      install.packages(pkg)
      library(pkg, character.only = TRUE)
      return(paste(pkg, "has been installed and loaded."))
    }else{
      message(paste(pkg, "has not been installed, however not done because the argument of 'install.avairable' is FALSE. "))
      return("LOAD ERROR")
    }

  }else{
    if(!pkg %in% (.packages())){
      library(pkg, character.only = TRUE)
      return(paste(pkg, "has loaded."))
    }else{
      return(paste(pkg, "has already loaded."))
    }
  }
}

# Packages

## Set up
load.pkg("usethis") # Set up and file creations
load.pkg("renv") # Create renv folder
load.pkg("config") # Create confifguration file

# Coding
load.pkg("progress") # Show a progress bar

# SQL
load.pkg("DBI") # Manipulate data bases
load.pkg("RSQLite") # SQLite

## Shiny
load.pkg("shiny") # Basic shiny
load.pkg("shinymanager") # Configuration
load.pkg("shinysurveys") # Create survey form

## Scraping
load.pkg("rvest") # Scraping

# Data manipulations
load.pkg("tidyverse") # Data Manipulation
load.pkg("tidyr") # Manipulate tidy tables
load.pkg("dplyr") # Data manipulation
load.pkg("stringr") # String manipulation
load.pkg("DT") # Interactive data table
load.pkg("stringi") # String manipulation

# Data visualization
load.pkg("ggthemes") # Rich plot themes
load.pkg("ggplot2") # Plotting
load.pkg("plotly") # Interactive plotting
load.pkg("ggpubr") # Show multiple ggplot in one window
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
load.pkg("GGally") # Variables
load.pkg("DataExplorer") # Create data summary report

# Google
load.pkg("googlesheets4")

# Statics
load.pkg("DescTools") # Correlation between factors
load.pkg("scales") # Rich scaling e.g. comma separated labels

# # Text mining
# if(!"RMeCab" %in% library()$results[,1]){
#   install.packages( "RMeCab", repos = "http://rmecab.jp/R", type = "source")
#   library(RMeCab)
# }else if(!"RMeCab" %in% (.packages())){
#   library(RMeCab)
# }

# Utilities ---------------------

# Progress bar
create_new_pb <- function(length)  {
  progress_bar$new(
    format = "(:spin) [:bar] :percent [Elaspsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = length,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100
  )
}

# Generate uid
generate_uid <- function(current.uids = list(), chr.length = 8){

  # Set basics
  characters <- c(LETTERS, letters, 0:9)

  # Generate new uid
  new.uid <- NULL
  duplication.check <- TRUE
  while(duplication.check){
    for(chr in 1:chr.length){
      chr.this <- sample(characters,1)
      if(length(new.uid) == 0){
        new.uid <- chr.this
      }else{
        new.uid <- paste(new.uid, chr.this, sep = "")
      }
    }
    if(!new.uid %in% current.uids || length(current.uids) == 0){
      duplication.check <- FALSE
    }
  }

  # Return new uid
  return(new.uid)

}
