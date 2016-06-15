## This function load the necessary scripts for using the 

#' Package which configures Rstudio sessions for analytical tasks for Jumba
#' @param No parameter, it's a side-effect function call
#' @keywords preparation / work directory / packages
#' @export
#' @examples prepwd() - will setup the necessary packages for analytical tasks for Jumba. Call once per instance (every time you open RStudio)
#' prepwd()

prepwd = function() { # start of prepwd() function
  
  
  pkgList = c(
    
    # R partitioning via decision trees and random forest classifiers
    "rpart",
    "randomForest",
    
    # Clustering
    "cluster",
    "mclust",
    "fpc",
    
    # General analytics
    "ape",
    "BayesBridge",
    
    # Performance
    "microbenchmark",
    
    # General utility
    "SnowballC",
    "zoo",
    "devtools",
    "RCurl",
    "xts",
    "httr",
    "dplyr",
    "rvest",
    "lattice",
    
    # Formatting
    "fasttime",
    "jsonlite",
    "XML",
    
    # Visualization 
    "wordcloud",
    "plotrix",
    "rpart.plot",
    "ggplot2",
    
    # Natural Language Processing and Topic Models
    "tm",
    "NLP",
    "lasso2",
    
    # I/O & databases
    "data.table",
    "elastic",
    "rmongodb",
    
    # Machine learning
    "e1071",
    "caret",
    
    # Shiny
    "shiny",
    "shinydashboard",
    "dygraphs",
    "sparkline",
    "googleVis"
  )
  
  for (pkg in pkgList) {
    
    if (!require(pkg, character.only = TRUE)) {
      
      install.packages(pkg)
      if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
      
    }
    
    if (pkg == tail(pkgList, 1)) cat("Configuration of workspace is complete!", 
                                     "If you see the following Warning message:", 
                                     "In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE...", 
                                     "It means that a package was not installed previously.",  
                                     "The warning message will not persist after the first time it was used", 
                                     sep = "\n")
  }
  
} 
# end of prepwd() function