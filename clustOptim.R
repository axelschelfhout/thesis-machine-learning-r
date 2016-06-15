## Function used for  generative cluster optimization

#' A function that returns a recommended, optional number of clusters for a given dataset
#' @param df - A numeric data frame. Input data frame should be sanitized and free of outliers. This function will normalize the data frame automatically
#' @keywords Cluster size / Recommendation for optimal cluster size
#' @export
#' @examples clustOptim(df) - where df is a numeric data frame.
#' clustOptim()

clustOptim = function (df) { # start of clustOptim function
  
  nr = nrow(df)
  
  # Rule of thumb estimator for maximum cluster size 
  cm = max(25, round(sqrt(nr/2), digits = 0))
  
  # Withins squared sum metric: startion point
  # Important for determining the lift factor of cluster size
  wss <- (nr - 1) * sum(apply(df, 2, var))
  
  # normalize data between [0;1] -> important for clustering
  # assumes clean-ish data: outlier detection, no missing data etc                                                                          
  #dn = scale(df)
  dn <- copy(df)
  
  # Compute additional elements per incremental cluster size increase 
  for (i in 2:cm) wss[i] <- sum(
    kmeans(dn,
           iter.max = 5000,
           centers = i)$withinss)
  
  # Descriptive graphics to help assess the reliability 
  qqnorm(wss); qqline(wss, col = 2)
  
  plot(1:cm, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  # assign upperbound limit for deviataion 
  tempMin = 1000000000
  
  # compute the point at which the remaining wss elements result in a variance of less than 1
  for (i in 1:(cm-1)) {
    
    if (tempMin > (mean(wss) - 1.96 * sd(wss[-c(1:i)]))) {
      tempMin = var(wss[-c(1:i)])
      print(tempMin)
      next()
    }
    cat("The recommended cluster size is:", i, "\n", "\n")
    
    # Return the recommended number of clusters    
    return (i)
  }
  
} # end of clustOptim function


