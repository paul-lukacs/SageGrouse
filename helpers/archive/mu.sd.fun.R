    mu.sd <- function(x){
      mu <- (max(x) + min(x))/2  
      s <- ((max(x) - mu)/2)/1.96
      c(mu, s)
    }
