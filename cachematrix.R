## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                        
    s <- function(y) {                   
      x <<- y                          
      i <<- NULL                       
    }
    g <- function() x                  
    
    seti <- function(inverse) i <<- inverse 
    geti <- function() i                    
    list(s = s, g = g, seti = seti, geti = geti) 
  }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("get cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}
