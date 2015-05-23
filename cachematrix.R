###################################################################################
## R Programming
##   Programming Assignment #2
##   Ken Berg (kdberg)  21 May 2015
###################################################################################

##   The first function is used to create and implement the Cache that allows
##   for processing of an operation to be completed in steps, caching the results
##   inbetween computations.  The second function will utilize cache to compute
##   the inverse of a matrix.

##   The makeCacheMatrix function accepts as input a matrix variable.  It then sets up the necessary
##   functions and variables in the cache.  Finally, it returns the list of functions to utilize
##   the cache. 
##
makeCacheMatrix <- function(x = matrix()) {
  
  j <- NULL               			## initialize to null

 
  set <- function(y) {				## set function
    x <<- y
    j <<- NULL
  }
  
  get <- function() x				## get function           
  
  setjmatrix <- function(jmatrix)	## set inverse matrix 
    j <<- jmatrix
  
  getjmatrix <- function() j		## get inverse matrix                 
  
  list(set = set, get = get,		## list names
       setjmatrix = setjmatrix,
       getjmatrix = getjmatrix)
}


##   The cacheSolve function accepts as input a matrix to be inverted that was created using
##   the makeCacheMatrix function.  cacheSolve checks to see if the inverted matrix is already
##   in cache, and if so returns it.  Otherwise the function computes the inverse of the matrix
##   and stores the results in cache.

cacheSolve <- function(x, ...) {

  j <- x$getjmatrix()				## get the value of j from cache
  
  if(!is.null(j)) {					## if inverse already computed, get from cache and return it
    message("getting cached data")
    return(j)
  }
 
  data <- x$get()              		## otherwisw get the matrix and compute the inverse
  j <- solve(data, ...)  
  x$setjmatrix(j)             		## set the matrix inverse
  j                         		## print the result
}
