
 
## create a cache object that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## store original matrix
  save.originaldata <- x;
  ## init the cache object = null
  save.cache <- NULL
  
  ## this is used to invert the matrix and store the value in the cache 
  save.fninvert <- function(){
     ## invert the matrix and store the value in the cache
     save.cache <<- solve( save.originaldata, LINPACK = TRUE)
     
     ## provide feed back then the matrix is inverted
     print("Inverts the matrix")
  }
  
  save.retrive <- function() save.cache
  save.data <- function() save.originaldata
  
  ## returns an interface for other object to interact with the cache
  list(invertmatrix = save.fninvert, getcache= save.retrive,getdata=save.data, getdata = save.originaldata)
}


 
## cachesolve takes a matrix and inverts it
cacheSolve <- function(x, ...) {
   ## Creates the Cache object and stores the original matrix      
   m <- makeCacheMatrix(x)
   
   ## creates a function that returns the matrix that is inverted
   m.computeInverse <- function( value = matrix()){
     ## retrive original matrix
     mdata <- m$getdata()
     ## compares the orginal matrix to the current on being passed in
     if(identical(value, mdata) && !is.null(m$getcache())) {
         ## if matrix is same and the inverse is cached
         ## return the cache and exit the function
         return(m$getcache())  
     }
     
     ## if you got here then the above test fail
     ## so invert the matrix and return the results stored in the cache
     m$invertmatrix();
     m$getcache()
   
   }
   ## Return a matrix that is the inverse of 'x'
   list(inverse = m.computeInverse)
}
