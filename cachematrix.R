## The goal of these functions is to create a matrix store it and then build a
## process that will cache its inverse.

## This FUN creates a matrix and additional functions for cacheSolve to use.
makeCacheMatrix <- function(x = matrix()) { 
     ## Create cache matrix
     mtxstr <- NULL
     
     ## Use this to reset values in the matrix
     set <- function(y){
          x <<- y
          mtxstr <<- NULL
     }
     
     ## Take input x
     get <- function() {x}
     
     ## Called by cacheSolve
     setsol <- function(rtnsolve){ mtxstr <<- rtnsolve }
     
     ## Called by cacheSolve
     getsol <- function() {mtxstr}
     
     ## Called by cacheSolve
     list(set = set,
          get = get,
          setsol = setsol,
          getsol = getsol)
}

## This FUN tests for a cache'd matrix and return that if found, if not it
## processes the inverse of the wanted matrix and cache's it for later.
cacheSolve <- function(x, ...) { 
     ## Get matrix from makeCacheMatrix
     mtxstr <- x$getsol()
     
     ## Test to see if null, if not message and return
     if(!is.null(mtxstr)){
          message("Using cached Matrix...")
          return(mtxstr)
     }
     ## If NULL, get data from makeCacheMatrix and inverse
     imtx <- solve(x$get())
     
     ## Cache inversed matrix
     x$setsol(imtx)
     
     ## Return result
     imtx
     
}