## The 2 functions below (makeCacheMatrix and cacheSolve) create a special object 
## that stores a matrix and caches its inverse in an environment that is different 
## from the current environment.
## If the inverse of the matrix is required more than once it can be retrieved
## from memory (cache) instead of being calculated again.
##
## Example to use functions for inverse of invertible matrix A:
## B <- makeCacheMatrix(A)
## invA <- cacheSolve(B)


## The first function, makeCacheMatrix constructs functions and creates a list object
## of these functions.  The constructed functions do the following: 
## 
## 1. sets the values of the matrix in cache
## 2. gets the values of the matrix from cache
## 3. sets the inverse of the matrix in cache
## 4. gets the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
      ## first empty cache
      m <- NULL
      
      ## 1. set matrix in cache
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## 2. get matrix from cache
      get <- function() x
      
      ## 3. calculate inverse (using solve function) and store in cache
      setinverse <- function(solve) m <<- solve
      
      ## 4. retrieve inverse from cache
      getinverse <- function() m
      
      ## return the list as output of function makeCacheMatrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## The following function calculates the inverse of the special "matrix" created  
## with the makeCacheMatrix. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the 
## value in the cache using the setinverse function constructed in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
## check if inverse has already been calculated
      m <- x$getinverse()

## If inverse is in cache, exit function with return statement and value from memory
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }

## else calculate inverse and store in memory 
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}







