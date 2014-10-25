## 2014.10.25 Coursera - Assignment 2
## The function makeCacheMatrix creates a list of functions that are used in the computation of the inverse of a matrix and 
## by storing and retrieving the original matrix, and its reverse.
## The function cacheSolve either retrieves the inversed matrix from cache, or computes, and stores the inverse of the calling matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse
## makeCacheMatrix returns a list of functions, which can are bound as components to the calling matrix

makeCacheMatrix <- function(x = matrix()) 
{
  InversedMatrix <- NULL                                                       ## Initialising the local variable to NULL
  set <- function(y) 
  {
    x <<- y                                                                     ## y is a free variable and without other knowledge assumed to be NA
    InversedMatrix <<- NULL                                                     ## Initialising the global variable to NULL
  }
  get <- function() x
  setMatrixInverse <- function(MatrixInverse) InversedMatrix <<- MatrixInverse  ## Preserves the inverse of the matrix to the global variable InversedMatrix
  getMatrixInverse <- function() InversedMatrix                                 ## Simply returns the inversed matrix
  list(set = set, get = get,                                                    ## The list of functions are returned
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## cacheSolve computes the inverse of the matrix if it is not available in the cache

cacheSolve <- function(x, ...) 
{
  InvMatrix <- x$getMatrixInverse()    ##Calling getMatrixInverse to see if the inverse matrix exists 
  if(!is.null(InvMatrix)) 
  {
    message("getting cached inversed matrix")   ## Notify the user that the cache is being used
    return(InvMatrix)
  }
  matrix <- x$get()                  ## The inverse of the matrix does not exist in the global variable, so
  InvMatrix <- solve(matrix)         ## calculate it by inverting the original matrix using R's "solve" function. 
  x$setMatrixInverse(InvMatrix)      ## Pass the inversed matrix to setMatrixInverse for storage in the global variable InversedMatrix
  InvMatrix                          ## Return the inverse of the matrix
}




