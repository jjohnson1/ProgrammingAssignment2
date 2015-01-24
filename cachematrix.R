## These functions provide a mechanism for storing the results of the potentially 
## time-consuming task of finding the inverse of a matrix in the cache. This allows the user
## to simply reference the cached value if it is available, rather than recomputing the inverse
## of the matrix
##
## History: Created by JJ, 1/24/15. Modified from skeleton code provided in https://github.com/rdpeng/ProgrammingAssignment2.


## makeCacheMatrix accepts a matrix as its only input argument and returns a list with the items set, get, setinverse,
## and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL # default value

      # set stores a new matrix as the original (non-inverted) matrix. It also sets inv to NULL to avoid returning the inverse 
      # of the wrong matrix.
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }

      get <- function() x # return the non-inverted matrix

      setinverse <- function(inverse) inv <<- inverse # store the inverted matrix

      getinverse <- function() inv # return the inverted matrix

      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # return list with items for each available method
}


## cacheSolve returns the inverse of the matrix stored in x$get. x must be a list return by a call to makeCaceMatrix. cacheSolve
## offers the potential to return the inverse of a matrix much faster than a call to solve, since this function only calculates
## the inverse if it has not already done so. If the inverse has already been calculated, cacheSolve simply returns the pre-calculated value.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      
      if(!is.null(inv)) {
            ## inv is not NULL, thus we have an up-to-date inverse matrix already calculated. Alert the user and return that matrix.
            message("getting cached data")
            return(inv)
      }
      
      ## If the inverse matrix has not already been calculated (inv == NULL), calculate the inverse, store it in x, and return the inverse
      ## matrix.
      data <- x$get() # get the original matrix
      inv <- solve(data) # calculate inverse (who would have guessed the function was named 'solve'?)
      x$setinverse(inv) # store the newly-calculated inverse for next time
      
      inv # return the inverse matrix
}
