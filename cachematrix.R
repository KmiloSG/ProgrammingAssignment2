## Put comments here that give an overall description of what your
## functions do:
## PROGRAMING ASSIGNMENT 2: LEXICAL SCOPING

# Use of the '<<-' operator to assign a value to an object in an environment
# that is different form the current environment.

## Write a short comment describing this function
##---------------------------------------------------------------
# Function makeCacheMatrix:
# Creates a special "matrix" object (actually a list),
# and stores in cache the inverse of a given matrix.
## NOTE: 
## -- It is assumed the matrix is a square matrix.
## -- It is assumed the matrix is invertible (i.e. Determinant(Matrix) != 0)

makeCacheMatrix <- function(M = matrix()) {
      M.inv <- NULL
      set <- function(X) {
            M <<- X
            M.inv <<- NULL
      }
      get <- function() M
      setinv <- function(Inv) M.inv <<- Inv
      getinv <- function() M.inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##---------------------------------------------------------------
# Function cacheSolve:
# Validates if a given Matrix is stored in cache. If so returns the 
# inverse matrix stored. If not, calculates the inverse of the matrix
# and stores it in cache.
## NOTE: 
## -- It is assumed the matrix is a square matrix.
## -- It is assumed the matrix is invertible (i.e. Determinant(Matrix) != 0)
cacheSolve <- function(x, ...) {
      M.inv <- x$getinv()
      if(!is.null(M.inv)){
            message("Getting cached data")
            return(M.inv)
      }
      A <- x$get()
      M.inv <- solve(A, ...)
      x$setinv(M.inv)
      M.inv
        ## Return a matrix that is the inverse of 'x'
}
