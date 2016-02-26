## These functions allow us to calculate an inverse of a matrix 
## and recall the inverse in case it was already calculated
## using the scoping rules that allow us to pass on values from one function to the other

## the first function creates a list of four actions (4 functions):
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


makeCacheMatrix <- function(X = matrix()) {
  
## setting the inverse empty each time a matrix is made
  
  inverse <- NULL
    
## define the 4 functions
  set <- function(Y) {
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(solution) inverse <<- solution
  getinverse <- function() inverse
  
## output is a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns a matrix that is the inverse of 'x
## Before calculating, the function checks whether the inverse was calculated before
## If yes, uses that value so that we don't calculate again

cacheSolve <- function(X, ...) {

  inverse <- X$getinverse
  ## checking if inverse exists in cache, gives comment if yes
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## when it doesn't exist yet, calculating inverse and setting it
  matrix <- X$get()
  inverse <- solve(matrix, ...)
  X$setinverse(inverse)
  inverse
}
