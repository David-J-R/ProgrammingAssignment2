## These functions together can calculate the inverse of a matrix
## And save the result in an CacheMatrix, so that you don't have 
## to calculate the result more than once.
## To use this functionality you first have to create a CacheMatrix
## out of an existing matrix with the help of makeCacheMatrix
## and then use cacheSolve on it, to get the inverse


## This function creates a CacheMatrix (represented as a list),
## which containes itself 4 different functions:
## set, which sets the value (a matrix) of the CacheMatrix
## get, which return you the value of the CacheMatrix
## setinverse, which sets the inverse of the value from the 
## CacheMatrix. (it is also a matrix)
## getinverse, which return you the inverse of the value 
## (if already enlisted and otherwise NULL)
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function checks, if there is already an inverse set 
## for the given CacheMatrix
## If the inverse isn't already existing, it is beeing evaluated 
## and added to the CacheMatrix
## Afterwards it returns the inverse
## Not that only quadratic matrices can have an inverse and not 
## all of them are invertible
## If you're CacheMatrix isn't invertible, you will get an error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
    i<-solve(data, ...)
  
  x$setinverse(i)
  i
  
}
