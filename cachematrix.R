## The functions are used to cache a calculated value of inverse of a matrix. 
## If the inverse has been cached the value from cache is returned. Else, the 
## inverse in calculated and stored in the cache.

## makeCacheMatrix takes a matrix as an argument and creates a special matrix
## with functions. The set type of function helps to set both the matrix and the
## inverse of the matrix. The get type of functions are used to return the 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  if(nrow(x) != ncol(x)){
    message ("Matrix is not square. Inverse cannot be computed")
    return()
  }
  set<- function(y){
    x <<- y
    inverse <<- NULL
  }
  get<-function() x
  createInv <- function(){
    inverse <<- solve(x)
  }
  setInv <- function(inv){
    inverse <<- inv
  }
  getInv <- function() inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The function checks if cached value exists. If cache value is available it 
## returns the value. Else, it computed the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if (!is.null(inverse)) {
    message("getting cached date")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInv(inverse)
  inverse
}
