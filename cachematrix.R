## These functions are used to calculate the inverse of a matrix. 
## If it is already calculated, the inverse is cached from the memory
##

## This function defines the setting and getting functions

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(B){
                      x <<- B
                      Inv <- NULL
  }
get <- function()x
setinv <- function(In) Inv <<- In
getinv <- function() Inv
list (get = get, set = set, setinv = setinv, getinv = getinv)
}


## This function calculate the inverse if it is not already available

cacheSolve <- function(x, ...) {
        Inv <- x$getinv()
        if(!is.null(Inv)){
          message("getting cached data")
          return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setinv(Inv)
        Inv
}