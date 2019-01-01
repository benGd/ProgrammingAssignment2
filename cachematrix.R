## Assignment2 Caching a matrix inverse 

## creates a matix that can cache the inverse of itself

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y) {
    		x <<- y
    		inv <<- NULL
  	} 
  	get <- function() x
  	setInv <- function(inverse) inv <<- inverse
  	getInv <- function() inv
  	list(set = set, get = get,
       		setInv = setInv,
       		getInv = getInv)
}


## finds the inverse of the matrix and stores it in the cache. If the inverse is already 
## cached it simply returned the cached entry.

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
  	if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data, ... )
  	x$setInv(inv)
  	inv
        
}
