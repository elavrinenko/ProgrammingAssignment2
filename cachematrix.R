## Returns a list of functions used to set and get cached matrix and value of its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setinv <- function (inversed) inv <<- inversed
	getinv <- function () inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
 
## Returns cached inverse for a matrix if there is one cached, and calculates inverse if there is no
cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Getting cached inverse value")
		return(inv)
	}       
	message("Setting new inverse value")
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
