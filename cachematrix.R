## - makeCacheMatrix function creates a special "matix" to get/set value & inverse of a matrix.
##
## - cacheSolve function calculates the inverse of the special "matix" created with the
##   makeCacheMatrix function. It has an internal cache to speed up computation time for 
##   matrix inverse that were already calculated.


## makeCacheMatrix creates a special "matix", which is really a list containing a #function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse,	getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matix" created with the
## makeCacheMatrix function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in
## the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
