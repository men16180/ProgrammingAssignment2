## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
##

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the inverse in the cache via the setin function.

cacheSolve <- function(x, ...) {

	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}