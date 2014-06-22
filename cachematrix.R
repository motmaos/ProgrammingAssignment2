## Put comments here that give an overall description of what your
## functions do

## We have two functions here, makeCacheMatrix() and cacheSolve(). 
## The first gets an invertible square matrix as argument ad returns 
## a list of functions which store in a cache and get the values of 
## the matrix andits inverse, the second one, which gets as argument 
## the list returned by the first function, returns the inverse of 
## the matrix calculating it if it is not yet in the cache, or 
## returning the cached value if there is one.

## Write a short comment describing this function

## this function accept as argument a matrix, my_mat, and defines a
## list of four functions that set and get the matrix and its inverse 
## in the cache.

makeCacheMatrix <- function(my_mat = matrix()) {
	inverse <- NULL
	set <- function(y) {
		my_mat <<- y
		inverse <<- NULL
	} 
	get <- function() my_mat
	setInverse <- function(z) { inverse <<- z }
	getInverse <- function() inverse 

	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

## This function accept as argument the list produced by 
## makeCacheMatrix and returns the cached inverse matrix if it
##  has already been calculated, otherwise it determines the
##  inverse (with every other argument you pass with "..." arguments),
## stores it in the cache and returns it
 
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getInverse()
	
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
