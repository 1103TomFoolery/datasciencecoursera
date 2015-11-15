## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## function makeCacheMatrix
## input parameter - numeric square matrix
## This function call creates a matrix object with values defined by the input matrix x and methods that operate on
## that matrix to get and set values.  After the function is called with a particular input matrix it stores the inverse
## of that matrix in a cache and sets a flag to NULL
## When a new matrix is created, the variable Inv is set to NULL
## internal function $set is used to update the value of the matrix and clears the cache
## internal function $get retrieves the input matrix x

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL
	set <- function(y) {
		x <<- y
		Inv <<- NULL
	}
	get <- function() x
	setInv <- function(solve) Inv <<- solve
	getInv <- function() Inv
	list(set = set, get = get, 
		setInv = setInv,
		getInv = getInv)
}


## Write a short comment describing this function
## 
## Function name:  cacheSolve
## Input parameter x is a matrix object with methods $get and $set
## 
## gets the value of the input matrix
## if the attribute Inv of the matrix object == NULL print "getting cached data" and return the matrix
## else get the value of the matrix, invert it by calling the solve function, set the matrix object to the inverted
## matrix and set Inv == NULL
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInv()
	if(!is.null(Inv)) {
		message("getting cached data")
		return(Inv)
	}
	data <- x$get()
	Inv <- solve(data, ...)
	x$setInv(Inv)
	Inv

}
