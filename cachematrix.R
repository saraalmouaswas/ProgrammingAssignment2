## The two functions below are used to store an invertible matrix and calculate then cache its inverse. To implement:
# Create the matrix, and cache the matrix 							  > my_matrix <- matrix(1:4, nrow=2, ncol=2)					          > mat <- makeCacheMatrix(my_matrix)
# Next line will return the calculated inverse					      > cacheSolve(mat, my_matrix)
# Next line will return the cached inverse						      > cacheSolve(mat, my_matrix)
# Next line will return the calculated inverse of the new matrix      > cacheSolve(mat, new_matrix) 


## This function creates 4 methods that allow for setting and getting the value of the matrix being cached, and setting/getting the value of the inversed matrix

makeCacheMatrix <- function(invertibleMatrix = matrix()) {
		inverted <- NULL
        set <- function(y) {
        	invertibleMatrix <<- y
            inverted <<- NULL
        }
        get <- function() invertibleMatrix
        setSolved <- function(solved) inverted <<- solved
        getSolved <- function() inverted
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)
}


## This function calcualtes the inverted value of the matrix. If the matrix has changed, it calcualtes and sets a new value. If the matrix has not changed, it checks for existing cached value before calculating and setting new value in cache.

cacheSolve <- function(cache, matrix, ...) {
	data <- cache$get()
	if (identical(data, matrix)){
		inverted <- cache$getSolved()
    	if(!is.null(inverted)) {
    		message("getting cached data")
    		return(inverted)
     	}
     	inverted <- solve(data, ...)
     	cache$setSolved(inverted)
     	return(inverted)
	}
	cache$set(matrix)
	inverted <- solve(matrix, ...)
	cache$setSolved(inverted)
	inverted
}