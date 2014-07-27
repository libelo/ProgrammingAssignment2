## return a cached inverse of a matrix if there is one, 
## or calculate the inverse of a matrix and cache it. 

## sample input : matrix(c(1,2,3,4), nrow = 2, ncol = 2)
## output : list of four functions.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    get <- function() { x }
    set <- function(y) { x <<- y; inverse_matrix <<- NULL }
    get_inverse <- function() { inverse_matrix }
    set_inverse <- function(new_inverse) { inverse_matrix <<- new_inverse }
    list(get = get, set = set, 
         get_inverse = get_inverse, set_inverse = set_inverse)
}


## input : a list created by makeCacheMatrix function, and options to be used by solve function
## output : a cached inverse of a matrix if there is one, or a calculated inverse of a matrix

cacheSolve <- function(x, ...) {
    inverse = x$get_inverse()
    if(!is.null(inverse)) { print('getting cached data'); return(inverse) }
    print('setting new cache')
    new_inverse = solve(x$get(), ...)
    x$set_inverse(new_inverse)
    new_inverse
}
