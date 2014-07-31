## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
##                      that can cache its inverse.
## inv_m is the variable to save the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
    inv_m <- NULL
    
    ## function 1: reset matrix
    set <- function(new_m) {
        m <<- new_m
        inv_m <<- NULL
    }
    
    ## function 2: return original matrix
    get <- function() m
    
    ## function 3: assign a matrix to variable 'inv_m'
    set_inverse_m <- function(inverse) inv_m <<- inverse
    
    ## function 4: return 'inv_m'
    get_inverse_m <- function() inv_m
    
    list(set = set, get = get,
         set_inverse = set_inverse_m,
         get_inverse = get_inverse_m
    )
    

}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
         
    inv_m <- x$get_inverse()
    
    if (!is.null(inv_m)) {
        message("Getting Cached DATA")
        return(inv_m)
    }
    
    a_matrix <- x$get()
    inv_m <- solve(a_matrix)
    
    x$set_inverse(inv_m)
    inv_m
}
