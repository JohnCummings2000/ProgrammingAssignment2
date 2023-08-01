makeCacheMatrix <- function(x = matrix()) {
    #Initialize the cache to store inverse matrix
    inv_matrix <- NULL
    
    #function to set matrix and null the cached inverse
    set <- function(y) {
        x<<-y
        inv_matrix <<- NULL
    }
    #functions that gets the matrix, sets the cached inverse and gets the cached inverse
    get <- function() x
    setinv_matrix <-function(inverse) inv_matrix <<- inverse
    getinv_matrix <- function() inv_matrix 

    #returns a list of functions to access the matrix and its inverse
    list(set = set, get = get, setinv_matrix = setinv_matrix, getinv_matrix = getinv_matrix)
}

cacheSolve <- function(x, ...) {
    #checks if the cached inverse is already calculated or not
    inv_matrix <- x$getinv_matrix
    if(!is.null(inv_matrix)) {
        return(inv_matrix)
    #if the inverse is not already cached, calculate the inverse
    } else {
        my_data <-x$get()
        inv_matrix <-solve(my_data,...)
        
        #cache the inverse
        x$setinv_matrix(inv_matrix)
        inv_matrix
    }
}