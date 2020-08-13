## Here there are a pair of functions that cache the inverse of a matrix if invertible.


## This function creates an object (matrix) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_matrix <<- inv
        getinv <- function() inv_matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}



## This function computes the inverse of an invertible matrix returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache. 
    
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinv()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        if (det(data) == 0){
                message("matrix not invertible")
        }
        else{
                inv_matrix <- solve(data, ...)
                x$setinv(inv_matrix)
                inv_matrix   
        }

}
