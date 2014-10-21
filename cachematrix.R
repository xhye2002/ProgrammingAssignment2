#Compute the inverse of a matrix and cache the result
#If the content of the matrix are not changing, just look up
#the inverse in the cache rather than recomputed

## makeCacheMatrix creates a special "matrix": 
## return a list of functions containing:
## 1. function to set the matrix;
## 2. function to get the matrix; 
## 3. function to set the inverse;
## 4. function to get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## set matrix
        set <- function(y) {
                x <<- y
                inv<- NULL
        }
        ## get matrix
        get <- function() x
        
        ## set inverse
        setInverse <- function(inverse) inv <<- inverse
        
        ## get inverse
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## calculate the inverse of the special "matrix" 
## created by makeCacheMatrix function; 
## if it is calculated before, return the cached inverse;
## otherwise, calculate the inverse, save it to the cache 
## and return the value
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        ## if it is calcuated already, return the cache
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
  
        ## caculate the inverse
        data <- x$get()
        inv <-solve(data, ...)
        
        ## cache the inverse
        x$setInverse(inv)
        
        ## return the inverse
        inv
}
