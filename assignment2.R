# makeCacheMatrix will return a list containing functions to set the values of a square matrix, get the values of a 
# square matrix, set the values of the inverse of a square matrix and get the values of the inverse of a square matrix. 

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

# cacheSolve will calculate the inverse of a the list created with makeCachematrix. It will first check to see if the 
# inverse has already been calculated. If so, it will get the mean from the cache and skip the calculation of the inverse.
# Otherwise, it will calculate the inverse of a square matrix and sets the value of the inverse in the cache via setinverse 
# function. 

cacheSolve <- function(x, ...){
        inv = x$getinverse()
        if(!is.null(inv) & !identical(inv, x$get())){
                return(inv)
        }
        data = x$get()
        inv = solve(data, ...)
        x$setinverse(inv)
        return(inv)
}



