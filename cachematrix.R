
# makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
        
        # caches the value or holds a NULL -- Default -- if nothing is cached
        
        cache <- NULL
        
        # contain the a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }

        # returning the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the supplied argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # returning the list
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# Inversing the matrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
