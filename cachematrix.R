## These two functions together compute the inverse of a matrix
## and cache the value for future recall without a need to recompute

## makeCacheMatrix creates a list of functions and data for use by
## cacheSolve. The value returned is cached here in the parent 
## environment.

makeCacheMatrix <- function(x = matrix()) { ##Define makeCacheMatrix fn
                                            ## takes one arg, x, matrix
  m <- NULL #set value of m to NULL
  set <- function(y) {  ##Define set fn
    x <<- y             ##Map arg y to x
    m <<- NULL          ##Reset m to NULL
  }
  get <- function() x   ##Define the get fn, returns value of x
  setinverse <- function(inverse) m <<- inverse ##Define fn, set value of m
  getinverse <- function() m ##Define fn, returns m
  list(set = set, get = get, ## Writes these functions and value to list
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve returns the inverse of a matrix by calling function pieces
## out of the list created by makeCacheMatrix. On first run it will
## cache the inverse of a matrix and return the cached value for future
## runs on the same matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() #Checks for cached inverse in list
  if(!is.null(m)) { #M is tested for cached value
    message("getting cached data")
    return(m)   #If the cached value exists, returns the value
  }
  data <- x$get() #Gets data from list
  m <- solve(data, ...) #Performs inverse on data (matrix)
  x$setinverse(m) #Caches the inverse in the list for future recall
  m #Returns the inverse to command line
}
