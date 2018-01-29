##2019-01-28 Cache matrix for inverse
## get , set , get inverse  & set inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  
  {
  m <- NULL
  set <- function (y)
    x <<- y 
    m <<- NULL
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    }


## Calculates the inverse for the given matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
