## Return the inverse of matrix after checking if it's not already in the cache

## We are saving the matrix and returning a list of function to set, get, set inverse and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setmatinv <- function(matinv) minv <<- matinv #setting matrix inverse so it is cached
  getmatinv <- function() minv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## Checks to see if inverse exists and if not creates the inverse and caches it

cacheSolve <- function(x, ...) {
       
  minvf2 <- x$getmatinv() #get inverse matrix if it exists
  if(!is.null(minvf2)) {
    message("getting cached data")
    return(minvf2)
  }
  data <- x$get()
  minvf2 <- solve(data, ...) # Calculates the inverse matrix
  x$setmatinv(minvf2) # caches the inverse matrix for future use
  minvf2 
}
