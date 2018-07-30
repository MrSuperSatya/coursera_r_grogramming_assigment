
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) inversion <<- solve
  getinversion <- function() inversion
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  inversion <- x$getinversion()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setinversion(inversion)
  inversion
}

# Example:
my_matrix = matrix(c(2,1,3,2), nrow=2, ncol=2)
cacheSolve(makeCacheMatrix(my_matrix))
