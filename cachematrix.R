#This code can save power computation by caching the inverse of a matrix.

#makeCacheMatrix allows to enter a new matrix into memory. It is assumed that
#the input is a square matrix and it's not singular.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#cacheSolve searches into cache for inverse of the previous matrix. If not,
#it calculates the inverse by using the 'solve' function. 

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i) }
  mat <- x$get()
  i <- solve(mat)
  x$setinv(i)
  i
}