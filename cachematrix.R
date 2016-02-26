
## This function creates a matrix object and caches its inverse
makeCacheMatrix <- function(mat = matrix()) {
  mt <- NULL
  
  set <- function(mt1){
      mat <<- mt1
      mt <<- NULL
  }
  
  get <- function() mat
  
  setmat <- function(solve) mt <<- solve 
  getmat <- function() mt 
  list(set = set, get = get, setmat = setmat, getmat = getmat)

}


## This function computes the inverse of the matrix object returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
  mt <- x$getmat()
  
  if(!is.null(mt)) {
    message("getting cached data") ## Return cached matrix
    return(mt)
  }
  data <- x$get()
  mt <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setmat(mt)
  mt
}
