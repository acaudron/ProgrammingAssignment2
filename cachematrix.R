
## makeCacheMatrix: create a new special matrix which can store its inverse
## usage: cx <- function(x) with x a invertible matrix
## cx$get(): get the original matrix element
## cx$set(newMatrix): change the matrix, it will put to NULL the inverse matrix
## cx$getInv(): return the inverse matrix, NULL if not yet computed
## cx$setInv(...): to compute the inverse matrix and store it, the ... refer to the possible arguments of the solve function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  ## setInv function directly compute the solve function to avoid user to set a random matrix
  ## can take as argument the argument of the solve function
  setInv <- function(...) inv <<- solve(x, ...)
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve: from a cacheMatrix element, extract from the cache or compute the inverse of the matrix and return it
## if the function compute the inverse matrix then it will also store it in the input element x 
## usage: inverseX <- cacheSolve(x, ...) 
## where x is a matrix object defined with the function makeCacheMatrix and the ... refer to the possible arguments of the solve function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) return(inv)
  ## if the inverse was not yet computed or if the matrix changed
  ## then "inv" is NULL so:
  x$setInv(...)
  x$getInv()
}

