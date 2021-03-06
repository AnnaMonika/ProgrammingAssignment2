#2 functions below tackle the problem of invertign large matrices and avoid re-doing it 
#especially if it was already once calculated


#this function 'makeCacheMatrix' creates a matrix instance represented by and accessed by 
# a list of fields given in a last row of makeCacheMatrix 

# set is a function which sets the matrix to be inverted
#get is the function which returns the matrix to be inverted
#setInv is the function which sets current inverse
#getInv is the function which returns the inverted matrix



makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#the function 'cacheSolve' returns the inverse of the given matrix
#but first checks if there is a need to invert it
#by making sure whether the getInv field of a matrix instance is null or not
#getInv field equal to null means that one needs to invert the matrix
#otherwise getInv keeps track of this inverted matrix that was calculated earlier
#inverse is calculated by using 'solve' function 
#after using 'solve' we use setInv to memorise the solution 

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}





