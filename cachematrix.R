## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL ##set m default value to NULL (if cacheSolve has not previously been used)
  set<-function(y){ ##set the value of the matrix
    x<<-y ## caches the inputted matrix
    m<<-NULL ##set m default value to NULL
  }
  get<-function() x
  setmatriu<-function(solve) m<<- solve
  getmatriu<-function() m
  ## Create a list for the functions used
  list(set=set, get=get,
       setmatriu=setmatriu,
       getmatriu=getmatriu)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatriu() ## get inverse matrix if it was previously calculated
  if(!is.null(m)){ ## check if cacheSolve has been run before
    message("getting cached data")
    return(m)
  }
  matriu<-x$get() ##load input matrix
  m<-solve(matriu, ...) ##calculate inverse matrix
  x$setmatriu(m) ##cache the calculated inverse matrix
  m ##return calculated inverse matrix
}
