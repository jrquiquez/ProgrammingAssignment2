## Put comments here that give an overall description of what your
## functions do

## This function really is a list of 4 functions to operate cached matrix.
## one function is to set values, other to get values for both (cached and inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL ##this initialize mat
  set<-function(y){
    x<<-y ##this gives x a value in the parent enviroment. 
    mat<<-NULL ##this gives mat a value in the parent environment. 
  }
  get<-function() x 
  setmatrix<-function(solve) mat<<- solve
  getmatrix<-function() mat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## this function creates a inverse of a "cached" Matrix. If this calculation
## has been done before it gives the cached answer otherwise it calculates it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat<-x$getmatrix()
  ##the next if checks if the matrix has been calculated before to skip calculation
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  #if the inverse of x has not been calculated it calculated now. 
  matrix<-x$get()
  mat<-solve(matrix, ...)
  #this makes sure to save the matrix in the cache. 
  x$setmatrix(mat)
  mat
}


