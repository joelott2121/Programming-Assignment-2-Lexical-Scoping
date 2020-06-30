##This is my solution for the Programming Assignment 2 (Lexical Scoping) for Week 3
##As required in the task this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x = matrix()) {
  
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
    
  }
  
  get<-function()x
  setInverse<-function(inverse) j<<-inverse
  getInverse<-function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

##Above I created a Matrix that can cache its inverse.
##Now this new function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve<-function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
    
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$setInverse(j)
  j
  
}

## I hope this is understandable and readable for you. I am looking forward to the feedback. 
