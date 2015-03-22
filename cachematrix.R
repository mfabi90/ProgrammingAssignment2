## Michele Fabi - R Programming Assigment 2 

## This page of code contains two main functions that
## compute matrix inversion by creating special 
## object endowed with several useful features. 
## Given a reference matrix to be inverted, the code provided in this document
## enables to carry out the following tasks:
## 1) setting and changing the matrix to be inverted;
## 2) inverting the matrix;
## 2) retriving the reference matrix and its inverse. 


## makeCacheMatrix creates a special kind of matrix object. In addition to store its numerical value, it can perform 
## matrix inversion, it can change the reference matrix and drop the one selected, and it can print both its numerical value
## and its inverse.
## The two variables defined in the environment of makeCacheMatrix are cMatrix and invCMatrix.
## cMatrix stores the matrix to be inverted, and makeCasheMatrix stores its inverse.
## all the functions defined within makeCacheMatrix perform elaborations on those matrices.

makeCacheMatrix <- function(cMatrix = matrix(nrow=0, ncol=0)) {
  # Setting empty matrix as default value for inverse.
  invCMatrix <- matrix(nrow=0, ncol=0)
  # setMatrix configures value for input matrix.
  setMatrix <- function(x = matrix(nrow=0, ncol=0)) {
    # Control on input type: if it is not matrix, an error is returned.  
    if (!is.matrix(x)) {stop("Input type must be matrix")}
    # Control on cMatrix: if it is already defined, an error occurs and requires
    # you to drop the previous value before inserting a new one.
    if (ncol(cMatrix) == 0)  {
      cMatrix <<- x
    } else if (ncol(cMatrix) != 0) {stop("CacheMatrix already defined")}  
  }
  
  # getMatrix retrieves the value of the input matrix.
  getMatrix <- function() cMatrix
  
  # solveMatrix computes the inverse of the input matrix and stores it into invCMatrix using the  <<-  operator.
  solveMatrix <- function() {
    invCMatrix <<- solve(cMatrix)
  }
  # getMatrix retrieves the value of the inverse matrix.
  getInvMatrix <- function() invCMatrix
  
  dropMatrix <- function() {
    # Control on cMatrix: if it is not defined, an error is printed because there is nothing to drop.
    if (!cMatrix)  {
      stop("CacheMatrix not defined")
    }
    else if (cMatrix) {
      cMatrix <<- matrix(nrow=0, ncol=0)
      print("CacheMatrix dropped")
    }
  }
  
  # Stores all the features of makeCacheMatrix in a list, so that when you have an object (mCM) of type
  # makeCacheMatrix, you can access it using mCM$function().
  list(dropMatrix = dropMatrix, solveMatrix = solveMatrix,
       setMatrix = setMatrix, getMatrix = getMatrix,
       getInvMatrix = getInvMatrix)
  
}


## cacheSolve takes as input an instance of makeCacheMatrix. It retrieves the inverse matrix if it is already computed;
## otherwise, it computes the inverse and then prints its value.

cacheSolve <- function(x, ...) {
  ## check if the inverse has been computed: if this is the case, its value is printed;
  ## if not, the function computes and prints its value.
  if (ncol(x$getInvMatrix()) != 0) {
    print(x$getInvMatrix())
  } 
  else {
    x$solveMatrix()
    print(x$getInvMatrix())
  }
  
}