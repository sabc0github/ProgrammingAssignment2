## makeCacheMatrix() will create matrix and will cache the matrix inverse.
## cacheSolve() will check and return if the matrix inverse is in cache, if not will calculate the matrix inverse.
## --------------------------------------------------------------------------------------------------------------

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## TEST DATA
## ---------
## 1. matrix(c(7, 0, -3, 2, 3, 4, 1, -1, -2), nrow = 3, ncol = 3)
## 2. matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
## 3. matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)

makeCacheMatrix <- function(inputMatrix = matrix()){
        
        ## Initialise matrixInverse variable 
        matrixInverse <- NULL
        
        ## sets user input Matrix
        ## Note: This function is not used in the assignment.
        setMatrix <- function(){
                matrixInverse <- NULL
        }
        
        ## gets user input Matrix
        getMatrix <- function(){
                inputMatrix
        }
        
        ## sets MatrixInverse
        setMatrixInverse <- function(tempMI){
                matrixInverse <<- tempMI
        }
        
        ## gets MatrixInverse 
        getMatrixInverse <- function(){
                matrixInverse
        }
        
        ## makeCacheMatrix() returns a list with all setters and getters
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)        
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(mcmFunctionHandle, ...) {
        
        ## get MatrixInverse from makeCacheMatrix()
        matrixInverse <- mcmFunctionHandle$getMatrixInverse()
        
        ## Check NULL over MatrixInverse
        if(!is.null(matrixInverse)){
                
                message("Getting Matrix Inverse from Cache...")
                
                ## get MatrixInverse from Cache
                matrixInverse <- mcmFunctionHandle$getMatrixInverse()
                
                ## return MatrixInverse
                return(matrixInverse)                
        }        
        
        message("Calculating Matrix Inverse...")
        
        ## get user input Matrix from makeCacheMatrix()
        inputMatrix <- mcmFunctionHandle$getMatrix()
        
        ## Calculate MatrixInverse
        tempMI <- solve(inputMatrix)
        
        ## set the MatrixInverse to Cache
        mcmFunctionHandle$setMatrixInverse(tempMI)
        
        ## get the MatrixInverse from Cache
        matrixInverse <- mcmFunctionHandle$getMatrixInverse()
        
        ## return MatrixInverse
        matrixInverse        
}
