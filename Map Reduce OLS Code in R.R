
#Map Reduce OLS Code in R

# load required libraries
library(rhdfs)
library(rmr)

# define function rH.lm
rH.lm <- function(input, output = NULL, informat = csvinformat){
  # extract predictors and dependents from each row
  # convert information from row into components
  # of square matrix and return this matrix 
  # passing on input keys
  map <- function(k, v){
    if(!is.null(v)){
      # extract predictors and bind 1 to the right
      x <- cbind(rep(1,nrow(v)), v[,-1])
      # extract dependent
      y <- v[,1]
      # compute outer of x and x for m predictors 
      # this should yield a (m + 1)x(m + 1) matrix,
      # multiply modified x with dependent
      # this should yield a vector of length (m + 1),
      # cbind these two and return as value
      # alongwith output key as input key (NULL)
      keyval(k, as.matrix(cbind(y%*%t(x), x%*%t(x))))
    }
  }
  # define reducer as function
  # sum up all the row-wise computed matrices here
  # can also used as combiner (big savings!)
  # so should be commutative and associative
  # returns key "betas" and summed matrix as value
  reduce <- function(k, vv){
    # use Reduce in reduce (heh) to sum all the matrices in the list
    keyval(k, Reduce("+", lapply(vv, as.matrix)))
  }
  # define as a map-reduce job and
  # get matrix from reduced output
  mapredcall <- mapreduce(input = input, 
                          output = output, 
                          #map = to.map(identity),
                          map = map,
                          #reduce = to.reduce(identity),
                          reduce = reduce,
                          #combine = TRUE,
                          input.format = csv.input.format(sep = ","))
  
  mat <-as.matrix(from.dfs(mapredcall())[[1]]$val)
  # extract t(x)*x, t(x)*y and
  # use solve to get betas
  return(as.vector(solve(mat[,-1], mat[,1])))
}

z <- rH.lm('/user/hadoop/regData2H')

# read in same data as a dataframe
testDF <- read.csv(file = "testInputData.csv", head = TRUE, sep = ",")

# use lm to fit the data
testReg <- lm(testSales ~ dummy_cont + dummy_flag1 + dummy_flag2 + dummy_spend1 + dummy_spend2 + dummy_spend3 + dummy_spend4, data = testDF)

# compare betas from RHadoopReg_v0.2 and lm
zbetaVal <- z - testReg$coefficients

#rHdp.lm('/user/hadoop/smallTest','/user/hadoop/rTestRegOut')
#rHdp.lm('/user/hadoop/regData','/user/hadoop/rTestRegOut')
