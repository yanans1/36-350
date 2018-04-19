print("hello world!") 

generate_data=function(n,p){
  covariates=matrix(rnorm(n*p,0,1),n,p)
  responses=rnorm(n)
  return (list(covariates=covariates,responses=responses))
}