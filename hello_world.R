print("hello world!") 

generate_data=function(n,p){
  covariates=matrix(rnorm(n*p,0,1),n,p)
  responses=rnorm(n)
  return (list(covariates=covariates,responses=responses))
}

model_select=function(covariates, responses,cutoff){
  res=lm(responses ~ covariates)
  res.sum=summary(res)
  p.val=res.sum$coefficients[2:(ncol(covariates)+1),"Pr(>|t|)"]
  valid_cov=covariates[,p.val<=cutoff]
  if(length(valid_cov)==0) return(c())
  new=lm(responses~valid_cov)
  new.sum=summary(new)
  new.pval=new.sum$coefficients[,"Pr(>|t|)"]
  return(new.pval)
}