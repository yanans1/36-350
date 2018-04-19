print("hello world!") 

generate_data=function(n,p){
  covariates=matrix(rnorm(n*p,0,1),n,p)
  responses=rnorm(n)
  return (list(covariates=covariates,responses=responses))
}

model_select=function(covariates, responses,cutoff){
  fit=lm(responses ~ covariates)
  fitsum=summary(fit)
  pvalue=fitsum$coefficients[2:(ncol(covariates)+1),"Pr(>|t|)"]
  fit.cov=covariates[,pvalue<=cutoff]
  if(length(fit.cov)==0) return(c())
  fit.new=lm(responses~fit.cov)
  fitsum.new=summary(fit.new)
  pvalue.new=fitsum.new$coefficients[,"Pr(>|t|)"]
  return(pvalue.new)
}

run_simulation=function(n_trials, n, p, cutoff){
  p.vals=c()
  for (i in 1:n_trials){
    res=generate_data(n,p)
    covariates=res$covariates
    responses=res$responses
    pval=model_select(covariates,responses, cutoff)
    p.vals=c(p.vals,pval)
  }
  save(p.vals,file="pvalues") 
}

make_plot=function(datapath){
  data=load(datapath)
  print(data)
  hist(data)
}