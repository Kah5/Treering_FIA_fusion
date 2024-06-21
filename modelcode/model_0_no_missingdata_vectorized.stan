data {

  int<lower=0> Nrow; //Ntrees
  int<lower=0> Ncol; //Nyears
  int<lower=0> Ndia; //total number of diameter measurements
  int<lower=0> Ninc; //total number of increment measurements
  int<lower=0> treedia[Ndia]; //index of z diameter measurements denoting the tree number
  int<lower=0> yeardia[Ndia]; //index of z diameter measurements denoting the year number
  real<lower = 0> z[Ndia];//diameter measurement vector
  real<lower = 0> y[Ninc]; // all the non-missing tree ring increments
  int<lower = 0> treeinc[Ninc];//index of y increment measurements denoting the tree number
  int<lower = 0> yearinc[Ninc];//index of y increment measurements denoting the year number

}
parameters {
    // variance parameters
   real<lower=0, upper =10> sigma_inc;
    real<lower=0, upper =10> sigma_add;
    real<lower=0, upper =10> sigma_dbh;
    
    //tree level random effect parameters
    real<lower=-10, upper =10> mutree;
    real alpha_TREE[Nrow];
    real<lower=1e-6> sigma_TREE;
   
   //setting inc and xinit as parameters
    matrix<lower=0> [Nrow, Ncol] inc;
    real<lower=0> xinit[Nrow];
    


}

transformed parameters{
   real<lower=1e-6> x[Nrow, Ncol]; //true diameter x is a transformed parameter

  for (i in 1:Nrow){

    x[i,1] = xinit[i] + inc[i, 1];

    for (t in 2:Ncol) {
      x[i, t] = x[i, t-1] + inc[i, t] ;
    }
}

}
model {
  
  //priors on tree-level random effect
  mutree ~ normal(0, 5);
  sigma_TREE ~ cauchy(0, 5);
  alpha_TREE ~ normal(mutree, sigma_TREE);
  
  
  //variance priors
  sigma_dbh ~ normal(1, 0.01); //normal(1,0.01) works well on base model
  sigma_inc ~ normal(0.035, 0.01); //based on SD of remeasured increments
  sigma_add ~ normal(0, 5); // wide (ish) prior given that max diameter inc is ~3.5
  
  //x initial prior-weakly informative --max tree size in data is 75
  xinit  ~ uniform(0, 75);
 
  
 // tree ring diameter increment process model
 for(i in 1:Nrow){//loop over the number of trees
           //} changed dec 20
       inc[i,1:Ncol] ~ lognormal( alpha_TREE[i] , sigma_add);
    
  }
  
  //tree ring increment data model
  for(j in 1:Ninc){ //loop over the number of increment measurements
     y[j] ~ normal(inc[treeinc[j], yearinc[j]], sigma_inc)T[0,];
     //increment data y corresponds to the true increment matrix through the tree and year increment indices
   }
 
 //diameter data model --moved from previous loop
   //  z[i,t] ~ normal(x[i,t], sigma_dbh);
   for(d in 1:Ndia){
     z[d] ~ normal(x[treedia[d], yeardia[d]], sigma_dbh);
     //diameter data z corresponds to the true diameter matrix through the tree and year diameter indices
   }
}
