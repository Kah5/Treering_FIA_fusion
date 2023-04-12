data {
    int<lower=0> Nrow;
    int<lower=0> Ncol;
    int<lower=0> Ncomp; // Number of non-missing values. 
    int<lower=0> Nmiss; // Number of missing values
    real dat_complete[Ncomp];   // Vector of non-missing values
    int ind_pres[Ncomp, 2];     // Matrix (row, col) of non-missing value indices
    int ind_miss[Nmiss, 2];     // Matrix (row, col) of missing value indices

    int<lower=0> Nrow_z;
    int<lower=0> Ncol_z;
    int<lower=0> Ncomp_z; // Number of non-missing values. 
    int<lower=0> Nmiss_z; // Number of missing values
    real dat_completez[Ncomp_z];   // Vector of non-missing values
    int ind_presz[Ncomp_z, 2];     // Matrix (row, col) of non-missing value indices
    int ind_missz[Nmiss_z, 2];     // Matrix (row, col) of missing value indices
    real tmaxAprMayJunscaled[Nrow, Ncol];
    real wateryrscaled[Nrow, Ncol];
    real MAP[Nrow];
    real MAT[Nrow];
    real SDI[Nrow, Ncol];
}
parameters {
    // Multivariate normal distribution parameters
    real mu;
    real<lower=0, upper =5> sigma_inc;
    real<lower=0, upper =5> sigma_add;
    real<lower=0, upper =5> sigma_dbh;
    // Vector containing "stochastic" nodes (for filling missing values
    real<lower=0> ymiss[Nmiss];
    real<lower=0> zmiss[Nmiss_z];
    real<lower=0> inc[Nrow, Ncol];
    real<lower=0> xinit[Nrow];
    real alpha_TREE[Nrow];
    real<lower=1e-6> sigma_TREE;
  
    real beta_YEAR[Ncol];
    real<lower=1e-6> sigma_YEAR;
    real betaX;
    real betaX_TREE[Nrow];
   // real betaX_TREE[Nrow];
    real betaTmax;
    real betaPrecip;
    real betaMAP;
    real betaMAT;
    real betaPrecip_MAP;
    real betaPrecip_MAT;
    real betaSDI;
    real betaPrecip_Tmax;
    real betaPrecip_SDI;
    real betaTmax_MAP;
    real betaTmax_MAT;
    real betaTmax_SDI;
    real betaX_SDI;
    real betaX_Precip;
    real betaX_Tmax;
    real betaX_MAP;
    real betaX_MAT;
    real betaMAP_MAT;
}

transformed parameters {
    real<lower=0> y[Nrow, Ncol];   // The "data" with interpolated missing values
    real<lower=0> z[Nrow, Ncol];   // The z "data" with interpolated missing values
    matrix<lower=0, upper = 100>[Nrow, Ncol] x;  // the estimated x values
   
    
     // Fill y with non-missing values 
    for(n in 1:Ncomp) {
        y[ind_pres[n,1], ind_pres[n,2]] = dat_complete[n];
    }
    // Fill the rest of y with missing value "parameters"
    for(n in 1:Nmiss){
        y[ind_miss[n, 1], ind_miss[n,2]] = ymiss[n];
    }
    
      // Fill x with non-missing values
     for(n in 1:Ncomp_z) {
         z[ind_presz[n,1], ind_presz[n,2]] = dat_completez[n];
     }
     // Fill the rest of y with missing value "parameters"
     for(n in 1:Nmiss_z){
         z[ind_missz[n, 1], ind_missz[n,2]] = zmiss[n];
     }
// estimate the true x value

 for(i in 1:Nrow){
   x[i,1] = xinit[i] + inc[i,1];
   
   for(t in 2:Ncol){ 
    x[i,t] = x[i,t-1] + inc[i,t];
    }
  }



#print("x =", x[1,1]);

  
}


model {
  
  for(i in 1:Nrow) {
    alpha_TREE[i] ~ normal(mu, sigma_TREE);
    betaX_TREE[i] ~ normal(betaX, sigma_TREE);
  }
  
  for(t in 1:Ncol) {
    beta_YEAR[t] ~ normal(0, sigma_YEAR);
  }


//betaX ~ normal(0, 10);

 betaTmax ~ normal(0, 10);
 betaPrecip ~ normal(0, 10);
 betaMAP ~ normal(0,10);
 betaMAT ~ normal(0,10);
 betaPrecip_MAP ~ normal(0,10);
 betaPrecip_MAT ~ normal(0,10);
 betaSDI ~ normal(0,10);
 betaPrecip_Tmax ~ normal(0,10);
 betaTmax_MAP ~ normal(0,10);
 betaTmax_MAT ~ normal(0,10);
 betaTmax_SDI ~ normal(0,10);
 betaPrecip_SDI ~ normal(0,10);
 betaX ~ normal(0, 10);
  betaX_Precip ~ normal(0, 10);
  betaX_Tmax ~ normal(0, 10);
  betaX_SDI ~ normal(0, 10);
  betaX_MAP ~ normal(0, 10);
  betaX_MAT ~ normal(0, 10);
  betaMAP_MAT ~ normal(0, 10);
 //sigma_dbh ~ uniform(0.0001, 5);
 // sigma_dbh ~ normal(0.1, 0.1); //normal(1,0.01) works well on base model
sigma_dbh ~ normal(1, 0.01); //normal(1,0.01) works well on base model
// sigma_dbh ~ normal(0.13, 0.1) //try this out 

 //sigma_dbh ~ normal(0.15, 0.08);
 //sigma_dbh ~ normal(0, 5);
 sigma_inc ~ normal(0.035, 0.01);
 sigma_add ~ uniform(0, 5);
 
//data & process model for increment
  for(i in 1:Nrow){
    
      // initial condition for diameter
    xinit[i]  ~ uniform(0, 75);
      y[i,1] ~ normal(inc[i,1], sigma_inc)T[0,];
        inc[i,1] ~ normal(mu, sigma_add);
      z[i,1] ~ normal(x[i,1], 5); //or normal(x[i,1], sigma_dbh);

     for(t in 2:Ncol){
       inc[i,t] ~ lognormal(alpha_TREE[i]+ beta_YEAR[t] + betaX_TREE[i]*x[i,t-1] + betaTmax*tmaxAprMayJunscaled[i,t]+ 
       betaPrecip*wateryrscaled[i,t]+ betaMAP*MAP[i] + betaMAT*MAT[i] + betaPrecip_MAT*wateryrscaled[i,t]*MAT[i] + 
       betaSDI*SDI[i,t] + betaPrecip_Tmax*wateryrscaled[i,t]*tmaxAprMayJunscaled[i,t] + 
       betaPrecip_SDI*wateryrscaled[i,t]*SDI[i,t] + betaTmax_SDI*tmaxAprMayJunscaled[i,t]*SDI[i,t] + 
       betaTmax_MAP*tmaxAprMayJunscaled[i,t]*MAP[i] + betaTmax_MAT*tmaxAprMayJunscaled[i,t]*MAT[i]+ 
       betaX_Tmax*x[i,t-1]*tmaxAprMayJunscaled[i,t]+ betaX_Precip*x[i,t-1]*wateryrscaled[i,t]+ 
       betaX_SDI*x[i,t-1]*SDI[i,t] + betaPrecip_MAP*wateryrscaled[i,t]*MAP[i] +
       betaMAP_MAT*MAP[i]*MAT[i] + betaX_MAP*x[i,t-1]*MAP[i] + betaX_MAT*x[i,t-1]*MAT[i], sigma_add);# + betaMAP*MAP[i] + betaMAT*MAT[i] + betaPrecip_MAT*wateryrscaled[i,t]*MAT[i] + betaSDI*SDI[i,t] + betaPrecip_Tmax*wateryrscaled[i,t]*tmaxAprMayJunscaled[i,t], sigma_add);
       //inc[i,t] ~ lognormal(alpha_TREE[i]+ beta_YEAR[t] , sigma_add);
       //+  + betaMAT*MAT[i] + betaPrecip_MAT*wateryrscaled[i,t]*MAT[i] + betaSDI*SDI[i,t] + betaPrecip_Tmax*wateryrscaled[i,t]*tmaxAprMayJunscaled[i,t], sigma_add);
       //inc[i,t] ~ lognormal(alpha_TREE[i]+ beta_YEAR[t] , sigma_add);
        y[i,t] ~ normal(inc[i,t], sigma_inc)T[0,];
        
         z[i,t] ~ normal(x[i,t], sigma_dbh);
         
    }
  }
  
#print("xinit = ", xinit)
}
