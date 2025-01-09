## Code developed for Menzies et al. "Bayesian methods for calibrating
## health policy models: a tutorial" Pharmacoeconomics.
##############################################
################### MODEL ####################
##############################################
# This function estimates outcomes describing epidemiology of a hypothetical disease
# as well as outcomes (life-years, costs) for estimating cost-effectiveness of a policy
# to expand treatment access to individual's with early stage disease.
mod <- function(par_vector,project_future=FALSE) {
  # par_vector: a vector of model parameters
  # project_future: TRUE/FALSE, whether to project outcomes for policy comparison
  pop_size <- 1e6 # population size hard-coded as 1 million
  mu_b <- 0.015 # background mortality rate hard-coded as 0.015
  mu_e <- par_vector[1] # cause-specific mortality with early-stage disease
  mu_l <- par_vector[2] # cause-specific mortality with late-stage disease
  mu_t <- par_vector[3] # cause-specific mortality on treatment
  p <- par_vector[4] # transition rate from early to late-stage disease
  r_l <- r_e <- par_vector[5] # rate of treatment uptake
  rho <- par_vector[6] # effective contact rate
  b <- par_vector[7] # fraction of population in at-risk group
  c <- par_vector[8] # annual cost of treatment
  ######## Prepare to run model ###################
  n_yrs <- if(project_future) { 51 } else { 30 }
  # no. years to simulate (30 to present, 51 for 20 year analytic horizon)
  sim <- if(project_future) { 1:2 } else { 1 }
  # which scenarios to simulate: 1=base case, 2=expanded treatment access
  v_mu <- c(0,0,mu_e,mu_l,mu_t)+mu_b
  # vector of mortality rates
  births <- pop_size*mu_b*c(1-b,b)
  # calculate birth rate for equilibrium population before epidemic
  init_pop <- pop_size*c(1-b,b-0.001,0.001,0,0,0)
  # creates starting vector for population
  trace <- matrix(NA,12*n_yrs,6)
  # creates a table to store simulation trace
  colnames(trace) <- c("N","S","E","L","T","D")
  results <- list()
  # creates a list to store results
  ######## Run model ###################
  for(s in sim) {
    P0 <- P1 <- init_pop
    for(m in 1:(12*n_yrs)) {
#browser()
      lambda <- rho*sum(P0[3:4])/sum(P0[2:5]) # calculates force of infection
      P1[1:2] <- P1[1:2]+births/12 # births
      P1[-6] <- P1[-6]-P0[-6]*v_mu/12 # deaths: N, S, E, L, T, to D
      P1[6] <- P1[6]+sum(P0[-6]*v_mu/12) # deaths:N, S, E, L, T, to D
      P1[2] <- P1[2]-P0[2]*lambda/12 # infection: S to E
      P1[3] <- P1[3]+P0[2]*lambda/12 # infection: S to E
      P1[3] <- P1[3]-P0[3]*p/12 # progression: E to L
      P1[4] <- P1[4]+P0[3]*p/12 # progression: E to L
      P1[4] <- P1[4]-P0[4]*r_l/12 # treatment uptake: L to T
      P1[5] <- P1[5]+P0[4]*r_l/12 # treatment uptake: L to T
      if(s==2 & m>(12*30)) {
#browser()
        P1[3] <- P1[3]-P0[3]*r_e/12 # treatment uptake: E to T (scen. 2)
        P1[5] <- P1[5]+P0[3]*r_e/12 # treatment uptake: E to T (scen. 2)
      }
      trace[m,] <- P0 <- P1 # fill trace, reset pop vectors
    }
    results[[s]] <- trace # save results for each scenario
  }
  ######## Report results ###################
  if(project_future==FALSE) {
    ## Return calibration metrics, if project_future = FALSE
    return(list(trace0 = results[[1]],
                # Trace without expanded treatment access
                prev = (rowSums(trace[,3:5])/rowSums(trace[,1:5]))[c(10,20,30)*12],
                # Prevalence at 10,20,30 years
                surv = 1/(v_mu[3]+p)+ p/(v_mu[3]+p)*(1/v_mu[4]),
                # HIV survival without treatment
                tx = trace[30*12,5]
                # Treatment volume at 30 years
    ) )
  } else {
    ## Policy projections for CE analysis, if project_future = TRUE
    return(list(trace0 = results[[1]],
                # Trace without expanded treatment access
                trace1 = results[[2]],
                # Trace with expanded treatment access
                inc_LY = sum(results[[2]][(30*12+1):(51*12),-6]-
                               results[[1]][(30*12+1):(51*12),-6])/12,
                # incr. LY lived with expanded tx
                inc_cost = sum(results[[2]][(30*12+1):(51*12),5]-
                                 results[[1]][(30*12+1):(51*12),5])*c/12
                # incr. cost with expanded tx
    ) )
  }
}
# Test it
save<-mod(rep(0.5,8),project_future=F) # works
save<-mod(rep(0.5,8),project_future=T) # works
