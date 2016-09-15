# Define the functions

inv_logit <- function(x)
{ # Return the inverse logit value

  return( exp(x) / (1+exp(x)) )
} # end function


get_econ_variables <- function( scenario )
{ # Get the economic scenario values
  # These are the CCAR values in 2015Q4-2016Q4
  
  if( scenario == "none" )
  { # Just freeze the economic variables
    mortgage_rate	<- rep(3.9, 5)
    unemployment	<- rep(1, 5)
    housing		<- rep(1, 5)
    yr_appreciation	<- rep(1, 5)
  } else if( scenario == "baseline" )
  {
    mortgage_rate	<- c( 3.9, 4.1, 4.2, 4.3, 4.5 )
    unemployment	<- c( 5.0, 4.9, 4.8, 4.7, 4.6 )
    housing		<- c( 183.1, 184.0, 185.2, 186.3, 187.5 )
    yr_appreciation	<- housing / c( 174.5, 177.3, 179.4, 181.7, 183.1 )
  } else if( scenario == "adverse" )
  {  
    mortgage_rate	<- c( 3.9, 3.5, 3.8, 4.0, 4.2 )
    unemployment	<- c( 5.0, 5.5, 6.1, 6.7, 7.1 )
    housing		<- c( 183.1, 181.2, 178.7, 175.9, 172.8 )
    yr_appreciation	<- housing / c( 174.5, 177.3, 179.4, 181.7, 183.1 )
  } else if( scenario == "severly_adverse" )
  {
    mortgage_rate	<- c( 3.9, 3.2, 3.7, 3.9, 4.1 )
    unemployment	<- c( 5.0, 6.0, 7.2, 8.3, 9.1 )
    housing		<- c( 183.1, 178.8, 173.5, 167.4, 160.8 )
    yr_appreciation	<- housing / c( 174.5, 177.3, 179.4, 181.7, 183.1 )
  } # end if

  # Interpolate the quarterly values to be monthly
  m 			<- c( 0, 3, 6, 9, 12 )	# indices for the months
  mortgage_rate_inter	<- approx( x=m, y=mortgage_rate, xout=0:12, method="linear" )$y
  unemployment_inter	<- approx( x=m, y=unemployment, xout=0:12, method="linear" )$y
  housing_inter		<- approx( x=m, y=housing, xout=0:12, method="linear" )$y
  yr_appreciation_inter <- approx( x=m, y=yr_appreciation, xout=0:12, method="linear" )$y

  # Get the change in economic variables
  mortgage_rate_ch	<- diff(mortgage_rate_inter) / mortgage_rate_inter[1:(length(mortgage_rate_inter)-1)]
  unemployment_ch	<- diff(unemployment_inter) / unemployment_inter[1:(length(unemployment_inter)-1)]
  housing_ch		<- diff(housing_inter) / housing_inter[1:(length(housing_inter)-1)]
  yr_appreciation_ch	<- yr_appreciation_inter[-1]

  return( list( mortgage_rate=mortgage_rate, mortgage_rate_ch=mortgage_rate_ch, unemployment_ch=unemployment_ch,
		housing_ch=housing_ch, yr_appreciation_ch=yr_appreciation_ch ) )

} # end function


pre_compute_LGD_parms <- function( fitted_mod, val_data, scenario )
{ # Pre-compute the LGD parameters
  # NOTE: ALLOWS THE ECONOMIC VARIABLES TO CHANGE, BUT FIXES CUM_DELINQUENT

  # Copy the data to not change values by reference
  val_data		<- copy(val_data)

  # Store the economic variables  
  econ_variables	<- get_econ_variables( scenario )
  mortgage_rate_ch	<- econ_variables$mortgage_rate_ch
  unemployment_ch	<- econ_variables$unemployment_ch
  housing_ch		<- econ_variables$housing_ch
  yr_appreciation_ch	<- econ_variables$yr_appreciation_ch
  
  LGD_parms	<- vector( mode="list", length=12 )
  for( t in 1:12 )
  {
    print( paste( "Computing LGD Parameters for Month", t ) )
    # Estimate the Beta Inflated distribution parameter values for the test data
    mu		<- inv_logit( predict(fitted_mod, what="mu",    newdata=val_data[,.SD, .SDcols=setdiff(colnames(val_data), "LOAN_STATUS")] ) )		# mu uses logit link
    sigma	<- inv_logit( predict(fitted_mod, what="sigma", newdata=val_data[,.SD, .SDcols=setdiff(colnames(val_data), "LOAN_STATUS")] ) )		# sigma uses logit link
    nu		<- exp( predict(fitted_mod, what="nu",          newdata=val_data[,.SD, .SDcols=setdiff(colnames(val_data), "LOAN_STATUS")] ) )		# nu uses log link
    tau		<- exp( predict(fitted_mod, what="tau", 	newdata=val_data[,.SD, .SDcols=setdiff(colnames(val_data), "LOAN_STATUS")] ) )		# tau uses log link

    # Make sure all of the parameters are in range
    eps		<- 1e-8
    mu		<- pmin( pmax( mu,    eps ), 1-eps )		# 0 < mu < 1
    sigma	<- pmin( pmax( sigma, eps ), 1-eps )		# 0 < sigma < 1
    sigma[is.na(sigma)] <- eps
    nu		<- pmax( nu, eps )				# nu > 0
    tau		<- pmax( tau, eps )				# tau > 0

    # Store the LGD parameters
    LGD_parms[[t]]	<- data.table( mu=mu, sigma=sigma, nu=nu, tau=tau )

    # Update the economic variables
    val_data[, Loan.Age:= ifelse( Loan.Age == 0, 0, Loan.Age + 1 ) ]		# Note: if the loan age is 0, that means its missing
    #val_data[, Cum_Delinquent:= ifelse( new_state_d[, LOAN_STATUS %in% as.character(1:4) ], Cum_Delinquent + 1, Cum_Delinquent ) ]
    val_data[, Unemployment:= Unemployment * ( 1 + unemployment_ch[t] ) ]
    val_data[, CUR_VAL:= CUR_VAL * ( 1 + housing_ch[t] ) ]
    val_data[, Cum_Underwater:= ifelse( CUR_VAL < LAST_UPB, Cum_Underwater + 1, Cum_Underwater ) ]
    val_data[, CUR_LTV:=  ifelse( CUR_VAL == 0, 0, LAST_UPB / CUR_VAL ) ]
    val_data[, Exp_Loss:= ifelse( CUR_LTV == 0, 0, 1 - 1 / CUR_LTV ) ]
    val_data[, CUR_1Yr_Appreciation:= yr_appreciation_ch[t] ]

  } # end for

  # Return the LGD parameters
  return( LGD_parms )

} # end function


pre_sim_LGDs <- function( LGD_parameters )
{ # Pre-simulate the LGDs for each month

  t		<- 1
  LGDs		<- data.table(	t1=sapply( 1:nrow(LGD_parameters[[t]]), 
					function(j) with( LGD_parameters[[t]], rBEINF( n=1, mu=mu[j], sigma=sigma[j], nu=nu[j], tau=tau[j] ) ) )
			     )
  for( t in 2:12 )
  {
    LGDs[, paste( "t", t, sep="" ) := sapply( 1:nrow(LGD_parameters[[t]]), 
					function(j) with( LGD_parameters[[t]], rBEINF( n=1, mu=mu[j], sigma=sigma[j], nu=nu[j], tau=tau[j] ) ) )
				 	]
  } # end for

  # Return the simulated LGDs
  return( LGDs )

} # end function


#get_1yr_loss <- function( seed_num, model, prediction, val.hex, val_data, LGD_parameters, month_factor, states, scenario, Result_location )
get_1yr_loss <- function( model, prediction, val.hex, val_data, LGD_parameters, month_factor, states, scenario, Result_location )
{ # Simulate the 1yr loss for each of the loans
  # Note: prediction is the 1 month prediction data
  #   model is the state transition model
  #   val.hex is the h2o data for the state transition model predictions
  
  # Set the seed number
  #set.seed(seed_num)
rand_name	<- round( runif(1)*1000 )

  # Store the economic variables 
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Econ Variables - ", rand_name, ".txt", sep="" ) ) )
  econ_variables	<- get_econ_variables( scenario )
  mortgage_rate		<- econ_variables$mortgage_rate
  mortgage_rate_ch	<- econ_variables$mortgage_rate_ch
  unemployment_ch	<- econ_variables$unemployment_ch
  housing_ch		<- econ_variables$housing_ch
  yr_appreciation_ch	<- econ_variables$yr_appreciation_ch

  # Pre-simulate the LGDs
  #LGDs			<- pre_sim_LGDs( LGD_parameters )

  new_predict		<- prediction
  current_state		<- val.hex$LOAN_STATUS
  current_state_d	<- val_data[, .SD, .SDcols="LOAN_STATUS" ]
  dat			<- val.hex
  loss<- data.table( Loss=rep(NA, nrow(val_data) ) )
  loss[, c("Modified","Delinquent"):= list(NA, 0) ]
  for( t in 2:13 )	# do for 1 year; Note that the 1st month is predicted above (input), but need 12 LGD predictions
  {
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Prediction ", t, " - ", rand_name, ".txt", sep="" ) ) )
    # Get the cumulative transition probabilities
    cumsum_prob		<- vector( mode="list", length=ncol(prediction) )
    cumsum_prob[[1]]	<- apply( new_predict, 1, function(x) { sum( x[,1:1] ) } )
    cumsum_prob[[2]]	<- apply( new_predict, 1, function(x) { sum( x[,1:2] ) } )
    cumsum_prob[[3]]	<- apply( new_predict, 1, function(x) { sum( x[,1:3] ) } )
    cumsum_prob[[4]]	<- apply( new_predict, 1, function(x) { sum( x[,1:4] ) } )
    cumsum_prob[[5]]	<- apply( new_predict, 1, function(x) { sum( x[,1:5] ) } )
    cumsum_prob[[6]]	<- apply( new_predict, 1, function(x) { sum( x[,1:6] ) } )
    cumsum_prob[[7]]	<- apply( new_predict, 1, function(x) { sum( x[,1:7] ) } )
    cumsum_prob[[8]]	<- apply( new_predict, 1, function(x) { sum( x[,1:8] ) } )
    #cumsum_prob[[9]]	<- apply( new_predict, 1, function(x) { sum( x[,1:9] ) } )
    cumsum_prob		<- h2o.cbind( cumsum_prob )
    colnames(cumsum_prob) <- colnames(new_predict)

    # Get the simulated state
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "New State ", t, " - ", rand_name, ".txt", sep="" ) ) )
    u			<- h2o.runif( dat$LOAN_ID )
    new_state		<- as.factor( apply( cumsum_prob < u, 1, function(x) { sum(x) + 1 } ) )
    new_state		<- h2o.setLevels( new_state, states[ as.numeric( h2o.levels(new_state) ) ] )
    colnames(new_state) <- "LOAN_STATUS"

    # Ensure absorbing states remain absorbing
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Absorbing State ", t, " - ", rand_name, ".txt", sep="" ) ) )
    state_levels	<- h2o.levels(new_state)
    new_state		<- ifelse( current_state %in% c( as.character(1:4), 'C' ), new_state, current_state )
    new_state		<- as.factor( new_state + 1 )
    new_state		<- h2o.setLevels( new_state, state_levels[ as.numeric( h2o.levels(new_state) ) ] )
    colnames(new_state) <- "LOAN_STATUS"

    # Determine whether there was a new foreclosure, modification, or delinquency
    # Note: if is.na(loss$Loss) == FALSE, then a loss was simulated previously for the loan (only simulate once)
    # Note: if is.na(loss$Modified) == FALSE, then a modifications was simulated previously for the loan (only simulate once)
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Is State ", t, " - ", rand_name, ".txt", sep="" ) ) )
    new_state_d		<- as.data.table( new_state )
    is_newly_foreclosed	<- ( new_state_d == 'F' ) & is.na(loss$Loss)
    is_newly_modified	<- ( new_state_d == 'Y' ) & is.na(loss$Modified)
    is_delinquent	<- new_state_d[, ifelse( ( LOAN_STATUS %in% as.character(1:4) ) & ( current_state_d == 'C' ), TRUE,
					 ifelse( ( LOAN_STATUS %in% as.character(2:4) ) & ( current_state_d == '1' ), TRUE,
 					 ifelse( ( LOAN_STATUS %in% as.character(3:4) ) & ( current_state_d == '2' ), TRUE,
					 ( current_state_d == '3' ) & ( new_state_d =='4' ) ) ) ) ]

    # If a loan is modified, then calculate its cost (the change in interest payments)
    # NOTE: ASSUMES THERE IS NO PRINCIPAL FOREGIVENESS
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Loss ", t, " - ", rand_name, ".txt", sep="" ) ) )
    loss[, Modified:= ifelse( is_newly_modified, pmax( ( 13 - t + 1 ) * ( (val_data$LAST_RT - mortgage_rate[t-1]) / 1200 ) * val_data$LAST_UPB, 0), Modified ) ]
#C_modir_cost = ( 13 - t + 1 ) * ( (LAST_RT - mortgage_rate[t-1]) / 1200 ) * LAST_UPB )
#C_modfb_cost = ( 13 - t + 1 ) * ( -1*(mortgage_rate[t-1] / 1200) * NON_INT_UPB )
#modfg_cost   = -1 * min(PRIN_FORG_UPB, 0, na.rm = TRUE)
#MODTOT_COST  = modfg_cost + C_modir_cost+ C_modfb_cost

    # If a loan has been delinquent add the missed interest payment
    loss[, Delinquent:= Delinquent + ifelse( is_delinquent, val_data$LAST_RT / 1200 * val_data$LAST_UPB, 0 ) ]

    # If a property is foreclosed, then calculate the LGD
    if( any( is_newly_foreclosed ) )
    {
      # Simulate an LGD for foreclosed properties
      #loss[, Loss:= ifelse( is_newly_foreclosed, LGDs[,t-1] * val_data$LAST_UPB, Loss ) ]
      loss[, Loss:= ifelse( is_newly_foreclosed, with( LGD_parameters[[t-1]], rBEINF( n=1, mu=mu, sigma=sigma, nu=nu, tau=tau ) ) * val_data$LAST_UPB, Loss ) ]

    } # end if

if( t <= 12 )
{ # NOTE: DO NOT NEED TO UPDATE ON THE FINAL LOOP
  #	  THE FINAL LOOP IS JUST NEEDED FOR THE LOSS CALCULATION

    # Update the loan state
    dat[, "LOAN_STATUS" ] <- new_state

    # Update the other fields (delinquency, loan age, unemployment, current home value, LTV, etc)
    # Note: Assumes the data starts with the 1st month (for Obs_Month)
    Obs_Month 		<- month_factor[[t]]
    colnames(Obs_Month)	<- "Obs_Month"
    Loan.Age		<- ifelse( dat$Loan.Age_NA == '1', 0, dat$Loan.Age + 1 )
    colnames(Loan.Age)	<- "Loan.Age"
    Cum_Delinquent	<- ifelse( dat$Cum_Delinquent_NA == '1', 0, ifelse( new_state %in% as.character(1:4), dat$Cum_Delinquent + 1, dat$Cum_Delinquent ) )
    colnames(Cum_Delinquent) <- "Cum_Delinquent"
    Unemployment	<- dat$Unemployment * ( 1 + unemployment_ch[t-1] )
    colnames(Unemployment) <- "Unemployment"
    CUR_VAL		<- dat$CUR_VAL * ( 1 + housing_ch[t-1] )
    colnames(CUR_VAL)	<- "CUR_VAL"
    Cum_Underwater	<- ifelse( dat$Cum_Underwater_NA == '1', 0, ifelse( CUR_VAL < dat$LAST_UPB, dat$Cum_Underwater + 1, dat$Cum_Underwater ) )
    colnames(Cum_Underwater) <- "Cum_Underwater"
    CUR_RT_Ratio	<- dat$CUR_RT_Ratio / ( 1 + mortgage_rate_ch[t-1] )
    colnames(CUR_RT_Ratio) <- "CUR_RT_Ratio"
    Ref_Burnout		<- ifelse( dat$Ref_Burnout_NA == '1', 0, CUR_RT_Ratio * ( dat$Ref_Burnout / dat$CUR_RT_Ratio + ifelse( CUR_RT_Ratio - 1 - 0.1 > 0, CUR_RT_Ratio - 1 - 0.1, 0 ) ) )
    colnames(Ref_Burnout) <- "Ref_Burnout"
    CUR_1Yr_Appreciation  <- h2o.rep_len( yr_appreciation_ch[t-1], nrow(dat) )
    colnames(CUR_1Yr_Appreciation) <- "CUR_1Yr_Appreciation"
    dat[, which( colnames(dat) %in% c( "Obs_Month", "Loan.Age", "Cum_Delinquent", 
				       "Unemployment", "CUR_VAL", "Cum_Underwater",
				       "CUR_RT_Ratio", "Ref_Burnout", "CUR_1Yr_Appreciation" ) ) ] 		<- NULL
    dat			<- h2o.cbind( dat, Obs_Month, Loan.Age, Cum_Delinquent, 
				           Unemployment, CUR_VAL, Cum_Underwater,
					   CUR_RT_Ratio, Ref_Burnout, CUR_1Yr_Appreciation )
    

    # Update predictions and current state for next month
    new_predict 	<- h2o.predict( model, dat )
    new_predict		<- new_predict[,2:ncol(new_predict)]
    current_state	<- new_state
    current_state_d	<- new_state_d

} # end if

  } # end inner for

  # Remove the unneeded h2o ids
#  h2o.rm( dat ); h2o.rm( new_predict ); h2o.rm( Obs_Month ); h2o.rm( Loan.Age ); h2o.rm( Cum_Delinquent );
#  h2o.rm( Unemployment ); h2o.rm( CUR_VAL ); h2o.rm( Cum_Underwater ); h2o.rm( CUR_RT_Ratio );
#  h2o.rm( Ref_Burnout ); h2o.rm( CUR_1Yr_Appreciation ); h2o.rm( cumsum_prob ); h2o.rm( u );
#  h2o.rm( current_state ); h2o.rm( new_state )
  rm( current_state_d ); gc()

  # Return the 1yr simulated loss
write.table(rand_name, file.path( Result_location, "ParallelTest", paste( "Saving - ", rand_name, ".txt", sep="" ) ) )
  save( loss, file=file.path( Result_location, "Loss", gsub( ":", "_", paste( "Loss ", scenario, " ", Sys.time(), ".RData", sep="" ) ) ) )
  #return(loss)
  return(NULL)

} # end function


run_simulation <- function( model, fitted_mod, val.hex, val_data, nsims, scenario, Result_location )
{ # Simulate the loss distribution at a 1yr horizon
  # NOTE: VAL.HEX IS FOR THE STATE TRANSITION MODEL
  #	  VAL_DATA IS FOR THE LGD MODEL

  # Pre-compute the observation months
  print( "Pre-computing Observation Months..." )
  month_factor	<- vector( mode="list", length=12 )
  for( t in 1:12 )
  {
    month_factor[[t]]	<- as.factor( h2o.rep_len( as.h2o( ifelse( t < 10, paste( 0, t, sep="" ), t ) ), nrow(val.hex) ) )
  } # end for

  # Pre-compute the LGD parameters
  # NOTE: ALLOWS THE ECONOMIC VARIABLES TO CHANGE, BUT FIXES CUM_DELINQUENT
  print( "Pre-computing LGD Parameters..." )
  if( file.exists( file.path( Result_location, "ECAP/LGD_parameters.RData" ) ) )
  {
    load( file.path( Result_location, "ECAP/LGD_parameters.RData" ) )		# LGD_parameters
  } else
  {
    LGD_parameters	<- pre_compute_LGD_parms( fitted_mod, val_data, scenario )
    save( LGD_parameters, file=file.path( Result_location, "ECAP/LGD_parameters.RData" ) )
  } # end if
  rm( fitted_mod ); gc()


  # Get the first month prediction
  print( "Getting First Month Predictions..." )
  prediction	<- h2o.predict( model, val.hex )
  prediction	<- prediction[,2:ncol(prediction)]
  states	<- sub( "p", "", colnames(prediction) )
  
  # Initialize the cluster to work in parallel
  # and randomly pick the seeds so that the clusters work with different random numbers
  cl 		<- makeCluster(10)
  print( cl )
  clusterEvalQ(cl, library(h2o))
  clusterEvalQ(cl, library(gamlss))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, h2o.init(nthreads=-1, max_mem_size='64g') )
  clusterExport(cl=cl, varlist=c("model", "prediction", "val.hex", "val_data", "LGD_parameters", "month_factor", "states", "scenario", "Result_location", 
				 "get_1yr_loss", "get_econ_variables"), envir=environment() )
  
  # Simulate the 1yr loss
  print( "Running Simulation..." )
  for( i in 1:1000 )
  {
    print( paste( "Parallel Iteration", i, "..." ) )
    ptm <- proc.time()
      sim_1yr_loss <- clusterEvalQ(cl, get_1yr_loss( model, prediction, val.hex, val_data, LGD_parameters, month_factor, states, scenario, Result_location ) )
    print( proc.time() - ptm )
  } # end for
  print( "Simulation Finished!!..." )
  Loss		<- do.call( cbind, sim_1yr_loss["Loss",] )
  Modified	<- do.call( cbind, sim_1yr_loss["Modified",] )
  Delinquent	<- do.call( cbind, sim_1yr_loss["Delinquent",] )
  
  Loss_dist	  <- colSums(Loss, na.rm=TRUE)
  Modified_dist	  <- colSums(Modified, na.rm=TRUE)
  Delinquent_dist <- colSums(Delinquent, na.rm=TRUE)
  Total_dist	  <- Loss_dist + Modified_dist + Delinquent_dist

  # Return the simulated loss values
  return( list( Loss=Loss, Modified=Modified, Delinquent=Delinquent,
		Loss_dist=Loss_dist, Modified_dist=Modified_dist, Delinquent_dist=Delinquent_dist,
		Total_dist=Total_dist ) )

} # end function


###############################################################################################################################################################################
# Compute the 1yr loss distribution
# Note: Need to run 'fannie_mae_code - Model Estimation.r' before running this code
# Note: Need to run 'fannie_mae_code - LGD Model.r' before running this code
# Note: Need to run 'fannie_mae_code - Model Setup Validation Data.r' before running this code


# Specify whether working on the server or personal computer
location	<- "Server"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1		# this is the transition frequency (the ECAP time horizon is 1yr)

# Define the economic scenario to analyze
scenario	<- "severly_adverse"	# "none", "baseline", "adverse", or "severly_adverse"

# Choose the number of simulations to estimate the 1 year probabilities
nsims		<- 1e4

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )


# Load the required libraries
chooseCRANmirror(ind=0)
if (!(require(data.table))) install.packages ("data.table")
if (!(require(h2o))) install.packages ("h2o")
if (!(require(gamlss))) install.packages ("gamlss")
if (!(require(parallel))) install.packages ("parallel")
h2o.init(nthreads=-1, max_mem_size='600g')
#h2o.init(nthreads=-1, max_mem_size='64g')
#h2o.init(nthreads=-1, max_mem_size='44g')


# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
loss_location	<- file.path( file_location, paste( "Loss_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- ifelse( location == "Server", file.path( file_location, "Results" ), file.path( file_location, "Results" ) )
val_location	<- file.path( Result_location, "Validation Data" )

# Create the ECAP and Loss folders if they do not already exist
dir.create( file.path( Result_location, "ECAP" ), showWarnings = FALSE )
dir.create( file.path( Result_location, "Loss" ), showWarnings = FALSE )

# Create the parallel log directory
unlink( file.path( Result_location, "ParallelTest" ), recursive = TRUE )
dir.create( file.path( Result_location, "ParallelTest" ), showWarnings = FALSE )


# Select the models to analyze
#PD_model_loc	<- ifelse( location == "Server", file.path( Result_location, "dl_grid_model_l1_l2_0" ), file.path( Result_location, "Model Results 5/dl_grid_model_l1_l2_0" ) )	
PD_model_loc	<- ifelse( location == "Server", file.path( Result_location, "DeepLearning_model_R_1472601160088_1" ), file.path( Result_location, "Model Results 6/DeepLearning_model_R_1472601160088_1" ) )	
LGD_model_loc	<- ifelse( location == "Server", file.path( Result_location, "LGD - Foreclosure/BEINF Fitted Model - Start Yr 2000.RData" ), file.path( Result_location, "LGD/BEINF Fitted Model - Start Yr 2000.RData" ) )	

# Store the validation file names
h2o_fname	<- file.path( val_location, "validation_data.csv" )
dt_fname	<- file.path( val_location, "validation_data.RData" )

# Store the loss file name
loss_fnames	<- list.files( loss_location, pattern = glob2rx("*csv"), full.names=TRUE )


# Load the validation data into h2o
col_names 	<- c( 'Obs_Year','VinYr','Monthly.Rpt.Prd','STATE','LOAN_ID','LOAN_STATUS','Loan.Age','LAST_RT','LAST_UPB','MOD_FLAG','Zero.Bal.Code','ZB_DTE',
		      'LPI_DTE','FCC_DTE','DISP_DT','FCC_COST','PP_COST','AR_COST','IE_COST','TAX_COST','NS_PROCS','CE_PROCS','RMW_PROCS',
		      'O_PROCS','NON_INT_UPB','PRIN_FORG_UPB','LOAN_STATUS_HORIZON','LOAN_STATUS_1yr','Horizon_C','Horizon_1','Horizon_2',
		      'Horizon_3','Horizon_4','Horizon_Y','Horizon_P','SBA_DefRate_Combo','SBA_DefRate_7a','SBA_DefRate_504','Obs_Month',
		      'ORIG_CHN','ORIG_RT','ORIG_AMT','OLTV','OCLTV','NUM_BO','DTI','FTHB_FLG','PURPOSE','PROP_TYP','NUM_UNIT','OCC_STAT',
		      'SATO','CSCORE_MN','ORIG_VAL','OpenMortgages','perc_delinquent','perc_foreclosure','foreclosure_delinquent_ratio',
		      'ORIG_VAL_Ratio','ORIG_RT_Ratio','ORIG_CSCORE_Ratio','ORIG_DTI_Ratio','CUR_VAL','CUR_RT_Ratio','CUR_CSCORE','CUR_DTI',
		      'Ref_Burnout','CUR_1Yr_Appreciation','Unemployment','Orig_AGI','Cur_AGI','AGI_Ratio','Judicious','Per_Paid','Cum_Underwater',
		      'Cum_Delinquent','Loan.Age_NA','LAST_UPB_NA','ORIG_RT_NA','ORIG_AMT_NA','OLTV_NA','OCLTV_NA','DTI_NA','SATO_NA',
		      'CSCORE_MN_NA','ORIG_VAL_NA','ORIG_VAL_Ratio_NA','ORIG_RT_Ratio_NA','ORIG_CSCORE_Ratio_NA','ORIG_DTI_Ratio_NA',
		      'CUR_VAL_NA','CUR_RT_Ratio_NA','CUR_CSCORE_NA','CUR_DTI_NA','Ref_Burnout_NA','CUR_1Yr_Appreciation_NA','Unemployment_NA',
		      'Orig_AGI_NA','Cur_AGI_NA','AGI_Ratio_NA','Per_Paid_NA','Cum_Underwater_NA','Cum_Delinquent_NA','Horizon_C_NA',
		      'Horizon_1_NA','Horizon_2_NA','Horizon_3_NA','Horizon_4_NA','Horizon_Y_NA','Horizon_P_NA','OpenMortgages_NA',
		      'perc_delinquent_NA','perc_foreclosure_NA','foreclosure_delinquent_ratio_NA','SBA_DefRate_Combo_NA','SBA_DefRate_7a_NA',
		      'SBA_DefRate_504_NA' )
col_types	<- c( 'int','enum','enum','enum','string','enum','int','real','real','enum','enum','enum',
		      'enum','enum','enum','real','real','real','real','real','real','real','real',
		      'real','real','real','enum','enum','real','real','real',
		      'real','real','real','real','real','real','real','enum',
		      'enum','real','int','int','int','enum','int','enum','enum','enum','enum','enum',
		      'real','int','real','real','real','real','real',
		      'real','real','real','real','real','real','real','real',
		      'real','real','real','real','real','real','enum','real','int',
		      'int','enum','enum','enum','enum','enum','enum','enum','enum',
		      'enum','enum','enum','enum','enum','enum',
		      'enum','enum','enum','enum','enum','enum','enum',
		      'enum','enum','enum','enum','enum','enum','enum',
		      'enum','enum','enum','enum','enum','enum','enum',
		      'enum','enum','enum','enum','enum',
		      'enum' )

print( "Importing Data into H2O...")
ptm <- proc.time()
  val.hex 	<- h2o.importFile(path=h2o_fname, header=TRUE, sep=",", col.names=col_names, col.types=col_types)
print( proc.time() - ptm )
print( "Finished Importing Data" )
gc(); h2o.clusterStatus()

# Load the validation data into R (data.table)
load( dt_fname )	# validation_data
val_data	<- validation_data
rm(validation_data); gc()
val_data[, h2o_order:= 1:.N ]	# keep track of the original order


# Specify the loss data file format
col_names 	<- c( 'LOAN_ID','Obs_Year','VinYr','Monthly.Rpt.Prd','STATE','LOAN_STATUS','Loan.Age','LAST_RT','LAST_UPB','MOD_FLAG','Zero.Bal.Code','ZB_DTE',
		      'LPI_DTE','FCC_DTE','DISP_DT','FCC_COST','PP_COST','AR_COST','IE_COST','TAX_COST','NS_PROCS','CE_PROCS','RMW_PROCS',
		      'O_PROCS','NON_INT_UPB','PRIN_FORG_UPB','LOAN_STATUS_HORIZON','LOAN_STATUS_1yr','Horizon_C','Horizon_1','Horizon_2',
		      'Horizon_3','Horizon_4','Horizon_Y','Horizon_P','SBA_DefRate_Combo','SBA_DefRate_7a','SBA_DefRate_504','Obs_Month',
		      'ORIG_CHN','ORIG_RT','ORIG_AMT','OLTV','OCLTV','NUM_BO','DTI','FTHB_FLG','PURPOSE','PROP_TYP','NUM_UNIT','OCC_STAT',
		      'SATO','CSCORE_MN','ORIG_VAL','OpenMortgages','perc_delinquent','perc_foreclosure','foreclosure_delinquent_ratio',
		      'ORIG_VAL_Ratio','ORIG_RT_Ratio','ORIG_CSCORE_Ratio','ORIG_DTI_Ratio','CUR_VAL','CUR_RT_Ratio','CUR_CSCORE','CUR_DTI',
		      'Ref_Burnout','CUR_1Yr_Appreciation','Unemployment','Orig_AGI','Cur_AGI','AGI_Ratio','Judicious','Per_Paid','Cum_Underwater',
		      'Cum_Delinquent','Loan.Age_NA','LAST_UPB_NA','ORIG_RT_NA','ORIG_AMT_NA','OLTV_NA','OCLTV_NA','DTI_NA','SATO_NA',
		      'CSCORE_MN_NA','ORIG_VAL_NA','ORIG_VAL_Ratio_NA','ORIG_RT_Ratio_NA','ORIG_CSCORE_Ratio_NA','ORIG_DTI_Ratio_NA',
		      'CUR_VAL_NA','CUR_RT_Ratio_NA','CUR_CSCORE_NA','CUR_DTI_NA','Ref_Burnout_NA','CUR_1Yr_Appreciation_NA','Unemployment_NA',
		      'Orig_AGI_NA','Cur_AGI_NA','AGI_Ratio_NA','Per_Paid_NA','Cum_Underwater_NA','Cum_Delinquent_NA','Horizon_C_NA',
		      'Horizon_1_NA','Horizon_2_NA','Horizon_3_NA','Horizon_4_NA','Horizon_Y_NA','Horizon_P_NA','OpenMortgages_NA',
		      'perc_delinquent_NA','perc_foreclosure_NA','foreclosure_delinquent_ratio_NA','SBA_DefRate_Combo_NA','SBA_DefRate_7a_NA',
		      'SBA_DefRate_504_NA','Fin_UPB','modfg_cost','C_modir_cost','C_modfb_cost','lpi2disp','zb2disp','INT_COST','total_expense',
		      'total_proceeds','NET_LOSS','NET_SEV','Total_Cost','Tot_Procs','Tot_Liq_Ex','LAST_DTE','MODIR_COST','MODFB_COST','MODTOT_COST' )

col_classes	<- c( 'character','integer','factor','character','character','factor','integer','numeric','numeric','factor','factor','character',
		      'character','character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','factor','factor','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','numeric','factor',
		      'factor','numeric','numeric','numeric','numeric','factor','numeric','factor','factor','factor','factor','factor',
		      'numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','factor','numeric','numeric',
		      'integer','factor','factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor',
		      'factor','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','character','numeric','numeric','numeric' )


# Read in the loss data and store as a data.table
print( "Reading in the Data..." )
ptm <- proc.time()
if( file.exists( file.path( loss_location, "Loss_Data.RData" ) ) )
{
  load( file.path( loss_location, "Loss_Data.RData" ) )		# Loss_Data
} else
{
  loss_list <- foreach(file_num=1:n_files, .inorder=FALSE,
           		.packages=c("data.table")) %dopar% 
  {
    Loss_Data 	<- fread( loss_fnames[file_num], sep = ",", header=TRUE, colClasses=col_classes, stringsAsFactors=FALSE, showProgress=0)
    setnames(Loss_Data, col_names )
  } # end for each
  Loss_Data	<- rbindlist( loss_list, use.names=TRUE, fill=TRUE )
  save( Loss_Data, file=file.path( loss_location, "Loss_Data.RData" ) )

} # end if
print( proc.time() - ptm )
print( "Finished Importing Data" )



# Load the state transition model
print( paste( "Loading", PD_model_loc, "..." ) )
model<- h2o.loadModel( PD_model_loc )

# Load the LGD model
print( paste( "Loading", LGD_model_loc, "..." ) )
load( LGD_model_loc )		# fitted_mod


########################################################################################################################################################################################################
# Get the train-validate data for the LGD model (from the original LGD fitting)
# NOTE: FOR NOW I AM GETTING THE LOSS_DATA AND KEEP_COLS MANUALLY
#	THE LGD CODE SHOULD OUTPUT THE DEFAULTED_LOANS DATA AND KEEP_COLS
# NOTE: THE TRAIN-VALIDATE DATA IS NEEDED FOR THE LGD MODEL PREDICTIONS

print( "Organizing Loss Data..." )
Loss_Data	<- subset( Loss_Data, LOAN_STATUS == 'F' )


# Ensure the LGD is capped between 0 and 1
paste( "Total Observations:", nrow(Loss_Data) )
paste( "Observations with negative loss:",  sum( Loss_Data$NET_SEV < 0 ) )
paste( "Observations with over 100% loss:", sum( Loss_Data$NET_SEV > 1 ) )
Loss_Data[, NET_SEV:= pmin( pmax( NET_SEV, 0 ), 1 ) ]

# Calculate the current LTV
Loss_Data[, CUR_LTV_NA:= as.factor( ifelse( (CUR_VAL_NA == 1) | (LAST_UPB_NA == 1), 1, 0 ) ) ]
Loss_Data[, CUR_LTV:= ifelse( CUR_LTV_NA == 0, LAST_UPB / CUR_VAL, 0 ) ]
Loss_Data[, Exp_Loss_NA:= as.factor( ifelse( (CUR_VAL_NA == 1) | (LAST_UPB_NA == 1), 1, 0 ) ) ]
Loss_Data[, Exp_Loss:= ifelse( Exp_Loss_NA == 0, 1 - CUR_VAL / LAST_UPB, 0 ) ]


# Define the x-variables to be used in modeling
model_x		<- c( "VinYr", "Loan.Age", "LAST_UPB", "ORIG_CHN", "LAST_RT", "ORIG_AMT", "OCLTV", "NUM_BO", "DTI", "FTHB_FLG", "PURPOSE",
   			"PROP_TYP", "NUM_UNIT", "OCC_STAT", "SATO", "CSCORE_MN", "ORIG_VAL", "perc_delinquent", "perc_foreclosure", "foreclosure_delinquent_ratio",
   			"CUR_VAL", "CUR_1Yr_Appreciation", "Unemployment", "Judicious", "Per_Paid", "Cum_Underwater", "Cum_Delinquent", "Loan.Age_NA", "LAST_UPB_NA",
   			"ORIG_AMT_NA", "OCLTV_NA", "DTI_NA", "SATO_NA", "CSCORE_MN_NA", "ORIG_VAL_NA",  
   			"Unemployment_NA", "Per_Paid_NA", "Cum_Underwater_NA", "Cum_Delinquent_NA", "perc_delinquent_NA", 
			"SBA_DefRate_Combo_NA", "CUR_LTV", "Exp_Loss" )  
y_val		<- "NET_SEV"
# Note: since all of the loans are 'F', do not use LOAN_STATUS as a variable


# Drop any factors with only 1 level
defaulted_loans	<- Loss_Data
drop_cols	<- sapply( defaulted_loans, function(x) length(levels(x)) )
drop_cols	<- names( which( drop_cols == 1, useNames=TRUE ) )
defaulted_loans[, which( colnames(defaulted_loans) %in% drop_cols ):=NULL ]

# Split between in and out of sample
max_train_yr		<- 2011
max_val_yr		<- 2012
t			<- 2000
yr_col			<- "Obs_Year"
keep_cols		<- c( y_val, model_x, yr_col )
keep_cols		<- intersect( keep_cols, colnames(defaulted_loans) )
train		        <- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) >= t ) & ( get(yr_col) <= max_train_yr ), select=-get(yr_col) )
train_validate		<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) >= t + (max_val_yr - max_train_yr) ) & ( get(yr_col) <= max_val_yr ), select=-get(yr_col) )

print( "Finished Organizing Loss Data..." )

#########################################################################################################################################################################################################
# Convert the validation data to a data.table so that the LGD model can use

# Select only the needed columns
print( "Getting correct columns..." )
val_data 	<- val_data[, .SD, .SDcols=colnames(val_data)[ colnames(val_data) %in% c(keep_cols, "LOAN_ID", "LOAN_STATUS", "CUR_VAL_NA", "LAST_UPB_NA", "h2o_order" ) ] ]

# Compute the current LTV and expected loss for the validation data (for LGD prediction)
print( "Computing Current LTV..." )
val_data[, CUR_LTV_NA:= as.factor( ifelse( (CUR_VAL_NA == 1) | (LAST_UPB_NA == 1), 1, 0 ) ) ]
val_data[, CUR_LTV:= ifelse( CUR_LTV_NA == 0, LAST_UPB / CUR_VAL, 0 ) ]
val_data[, Exp_Loss_NA:= as.factor( ifelse( (CUR_VAL_NA == 1) | (LAST_UPB_NA == 1), 1, 0 ) ) ]
val_data[, Exp_Loss:= ifelse( Exp_Loss_NA == 0, 1 - CUR_VAL / LAST_UPB, 0 ) ]
val_data[, c( "CUR_VAL_NA", "LAST_UPB_NA", "CUR_LTV_NA", "Exp_Loss_NA" ) := NULL ]

# Merge the new severity values to the validation data
print( "Merging NET_SEV..." )
val_data[, NET_SEV:= NA ]	# Note: the NET_SEV column just needs to exist for the LGD prediction
#val_data 	<- merge( x=val_data, y=defaulted_loans[,.SD, .SDcols=c("LOAN_ID","NET_SEV")], by.x="LOAN_ID", by.y="LOAN_ID", all.x=TRUE )

# Match the sort order of the h2o data
setorder( val_data, h2o_order )
val_data[, h2o_order:= NULL ]

# Match the columns with those used in the LGD model
val_data	<- val_data[, .SD, .SDcols=setdiff( c( keep_cols, "LOAN_STATUS" ), yr_col ) ] 
save( val_data, file=file.path( Result_location, "ECAP/val_data.RData" ) )

#########################################################################################################################################################################################################

# Run the simulation
#set.seed(100)
sim_1yr_results	<- run_simulation( model, fitted_mod, val.hex, val_data, nsims, scenario, Result_location )
save( sim_1yr_results, file=file.path( Result_location, "ECAP/sim_1yr_results.RData" ) )



#########################################################################################################################################################################################################
# NOTE: DOES NOT RUN THIS PART FOR NOW

if( FALSE )
{
# Load the LGD model results
load( "BEINF_LGD_results.RData" )	# BEINF_LGD_results


# Unlist the LGD results
max_train_yr		<- 2011
max_val_yr		<- 2012
yr_col			<- "Obs_Year"
keep_cols		<- BEINF_LGD_results[["keep_cols"]]
defaulted_loans		<- BEINF_LGD_results[["defaulted_loans"]]
fit_results		<- BEINF_LGD_results[["fit_results"]]


# Simulate the loss distribution by year (1yr-5yrs) using the start year LGD distribution with the min validation global deviance (VGD)
fit_VGD			<- sapply( fit_results, function(x) x$val_GD )
min_VGD			<- min(fit_VGD)
i			<- names(fit_results)[which.min(fit_VGD)]
fitted_train_val	<- fit_results[[i]][["fitted_train_val"]]
train_validate		<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) >= as.numeric(i) + (max_val_yr - max_train_yr) ) & ( get(yr_col) <= max_val_yr ), select=-get(yr_col) )
loss_dist_summary	<- replicate( n=nreps, simulate_loss_distribution( portfolio_data, portfolio_PD, portfolio_LGD_all, fitted_train_val,
					 	  yr_range, nsims, show_plot=FALSE, start_yr=NULL )
				    )
loss_dist_summary	<- t( loss_dist_summary	)
write.csv( loss_dist_summary, file.path( Result_location, "Loss Distribution Summary - Start Year Min VGD.csv" ) )


# Compute the summary statistics for the loss distribution simulations
loss_dist_sim_summary 	<- apply( loss_dist_summary, 2, function(x) 
				   c( mean=mean(x), sd=sd(x),
				      conf_025=quantile( x, probs=0.025 ),
				      conf_975=quantile( x, probs=0.975 ) 
				 ) )
write.csv( loss_dist_sim_summary, "Outputs/Loss Distribution Sim Summary - Start Year Min VGD.csv" )

} # end if

