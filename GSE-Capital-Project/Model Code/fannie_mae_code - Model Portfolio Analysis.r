# Define the Functions

get_loan_universe <- function( data.hex, universe_size )
{
  # Get the first month for each loan id
  data.hex	<- data.hex[ data.hex$Obs_Month == '01', ]

  # Get the universe size
  universe_size	<- min( universe_size, nrow(data.hex) )
  print( paste( "Universe Size:", universe_size ) )

  # Randomly select the loan universe to have size universe_size
  keep_loan	<- sample( nrow(data.hex), nrow(data.hex), replace=FALSE )
  keep_loan	<- as.h2o( keep_loan <= universe_size )
  loan_universe	<- data.hex[ keep_loan == 'TRUE', ]

  # Return the selected loan universe
  return(loan_universe)

} # end function


select_portfolio <- function( model_predictions, loan_universe, N )
{ # Choose the N loans with the highest predicted probability of being current

  N				<- min( N, nrow(model_predictions) )
  #model_predictions		<- h2o.cbind( model_predictions, loan_universe[, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") ] )
  #model_predictions 		<- as.data.table(model_predictions)
  model_predictions		<- cbind( as.data.table(model_predictions), as.data.table( loan_universe[, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") ] ) )
  setorder(model_predictions, -C)
  model_predictions		<- model_predictions[1:N, ]
  return(model_predictions)

} # end function


model_portfolio_comparison_plot <- function( model_predictions, loan_universe, type="Month" )
{ # Rank the probability of remaining current for each of the models and
  # plot the percentage of loans chosen against the number of loans not current

  loan_status_col	<- ifelse( type == "Month", "LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr" )

  # Compute the modeled scores
  res	<- data.frame()
  for( i in 1:length(model_predictions) )
  {
    #model_predictions[[i]]		<- h2o.cbind( model_predictions[[i]], loan_universe[, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") ] )
    #model_predictions[[i]]		<- as.data.table(model_predictions[[i]])
    model_predictions[[i]]		<- cbind( as.data.table(model_predictions[[i]]), as.data.table( loan_universe[, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") ] ) )
    setorder(model_predictions[[i]], -C)
    model_predictions[[i]]$loan_perc	<- 1:nrow(model_predictions[[i]]) / nrow(model_predictions[[i]])
    model_predictions[[i]]$not_current	<- cumsum( !(model_predictions[[i]][,get(loan_status_col)] == 'C') )
    res	<- rbind( res, cbind( rep( names(model_predictions)[i], nrow(model_predictions[[i]]) ), model_predictions[[i]][, list( loan_perc, not_current) ] ) )
  } # end for

  # Compute the perfect model score
  perfect		<- as.data.table( loan_universe[, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") ] )
  perfect_C		<- subset( perfect, get(loan_status_col) == 'C' )
  perfect_NC		<- subset( perfect, !(get(loan_status_col) == 'C') )
  perfect		<- rbind( perfect_C, perfect_NC )
  perfect$loan_perc	<- 1:nrow(perfect) / nrow(perfect)
  perfect$not_current	<- cumsum( !(perfect[,get(loan_status_col)] == 'C') )
  res	<- rbind( res, cbind( rep( "Perfect", nrow(perfect) ), perfect[, list( loan_perc, not_current) ] ) )
  colnames(res)[ colnames(res) == "V1" ] <- "Model"

  # Show the model comparison plot with the perfect score
  print( qplot( x=loan_perc*100, y=not_current, data=res, color=Model, 
	 xlab="Loans chosen from pool of 100,000 (in percent)",
	 ylab="Number of loans not current",
	 main=paste( "Loan Ranking Analysis (1", type, "ahead)" ) ) + theme(legend.position = 'bottom') ) 

} # end function


create_pools <- function( data.hex, pool_by, order_by, pool_size, universe_size )
{ # Create mortgages pools of size pool_size grouped by pool_by
  
  # Only use the loans with non-missing pool_by values
  pool_by_NA	<- paste( pool_by, "NA", sep="_" )
  for( i in 1:length(pool_by) )
  {
    data.hex	<- data.hex[ data.hex[, pool_by_NA[i] ] == '0', ]
  } # end for

  # Randomly select 1 month per loan id
  loan_universe	<- get_loan_universe( data.hex, universe_size )

  # Assign the loan universe in pools according to the pool_by variable
  for( i in 1:length(pool_by) )
  {
    temp		<- as.data.table( loan_universe[, pool_by[i] ] )
    temp[, Orig_Order:= 1:.N ]
    setorderv( temp, pool_by[i], order=order_by[i] )
    pool_name		<- paste( "Pool", pool_by[i], sep="_" )
    temp[, pool_name := 0:(.N-1) %/% pool_size ]
    pool_count		<- temp[, .N, by=pool_name ]
    small_pool		<- subset( pool_count, N < pool_size )
    temp[ pool_name %in% small_pool$pool_name, "pool_name" ] <- NA
    colnames(temp)[ colnames(temp) == "pool_name" ] <- pool_name
    setorderv( temp, "Orig_Order", order=1 )
    temp		<- as.h2o( temp[,get(pool_name)] ) 
    colnames(temp) <- pool_name
    loan_universe	<- h2o.cbind( loan_universe, temp )
  } # end for

  # Return the loan universe with the pool ids
  return( loan_universe )

} # end function


predict_loan_pools <- function( model, loan_pools, type="Month", nsims=0, method )
{ # Predict on the loan pool data

  keep_cols	<- grep( "Pool_*", colnames(loan_pools), value=TRUE )
  keep_cols	<- c( keep_cols, c("LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr") )
  if( type == "Month" )
  {
    prediction 	<- h2o.predict(model, loan_pools)
  } else if( type == "Year" )
  {
    prediction	<- h2o_predict_1yr(model, loan_pools, nsims, method)
  }
  #prediction 	<- h2o.cbind(prediction, loan_pools[,keep_cols])
  #return( as.data.table(prediction) )
  prediction	<- cbind( as.data.table(prediction), as.data.table(loan_pools[,keep_cols]) )
  return( prediction )

} # end function


summarize_pool_prepayment_prediction <- function( pool_prediction, type="Month" )
{ # For each of the pool_by variables, count the number of observed vs predicted prepayments by pool
  
  loan_status_col	<- ifelse( type == "Month", "LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr" )

  pool_cols	<- grep( "Pool_*", colnames(pool_prediction), value=TRUE )
  res		<- vector( mode="list", length=length(pool_cols) )
  names(res)	<- pool_cols
  for( i in 1:length(pool_cols) )
  {
    temp	<- ddply( pool_prediction, .(get(pool_cols[i])), function(x) 
				{ data.frame( Observed_Prepayment=sum(x[,loan_status_col] == 'P'),
					      Predicted_Prepayment=sum(x$P) ) 
				} )
    colnames(temp)[1]	<- pool_cols[i]
    temp	<- subset( temp, !is.na(temp[,pool_cols[i]]) )
    res[[i]]	<- temp

  } # end for

  # Return the results
  return(res)

} # end function


pool_by_plot <- function( pool_prepayment_summary, type="Month" )
{ # Plot the model comparison for each of the pool_by variables

  pool_cols	<- names(pool_prepayment_summary[[1]])
  for( i in 1:length(pool_cols) )
  {
    # Organize the results by the pool_by variable
    pool_summary <- lapply( seq(pool_prepayment_summary), function(j) 
				{ temp 		<- pool_prepayment_summary[[j]][[ pool_cols[i] ]]
			  	  temp$Model 	<- names(pool_prepayment_summary)[j]
			  	  return(temp) 
				} )
    pool_summary <- do.call( rbind, pool_summary )
  

    # Add the ideal line to the data
    axis_lim		<- max( pool_summary[,c("Observed_Prepayment","Predicted_Prepayment")] )
    ideal		<- pool_summary[1:2,]
    ideal$Observed_Prepayment	<- c(0,axis_lim)
    ideal$Predicted_Prepayment 	<- c(0,axis_lim)
    ideal$Model		<- "Ideal"

    # Plot the model comparison for the pool_by variable
    print( qplot( x=Observed_Prepayment, y=Predicted_Prepayment, data=pool_summary, color=Model, 
	 xlab="Observed Number of Prepayments",
	 ylab="Predicted Number of Prepayments" ) +
	 ggtitle( paste( "Portfolio Loan Level Analysis - ", pool_cols[i], "\n (1 ", type, " ahead, portfolio size ", pool_size, ")", sep="" ) ) +
	 theme(legend.position = 'bottom') + geom_line(aes(x=Observed_Prepayment, y=Predicted_Prepayment), color="black", data=ideal) )

  } # end for

} # end function


sim_1yr_loan_state <- function( model, prediction, loan_universe, states )
{ # Get the simulated loan state in 1yr
  # Note: prediction is the 1 month prediction data

  # Define the functions to compute the cumsum in h2o
  # Note: for some reason h2o will not accept passed parameters and writing out the function also did seem to help
  #h2o_cumsum	<- lapply( seq(ncol(prediction)), function(j) paste( "function(x) { sum( x[,1:", j, "] ) }", sep="" ) )
  #h2o_cumsum	<- lapply( h2o_cumsum, function(x) eval(parse(text=x)) )

  new_predict		<- prediction
  current_state		<- loan_universe$LOAN_STATUS
  dat			<- loan_universe
  for( t in 2:12 )	# do for 1 year; Note that the 1st month is predicted above (input)
  {
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
    cumsum_prob[[9]]	<- apply( new_predict, 1, function(x) { sum( x[,1:9] ) } )
    cumsum_prob		<- h2o.cbind( cumsum_prob )
    colnames(cumsum_prob) <- colnames(new_predict)
    #cumsum_prob	<- lapply( seq(ncol(new_predict)), function(j) apply( new_predict, 1, h2o_cumsum[[j]] ) )

    # Get the simulated state
    u			<- h2o.runif( dat$LOAN_ID )
    new_state		<- as.factor( apply( cumsum_prob < u, 1, function(x) { sum(x) + 1 } ) )
    new_state		<- h2o.setLevels( new_state, states[ as.numeric( h2o.levels(new_state) ) ] )
    colnames(new_state) <- "LOAN_STATUS"

    # Ensure absorbing states remain absorbing
    state_levels	<- h2o.levels(new_state)
    new_state		<- ifelse( current_state %in% c( as.character(1:4), 'C' ), new_state, current_state )
    new_state		<- as.factor( new_state + 1 )
    new_state		<- h2o.setLevels( new_state, state_levels[ as.numeric( h2o.levels(new_state) ) ] )
    colnames(new_state) <- "LOAN_STATUS"

    # Update the loan state
    dat[, which( colnames(dat) == "LOAN_STATUS" ) ] 	<- NULL
    dat			<- h2o.cbind( dat, new_state )

    # Update the other fields (delinquency, loan age, etc)
    # Note: Unemployment, interest rates, home values are assumed to be frozen
    # Note: Assumes the data starts with the 1st month
    Obs_Month 		<- as.factor( as.h2o( rep( ifelse( t < 10, paste( 0, t, sep="" ), t ), nrow(dat) ) ) )
    colnames(Obs_Month)	<- "Obs_Month"
    Loan.Age		<- ifelse( dat$Loan.Age_NA == '1', 0, dat$Loan.Age + 1 )
    colnames(Loan.Age)	<- "Loan.Age"
    Cum_Delinquent	<- ifelse( dat$Cum_Delinquent_NA == '1', 0, ifelse( new_state %in% as.character(1:4), dat$Cum_Delinquent + 1, dat$Cum_Delinquent ) )
    colnames(Cum_Delinquent) <- "Cum_Delinquent"
    dat[, which( colnames(dat) %in% c( "Obs_Month", "Loan.Age", "Cum_Delinquent" ) ) ] 	<- NULL
    dat		<- h2o.cbind( dat, Obs_Month, Loan.Age, Cum_Delinquent )

    # Update predictions and current state for next month
    new_predict 	<- h2o.predict( model, dat )
    new_predict		<- new_predict[,2:ncol(new_predict)]
    current_state	<- new_state

  } # end inner for

  # Return the 1yr simulated loan state
  return(current_state)

} # end function


predict_1yr_trans_matrix <- function( model, prediction, loan_universe, states )
{ # Predict the 1yr loan states by freezing the variables and using the transition matrix approach

  # Freeze the variables and use a transition matrix to compute the 1yr probability distribution
  dat			<- loan_universe
  state_trans_probabilities	<- vector( mode="list", length=length(states) )
  names(state_trans_probabilities)	<- states
  for( s in states )
  {
    if( s %in% c( as.character(1:4), 'C' ) )
    { # If non-absorbing state
      # Update the loan state
      new_state		<- as.factor( as.h2o( rep( s, nrow(dat) ) ) )
      colnames(new_state) <- "LOAN_STATUS"
      dat[, which( colnames(dat) == "LOAN_STATUS" ) ] 	<- NULL
      dat		<- h2o.cbind( dat, new_state )
      temp		<- h2o.predict( model, dat )
      temp		<- as.matrix( temp[,2:ncol(temp)] )

    } else
    { # If absorbing state
      temp		<- matrix(0, nrow=nrow(dat), ncol=length(states) )
      colnames(temp)	<- states
      for( k in states )
      {
        if( k == s )
        {
          temp[,k]	<- 1
        } else
        {
          temp[,k]	<- 0
        } # end if
      } # end for
        
    } # end if
    colnames(temp)	<- states
    state_trans_probabilities[[s]]	<- temp

   } # end for

  # Get the row matching the original loan state
  LOAN_STATUS		<- as.data.table( loan_universe$LOAN_STATUS )
  LOAN_STATUS[, N:=1:.N ]
  LOAN_STATUS[, row_num:= which( states == LOAN_STATUS ), by=N ]

  yr_trans_vector_list <- foreach(i=1:nrow(dat), .inorder=FALSE,
           		.packages=c("data.table", "expm")) %dopar% 
  {
    # Convert the transition probabilties into a matrix for each loan id
    trans_matrix	<- do.call( rbind, lapply( state_trans_probabilities, function(y) y[i,] ) )

    # Compute the 1yr transition matrix
    yr_trans_matrix	<- trans_matrix %^% 12
    
    # Get the 1yr transition probability vectors based on the original loan state
    yr_trans_vector	<- yr_trans_matrix[ LOAN_STATUS[i, row_num], ]
    
    # Return the probability vector
    return( yr_trans_vector )
 
  } # end for each

  # Combine into a single frame in h2o
  yr_trans_vector	<- do.call( rbind, yr_trans_vector_list )
  #yr_trans_vector	<- as.h2o( yr_trans_vector )
  colnames(yr_trans_vector)	<- states
  dim(yr_trans_vector)

  # Return the 1yr transition probabilities
  return( yr_trans_vector )

} # end function


h2o_predict_1yr <- function( model, loan_universe, nsims, method )
{ # Successively predict on the dataset to get the 1yr prediction

  # Get the first month prediction
  prediction	<- h2o.predict( model, loan_universe )
  prediction	<- prediction[,2:ncol(prediction)]
  states	<- sub( "p", "", colnames(prediction) )

  if( method == "Sim" )
  {
    # Simulate the 1yr state probability distribution
    sim_1yr_state	<- replicate( nsims, sim_1yr_loan_state( model, prediction, loan_universe, states ) )
    sim_1yr_state 	<- h2o.cbind( sim_1yr_state )

    # Compute the fraction of time in each state for each loan id
    sim_1yr_state_frac	<- lapply( states, function(s) sim_1yr_state == s )
    sim_1yr_state_frac 	<- lapply( sim_1yr_state_frac, function(x) apply(x, 1, sum) )
    sim_1yr_state_frac 	<- h2o.cbind(sim_1yr_state_frac)
    sim_1yr_state_frac	<- sim_1yr_state_frac / nsims 
    colnames(sim_1yr_state_frac) <- states

  } else
  {
    # Predict the 1yr loan states by freezing the variables and using the transition matrix approach
    yr_trans_vector	<- predict_1yr_trans_matrix( model, prediction, loan_universe, states )

  } # end if

  # Return the 1yr loan state prediction probabilities
  if( method == "Sim" )
  {
    return( sim_1yr_state_frac )
  } else
  {
    return( yr_trans_vector )
  } # end if

} # end function


summarize_portfolio_performance <- function( loan_universe, selected_portfolio, type="Month", Result_location, method )
{
  model_id		<- names(selected_portfolio)
  loan_status_col	<- ifelse( type == "Month", "LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr" )
  
  # Summarize the 1-month/year portfolio loan performance for each portfolio (model)
  # It gives the percentage of the portfolio which is in each state at a 1 month/year time horizon
  portfolio_performance	<- lapply( selected_portfolio, function(x) table(x[,get(loan_status_col)]) / nrow(x) )
  names(portfolio_performance) <- model_id
  print( paste( model_id, "Portfolio Performance 1 Month..." ) )
  print( portfolio_performance )
  write.csv( do.call( cbind, portfolio_performance ), file.path( Result_location, paste( "Portfolio Performance 1 ", type, " ", method, " - ", model_id[2] ,".csv", sep="" ) ), row.names=TRUE )

  # For comparison, get the performance if the models randomly selected the loan portfolios
  naive_performance	<- h2o.table(loan_universe[,loan_status_col])
  naive_performance$Per	<- naive_performance$Count / sum(naive_performance$Count)
  naive_performance	<- as.data.frame( naive_performance )
  print( paste( model_id, "Naive Performance 1 Month..." ) )
  print( naive_performance )
  write.csv( naive_performance, file.path( Result_location, paste( "Naive Performance 1 ", type, " ", method, " - ", model_id[2] ,".csv", sep="" ) ), row.names=FALSE )

  invisible(NULL) 

} # end function


###############################################################################################################################################################################
# Create Model Variable Profile Plots
# Note: Need to run 'fannie_mae_code - Model Estimation.r' before running this code


# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Set the time horizon (in months)
time_horizon	<- 1

# Specify whether or not to do the 1 year analysis
do_1yr_analysis	<- TRUE		# TRUE or FALSE

# Choose the 1yr analysis method
method		<- "TransMatrix"	# "Sim" or "TransMatrix"

# Choose the number of simulations to estimate the 1 year probabilities (if type == "Sim", otherwise nsims is not used)
nsims		<- ifelse( location == "Server", 1e4, 10 )

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )


# Load the required libraries
chooseCRANmirror(ind=0)
if (!(require(data.table))) install.packages ("data.table")
if (!(require(plyr))) install.packages ("plyr")
if (!(require(h2o))) install.packages ("h2o")
if (!(require(ggplot2))) install.packages ("ggplot2")
if (!(require(expm))) install.packages ("expm")
if (!(require(foreach))) install.packages ("foreach")
h2o.init(nthreads=-1, max_mem_size='1500g', enable_assertions=FALSE)

# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
chunk_location	<- file.path( file_location, paste( "Chunk_TimeHorizon1yr_Fixed_", time_horizon, sep="" ) )
Result_location	<- ifelse( location == "Server", file.path( file_location, "Results" ), file.path( file_location, "Results/Model Results 5" ) )

# Select the models to analyze (should be in the "Results" folder)
model_ids	<- list.files( Result_location, pattern = glob2rx("*_model_R*"), full.names=FALSE)
model_ids	<- c( model_ids, list.files( Result_location, pattern = glob2rx("*_grid_model_*"), full.names=FALSE) )
model_ids	<- grep( ".pdf", model_ids, value=TRUE, invert=TRUE )
model_ids	<- grep( ".csv", model_ids, value=TRUE, invert=TRUE )

# Get the monthly batched data table files names
fnames		<- list.files( chunk_location, pattern = glob2rx("Combined_Data*csv"), full.names=TRUE)
if( location == "Personal" )
{
  fnames <- fnames[1]
}

# Load the data into h2o
col_names	<- c( "Obs_Year", "VinYr", "Monthly.Rpt.Prd", "STATE", "LOAN_ID", "LOAN_STATUS", "Loan.Age", "LAST_UPB", 
		      "LOAN_STATUS_HORIZON", "LOAN_STATUS_1yr", "Horizon_C", "Horizon_1", "Horizon_2", "Horizon_3", "Horizon_4", "Horizon_Y", 
		      "Horizon_P", "Obs_Month", "ORIG_CHN", "ORIG_RT", "ORIG_AMT", "OLTV", "OCLTV", "NUM_BO", 
		      "DTI", "FTHB_FLG", "PURPOSE", "PROP_TYP", "NUM_UNIT", "OCC_STAT", "SATO", "CSCORE_MN", 
		      "ORIG_VAL", "ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio", "ORIG_DTI_Ratio", "CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", 
		      "CUR_DTI", "Ref_Burnout", "CUR_1Yr_Appreciation", "Unemployment", "Orig_AGI", "Cur_AGI", "AGI_Ratio", "Judicious", 
		      "Per_Paid", "Cum_Underwater", "Cum_Delinquent", "Loan.Age_NA", "LAST_UPB_NA", "ORIG_RT_NA", "ORIG_AMT_NA", "OLTV_NA", 
		      "OCLTV_NA", "DTI_NA", "SATO_NA", "CSCORE_MN_NA", "ORIG_VAL_NA", "ORIG_VAL_Ratio_NA", "ORIG_RT_Ratio_NA", "ORIG_CSCORE_Ratio_NA", 
		      "ORIG_DTI_Ratio_NA", "CUR_VAL_NA", "CUR_RT_Ratio_NA", "CUR_CSCORE_NA", "CUR_DTI_NA", "Ref_Burnout_NA", "CUR_1Yr_Appreciation_NA", "Unemployment_NA", 
		      "Orig_AGI_NA", "Cur_AGI_NA", "AGI_Ratio_NA", "Per_Paid_NA", "Cum_Underwater_NA", "Cum_Delinquent_NA", "Horizon_C_NA", "Horizon_1_NA", 
		      "Horizon_2_NA", "Horizon_3_NA", "Horizon_4_NA", "Horizon_Y_NA", "Horizon_P_NA" )
col_types	<- c( "int", "enum", "enum", "enum", "int", "enum", "int", "real", 
		      "enum", "enum", "real", "real", "real", "real", "real", "real", 
		      "real", "enum", "enum", "real", "int", "int", "int", "enum", 
		      "int", "enum", "enum", "enum", "enum", "enum", "real", "int", 
		      "real", "real", "real", "real", "real", "real", "real", "real", 
		      "real", "real", "real", "real", "real", "real", "real", "enum", 
		      "real", "int", "int", "enum", "enum", "enum", "enum", "enum", 
		      "enum", "enum", "enum", "enum", "enum", "enum", "enum", "enum", 
		      "enum", "enum", "enum", "enum", "enum", "enum", "enum", "enum", 
		      "enum", "enum", "enum", "enum", "enum", "enum", "enum", "enum", 
		      "enum", "enum", "enum", "enum", "enum" )

print( "Importing Data into H2O...")
ptm <- proc.time()
  data.hex 	<- h2o.importFile(path=fnames, header=TRUE, sep=",", col.names=col_names, col.types=col_types)
print( proc.time() - ptm )
print( "Finished Importing Data" )
h2o.clusterStatus()

# Split between training and validation set
print( "Splitting between Train and Validate..." )
cut_yr		<- 2011
max_yr		<- 2012		# Note: the MBS data starts in 2013
#train.hex	<- data.hex[ data.hex$Obs_Year <= cut_yr, ]
val.hex		<- data.hex[ ( data.hex$Obs_Year > cut_yr ) & ( data.hex$Obs_Year <= max_yr ), ]
#print( paste( "Train Dim:", dim( train.hex ) ) )
print( paste( "Validate Dim:", dim( val.hex ) ) )
h2o.rm(data.hex); gc(); h2o.clusterStatus()


# Subset to the validation loans with a known 1 year loan status
val.hex		<- val.hex[ !is.na(val.hex$LOAN_STATUS_1yr) && !(val.hex$LOAN_STATUS_1yr == 'X'), ]
dim(val.hex)

# Randomly choose 100,000 loans (from the entire dataset)
# and randomly select 1 loan from each loan id
set.seed(100)
universe_size	<- 100000
loan_universe	<- get_loan_universe( val.hex, universe_size )
dim(loan_universe)

# Create pools of 1,000 mortgages grouped by FICO, interest rate, LTV, (and need to do logistic regression predicted probability of current)
universe_size	<- 2*1e6
pool_size	<- ifelse( location == "Server", 1000, 100 )
pool_by		<- c( "CSCORE_MN", "ORIG_RT", "OLTV", "Horizon_C", "Horizon_1", "Horizon_2", "Horizon_3", "Horizon_4", "Horizon_Y",
		      "Horizon_P", "ORIG_AMT", "OCLTV", "DTI", "SATO", "ORIG_VAL", "ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio",
		      "ORIG_DTI_Ratio", "CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", "CUR_DTI", "Ref_Burnout", "CUR_1Yr_Appreciation", 
		      "Unemployment", "Per_Paid", "Cum_Underwater", "Cum_Delinquent", "Loan.Age" )
order_by	<- rep(1, length(pool_by))		# -1 means sort in descending order, 1 means sort in ascending order (do so can keep best loans)
loan_pools	<- create_pools( val.hex, pool_by, order_by, pool_size, universe_size )


GLM_model	<- h2o.loadModel( file.path( Result_location, grep( "GLM", model_ids, value=TRUE ) ) )
model_ids	<- grep( "GLM", model_ids, value=TRUE, invert=TRUE ) 
# Analyze each of the models
for( model_id in model_ids )
{
  # Load the models to analyze
  # NOTE: SINCE SOME OF THE MODELS HAVE THE SAME MODEL_ID AND H2O.PREDICT LOOKS UP THE MODEL IDS
  #	  NEED TO LOAD THE MODELS INDIVIDUALLY
  print( paste( "Analyzing", model_id, "..." ) )
  model		<- h2o.loadModel( file.path( Result_location, model_id ) )
  model		<- list( GLM_model, model )
  names(model)	<- c( "GLM", model_id )

  # For each model, predict on the randomly selected loan universe
  model_predictions	   <- lapply( model, function(x) h2o.predict(x, loan_universe) )
  names(model_predictions) <- names(model)

  # For each of the models, choose the N loans with the highest predicted probability of being current
  N				<- 20000
  selected_portfolio		<- lapply( model_predictions,     function(x) select_portfolio(x, loan_universe, N ) )

  # Summarize the 1-month portfolio loan performance for each portfolio (model)
  # It gives the percentage of the portfolio which is in each state at a 1 month time horizon
  # And compare with the naive model predictions
  summarize_portfolio_performance( loan_universe, selected_portfolio, type="Month", Result_location, method=NULL )  

  # Rank the probability of remaining current for each of the models and
  # plot the percentage of loans chosen against the number of loans not current - 1 Month
  pdf( file.path( Result_location, paste( "Portfolio Analysis - Model Comparison", model_id, "1 Month.pdf" ) ), paper="USr" )
    model_portfolio_comparison_plot( model_predictions, loan_universe, type="Month" )
  dev.off()

  # Predict on the loan pool data
  pool_prediction		<- lapply( model, function(x) predict_loan_pools( x, loan_pools, type="Month", method=method ) )

  # For each of the pool_by variables, count the number of observed vs predicted prepayments by pool
  pool_prepayment_summary	 <- lapply( pool_prediction, function(x) summarize_pool_prepayment_prediction(x, type="Month") )
  names(pool_prepayment_summary) <- names(model)
  
  # Plot the model comparison for each of the pool_by variables - 1 Month
  pdf( file.path( Result_location, paste( "Pool By Analysis - Model Comparison", model_id, "1 Month.pdf" ) ), paper="USr" )
    pool_by_plot(pool_prepayment_summary, type="Month")
  dev.off()


  if( do_1yr_analysis == TRUE )
  {
    # Make 1 year predictions
    model_predictions_1yr	 <- lapply( model, function(x) h2o_predict_1yr( x, loan_universe, nsims, method ) )
    names(model_predictions_1yr) <- names(model)

    # For each of the models, choose the N loans with the highest predicted probability of being current
    selected_portfolio_1yr	<- lapply( model_predictions_1yr, function(x) select_portfolio(x, loan_universe, N ) )

    # Summarize the 1-year portfolio loan performance for each portfolio (model)
    # It gives the percentage of the portfolio which is in each state at a 1 year time horizon
    summarize_portfolio_performance( loan_universe, selected_portfolio_1yr, type="Year",  Result_location, method )
    
    # Rank the probability of remaining current for each of the models and
    # plot the percentage of loans chosen against the number of loans not current - 1 Year
    pdf( file.path( Result_location, paste( "Portfolio Analysis - Model Comparison", model_id, method, "1 Year.pdf" ) ), paper="USr" )
      model_portfolio_comparison_plot( model_predictions_1yr, loan_universe, type="Year" )
    dev.off()

    # Predict on the loan pool data
    pool_prediction_1yr		<- lapply( model, function(x) predict_loan_pools( x, loan_pools, type="Year", nsims=nsims, method=method ) )

    # For each of the pool_by variables, count the number of observed vs predicted prepayments by pool
    pool_prepayment_summary_1yr	 <- lapply( pool_prediction_1yr, function(x) summarize_pool_prepayment_prediction(x, type="Year") )
    names(pool_prepayment_summary_1yr) <- names(model)

    # Plot the model comparison for each of the pool_by variables - 1 Year
    pdf( file.path( Result_location, paste( "Pool By Analysis - Model Comparison", model_id, method, "1 Year.pdf" ) ), paper="USr" )
      pool_by_plot(pool_prepayment_summary_1yr, type="Year")
    dev.off()

  } # end if

} # end for
