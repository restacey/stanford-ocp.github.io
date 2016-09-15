# Define the functions

get_mortgage_rate <- function( scenario )
{ # Get the mortgage rate values based on the scenario
  # These are the CCAR values in 2015Q4-2016Q4
  
  econ_variables	<- get_econ_variables( scenario )
  mortgage_rate		<- econ_variables$mortgage_rate
  return(mortgage_rate)

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

  return( list( mortgage_rate=mortgage_rate_inter[-1], mortgage_rate_ch=mortgage_rate_ch, unemployment_ch=unemployment_ch,
  		housing_ch=housing_ch, yr_appreciation_ch=yr_appreciation_ch ) )

} # end function


perc_format <- function(x)
{
  return( paste( signif(x,2), "%", sep="" ) )
} # end function


mill_format <- function(x)
{
  return( paste( round(x/1e6,1), "M", sep="" ) )
} # end function


plot_loss_distribution <- function( dat, name, actual, EAD, normalize )
{ # Plot the loss distribution
  # Input: dat is a column vector of numeric values
  #	   name is a character string for the plot title

  if( normalize == TRUE )
  {
    dat		<- copy(dat)
    dat		<- dat / EAD * 100
    actual	<- actual / EAD * 100
    x_str	<- paste( name, "Loss %" )
    title_str	<- paste( "Histogram of", name, "1 Year Loss %" )
    format_fn	<- perc_format
  } else
  {
    x_str	<- paste( name, "Loss" )
    title_str	<- paste( "Histogram of", name, "1 Year Loss" )
    format_fn	<- mill_format
  } # end if
  
  # Calculate the VaR and CVaR at 95% and 99% by year
  VaR95			<- quantile( dat, probs=0.95 )
  VaR99			<- quantile( dat, probs=0.99 )
  CVaR95		<- mean( dat[ dat >= VaR95 ] )
  CVaR99		<- mean( dat[ dat >= VaR99 ] )
  EL			<- mean( dat )		# expected loss
  UL			<- sd( dat )		# standard deviation

  # Get the percentile associated with the actual portfolio losses by year
  actual_loss		<- actual
  actual_loss_perc	<- ecdf(dat)( actual_loss )


  # Plot the Portfolio Loss distribution
  hist( dat, breaks="FD",
	  xlab=x_str, 
	  main=title_str )
          abline(v=VaR95, col="red", lwd=1.2)
          abline(v=VaR99, col="blue", lwd=1.2)
          abline(v=EL, col="green", lwd=1.2)
          abline(v=actual_loss, col="black", lwd=1.2)
          legend( x="topright", legend=c("VaR95", "VaR99", "EL", "Actual"), bty ="n",
	    lwd=1.2, col=c("red", "blue", "green", "black") )
	  legend( x="topright", legend=c("VaR95", "VaR99", "EL"), bty ="n",
	    lwd=1.2, col=c("red", "blue", "green") )
	  mtext( side=3, paste( "Actual Loss: ", format_fn(actual_loss),
				"   Actual Loss%: ", round(actual_loss_perc*100,2), "%",
	  			"   Mean: ", format_fn(EL),
				"   SD: ", format_fn(UL),
				"   VaR 95%: ", format_fn(VaR95),
				"   CVaR 95%: ", format_fn(CVaR95),
				"   VaR 99%: ", format_fn(VaR99),
				"   CVaR 99%: ", format_fn(CVaR99),
			 sep="" ) )

  hist( dat, breaks=sqrt(length(dat)),
	  xlab=x_str, 
	  main=title_str )
          abline(v=VaR95, col="red", lwd=1.2)
          abline(v=VaR99, col="blue", lwd=1.2)
          abline(v=EL, col="green", lwd=1.2)
          abline(v=actual_loss, col="black", lwd=1.2)
          legend( x="topright", legend=c("VaR95", "VaR99", "EL", "Actual"), bty ="n",
	    lwd=1.2, col=c("red", "blue", "green", "black") )
	  legend( x="topright", legend=c("VaR95", "VaR99", "EL"), bty ="n",
	    lwd=1.2, col=c("red", "blue", "green") )
	  mtext( side=3, paste( "Actual Loss: ", format_fn(actual_loss),
				"   Actual Loss%: ", round(actual_loss_perc*100,2), "%",
	  			"   Mean: ", format_fn(EL),
				"   SD: ", format_fn(UL),
				"   VaR 95%: ", format_fn(VaR95),
				"   CVaR 95%: ", format_fn(CVaR95),
				"   VaR 99%: ", format_fn(VaR99),
				"   CVaR 99%: ", format_fn(CVaR99),
			 sep="" ) )

  # Return the summary values
  return( c( VaR95=VaR95, VaR99=VaR99, CVaR95=CVaR95, CVaR99=CVaR99, EL=EL, UL=UL,
		actual_loss=actual_loss, actual_loss_perc=actual_loss_perc ) )

} # end function

##########################################################################################################################################################################
# Analyze the simulated loss distribution
# Note: Need to run 'fannie_mae_code - ECAP Model - Preprocessed.r' before running this code


# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1

# Define the economic scenario to analyze
scenario	<- "baseline"	# "none", "baseline", "adverse", or "severly_adverse"

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )


# Load the required libraries
chooseCRANmirror(ind=0)
if (!(require(data.table))) install.packages ("data.table")

# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
Result_location	<- ifelse( location == "Server", file.path( file_location, "Results" ), file.path( file_location, "Results" ) )
val_location	<- file.path( Result_location, "Validation Data" )
loss_location	<- file.path( file_location, paste( "Loss_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Loss_location	<- file.path( Result_location, "Loss DeepLearning_model_R_1472601160088_1", scenario )
ECAP_location	<- file.path( Result_location, "ECAP", "Capital Results DeepLearning_model_R_1472601160088_1", scenario )

# Create the ECAP folder if it does not already exist
dir.create( ECAP_location, showWarnings = FALSE )


# Load the validation data (the true observations)
load( file.path( val_location, "validation_data.RData" ) )	# validation_data
val_total_UPB	<- sum( validation_data$LAST_UPB )

# Load the loss data
load( file.path( loss_location, "Loss_Data.RData" ) )		# Loss_Data

# Compute the observed losses from foreclosure
foreclosure_data <- subset( Loss_Data, ( LOAN_STATUS_1yr == 'F' ) & ( Obs_Year %in% unique( validation_data$Obs_Year ) ) )
val_total_LGD	 <- sum( foreclosure_data$NET_LOSS ) 

# Compute the observed losses from modified loans (assumes modified in 6th month)
#mortgage_rate	   <- get_mortgage_rate( scenario )
#modified_data	   <- subset( validation_data, ( LOAN_STATUS_1yr == 'Y' ) & ( Obs_Year %in% unique( validation_data$Obs_Year ) ) )
#val_total_modified <- sum( with( modified_data, pmax( 6 * ( LAST_RT - mean(mortgage_rate) ) / 1200 * LAST_UPB, 0 ) ) )
#val_total_modified <- sum( with( modified_data, pmax( ( 13 - as.numeric(Obs_Month) ) * ( LAST_RT - mortgage_rate[as.numeric(Obs_Month)] ) / 1200 * LAST_UPB, 0 ) ) )
#val_total_modified <- sum( with( modified_data, pmax( ( 13 - as.numeric(Obs_Month) ) * ( LAST_RT - mean(mortgage_rate) ) / 1200 * LAST_UPB, 0 ) ) )
load( file.path( val_location, "validation_modified_cost.RData" ) )		# Modified
val_total_modified <- Modified

# Compute the observed delinquency costs (assumes delinquency only goes in one direction)
#delinquent_data	<- subset( validation_data, ( LOAN_STATUS %in% c( as.character(1:4), 'C' ) ) & ( LOAN_STATUS_1yr %in% as.character(1:4) ) )
#delinquent_data[, LOAN_STATUS:= ifelse( LOAN_STATUS == 'C', 0, as.numeric(LOAN_STATUS) ) ]
#delinquent_data[, LOAN_STATUS_1yr:= as.numeric(as.character(LOAN_STATUS_1yr)) ]
#num_mon_delinquent   <- pmax( delinquent_data$LOAN_STATUS_1yr - delinquent_data$LOAN_STATUS, 0 )
#val_total_delinquent <- sum( num_mon_delinquent * delinquent_data$LAST_RT / 1200 * delinquent_data$LAST_UPB )
load( file.path( val_location, "validation_delinquent_cost.RData" ) )		# Delinquent
val_total_delinquent <- Delinquent

# Store the summary values
val_total	<- val_total_LGD + val_total_modified + val_total_delinquent
val_summary	<- c( val_total_UPB=val_total_UPB, val_total_LGD=val_total_LGD, 
		      val_total_modified=val_total_modified, val_total_delinquent=val_total_delinquent,
		      val_total=val_total )


# Store the simulated loss distribution file names
fnames		<- list.files( Loss_location, pattern = glob2rx( paste( "Loss*", scenario, "*.RData", sep="") ), full.names=TRUE )
n_files		<- length(fnames)
n_files		<- min( n_files, 1000 )


# Load the simulate data
print( "Loading Loss Simulation Data..." )
if( file.exists( file.path( ECAP_location, "loss_summary_list.RData" ) ) )
{
  load( file.path( ECAP_location, "loss_summary_list.RData" ) )		# loss_summary_list
} else
{
  loss_summary_list <- vector( mode="list", length=n_files )
  for( i in 1:n_files )
  {
    print( paste( "Loading File", i, "..." ) )
    load( fnames[i] )	# loss
    loss_summary_list[[i]] <- colSums( loss, na.rm=TRUE )

  } # end for
  save( loss_summary_list, file=file.path( ECAP_location, "loss_summary_list.RData" ) )

} # end if
print( "Finished Loading Loss Simulation Data..." )

# Merge the data into a single data.table and calculate the total loss
sim_1yr_loss	<- do.call( rbind, loss_summary_list )
sim_1yr_loss	<- as.data.table( sim_1yr_loss )
sim_1yr_loss[, Total:= Loss + Modified + Delinquent ]

# Plot the loss distributions
pdf( file.path( ECAP_location, paste( "Loss Distribution - ", scenario, ".pdf", sep="" ) ), height=8, width=15 )
  plot_loss_distribution( dat=sim_1yr_loss$Loss, name="Foreclosure", actual=val_total_LGD, EAD=val_total_UPB, normalize=FALSE )
  plot_loss_distribution( dat=sim_1yr_loss$Modified, name="Modified", actual=val_total_modified, EAD=val_total_UPB, normalize=FALSE )
  plot_loss_distribution( dat=sim_1yr_loss$Delinquent, name="Delinquent", actual=val_total_delinquent, EAD=val_total_UPB, normalize=FALSE )
  plot_loss_distribution( dat=sim_1yr_loss$Total, name="Total", actual=val_total, EAD=val_total_UPB, normalize=FALSE )
dev.off()

pdf( file.path( ECAP_location, paste( "Loss Distribution Normalized - ", scenario, ".pdf", sep="" ) ), height=8, width=15 )
  plot_loss_distribution( dat=sim_1yr_loss$Loss, name="Foreclosure", actual=val_total_LGD, EAD=val_total_UPB, normalize=TRUE )
  plot_loss_distribution( dat=sim_1yr_loss$Modified, name="Modified", actual=val_total_modified, EAD=val_total_UPB, normalize=TRUE )
  plot_loss_distribution( dat=sim_1yr_loss$Delinquent, name="Delinquent", actual=val_total_delinquent, EAD=val_total_UPB, normalize=TRUE )
  plot_loss_distribution( dat=sim_1yr_loss$Total, name="Total", actual=val_total, EAD=val_total_UPB, normalize=TRUE )
dev.off()
