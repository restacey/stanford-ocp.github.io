# Define the functions

get_mortgage_rate <- function()
{ # Get the 2012 mortgage rate values
  # These are the CCAR (historical) values in 2011Q4-2012Q4

  # Store the historical quarterly values  
  mortgage_rate	<- c( 4.0, 3.9, 3.8, 3.5, 3.4 )

  # Interpolate the quarterly values to be monthly
  m 			<- c( 0, 3, 6, 9, 12 )	# indices for the months
  mortgage_rate_inter	<- approx( x=m, y=mortgage_rate, xout=0:12, method="linear" )$y
  
  return( mortgage_rate_inter[-1] )

} # end function


###################################################################################################################################################

# Output the validation data total delinquency cost
# Note: Need to run 'fannie_mae_code - Model Setup Data.r' before running this code

# Run the code
# Note: Need to run 'fannie_mae_code - Model Setup Data.r' before running this code

# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )

# Load the required libraries
chooseCRANmirror(ind=0)
if (!(require(h2o))) install.packages ("h2o")
if (!(require(data.table))) install.packages ("data.table")
h2o.init(nthreads=-1, max_mem_size='1000g')


# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
chunk_location	<- file.path( file_location, paste( "Chunk_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- file.path( file_location, "Results/Validation Data" )

# Create the Validation folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Get the monthly batched data table files names
fnames		<- list.files( chunk_location, pattern = glob2rx("Combined_Data*csv"), full.names=TRUE)

# Load the data into h2o
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
  data.hex 	<- h2o.importFile(path=fnames, header=TRUE, sep=",", col.names=col_names, col.types=col_types)
print( proc.time() - ptm )
print( "Finished Importing Data" )
h2o.clusterStatus()
as.data.frame( h2o.table(data.hex$Obs_Year) )
h2o.describe(data.hex)

# Define Repuchased as Prepaid
data.hex$LOAN_STATUS_HORIZON	<- ifelse( data.hex$LOAN_STATUS_HORIZON == 'R', 'P', data.hex$LOAN_STATUS_HORIZON )

# Split between training and validation set
print( "Splitting between Train and Validate..." )
cut_yr		<- 2011
max_yr		<- 2012		# Note: the MBS data starts in 2013
#train.hex	<- data.hex[ data.hex$Obs_Year <= cut_yr, ]
val.hex		<- data.hex[ ( data.hex$Obs_Year > cut_yr ) & ( data.hex$Obs_Year <= max_yr ), ]
#print( paste( "Train Dim:", dim( train.hex ) ) )
print( paste( "Validate Dim:", dim( val.hex ) ) )
h2o.rm(data.hex); gc(); h2o.clusterStatus()



# Calculate the validation delinquency cost
is_delinquent	<- ifelse( ( val.hex$LOAN_STATUS_HORIZON %in% as.character(1:4) ) & ( val.hex$LOAN_STATUS == 'C' ), TRUE,
		   ifelse( ( val.hex$LOAN_STATUS_HORIZON %in% as.character(2:4) ) & ( val.hex$LOAN_STATUS == '1' ), TRUE,
 		   ifelse( ( val.hex$LOAN_STATUS_HORIZON %in% as.character(3:4) ) & ( val.hex$LOAN_STATUS == '2' ), TRUE,
			   ( val.hex$LOAN_STATUS == '3' ) & ( val.hex$LOAN_STATUS_HORIZON == '4' ) ) ) )
Delinquent	<- sum( ifelse( is_delinquent, val.hex$LAST_RT / 1200 * val.hex$LAST_UPB, 0 ), na.rm=TRUE )


# Write to file
print( "Writing Delinquent Cost to File..." )
save( Delinquent, file=file.path( Result_location, "validation_delinquent_cost.RData" ) )


# Calculate the validation modification cost
# Note: mortgage_rate are the mortgage rate values from the CCAR paper in 2012 (historical values)
mortgage_rate	<- get_mortgage_rate()
modified_data	<- val.hex[ val.hex$LOAN_STATUS_HORIZON == 'Y', ]
modified_data	<- as.data.table(modified_data)
Obs_Month	<- as.numeric( modified_data$Obs_Month )
Modified	<- sum( pmax( ( 13 - Obs_Month ) * ( modified_data$LAST_RT - mortgage_rate[Obs_Month] ) / 1200 * modified_data$LAST_UPB, 0 ), na.rm=TRUE )

# Write to file
print( "Writing Modified Cost to File..." )
save( Modified, file=file.path( Result_location, "validation_modified_cost.RData" ) )
