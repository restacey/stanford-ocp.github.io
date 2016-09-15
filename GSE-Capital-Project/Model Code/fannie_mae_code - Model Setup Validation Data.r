# Output the validation data
# Note: Need to run 'fannie_mae_code - Model Setup Data.r' before running this code
# Note: fwrite() requires the development data.table package

# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1		# this is the transition frequency (the ECAP time horizon is 1yr)

# Choose the validation year
val_year	<- 2012

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )


# Load the required libraries
chooseCRANmirror(ind=0)
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
h2o.init(nthreads=-1, max_mem_size='1500g')

# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
chunk_location	<- file.path( file_location, paste( "Chunk_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- file.path( file_location, "Results/Validation Data" )

# Create the ECAP folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )


# Get the monthly batched data table files names
fnames		<- list.files( chunk_location, pattern = glob2rx("Combined_Data*csv"), full.names=TRUE)
if( location == "Personal" )
{
  fnames <- fnames[1]
}
n_files		<- length( fnames )

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
col_types	<- c( 'integer','factor','factor','factor','string','factor','integer','numeric','numeric','factor','factor','factor',
		      'factor','factor','factor','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','factor','factor','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','numeric','factor',
		      'factor','numeric','integer','integer','integer','factor','integer','factor','factor','factor','factor','factor',
		      'numeric','integer','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric',
		      'numeric','numeric','numeric','numeric','numeric','numeric','factor','numeric','integer',
		      'integer','factor','factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor','factor','factor',
		      'factor','factor','factor','factor','factor',
		      'factor' )


print( "Getting Validation Data...")
ptm <- proc.time()
  val_list <- foreach(file_num=1:n_files, .inorder=FALSE,
           		.packages=c("data.table")) %dopar% 
  {
    dat 		<- fread( fnames[file_num], sep = ",", colClasses=col_types, header=TRUE, showProgress=0)
    dat			<- subset( dat, ( Obs_Year == val_yr ) & ( Obs_Month == '01' ) & !is.na(LOAN_STATUS_1yr) & ( LOAN_STATUS_1yr != 'X' ) )
    return(dat)
  } # end for each
print( proc.time() - ptm )
print( "Finished Importing Data" )

# Merge to a single data.table
print( "Binding Validation Data..." )
validation_data	<- rbindlist( val_list, use.names=TRUE, fill=TRUE )

# Write to file
print( "Writing to File..." )
fwrite( validation_data, file.path( Result_location, "validation_data.csv" ), turbo=FALSE )
save( validation_data, file=file.path( Result_location, "validation_data.RData" ) )

