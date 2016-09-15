# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "tree" version 1.0-37
# package "gamlss" version 4.3-8
# package "corrgram" version 1.9

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 

# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# You will need to run "fannie_mae_code - Model Setup Data.r" before running this code.
# The program will read in the loss data and estimate an LGD model
# The model will be saved to the "Results" folder.The folder will be created if it does not already exist.


# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )


# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
# Note: the unemployment location should be where the AGI, Mortgage_CBSA_Panel, and SBA data is also located
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
loss_location	<- file.path( file_location, paste( "Loss_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- file.path( file_location, "Results" )

# Load Necessary Packages for this analysis
chooseCRANmirror(ind=0)
#if (!(require(corrgram))) install.packages ("corrgram")
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(tree))) install.packages ("tree")
if (!(require(gamlss))) install.packages ("gamlss")
ifelse( location == "Server", source( "Fannie_Mae_Loan_Level_Data/RCode/fannie_mae_code - LGD Model Functions.r" ), source( "R Code/fannie_mae_code - LGD Model Functions.r" ) )

 
# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )
Result_location	<- file.path( Result_location, "LGD" )
dir.create( Result_location, showWarnings = FALSE )


# Store the loss file names
loss_fnames	<- list.files( loss_location, pattern = glob2rx("*csv"), full.names=TRUE )
n_files		<- length( loss_fnames )


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


##############################################################################################################################################################
# Estimate LGD model using Inflated Beta distribution

# NOTE: LGD == NET_SEV
#	NET_SEV IS DEFINED AS NET_LOSS / LAST_UPB
#	NET_LOSS == LAST_UPB + INT_COST + total_expense - total_proceeds

# Just used foreclosures for now, the modified loan LGD definition should be different
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
#not_used	<- c( 'Obs_Year','Monthly.Rpt.Prd','STATE','LOAN_ID','MOD_FLAG','Zero.Bal.Code','ZB_DTE',
#		      'LPI_DTE','FCC_DTE','DISP_DT','FCC_COST','PP_COST','AR_COST','IE_COST','TAX_COST','NS_PROCS','CE_PROCS','RMW_PROCS',
#		      'O_PROCS','NON_INT_UPB','PRIN_FORG_UPB','LOAN_STATUS_HORIZON','LOAN_STATUS_1yr',
#		      'Fin_UPB','modfg_cost','C_modir_cost','C_modfb_cost','lpi2disp','zb2disp','INT_COST','total_expense',
#		      'total_proceeds','NET_LOSS','NET_SEV','Total_Cost','Tot_Procs','Tot_Liq_Ex','LAST_DTE','MODIR_COST','MODFB_COST','MODTOT_COST' )
model_x		<- c( "VinYr", "Loan.Age", "LAST_UPB", "ORIG_CHN", "LAST_RT", "ORIG_AMT", "OCLTV", "NUM_BO", "DTI", "FTHB_FLG", "PURPOSE",
   			"PROP_TYP", "NUM_UNIT", "OCC_STAT", "SATO", "CSCORE_MN", "ORIG_VAL", "perc_delinquent", "perc_foreclosure", "foreclosure_delinquent_ratio",
   			"CUR_VAL", "CUR_1Yr_Appreciation", "Unemployment", "Judicious", "Per_Paid", "Cum_Underwater", "Cum_Delinquent", "Loan.Age_NA", "LAST_UPB_NA",
   			"ORIG_AMT_NA", "OCLTV_NA", "DTI_NA", "SATO_NA", "CSCORE_MN_NA", "ORIG_VAL_NA",  
   			"Unemployment_NA", "Per_Paid_NA", "Cum_Underwater_NA", "Cum_Delinquent_NA", "perc_delinquent_NA", 
			"SBA_DefRate_Combo_NA", "CUR_LTV", "Exp_Loss" )  
y_val		<- "NET_SEV"
# Note: since all of the loans are 'F', do not use LOAN_STATUS as a variable

# Count the number of observations per year
defaults_by_year		<- Loss_Data[, nrow(.SD), by=Obs_Year ]
setnames(defaults_by_year, c("Obs_Year", "NumDefaulted") )
setorder(defaults_by_year, Obs_Year)
defaults_by_year[, cumPerc := cumsum(NumDefaulted) / sum(NumDefaulted) ]
defaults_by_year

# Plot the LGD data
#plot_LGD_data( Loss_Data, LGD_name=y_val, model_x, Result_location )


# Train and Analyze the Beta Inflated LGD model allowing the starting time window to vary
# Uses stepTGDAll.A for selecting the model parameters
# Uses Beta Inflated (BEINF) which puts positive mass at 0 and 1
max_train_yr			<- 2011
max_val_yr			<- 2012
p_max				<- 0.1
BEINF_LGD_results		<- analyze_BEINF_LGD( Loss_Data, y_val=y_val, model_x, max_train_yr, max_val_yr, p_max, Result_location )
save( BEINF_LGD_results, file=file.path( Result_location, "BEINF_LGD_results.RData" ) )

