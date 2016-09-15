create_variable_profile_plot <- function( dat, model, x_variables )
{ # Create Model Variable Profile Plots
  # Will have a line plot for each of the loan states
  
  # Get the predicted probability of transitioning to each of the loan states
  predicted_vals	<- h2o.predict( model, dat )
predicted_vals[,"R"] <- NULL

  # Get the x-variable and values
  variable		<- as.character( unlist( as.data.frame( dat$Variable[1] ) ) )
  x_vals		<- unlist( as.data.frame( dat[,variable] ) )
  
  # Get the y-values
  y_col_names		<- setdiff( colnames(predicted_vals), "predict" )
  y_vals		<- predicted_vals[,y_col_names]

  # Define the y labels
  y_labels		<- y_col_names
  y_labels[ which(y_labels=="p1") ]	<- "1 Month Delinquent"
  y_labels[ which(y_labels=="p2") ]	<- "2 Months Delinquent"
  y_labels[ which(y_labels=="p3") ]	<- "3 Months Delinquent"
  y_labels[ which(y_labels=="p4") ]	<- "4 Months Delinquent"
  y_labels[ which(y_labels=="C") ]	<- "Current"
  y_labels[ which(y_labels=="P") ]	<- "Prepaid"
  y_labels[ which(y_labels=="Y") ]	<- "Modified"
  y_labels[ which(y_labels=="F") ]	<- "Foreclosed"

  # Plot the variable profile
  if( variable %in% x_variables )
  {
    op <- par(mfrow = c(3, 3) )
    for( i in 1:ncol(y_vals) )
    {
      plot( x=x_vals, y=unlist( as.data.frame(y_vals[,i]) ), type="l", 
	    xlab=variable, ylab=paste( "Predicted Probability:", y_labels[i] ) )
    }
    par(op)
  } # end if
  invisible(NULL)

} # end function

###############################################################################################################################################################################
# Create Model Variable Profile Plots
# Note: Need to run 'fannie_mae_code - Model Estimation.r' before running this code

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
h2o.init(nthreads=-1, max_mem_size='1000g')

# Set the file locations
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
chunk_location	<- file.path( file_location, paste( "Chunk_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- ifelse( location == "Server", file.path( file_location, "Results" ), file.path( file_location, "Results/Model Results 6" ) )

# Select the models to analyze (should be in the "Results" folder)
#model_id	<- list.files( Result_location, pattern = glob2rx("*_model_R*"), full.names=FALSE)
#model_id	<- c( model_id, list.files( Result_location, pattern = glob2rx("*_grid_model_*"), full.names=FALSE) )
#model_id	<- grep( ".pdf", model_id, value=TRUE, invert=TRUE )
model_id 	<- c( "DeepLearning_model_R_1472601160088_1", "GLM_model_R_1472759680147_1" )

# Load the models to analyze and store which variables are used in the prediction
models		<- lapply( model_id, function(x) h2o.loadModel( file.path( Result_location, x ) ) )
x_variables	<- lapply( models, function(m) m@parameters$x )


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


# Get the average value for numeric columns and the most common value for factors
base_values	<- data.frame( matrix( NA, nrow=1, ncol=ncol(data.hex) ) )
range_values	<- vector( mode="list", length=ncol(data.hex) )
for( i in 1:ncol(data.hex) )
{
  if( col_types[i] == "enum" )
  {
    temp		<- h2o.table( data.hex[,i] )
    base_values[i]	<- as.character( unlist( as.data.frame( temp[ temp$Count == max(temp$Count), 1] ) ) )	# Most Common Value
    range_values[[i]]	<- h2o.levels(data.hex, i)								# Factor Levels
  } else if( col_types[i] %in% c('real', 'int') )
  {
    base_values[i]	<- h2o.mean( data.hex[,i], na.rm=TRUE )
    range_values[[i]]	<- range( data.hex[,i], na.rm=TRUE )
  } # end if
 
} # end for
colnames(base_values)   <- col_names
names(range_values)	<- col_names


# Set the NA indicators to zero
base_values[ grep("NA", col_names ) ]	<- 0

# Save the base and range values
save( base_values,  file=file.path( Result_location, "Base_Values.RData" ) )
save( range_values, file=file.path( Result_location, "Range_Values.RData" ) )
#load( file.path( Result_location, "Base_Values.RData" ) )
#load( file.path( Result_location, "Range_Values.RData" ) )

# Make table of values to predict on
vals_per_numeric	<- 100
predict_data		<- vector( mode="list", length=length(col_names) )
for( i in 1:length(col_names) )
{
  temp			<- base_values
  if( col_types[i] == "enum" )
  {
    if( !is.null(range_values[[i]]) )
    {
      n				<- length( range_values[[i]] )
      temp			<- temp[ rep(1,n), ]
      temp[,col_names[i]]	<- range_values[[i]]
    } # end inner if

  } else if( col_types[i] %in% c('real', 'int') )
  {
    if( !is.na(range_values[[i]][1]) )
    {
      n				<- vals_per_numeric
      temp			<- temp[ rep(1,n), ]
      temp[,col_names[i]]	<- seq( from=range_values[[i]][1], to=range_values[[i]][2], length.out=n )
    } # end inner if

  } # end if
  temp$Variable		<- col_names[i]
  predict_data[[i]]	<- temp

} # end for
predict_data		<- do.call( rbind, predict_data ) 

# Correct the data types
for( i in 1:length(col_names) )
{
  if( col_types[i] == "enum" )
  {
    predict_data[, col_names[i] ] <- as.factor( predict_data[, col_names[i] ] )
  } else if( col_types[i] %in% c('real', 'int') )
  {
    predict_data[, col_names[i] ] <- as.numeric( predict_data[, col_names[i] ] )
  } # end if

} # end for
predict_data		<- as.h2o(predict_data)


# Make the Model Variable Profile Plots
for( k in 1:length(model_id) )
{
  pdf( file.path( Result_location, paste( model_id[k], "Variable Profile Plots.pdf" ) ) )
    for( x in x_variables[[k]] )
    {
      #dat	<- h2o.assign(predict_data[ predict_data$Variable == x,], "dat.hex")
      create_variable_profile_plot(predict_data[ predict_data$Variable == x,], models[[k]], x_variables[[k]])
    } # end inner for
  dev.off()
} # end outer for

