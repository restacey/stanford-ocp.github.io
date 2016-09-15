# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "zoo" version 1.7-7

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 

# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Set the time horizon (in months)
time_horizon	<- 1

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )

# Set the number of batches (number of splits in each monthly file)
num_batches		<- 5		#### NOTE: CAN INCREASE THIS IF MEMORY IS A PROBLEM ####

# Load Necessary Packages for this analysis
chooseCRANmirror(ind=0)
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")
if (!(require(plyr))) install.packages ("plyr")
ifelse( location == "Server", source( "Fannie_Mae_Loan_Level_Data/RCode/fannie_mae_code - glm Model Functions.r" ), source( "R Code/fannie_mae_code - glm Model Functions.r" ) )

 
# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website at https://loanperformancedata.fanniemae.com/lppub/index.html.
# For more detail please refer to the accompanied presentation. After downloading the files you will need to unzip the files. 
# Though read.table function in R can read zipped files, we have used the "fread" function from data.table package 
# to read these files for efficiency and speed. Unfortunately, fread cannot read zipped files.
# While this program will run with any number of pairs of files, we encourage users to download the entire set of Acquisition and Performance 
# files. The naming of the files should remain the same after download and unzipping process so that the files are saved in order. 
# This program will create the monthly state transition matrices. The final matrices will be saved to the "Results" folder.
# The folder will be created if it does not already exist.


# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
# Note: the unemployment location should be where the AGI data is also located
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
data_location	<- file.path( file_location, "Data" )
trans_location	<- file.path( file_location, paste( "Monthly_Trans_", time_horizon, sep="" ) )
Result_location	<- file.path( file_location, "Results" )

# Create the output folder if it does not already exist
dir.create( trans_location, showWarnings = FALSE )
dir.create( Result_location, showWarnings = FALSE )

# Check the number of files downloaded (should be even, equal number of Acquisition and Performance Files).
numberoffiles	<- length( list.files( data_location, pattern = glob2rx("*txt"), full.names=TRUE) )

# The "foreach" package contructs a loop so that R can iterate through all pairs of related Acquisition and Performance files.
# Calculate the number of iterations/cores in parallel processing allowing each pair to be processed simultaneously.
numberofloops	<- (numberoffiles/2)


states	<- c( 'C', 1:4, 'Y', 'P', 'R', 'F' )
trans_list <- foreach(file_num=1:numberofloops, .inorder=FALSE,
           		.packages=c("data.table", "zoo")) %dopar% 
{
  # Define Performance variables and classes, and read the files into R.
  Performance_Variables = c("LOAN_ID", "Monthly.Rpt.Prd", "Servicer.Name", "LAST_RT", "LAST_UPB", "Loan.Age", "Months.To.Legal.Mat",
                         "Adj.Month.To.Mat", "Maturity.Date", "MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code", 
                         "ZB_DTE", "LPI_DTE", "FCC_DTE","DISP_DT", "FCC_COST", "PP_COST", "AR_COST", "IE_COST", "TAX_COST", "NS_PROCS",
                         "CE_PROCS", "RMW_PROCS", "O_PROCS", "NON_INT_UPB", "PRIN_FORG_UPB")

  Performance_ColClasses = c("character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
                         "character", "character", "character", "character", "character", "character", "character", "character",
                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

  Performance <- list.files(data_location, pattern = glob2rx("*Performance*txt"), full.names=TRUE)


  # Read and Process the Performance loan id data (1st column)
  Data_P 		<- fread( Performance[file_num], sep = "|", colClasses = Performance_ColClasses, showProgress=0, select=1 )
  setnames(Data_P, "LOAN_ID")
  setkey(Data_P, "LOAN_ID")

  # Store the unique loan ids
  unique_IDs		<- unique( Data_P[,LOAN_ID] )
  unique_IDs_split	<- vector( mode="list", length=num_batches )
  id_breaks		<- ceiling( seq( from=0, to=length(unique_IDs), length.out=(num_batches+1) ) )
  for( i in 2:length(id_breaks) )
  {
    unique_IDs_split[[i-1]] <- unique_IDs[ ( id_breaks[i-1] + 1 ) : id_breaks[i] ]
  }

  # Get the row numbers for the unique loan ID splits
  ending_ids		<- sapply( unique_IDs_split, function(x) tail(x,1) )
  split_id_rows 	<- Data_P[,.N, by=LOAN_ID]
  split_id_rows		<- split_id_rows[, end:= cumsum(N)][, N := NULL]
  split_id_rows		<- subset( split_id_rows, LOAN_ID %in% ending_ids )
  split_id_rows[1,"start"] <- 1
  for( i in 2:num_batches )
  {
    split_id_rows[i,"start"] 	<- split_id_rows[i-1,end] + 1 
  }
  split_id_rows	<- split_id_rows[, nrows:= end - start + 1 ]
  rm(Data_P); gc()


  # Split the Performance data into batches based on the loan ids to help with memory management
  temp_trans	<- vector( mode="list", length=num_batches )
  for( split_num in 1:num_batches )
  {
    # Read and Process the Performance loan data
    col_names_keep	<- c("LOAN_ID", "Monthly.Rpt.Prd", "Loan.Age", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code")
    col_ind		<- which( Performance_Variables %in% col_names_keep )
    Data_P 		<- fread( Performance[file_num], sep = "|", colClasses = Performance_ColClasses, showProgress=0, select=col_ind,
  				  nrows=split_id_rows[split_num,nrows], skip=(split_id_rows[split_num,start]-1) )
    setnames(Data_P, Performance_Variables[col_ind] )
  
    # Sort data by Loan ID and Monthly Reporting Period
    setorderv(Data_P, c("LOAN_ID", "Monthly.Rpt.Prd"))
    setkey(Data_P, "LOAN_ID")

    # Standardize Delinquency Status Codes
    Data_P[, "Delq.Status":= as.numeric( ifelse( Delq.Status=="X" | is.na(Delq.Status), "999", Delq.Status) ) ]

    # Define the last status of a loan (current, months delinquent, paid-off, foreclosure, etc.)
    # Note: Treats modified loans (Modified=Y) as an absorbing state
    # Note: Since none of the loans matured, that state is ignored (in pre-payments)
    Data_P[, LOAN_STATUS:= 
           ifelse(MOD_FLAG=='Y','Y',ifelse(Zero.Bal.Code=='01','P',ifelse(Delq.Status >4,'4', ifelse(Zero.Bal.Code=='06', 'R', ifelse(Zero.Bal.Code %chin% c('03', '09'), 'F', ifelse(Delq.Status=='999','X', ifelse(Delq.Status==0, 'C', as.character(Delq.Status)))))))),
        ]
 
    # Check if there are any missing months
    # Data_P[,Max_month_diff:=max(diff(Loan.Age)),by=LOAN_ID]
    # range(Data_P$Max_month_diff)
    # table(Data_P$Max_month_diff)
    # View( subset( Data_P, Max_month_diff > 1 ) )
    # Note: there are missing months in the data

    # Add in the missing missing months
    temp	<- Data_P[, min(Loan.Age):max(Loan.Age), by=LOAN_ID]
    setnames(temp, c("LOAN_ID","Loan.Age") )
    setkeyv(temp,  c("LOAN_ID","Loan.Age") )
    setkeyv(Data_P, c("LOAN_ID","Loan.Age") )
    Data_P	<- merge( x=Data_P, y=temp, all=TRUE )
    setorderv(Data_P, c("LOAN_ID", "Loan.Age"))

    # Confirm all of the missing months are added
    # Note: should return c(1,1) (it does)
    # range( Data_P[,max(diff(Loan.Age)),by=LOAN_ID]$V1 ) 

    # Store the next time horizon'sloan status
    Data_P[, LOAN_STATUS_HORIZON:= shift(LOAN_STATUS, n=time_horizon, type="lead"), by=LOAN_ID ]

    # Drop the unneeded columns
    Data_P[, c("Loan.Age", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code"):=NULL ]

    # Subset to the data with a current and time horizon loan status (and a known status)
    Data_P	<- subset( Data_P, ( !is.na(LOAN_STATUS) & !is.na(LOAN_STATUS_HORIZON) & 
    				!(LOAN_STATUS=='X') & !(LOAN_STATUS_HORIZON=='X') ) )

    # Define the state variables
    Data_P[, LOAN_STATUS:=factor(LOAN_STATUS, levels=states, exclude=NULL) ]
    Data_P[, LOAN_STATUS_HORIZON:=factor(LOAN_STATUS_HORIZON, levels=states, exclude=NULL) ]

    # Compute the transition frequency count by monthly report period
    trans_count	<- Data_P[, table( LOAN_STATUS, LOAN_STATUS_HORIZON ), by=Monthly.Rpt.Prd ]

    # Delete the performance data from memory
    rm(Data_P); gc()

    # Get the transitions for each monthly report period
    monthly_pd	<- unique( trans_count$Monthly.Rpt.Prd )
    trans_count	<- matrix( trans_count$V1, nrow=length(monthly_pd), byrow=TRUE )	# rows are monthly report periods, columns are loan states
    
    # Store the transitions
    trans_count		<- as.data.frame(trans_count)
    trans_count$Month	<- monthly_pd
    temp_trans[[split_num]] <- trans_count

  } # end for

  # Row bind the results
  temp_trans	<- rbindlist(temp_trans)
  
  # Sum the rows by monthly report period
  state_cols	<- setdiff( colnames(temp_trans), "Month" )
  temp_trans	<- ddply( temp_trans, .(Month), function(x) colSums(x[state_cols]) )
  
  # Return the monthly transition frequencies
  save( temp_trans, file=file.path(trans_location, paste("Monthly_Trans_", file_num, ".RData", sep="") ) )
  return(temp_trans)

} # end for each

# Row bind the results
trans_table	<- rbindlist(trans_list)
  
# Sum the rows by monthly report period
state_cols	<- setdiff( colnames(trans_table), "Month" )
trans_table	<- ddply( trans_table, .(Month), function(x) colSums(x[state_cols]) )
  
# Convert into a transition frequency table (for each reporting month)
months		<- trans_table$Month
monthly_trans_freq		<- vector( mode="list", length=length(months) )
names(monthly_trans_freq) 	<- months
trans_table$Month		<- NULL
for( k in 1:length(months) )
{
  temp				<- matrix( trans_table[k,], nrow=length(states) )
  rownames(temp) 		<- states
  colnames(temp)		<- states
  monthly_trans_freq[[k]] 	<- temp
} 

# Calculate the transition probability (for each reporting month)
monthly_trans_prob	<- monthly_trans_freq
for( k in 1:length(monthly_trans_prob) )
{
  for( j in 1:nrow(monthly_trans_prob[[k]]) )
  {
    temp			<- as.numeric( monthly_trans_freq[[k]][j,] )
    monthly_trans_prob[[k]][j,]	<- temp / sum(temp) 
  } # end inner for
} # end outer for

# Save the monthly transition frequencies and probabilities
save( monthly_trans_freq, file=file.path(trans_location, "Monthly_Trans_Freq_Tables.RData") )
save( monthly_trans_prob, file=file.path(trans_location, "Monthly_Trans_Prob_Tables.RData") )

# Convert the monthly transition probability tables into a single data frame
monthly_trans_prob_df	<- list()
for( k in 1:length(monthly_trans_prob) )
{
  monthly_trans_prob[[k]]			<- as.data.frame(monthly_trans_prob[[k]])
  monthly_trans_prob[[k]]$Monthly.Rpt.Prd	<- names(monthly_trans_prob)[k]
  monthly_trans_prob[[k]]$LOAN_STATUS		<- rownames(monthly_trans_prob[[k]])
  monthly_trans_prob_df				<- rbind( monthly_trans_prob_df, monthly_trans_prob[[k]] )
}

# Drop any rows where there is no transition
row_sum						<- apply( monthly_trans_prob_df[,states], 1, function(x) sum( as.numeric(x) ) )
monthly_trans_prob_df				<- subset( monthly_trans_prob_df, !is.na(row_sum) )

# Drop any columns that are never transitioned to
col_sum						<- apply( monthly_trans_prob_df[,states], 2, function(x) sum( as.numeric(x), na.rm=TRUE ) )
zero_cols					<- colnames( monthly_trans_prob_df[,states] )[ which(col_sum==0) ]
keep_cols					<- setdiff( colnames(monthly_trans_prob_df), zero_cols )
monthly_trans_prob_df				<- monthly_trans_prob_df[,keep_cols]

# Clarify the column names
colnames(monthly_trans_prob_df)[ colnames(monthly_trans_prob_df) %in% states ] <- paste( "Horizon", states, sep="_" )[ colnames(monthly_trans_prob_df) %in% states ]

# Save the monthly transition probabilities
save( monthly_trans_prob_df, file=file.path(Result_location, paste( "Monthly_Trans_Probabilities_TimeHorizon", time_horizon, ".RData", sep="" ) ) )
