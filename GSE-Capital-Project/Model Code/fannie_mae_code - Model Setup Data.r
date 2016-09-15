# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "devtools" version 1.12.0
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "zoo" version 1.7-7
# package "plyr" version 1.8.3
# package "lubridate" version 1.5.6

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 

# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# Specify whether working on the server or personal computer
location	<- "Personal"	# "Personal" or "Server"

# Specify whether to use the Zillow or Fannie Mae data for the home price indices
home_value	<- "Zillow"	# "Zillow" or "Fannie"

# Set the time horizon (in months)
time_horizon	<- 1

# Set the working directory
ifelse( location == "Server", setwd("/scratch/PI/giesecke/Stacey"), setwd("C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data") )

# Set the number of batches (number of splits in each monthly file)
num_batches	<- 5		#### NOTE: CAN INCREASE THIS IF MEMORY IS A PROBLEM ####

# Set the seed number for shuffling
set.seed(100)

# Install development version of data.table (to use fwrite)
# Note: To use the developmental version of data.table you need to have Rtools installed:
#	https://cran.r-project.org/bin/windows/Rtools/
remove.packages("data.table")
if (!(require(devtools))) install.packages ("devtools")
library(devtools)
devtools::install_github("Rdatatable/data.table", build_vignettes=FALSE)


# Load Necessary Packages for this analysis
chooseCRANmirror(ind=0)
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")
if (!(require(plyr))) install.packages ("plyr")
if (!(require(lubridate))) install.packages ("lubridate")
ifelse( location == "Server", source( "Fannie_Mae_Loan_Level_Data/RCode/fannie_mae_code - glm Model Functions.r" ), source( "R Code/fannie_mae_code - glm Model Functions.r" ) )

 
# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website at https://loanperformancedata.fanniemae.com/lppub/index.html.
# For more detail please refer to the accompanied presentation. After downloading the files you will need to unzip the files. 
# Though read.table function in R can read zipped files, we have used the "fread" function from data.table package 
# to read these files for efficiency and speed. Unfortunately, fread cannot read zipped files.
# While this program will run with any number of pairs of files, we encourage users to download the entire set of Acquisition and Performance 
# files. The naming of the files should remain the same after download and unzipping process so that the files are saved in order. 
# This program will create the datasets needed for modeling. The data will results will be output to the "Chunk_TimeHorizon_" folder.
# The loss data will be output to the "Loss_TimeHorizon_" folder. The folders will be created if they do not already exist.
# NOTE: NEED TO RUN  download_unemployment_data.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  organize_AGI_data.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  organize_Mortgage_CBSA_Panel.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  organize_Zillow_data.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  organize_SBA_data.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  "fannie_mae_code - Make Summary Tables.r" BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  "fannie_mae_code - State Monthly Transition Matrix.r" BEFORE RUNNING THIS CODE


# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
# Note: the unemployment location should be where the AGI, Mortgage_CBSA_Panel, and SBA data is also located
file_location	<- ifelse( location == "Server", file.path( getwd(), "Fannie_Mae_Loan_Level_Data" ), getwd() )
data_location	<- file.path( file_location, "Data" )
chunk_location	<- file.path( file_location, paste( "Chunk_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
loss_location	<- file.path( file_location, paste( "Loss_TimeHorizon1yr_Fixed_", home_value, "_", time_horizon, sep="" ) )
Result_location	<- file.path( file_location, "Results" )
Unemployment_location <- ifelse( location == "Server", file.path( getwd(), "Economic Data/Results" ), "C:/Users/robert/Desktop/Economic Data/Results" )

# Create the output folder if it does not already exist
dir.create( chunk_location, showWarnings = FALSE )
dir.create( loss_location, showWarnings = FALSE )

# Check the number of files downloaded (should be even, equal number of Acquisition and Performance Files).
numberoffiles	<- length( list.files( data_location, pattern = glob2rx("*txt"), full.names=TRUE) )

# The "foreach" package contructs a loop so that R can iterate through all pairs of related Acquisition and Performance files.
# Calculate the number of iterations/cores in parallel processing allowing each pair to be processed simultaneously.
numberofloops	<- (numberoffiles/2)


# Load the unemployment data
load( file.path( Unemployment_location, "MSA_Unemp_Table.RData" ) )		# MSA_unemp_table
load( file.path( Unemployment_location, "State_Unemp_Table.RData" ) )		# State_unemp_table

# Format the unemployment data
colnames(MSA_unemp_table)[ colnames(MSA_unemp_table) == "value" ] 	<- "Unemployment"
colnames(State_unemp_table)[ colnames(State_unemp_table) == "value" ] 	<- "Unemployment"
MSA_unemp_table$date	<- as.Date(MSA_unemp_table$date) 
State_unemp_table$date	<- as.Date(State_unemp_table$date) 
MSA_unemp_table$MSA	<- as.character(MSA_unemp_table$MSA)
MSA_unemp_table		<- aggregate( Unemployment ~ date + MSA, mean, data=MSA_unemp_table )

# Load the AGI data
load( file.path( Unemployment_location, "AGI_data.RData" ) )			# AGI_data
AGI_data$Year 		<- as.character(AGI_data$Year)
AGI_data$MSA 		<- as.character(AGI_data$MSA)

# Load the transition probabilities
load( file.path( Result_location, paste( "Monthly_Trans_Probabilities_TimeHorizon", time_horizon, ".RData", sep="" ) ) )	# monthly_trans_prob_df
monthly_trans_prob_df$Monthly.Rpt.Prd	<- as.Date(monthly_trans_prob_df$Monthly.Rpt.Prd, "%m/%d/%Y")
horizon_cols		<- grep( "Horizon", colnames( monthly_trans_prob_df ) )
for( i in horizon_cols )
{
  monthly_trans_prob_df[,i] <- as.numeric( monthly_trans_prob_df[,i] )
}

# Load the Mortgage_CBSA_Panel data
load( file.path( Unemployment_location, "mortgage_panel_data.RData" ) )		# mortgage_data
mortgage_data		<- mortgage_data[, setdiff( colnames(mortgage_data), c("Delinquencies","Foreclosures") ) ]
mortgage_data$Date	<- as.Date(mortgage_data$Date)

# Load the Zillow sales price data
load( file.path( Result_location, "Summary Tables/zillow_MSA_SalePrice_data.RData" ) )		# zillow_msa
load( file.path( Result_location, "Summary Tables/zillow_State_SalePrice_data.RData" ) )	# zillow_state

# Load the SBA data
load( file.path( Unemployment_location, "SBA_data.RData" ) )		# SBA_data
SBA_data[, Date:=as.Date(Date) ]


table_list <- foreach(file_num=1:numberofloops, .inorder=FALSE,
           		.packages=c("data.table", "zoo", "plyr", "lubridate")) %dopar% 
{
  # Define Acquisition variables and classes, and read the files into R.
  Acquisitions <- list.files(data_location, pattern = glob2rx("*Acquisition*txt"), full.names=TRUE)

  Acquisitions_Variables = c("LOAN_ID", "ORIG_CHN", "Seller.Name", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE"
                          ,"FRST_DTE", "OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP"
                          ,"NUM_UNIT", "OCC_STAT", "STATE", "ZIP_3", "MI_PCT", "Product.Type", "CSCORE_C")

  Acquisition_ColClasses = c("character", "character", "character", "numeric", "numeric", "integer", "character", "character", "numeric",
                          "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", 
                          "character", "character", "numeric", "character", "numeric")

  # Read and Process the Performance loan data
  col_names_keep	<- c("LOAN_ID", "ORIG_CHN", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE",
			  	"OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP",
                          	"NUM_UNIT", "OCC_STAT", "STATE", "CSCORE_C")
  col_ind		<- which( Acquisitions_Variables %in% col_names_keep )
  Data_A 		<- fread(Acquisitions[file_num], sep = "|", colClasses = Acquisition_ColClasses, showProgress=0, select=col_ind)
  setnames(Data_A, Acquisitions_Variables[col_ind])
  setkey(Data_A, "LOAN_ID")

  # Apply function to backfill missing OLTV, OCLTV, DTI, CSCORE_B, CSCORE_C
  Data_A[, c("OLTV", "OCLTV", "DTI", "CSCORE_B", "CSCORE_C") := 
		list( na.lomf(OLTV), na.lomf(OCLTV), na.lomf(DTI), na.lomf(CSCORE_B), na.lomf(CSCORE_C) ), 
		by="LOAN_ID" ] 

  # Calculate Spread at Origination (SATO)
  Vint.SATO1 		<- addmargins(xtabs(ORIG_RT*ORIG_AMT~ORIG_DTE, data=Data_A))
  Vint.SATO2 		<- addmargins(xtabs(ORIG_AMT~ORIG_DTE, data=Data_A))
  Vint.SATO		<- as.data.frame(Vint.SATO1/Vint.SATO2)
  colnames(Vint.SATO) 	<- c("ORIG_DTE","Avg.NoteRt")
  Data_A		<- as.data.table(merge(Data_A, Vint.SATO, by="ORIG_DTE"))
  Data_A[, SATO:= ORIG_RT - Avg.NoteRt ]
  Data_A[, Avg.NoteRt:=NULL]

  # Store the vintage year
  Data_A[, VinYr:= format(as.yearmon(ORIG_DTE, format="%m/%Y"), "%Y") ]

  # Obtain the Minimum Fico Score of the Borrower and Co-Borrower, Calculate House Price, and Replace Missing OCLTV values with OLTV values where available
  Data_A[, c("CSCORE_MN", "ORIG_VAL", "OCLTV"):= list(pmin(CSCORE_B,CSCORE_C, na.rm = TRUE),
                                                   (ORIG_AMT/(OLTV/100)),
                                                   ifelse(is.na(OCLTV), OLTV, OCLTV))]

  # Delete unnecessary Acquisition variables
  Data_A[,c("CSCORE_B","CSCORE_C"):=NULL]

  # Remove not-needed Acquisition data from R environment
  rm('Acquisitions_Variables', 'Acquisition_ColClasses')


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
  for( split_num in 1:num_batches )
  {
    # Read and Process the Performance loan data
    col_names_keep	<- c("LOAN_ID", "Monthly.Rpt.Prd", "LAST_UPB", "Loan.Age", "MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code",
				"ZB_DTE", "LPI_DTE", "FCC_DTE", "DISP_DT", "FCC_COST", "PP_COST", "AR_COST", "IE_COST", "TAX_COST", "NS_PROCS",
                         	"CE_PROCS", "RMW_PROCS", "O_PROCS", "NON_INT_UPB", "PRIN_FORG_UPB", "LAST_RT")
    col_ind		<- which( Performance_Variables %in% col_names_keep )
    Data_P 		<- fread( Performance[file_num], sep = "|", colClasses = Performance_ColClasses, showProgress=0, select=col_ind,
  				  nrows=split_id_rows[split_num,nrows], skip=(split_id_rows[split_num,start]-1) )
    setnames(Data_P, Performance_Variables[col_ind] )
  
    # Sort data by Loan ID and Monthly Reporting Period
    setorderv(Data_P, c("LOAN_ID", "Loan.Age"))
    setkey(Data_P, "LOAN_ID")

    # Standardize Delinquency Status Codes
    Data_P[, "Delq.Status":= as.numeric( ifelse( Delq.Status=="X" | is.na(Delq.Status), "999", Delq.Status) ) ]

    # Define the last status of a loan (current, months delinquent, paid-off, foreclosure, etc.)
    # Note: Treats modified loans (Modified=Y) as an absorbing state
    # Note: Since none of the loans matured, that state is ignored (in pre-payments)
    Data_P[, LOAN_STATUS:= 
           ifelse(MOD_FLAG=='Y','Y', ifelse(Zero.Bal.Code=='01','P', ifelse(Zero.Bal.Code=='06', 'R', ifelse(Zero.Bal.Code %chin% c('03', '09'), 'F', ifelse(Delq.Status=='999','X', ifelse(Delq.Status >4,'4', ifelse(Delq.Status==0, 'C', as.character(Delq.Status)))))))),
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

    # Correct the loan delinquency status
    Data_P[, LOAN_STATUS:= fix_loan_status(LOAN_STATUS), by=LOAN_ID ] 

    # Store the next time horizons loan status and the loan status in 1yr
    Data_P[, LOAN_STATUS_HORIZON:= shift(LOAN_STATUS, n=time_horizon, type="lead"), by=LOAN_ID ]
    Data_P[, LOAN_STATUS_1yr:= shift(LOAN_STATUS, n=12, type="lead"), by=LOAN_ID ]
    Data_P[, c("N", "temp_P", "temp_Y", "temp_R", "temp_F") := 
		list( 1:.N, as.numeric( suppressWarnings( min( which( LOAN_STATUS %in% c('P') ) ) ) ),
    		            as.numeric( suppressWarnings( min( which( LOAN_STATUS %in% c('Y') ) ) ) ),
			    as.numeric( suppressWarnings( min( which( LOAN_STATUS %in% c('R') ) ) ) ),
			    as.numeric( suppressWarnings( min( which( LOAN_STATUS %in% c('F') ) ) ) )
		     ), by=LOAN_ID ]
    Data_P[, LOAN_STATUS_1yr:= ifelse( (temp_P < pmin(temp_Y,temp_R,temp_F)) & (N+12 >= temp_P), 'P', 
		    ifelse( (temp_Y < pmin(temp_P,temp_R,temp_F)) & (N+12 >= temp_Y), 'Y',
		    ifelse( (temp_R < pmin(temp_P,temp_Y,temp_F)) & (N+12 >= temp_R), 'R',
		    ifelse( (temp_F < pmin(temp_P,temp_Y,temp_R)) & (N+12 >= temp_F), 'F',
 		LOAN_STATUS_1yr ) ) ) ) ]
    Data_P[, c("N", "temp_P", "temp_Y", "temp_R", "temp_F") := NULL ]
   

    # Drop the unneeded columns
#   Data_P[, c("MOD_FLAG", "Zero.Bal.Code"):=NULL ]

    # Subset to the data with a current and time horizon loan status (and a known status)
    # NOTE: NEED TO KEEP MISSING HORIZON STATUS DATA FOR LOSS ANALYSIS
    Data_P 	<- subset( Data_P, ( !is.na(LOAN_STATUS) & !(LOAN_STATUS=='X') ) & 
				   ( is.na(LOAN_STATUS_HORIZON) | !(LOAN_STATUS_HORIZON=='X') ) )

    # Define the state variables
    states	<- c( 'C', 1:4, 'Y', 'P', 'R', 'F' )
    Data_P[, LOAN_STATUS:=factor(LOAN_STATUS, levels=states, exclude=NULL) ]
    Data_P[, LOAN_STATUS_HORIZON:=factor(LOAN_STATUS_HORIZON, levels=states, exclude=NULL) ]

    # Convert character variables to Date type
    Data_P[, "Monthly.Rpt.Prd":= as.Date(Monthly.Rpt.Prd, "%m/%d/%Y") ]

   
    # Merge the transition probabilities by the month - time_horizon and current loan_status
    Data_P[, Month_Minus_TimeHorizon:= Monthly.Rpt.Prd - months(time_horizon) ]
    Data_P	<- merge( x=Data_P, y=monthly_trans_prob_df, by.x=c("Month_Minus_TimeHorizon","LOAN_STATUS"),
				by.y=c("Monthly.Rpt.Prd","LOAN_STATUS"), all.x=TRUE )
    Data_P[, Month_Minus_TimeHorizon:= NULL ]


    # Merge the SBA data
    Data_P	<- merge( x=Data_P, y=SBA_data, by.x=c("MSA", "Monthly.Rpt.Prd"), by.y=c("MSA", "Date"), all.x=TRUE )


    # Store the observation month and year
    Data_P[, c("Obs_Month", "Obs_Year"):= list( format(Monthly.Rpt.Prd, "%m"), format(Monthly.Rpt.Prd, "%Y") )]

    # Apply function to backfill missing current UPBs
    Data_P[, LAST_UPB:= na.lomf(LAST_UPB), by="LOAN_ID"]

    # Apply function to use next observed missing current UPBs
    # Note: early missing UPBs occur because Fannie does not disclose UPBs in the 1st few months
    Data_P[, LAST_UPB:=na.locf(LAST_UPB, na.rm=FALSE, fromLast=TRUE), by="LOAN_ID" ]

    # Merge the acquisition and performance data files
    Combined_Data	<- merge( x=Data_P, y=Data_A, by.x="LOAN_ID", by.y="LOAN_ID", all.x=TRUE )

    # Delete the performance data from memory
    rm(Data_P); gc()

    # Merge the mortgage data
    Combined_Data	<- merge( x=Combined_Data, y=mortgage_data, by.x=c("Monthly.Rpt.Prd", "STATE", "MSA"), by.y=c("Date", "State", "MSA"), all.x=TRUE )


    # Load the State and MSA Rate, FICO, DTI, and Value tables
    load( file.path( Result_location, "Summary Tables/FNMA_MSA_Summary_Table.RData" ) )		# MSA_Summary
    load( file.path( Result_location, "Summary Tables/FNMA_State_Summary_Table.RData" ) )	# State_Summary
    if( ( file_num == 1 ) & ( split_num == 1 ) )
    {
      if( home_value == "Fannie" )
      {
        MSA_Summary	<- calc_1yr_appreciation( MSA_Summary,   type="MSA" )
        State_Summary	<- calc_1yr_appreciation( State_Summary, type="STATE" )
        save( MSA_Summary,   file=file.path( Result_location, "Summary Tables/FNMA_MSA_Summary_Table.RData" ) )
        save( State_Summary, file=file.path( Result_location, "Summary Tables/FNMA_State_Summary_Table.RData" ) )
      } else if( home_value == "Zillow" )
      {
        zillow_msa	<- calc_1yr_appreciation( zillow_msa,   type="MSA" )
        zillow_state	<- calc_1yr_appreciation( zillow_state, type="STATE" )
        save( zillow_msa,   file=file.path( Result_location, "Summary Tables/zillow_MSA_SalePrice_data.RData" ) )
        save( zillow_state, file=file.path( Result_location, "Summary Tables/zillow_State_SalePrice_data.RData" ) )
      }
    }
    if( home_value == "Zillow" )
    {
      # Merge the Zillow data to the Fannie summary data
      MSA_Summary[,c("W_ORIG_VAL", "W_1Yr_Appreciation"):=NULL] 
      State_Summary[,c("W_ORIG_VAL", "W_1Yr_Appreciation"):=NULL]
      zillow_msa[,MSA:=as.character(MSA)]
      MSA_Summary	<- merge( x=MSA_Summary,   y=zillow_msa, by.x=c("ORIG_DTE", "MSA"), by.y=c("ORIG_DTE", "MSA"), all.x=TRUE )
      State_Summary	<- merge( x=State_Summary, y=zillow_state, by.x=c("ORIG_DTE", "STATE"), by.y=c("ORIG_DTE", "STATE"), all.x=TRUE )
    }
    National_Summary	<- ddply( State_Summary, .(ORIG_DTE), function(x) colMeans( x[,c("W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL", "W_1Yr_Appreciation")], na.rm=TRUE ) )
    

    # Merge the MSA summary tables by origination date and reporting month
    Combined_Data[, ORIG_DTE:=as.Date( as.yearmon( ORIG_DTE, "%m/%Y" ) )] 
    setnames(MSA_Summary,   sub( "W_", "MSA_",   colnames(MSA_Summary) ) )
    Combined_Data	<- merge( x=Combined_Data, y=MSA_Summary, by.x=c("ORIG_DTE", "MSA"), by.y=c("ORIG_DTE", "MSA"), all.x=TRUE )
    Combined_Data[, c("MSA_ORIG_VAL_Ratio", "MSA_ORIG_RT_Ratio", "MSA_ORIG_CSCORE_Ratio", "MSA_ORIG_DTI_Ratio") :=
		   list( ORIG_VAL/MSA_ORIG_VAL, ORIG_RT/MSA_ORIG_RT, CSCORE_MN/MSA_CSCORE_MN, DTI/MSA_DTI ) ]
    Combined_Data[, c("MSA_ORIG_VAL", "MSA_ORIG_RT", "MSA_CSCORE_MN", "MSA_DTI") := list( NULL, NULL, NULL, NULL ) ]

    setnames(MSA_Summary, sub( "MSA_", "MSA.Last_", colnames(MSA_Summary) ) )  
    Combined_Data	<- merge( x=Combined_Data, y=MSA_Summary, by.x=c("Monthly.Rpt.Prd", "MSA"), by.y=c("ORIG_DTE", "MSA"), all.x=TRUE ) 
    Combined_Data[, c("MSA_CUR_VAL", "MSA_CUR_RT_Ratio", "MSA_CUR_CSCORE", "MSA_CUR_DTI") :=
		   list( MSA_ORIG_VAL_Ratio*MSA.Last_ORIG_VAL, ORIG_RT/MSA.Last_ORIG_RT, MSA_ORIG_CSCORE_Ratio*MSA.Last_CSCORE_MN, MSA_ORIG_DTI_Ratio*MSA.Last_DTI ) ]
    setorderv(Combined_Data, c("LOAN_ID", "Monthly.Rpt.Prd"))
    Combined_Data[, MSA_Ref_Burnout:=MSA_CUR_RT_Ratio * cumsum( pmax( (ORIG_RT-MSA.Last_ORIG_RT)/MSA.Last_ORIG_RT - 0.1, 0 ) ), by=LOAN_ID ]
    Combined_Data[, c("MSA.Last_ORIG_VAL", "MSA.Last_ORIG_RT", "MSA.Last_CSCORE_MN", "MSA.Last_DTI") := list( NULL, NULL, NULL, NULL ) ]

			 
    # Merge the State summary tables by origination date and reporting month
    setnames(State_Summary, sub( "W_", "State_", colnames(State_Summary) ) )
    Combined_Data	<- merge( x=Combined_Data, y=State_Summary, by.x=c("ORIG_DTE", "STATE"), by.y=c("ORIG_DTE", "STATE"), all.x=TRUE )
    Combined_Data[, c("State_ORIG_VAL_Ratio", "State_ORIG_RT_Ratio", "State_ORIG_CSCORE_Ratio", "State_ORIG_DTI_Ratio") :=
		   list( ORIG_VAL/State_ORIG_VAL, ORIG_RT/State_ORIG_RT, CSCORE_MN/State_CSCORE_MN, DTI/State_DTI ) ]
    Combined_Data[, c("State_ORIG_VAL", "State_ORIG_RT", "State_CSCORE_MN", "State_DTI") := list( NULL, NULL, NULL, NULL ) ]

    setnames(State_Summary, sub( "State_", "State.Last_", colnames(State_Summary) ) )  
    Combined_Data 	<- merge( x=Combined_Data, y=State_Summary, by.x=c("Monthly.Rpt.Prd", "STATE"), by.y=c("ORIG_DTE", "STATE"), all.x=TRUE ) 
    Combined_Data[, c("State_CUR_VAL", "State_CUR_RT_Ratio", "State_CUR_CSCORE", "State_CUR_DTI") :=
		   list( State_ORIG_VAL_Ratio*State.Last_ORIG_VAL, ORIG_RT/State.Last_ORIG_RT, State_ORIG_CSCORE_Ratio*State.Last_CSCORE_MN, State_ORIG_DTI_Ratio*State.Last_DTI ) ]
    setorderv(Combined_Data, c("LOAN_ID", "Monthly.Rpt.Prd"))
    Combined_Data[, State_Ref_Burnout:=State_CUR_RT_Ratio * cumsum( pmax( (ORIG_RT-State.Last_ORIG_RT)/State.Last_ORIG_RT - 0.1, 0 ) ), by=LOAN_ID ]
    Combined_Data[, c("State.Last_ORIG_VAL", "State.Last_ORIG_RT", "State.Last_CSCORE_MN", "State.Last_DTI") := list( NULL, NULL, NULL, NULL ) ]

    
    # Merge the National summary tables by origination date and reporting month
    setnames(National_Summary, sub( "W_", "National_", colnames(National_Summary) ) )
    Combined_Data	<- merge( x=Combined_Data, y=National_Summary, by.x=c("ORIG_DTE"), by.y=c("ORIG_DTE"), all.x=TRUE )
    Combined_Data[, c("National_ORIG_VAL_Ratio", "National_ORIG_RT_Ratio", "National_ORIG_CSCORE_Ratio", "National_ORIG_DTI_Ratio") :=
		   list( ORIG_VAL/National_ORIG_VAL, ORIG_RT/National_ORIG_RT, CSCORE_MN/National_CSCORE_MN, DTI/National_DTI ) ]
    Combined_Data[, c("National_ORIG_VAL", "National_ORIG_RT", "National_CSCORE_MN", "National_DTI") := list( NULL, NULL, NULL, NULL ) ]

    setnames(National_Summary, sub( "National_", "National.Last_", colnames(National_Summary) ) )  
    Combined_Data 	<- merge( x=Combined_Data, y=National_Summary, by.x=c("Monthly.Rpt.Prd"), by.y=c("ORIG_DTE"), all.x=TRUE ) 
    Combined_Data[, c("National_CUR_VAL", "National_CUR_RT_Ratio", "National_CUR_CSCORE", "National_CUR_DTI") :=
		   list( National_ORIG_VAL_Ratio*National.Last_ORIG_VAL, ORIG_RT/National.Last_ORIG_RT, National_ORIG_CSCORE_Ratio*National.Last_CSCORE_MN, National_ORIG_DTI_Ratio*National.Last_DTI ) ] 
    setorderv(Combined_Data, c("LOAN_ID", "Monthly.Rpt.Prd"))
    Combined_Data[, National_Ref_Burnout:=National_CUR_RT_Ratio * cumsum( pmax( (ORIG_RT-National.Last_ORIG_RT)/National.Last_ORIG_RT - 0.1, 0 ) ), by=LOAN_ID ]
    Combined_Data[, c("National.Last_ORIG_VAL", "National.Last_ORIG_RT", "National.Last_CSCORE_MN", "National.Last_DTI") := list( NULL, NULL, NULL, NULL ) ]

    # If the MSA data is not available use the state data, otherwise use the national
    Combined_Data[, c("ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio", "ORIG_DTI_Ratio"):=
		  list( ifelse( is.na(MSA_ORIG_VAL_Ratio),    State_ORIG_VAL_Ratio,    MSA_ORIG_VAL_Ratio ),
			ifelse( is.na(MSA_ORIG_RT_Ratio),     State_ORIG_RT_Ratio,     MSA_ORIG_RT_Ratio ),
			ifelse( is.na(MSA_ORIG_CSCORE_Ratio), State_ORIG_CSCORE_Ratio, MSA_ORIG_CSCORE_Ratio ),
			ifelse( is.na(MSA_ORIG_DTI_Ratio),    State_ORIG_DTI_Ratio,    MSA_ORIG_DTI_Ratio )
		      ) ]
    Combined_Data[, c("ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio", "ORIG_DTI_Ratio"):=
		  list( ifelse( is.na(ORIG_VAL_Ratio),    National_ORIG_VAL_Ratio,    ORIG_VAL_Ratio ),
			ifelse( is.na(ORIG_RT_Ratio),     National_ORIG_RT_Ratio,     ORIG_RT_Ratio ),
			ifelse( is.na(ORIG_CSCORE_Ratio), National_ORIG_CSCORE_Ratio, ORIG_CSCORE_Ratio ),
			ifelse( is.na(ORIG_DTI_Ratio),    National_ORIG_DTI_Ratio,    ORIG_DTI_Ratio )
		      ) ]
    Combined_Data[, c("MSA_ORIG_VAL_Ratio", "MSA_ORIG_RT_Ratio", "MSA_ORIG_CSCORE_Ratio", "MSA_ORIG_DTI_Ratio", 
		      "State_ORIG_VAL_Ratio", "State_ORIG_RT_Ratio", "State_ORIG_CSCORE_Ratio", "State_ORIG_DTI_Ratio",
		      "National_ORIG_VAL_Ratio", "National_ORIG_RT_Ratio", "National_ORIG_CSCORE_Ratio", "National_ORIG_DTI_Ratio" ) := NULL ]

    Combined_Data[, c("CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", "CUR_DTI", "Ref_Burnout", "CUR_1Yr_Appreciation"):=
		  list( ifelse( is.na(MSA_CUR_VAL),      State_CUR_VAL,      MSA_CUR_VAL ),
			ifelse( is.na(MSA_CUR_RT_Ratio), State_CUR_RT_Ratio, MSA_CUR_RT_Ratio ),
			ifelse( is.na(MSA_CUR_CSCORE),   State_CUR_CSCORE,   MSA_CUR_CSCORE ),
			ifelse( is.na(MSA_CUR_DTI),      State_CUR_DTI,      MSA_CUR_DTI ),
			ifelse( is.na(MSA_Ref_Burnout),  State_Ref_Burnout,  MSA_Ref_Burnout ),
			ifelse( is.na(MSA.Last_1Yr_Appreciation), State.Last_1Yr_Appreciation, MSA.Last_1Yr_Appreciation )
		      ) ]
    Combined_Data[, c("CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", "CUR_DTI", "Ref_Burnout", "CUR_1Yr_Appreciation"):=
		  list( ifelse( is.na(CUR_VAL),      National_CUR_VAL,      CUR_VAL ),
			ifelse( is.na(CUR_RT_Ratio), National_CUR_RT_Ratio, CUR_RT_Ratio ),
			ifelse( is.na(CUR_CSCORE),   National_CUR_CSCORE,   CUR_CSCORE ),
			ifelse( is.na(CUR_DTI),      National_CUR_DTI,      CUR_DTI ),
			ifelse( is.na(Ref_Burnout),  National_Ref_Burnout,  Ref_Burnout ),
			ifelse( is.na(CUR_1Yr_Appreciation), National.Last_1Yr_Appreciation, CUR_1Yr_Appreciation )
		      ) ]
    Combined_Data[, c("MSA_CUR_VAL", "MSA_CUR_RT_Ratio", "MSA_CUR_CSCORE", "MSA_CUR_DTI", "MSA_Ref_Burnout",
		      "State_CUR_VAL", "State_CUR_RT_Ratio", "State_CUR_CSCORE", "State_CUR_DTI", "State_Ref_Burnout",
		      "National_CUR_VAL", "National_CUR_RT_Ratio", "National_CUR_CSCORE", "National_CUR_DTI", "National_Ref_Burnout") := NULL ]
    Combined_Data[, c("MSA_1Yr_Appreciation", "MSA.Last_1Yr_Appreciation", "State_1Yr_Appreciation",
		      "State.Last_1Yr_Appreciation", "National_1Yr_Appreciation", "National.Last_1Yr_Appreciation"):= NULL ]


    # Merge the unemployment data by MSA (if no match, use state)
    Combined_Data	<- merge( x=Combined_Data, y=MSA_unemp_table[,c("date","MSA","Unemployment")], 
				by.x=c("Monthly.Rpt.Prd","MSA"), by.y=c("date","MSA"), all.x=TRUE )
    Combined_Data_MSA 	<- subset( Combined_Data, !is.na(Unemployment) )
    Combined_Data_State <- subset( Combined_Data, is.na(Unemployment) )
    Combined_Data_State[,Unemployment:=NULL]
    rm(Combined_Data); gc()
    Combined_Data_State	<- merge( x=Combined_Data_State, y=State_unemp_table[,c("date","State","Unemployment")], 
				by.x=c("Monthly.Rpt.Prd","STATE"), by.y=c("date","State"), all.x=TRUE )
    Combined_Data	<- rbindlist( list( Combined_Data_MSA, Combined_Data_State ), use.names=TRUE, fill=TRUE )
    rm(Combined_Data_MSA, Combined_Data_State); gc()


    # Merge the AGI data by MSA (at origination and current year) and compute ratio
    Combined_Data	<- merge( x=Combined_Data, y=AGI_data[,c("Year","MSA","Avg_AGI")], 
				by.x=c("VinYr","MSA"), by.y=c("Year","MSA"), all.x=TRUE )
    setnames(Combined_Data, sub( "Avg_AGI", "Orig_AGI", colnames(Combined_Data) ) ) 
    Combined_Data	<- merge( x=Combined_Data, y=AGI_data[,c("Year","MSA","Avg_AGI")], 
				by.x=c("Obs_Year","MSA"), by.y=c("Year","MSA"), all.x=TRUE )
    setnames(Combined_Data, sub( "Avg_AGI", "Cur_AGI", colnames(Combined_Data) ) ) 
    Combined_Data[, AGI_Ratio:= Cur_AGI / Orig_AGI ]


    # Define judicious states (according to Freddie Mac)
    judicious_states	<- c( "DE", "FL", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
			      "ND", "NE", "NJ", "NM", "NY", "OH", "OK", "PA", "SC", "SD" )
    Combined_Data[, STATE := as.character(STATE) ]
    Combined_Data[, Judicious:= ifelse( STATE %chin% judicious_states, 1, 0 ) ]

    # Compute the percentage of the loan paid
    Combined_Data[,Per_Paid:=LAST_UPB/ORIG_AMT]

    # Count the number of times LTV > 100
    setorderv(Combined_Data, c("LOAN_ID"))
    Combined_Data[, temp:= ifelse( is.na(CUR_VAL < LAST_UPB), 0, CUR_VAL < LAST_UPB ) ]
    Combined_Data[, Cum_Underwater:= cumsum(temp), by=LOAN_ID ]    
    Combined_Data[, temp:=NULL ]

    # Count the number of months a loan has been considered delinquent
    setorderv(Combined_Data, c("LOAN_ID", "Monthly.Rpt.Prd"))
    Combined_Data[, Delq.Status:= ifelse( is.na(Delq.Status), 999, Delq.Status ) ]
    Combined_Data[, Cum_Delinquent:= cumsum( !(Delq.Status %in% c('0','999')) ), by=LOAN_ID ]

    # Drop unneccessary columns
    Combined_Data[,c("MSA","ORIG_DTE","Delq.Status","ORIG_TRM"):=NULL]
 
    # Standardize the number of reported borrowers
    Combined_Data[, NUM_BO:= ifelse( as.integer(NUM_BO) > 3, '3', NUM_BO ) ]
    
    # Add an NA indicator column for the numeric variables and replace NA's with zero
    numeric_cols	<- c( "Loan.Age", "LAST_UPB", "ORIG_RT", "ORIG_AMT", "OLTV", "OCLTV", "DTI",
            		      "SATO", "CSCORE_MN", "ORIG_VAL", "ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio", "ORIG_DTI_Ratio",
			      "CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", "CUR_DTI", "Ref_Burnout", "CUR_1Yr_Appreciation",
			      "Unemployment", "Orig_AGI", "Cur_AGI", "AGI_Ratio", "Per_Paid", "Cum_Underwater", "Cum_Delinquent",
			      "Horizon_C", "Horizon_1", "Horizon_2", "Horizon_3", "Horizon_4", "Horizon_Y", "Horizon_P",
			      "OpenMortgages", "perc_delinquent", "perc_foreclosure", "foreclosure_delinquent_ratio",
			      "SBA_DefRate_Combo", "SBA_DefRate_7a", "SBA_DefRate_504" )
    NA_col_names	<- paste(numeric_cols, "NA", sep="_")
    for( k in seq(numeric_cols) )
    {
      Combined_Data[, NA_col_names[k] := ifelse( is.na( get(numeric_cols[k]) ), 1, 0 ) ]
      Combined_Data[, numeric_cols[k] := ifelse( is.na( get(numeric_cols[k]) ), 0, get(numeric_cols[k]) ) ]
    }

    # Check if there are any missing
    # Combined_Data[, lapply(.SD,function(x){ any(is.na(x)) })]

    # Subset to the loss data
    Loss_Data		<- subset( Combined_Data, !is.na(FCC_COST) | !is.na(PP_COST) | !is.na(AR_COST) |
					!is.na(IE_COST) | !is.na(TAX_COST) | !is.na(NS_PROCS) |
					!is.na(CE_PROCS) | !is.na(RMW_PROCS) | !is.na(O_PROCS) |
					!is.na(NON_INT_UPB) | !is.na(PRIN_FORG_UPB) )

    # Summarize loss data by keeping only the last row of a loan's activity
    Loss_Data		<- Loss_Data[, .SD[.N], by ="LOAN_ID"]

    # Subset to the non-absorbing previous loan states
    Combined_Data	<- subset( Combined_Data, LOAN_STATUS %in% c( as.character(1:4), 'C') )
    Combined_Data	<- subset( Combined_Data, !is.na(LOAN_STATUS_HORIZON) )

#############
# Compute some summary loss statistics

    # Convert character variables to Date type
    Loss_Data[, "DISP_DT":= as.Date(DISP_DT, "%m/%d/%Y") ]
    Loss_Data[, "FCC_DTE":= as.Date(FCC_DTE, "%m/%d/%Y") ]

    # Apply function to backfill missing current UPBs and NON_INT_UPB
    Loss_Data[, c("LAST_UPB", "NON_INT_UPB") :=list(na.lomf(LAST_UPB), na.lomf(NON_INT_UPB)), by = "LOAN_ID"]


    Loss_Data[, c("NON_INT_UPB", "PRIN_FORG_UPB"):= list( -1*NON_INT_UPB,
                                                          -1*PRIN_FORG_UPB ), by = "LOAN_ID" ]

    Loss_Data[, Fin_UPB := rowSums(.SD, na.rm = TRUE), .SDcols = c("LAST_UPB", "NON_INT_UPB", "PRIN_FORG_UPB")]

    Loss_Data[, c("modir_cost", "modfb_cost", "modfg_cost" ) := list(ifelse(LOAN_STATUS =="Y", ((ORIG_RT - LAST_RT) / 1200) * LAST_UPB, 0),
                                                              ifelse(LOAN_STATUS =="Y" & !is.na(NON_INT_UPB), -1*(LAST_RT / 1200) * NON_INT_UPB, 0),
                                                              ((-1*min(PRIN_FORG_UPB,0, na.rm = TRUE)) )), by = "LOAN_ID" ]
    Loss_Data[, c("C_modir_cost", "C_modfb_cost"):=list(cumsum(modir_cost),
                                                 cumsum(modfb_cost)), by = "LOAN_ID"]


    # Calculate the months between Last Paid Installment and Disposition date (for Lost Interest calculation)  
    Loss_Data[, c("lpi2disp", "zb2disp"):= 
         list(ifelse(LPI_DTE!="" & !(is.na(DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(LPI_DTE, "%m/%d/%Y")))*12+month(DISP_DT)-month(as.yearmon(LPI_DTE, "%m/%d/%Y"))), 0),
            ifelse(!(is.na(ZB_DTE)) & !(is.na(DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(ZB_DTE, "%m/%Y")))*12+month(DISP_DT)-month(as.yearmon(ZB_DTE, "%m/%Y"))), 0)
        )]

    # Calculate Interest Cost, total expenses and total proceeds
    Loss_Data[, c("INT_COST","total_expense", "total_proceeds") := 
       	list( Fin_UPB *(((LAST_RT/100) - .0035)/12)*lpi2disp,
              rowSums(Loss_Data[, list(FCC_COST,PP_COST,AR_COST,TAX_COST,IE_COST)], na.rm = TRUE),
              -1*rowSums(Loss_Data[, list(NS_PROCS,CE_PROCS,RMW_PROCS,O_PROCS)], na.rm = TRUE) ) ]

    # Calculate Net Loss, Net Severity, Total Costs, Total Proceeds, and Total Liquidation Expenses.  Define Last Date variable.
    Loss_Data[,c("NET_LOSS", "NET_SEV", "Total_Cost", "Tot_Procs", "Tot_Liq_Ex", "LAST_DTE"):=
    	list( rowSums(Loss_Data[, list(LAST_UPB,INT_COST,total_expense,total_proceeds)], na.rm=TRUE),
              rowSums(Loss_Data[, list(LAST_UPB,INT_COST,total_expense,total_proceeds)], na.rm=TRUE)/LAST_UPB,
              rowSums(Loss_Data[, list(LAST_UPB, INT_COST,FCC_COST,PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE),
              rowSums(Loss_Data[, list(NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS)], na.rm = TRUE),
              rowSums(Loss_Data[, list(FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE),
              as.Date(ifelse(!(is.na(DISP_DT)), DISP_DT, Monthly.Rpt.Prd)) ) ]

    # Calculate Modification Costs when loans default
    Loss_Data[,c("MODIR_COST","MODFB_COST"):=
         list((ifelse(((LOAN_STATUS =="Y") & !is.na(DISP_DT) ), zb2disp*((ORIG_RT - LAST_RT) / 1200) * LAST_UPB, 0)) + C_modir_cost,
              (ifelse(((LOAN_STATUS =="Y") & !is.na(DISP_DT) & !is.na(NON_INT_UPB) ),zb2disp*(LAST_RT / 1200) * (-1*NON_INT_UPB), 0)) + C_modfb_cost)]

    Loss_Data[, MODTOT_COST :=rowSums(.SD, na.rm = TRUE), .SDcols = c("modfg_cost", "MODIR_COST","MODFB_COST")]
    Loss_Data[,c("modir_cost", "modfb_cost"):=NULL]

#############

    # Scramble the combined data (neural network needs shuffled data)
    Combined_Data	<- Combined_Data[ sample(nrow(Combined_Data)), ]

    # Make sure the dates are saved as characters (otherwise will write as an integer to file)
    Loss_Data[, c( "Monthly.Rpt.Prd", "ZB_DTE", "LPI_DTE", "FCC_DTE", "DISP_DT", "LAST_DTE" ) :=
		list(   as.character(Monthly.Rpt.Prd), as.character(ZB_DTE), as.character(LPI_DTE),
			as.character(FCC_DTE), as.character(DISP_DT), as.character(LAST_DTE) )  ]
    Combined_Data[, c( "Monthly.Rpt.Prd", "ZB_DTE", "LPI_DTE", "FCC_DTE", "DISP_DT") :=
		list(   as.character(Monthly.Rpt.Prd), as.character(ZB_DTE), as.character(LPI_DTE),
			as.character(FCC_DTE), as.character(DISP_DT) )  ]

 
    # Write out the Data as a csv file (so that h2o can read in quickly)
    fwrite( Combined_Data, file.path( chunk_location, paste( "Combined_Data_Time_Horizon_", time_horizon, "_", file_num, "_", split_num, ".csv", sep="" ) ),
		turbo=FALSE )
    fwrite( Loss_Data, file.path( loss_location, paste( "Loss_Data_Time_Horizon_", time_horizon, "_", file_num, "_", split_num, ".csv", sep="" ) ),
		turbo=FALSE ) 
    rm(Loss_Data, Combined_Data); gc()

  } # end for

  return(NULL)

} # end for each

