# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "zoo" version 1.7-7
# package "XLConnect" version 0.2-12

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 

# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# Load Necessary Packages for this analysis
# Note: Need to have Java installed for XLConnect to work
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")
if (!(require(XLConnect))) install.packages ("XLConnect")

# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website at https://loanperformancedata.fanniemae.com/lppub/index.html.
# For more detail please refer to the accompanied presentation. After downloading the files you will need to unzip the files. 
# Though read.table function in R can read zipped files, we have used the "fread" function from data.table package 
# to read these files for efficiency and speed. Unfortunately, fread cannot read zipped files.
# While this program will run with any number of pairs of files, we encourage users to download the entire set of Acquisition and Performance 
# files. The naming of the files should remain the same after download and unzipping process so that the files are saved in order. 
# This program will tabulate the overall and the prepaid/defaulted, unmodified loan data. It will save the monthly acquisition tables into the "Default_Prepay" folder.
# The folder will be created if it does not already exist. The program will merge the quarterly tables and write
# the overall table to an RData file named "FNMA_All_Def_Pre_Count_Tables.RData" in the "Results" folder. 
# The folder will be created if it does not already exist.
# NOTE: NEED TO RUN  download_unemployment_data.r BEFORE RUNNING THIS CODE
# NOTE: NEED TO RUN  "fannie_mae_code - Make Summary Tables.r" BEFORE RUNNING THIS CODE


# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
data_location	<- file.path( file_location, "Data" )
temp_location	<- file.path( file_location, "Default_Prepay" )
Result_location	<- file.path( file_location, "Results" )
Unemployment_location <- "C:/Users/robert/Desktop/Economic Data/Results"

# Create the output folder if it does not already exist
dir.create( temp_location, showWarnings = FALSE )
dir.create( Result_location, showWarnings = FALSE )

# Check the number of files downloaded (should be even, equal number of Acquisition and Performance Files).
numberoffiles	<- length( list.files( data_location, pattern = glob2rx("*txt"), full.names=TRUE) )

# The "foreach" package contructs a loop so that R can iterate through all pairs of related Acquisition and Performance files.
# Calculate the number of iterations/cores in parallel processing allowing each pair to be processed simultaneously.
numberofloops	<- (numberoffiles/2)


# Create function to handle missing Current UPBs in the last record by setting them to the record prior
na.lomf <- function(x) 
{
  
  na.lomf.0 <- function(x) 
  {
    non.na.idx <- intersect(which(!is.na(x)),which(x>0))
    if (is.na(x[1L]) || x[1L]==0) 
    {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  } # end function
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) 
  {
    na.lomf.0(x)
  } else 
  {
    apply(x, dim.len, na.lomf.0)
  }

} # end function


na.lomf_L <- function(x) 
{
  
  non.na.idx <- intersect(which(!is.na(x)),which(x[length(x)-1]>0))
  if (is.na(x[length(x)]) || x[length(x)]==0) 
  {
    XX <- c(x[1:length(x)-1], rep.int(x[length(x)-1], 1))
  } else 
  {
    XX <- x
  }
  
} # end function


####################################################################
# Create the Overall and Prepaid/Default Frequency Table
####################################################################

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


# After defining the Acquisition and Performance variables and their classes, the files are read into R and then data manipulation is carried out. 
# The overall and the prepaid, unmodified loan data will be summarized and saved to file
table_list <- foreach(k=1:numberofloops, .inorder=FALSE,
           		.packages=c("data.table", "zoo")) %do% {

# Define Acquisition variables and classes, and read the files into R.
Acquisitions <- list.files(data_location, pattern = glob2rx("*Acquisition*txt"), full.names=TRUE)

Acquisitions_Variables = c("LOAN_ID", "ORIG_CHN", "Seller.Name", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE"
                          ,"FRST_DTE", "OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP"
                          ,"NUM_UNIT", "OCC_STAT", "STATE", "ZIP_3", "MI_PCT", "Product.Type", "CSCORE_C")

Acquisition_ColClasses = c("character", "character", "character", "numeric", "numeric", "integer", "character", "character", "numeric",
                          "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", 
                          "character", "character", "numeric", "character", "numeric")

# Read and Process the Performance loan data
col_names_keep	<- c("LOAN_ID", "ORIG_CHN", "Seller.Name", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE",
			  "OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP",
                          "NUM_UNIT", "OCC_STAT", "STATE", "CSCORE_C")
col_ind		<- which( Acquisitions_Variables %in% col_names_keep )
Data_A 		<- fread(Acquisitions[k], sep = "|", colClasses = Acquisition_ColClasses, showProgress=FALSE, select=col_ind)
setnames(Data_A, Acquisitions_Variables[col_ind])
setkey(Data_A, "LOAN_ID")

# Calculate Spread at Origination (SATO)
Vint.SATO1 	<- addmargins(xtabs(ORIG_RT*ORIG_AMT~ORIG_DTE, data=Data_A))
Vint.SATO2 	<- addmargins(xtabs(ORIG_AMT~ORIG_DTE, data=Data_A))
Vint.SATO	<- as.data.frame(Vint.SATO1/Vint.SATO2)
colnames(Vint.SATO) <- c("ORIG_DTE","Avg.NoteRt")
Data_A		<- as.data.table(merge(Data_A, Vint.SATO, by="ORIG_DTE"))
Data_A[, SATO:= ORIG_RT - Avg.NoteRt ]
Data_A[, Avg.NoteRt:=NULL]

# Store the vintage year
Data_A[, VinYr:= format(as.yearmon(ORIG_DTE, format="%m/%Y"), "%Y") ]

# Obtain the Minimum Fico Score of the Borrower and Co-Borrower, Calculate House Price, and Replace Missing OCLTV values with OLTV values where available
Data_A[, c("CSCORE_MN", "ORIG_VAL", "OCLTV"):= list(pmin(CSCORE_B,CSCORE_C, na.rm = TRUE),
                                                   (ORIG_AMT/(OLTV/100)),
                                                   ifelse(is.na(OCLTV), OLTV, OCLTV))]

# Delete unnecessary Acquisition variables.
Data_A[,c("CSCORE_B","CSCORE_C"):=NULL]

# Remove not-needed Acquisition data from R environment.
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
Data_P 		<- fread( Performance[k], sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE, select=1 )
setnames(Data_P, "LOAN_ID")
setkey(Data_P, "LOAN_ID")

# Store the unique loan ids
num_batches		<- 5		#### NOTE: CAN INCREASE THIS IF MEMORY IS A PROBLEM ####
unique_IDs		<- unique( Data_P[,LOAN_ID] )
unique_IDs_split	<- vector( mode="list", length=num_batches )
id_breaks		<- ceiling( seq( from=0, to=length(unique_IDs), length.out=(num_batches+1) ) )
for( i in 2:length(id_breaks) )
{
  unique_IDs_split[[i-1]] <- unique_IDs[ ( id_breaks[i-1] + 1 ) : id_breaks[i] ]
}

# Get the row numbers for the unique loan ID splits
ending_ids	<- sapply( unique_IDs_split, function(x) tail(x,1) )
split_id_rows 	<- Data_P[,.N, by=LOAN_ID]
split_id_rows	<- split_id_rows[, end:= cumsum(N)][, N := NULL]
split_id_rows	<- subset( split_id_rows, LOAN_ID %in% ending_ids )
split_id_rows[1,"start"] <- 1
for( i in 2:num_batches )
{
  split_id_rows[i,"start"] 	<- split_id_rows[i-1,end] + 1 
}
split_id_rows	<- split_id_rows[, nrows:= end - start + 1 ]
rm(Data_P)
gc()

# Split the Performance data into batches based on the loan ids to help with memory management
all_table	<- vector( mode="list", length=num_batches )
def_table	<- vector( mode="list", length=num_batches )
pre_table	<- vector( mode="list", length=num_batches )
for( split_num in 1:num_batches )
{
  # Read and Process the Performance loan data
  col_names_keep	<- c("LOAN_ID", "Monthly.Rpt.Prd", "Servicer.Name", "LAST_UPB", "Loan.Age",
				"MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code")
  col_ind		<- which( Performance_Variables %in% col_names_keep )
  Data_P 		<- fread( Performance[k], sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE, select=col_ind,
  				  nrows=split_id_rows[split_num,nrows], skip=(split_id_rows[split_num,start]-1) )
  setnames(Data_P, Performance_Variables[col_ind] )
  setkey(Data_P, "LOAN_ID")

  # Convert character variables to Date type
  Data_P[, "Monthly.Rpt.Prd":= as.Date(Monthly.Rpt.Prd, "%m/%d/%Y") ]

  # Store the observation month and year
  Data_P[, c("Obs_Month", "Obs_Year"):= list( format(Monthly.Rpt.Prd, "%m"), format(Monthly.Rpt.Prd, "%Y") )]

  # Calculate the loan age in years
  Data_P[, Loan_Age_Yr:= round(Loan.Age/12)]
  Data_P[, Loan.Age:=NULL]

  # Apply function to backfill missing current UPBs
  Data_P[, LAST_UPB:= na.lomf(LAST_UPB), by="LOAN_ID"] 

  # Subset to the loans that have a transition (more than 1 observation)
  Data_P[,N:=1:.N,by=LOAN_ID]	# count the number of observations per loan id
  Data_P 	<- subset( Data_P, N>1 )
  Data_P[,N:=NULL]	# drop the count column

  # Merge the acquisition and performance data files
  Combined_Data	<- merge( x=Data_P, y=Data_A, by.x="LOAN_ID", by.y="LOAN_ID", all.x=TRUE )

  # Delete the performance data from memory
  rm(Data_P); gc()

  # Load the State and MSA Rate, FICO, DTI, and Value tables
  load( file.path( Result_location, "Summary Tables/FNMA_MSA_Summary_Table.RData" ) )	# MSA_Summary
  load( file.path( Result_location, "Summary Tables/FNMA_State_Summary_Table.RData" ) )	# State_Summary


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
  Combined_Data[, c("MSA.Last_ORIG_VAL", "MSA.Last_ORIG_RT", "MSA.Last_CSCORE_MN", "MSA.Last_DTI") := list( NULL, NULL, NULL, NULL ) ]

			 
  # Merge the State summary tables by origination date and reporting month
  setnames(State_Summary, sub( "W_", "State_", colnames(State_Summary) ) )
  Combined_Data	<- merge( x=Combined_Data, y=State_Summary, by.x=c("ORIG_DTE", "STATE"), by.y=c("ORIG_DTE", "STATE"), all.x=TRUE )
  Combined_Data[, c("State_ORIG_VAL_Ratio", "State_ORIG_RT_Ratio", "State_ORIG_CSCORE_Ratio", "State_ORIG_DTI_Ratio") :=
		   list( ORIG_VAL/State_ORIG_VAL, ORIG_RT/State_ORIG_RT, CSCORE_MN/State_CSCORE_MN, DTI/State_DTI ) ]
  Combined_Data[, c("State_ORIG_VAL", "State_ORIG_RT", "State_CSCORE_MN", "State_DTI") := list( NULL, NULL, NULL, NULL ) ]

  setnames(State_Summary, sub( "State_", "State.Last_", colnames(State_Summary) ) )  
  Combined_Data	<- merge( x=Combined_Data, y=State_Summary, by.x=c("Monthly.Rpt.Prd", "STATE"), by.y=c("ORIG_DTE", "STATE"), all.x=TRUE ) 
  Combined_Data[, c("State_CUR_VAL", "State_CUR_RT_Ratio", "State_CUR_CSCORE", "State_CUR_DTI") :=
		   list( State_ORIG_VAL_Ratio*State.Last_ORIG_VAL, ORIG_RT/State.Last_ORIG_RT, State_ORIG_CSCORE_Ratio*State.Last_CSCORE_MN, State_ORIG_DTI_Ratio*State.Last_DTI ) ] 
  Combined_Data[, c("State.Last_ORIG_VAL", "State.Last_ORIG_RT", "State.Last_CSCORE_MN", "State.Last_DTI") := list( NULL, NULL, NULL, NULL ) ]

  # If the MSA data is not available use the state data
  Combined_Data[, c("ORIG_VAL_Ratio", "ORIG_RT_Ratio", "ORIG_CSCORE_Ratio", "ORIG_DTI_Ratio"):=
		  list( ifelse( is.na(MSA_ORIG_VAL_Ratio),    State_ORIG_VAL_Ratio,    MSA_ORIG_VAL_Ratio ),
			ifelse( is.na(MSA_ORIG_RT_Ratio),     State_ORIG_RT_Ratio,     MSA_ORIG_RT_Ratio ),
			ifelse( is.na(MSA_ORIG_CSCORE_Ratio), State_ORIG_CSCORE_Ratio, MSA_ORIG_CSCORE_Ratio ),
			ifelse( is.na(MSA_ORIG_DTI_Ratio),    State_ORIG_DTI_Ratio,    MSA_ORIG_DTI_Ratio )
		      ) ]
  Combined_Data[, c("MSA_ORIG_VAL_Ratio", "MSA_ORIG_RT_Ratio", "MSA_ORIG_CSCORE_Ratio", "MSA_ORIG_DTI_Ratio", 
		    "State_ORIG_VAL_Ratio", "State_ORIG_RT_Ratio", "State_ORIG_CSCORE_Ratio", "State_ORIG_DTI_Ratio") :=
		  list( NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL ) ]
  Combined_Data[, c("CUR_VAL", "CUR_RT_Ratio", "CUR_CSCORE", "CUR_DTI"):=
		  list( ifelse( is.na(MSA_CUR_VAL),      State_CUR_VAL,      MSA_CUR_VAL ),
			ifelse( is.na(MSA_CUR_RT_Ratio), State_CUR_RT_Ratio, MSA_CUR_RT_Ratio ),
			ifelse( is.na(MSA_CUR_CSCORE),   State_CUR_CSCORE,   MSA_CUR_CSCORE ),
			ifelse( is.na(MSA_CUR_DTI),      State_CUR_DTI,      MSA_CUR_DTI )
		      ) ]
  Combined_Data[, c("MSA_CUR_VAL", "MSA_CUR_RT_Ratio", "MSA_CUR_CSCORE", "MSA_CUR_DTI",  
		    "State_CUR_VAL", "State_CUR_RT_Ratio", "State_CUR_CSCORE", "State_CUR_DTI") :=
		  list( NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL ) ]

  # Merge the unemployment data by MSA (if no match, use state)
  Combined_Data		<- merge( x=Combined_Data, y=MSA_unemp_table[,c("date","MSA","Unemployment")], 
				by.x=c("Monthly.Rpt.Prd","MSA"), by.y=c("date","MSA"), all.x=TRUE )
  Combined_Data_MSA 	<- subset( Combined_Data, !is.na(Unemployment) )
  Combined_Data_State 	<- subset( Combined_Data, is.na(Unemployment) )
  Combined_Data_State[,Unemployment:=NULL]
  rm(Combined_Data); gc()
  Combined_Data_State	<- merge( x=Combined_Data_State, y=State_unemp_table[,c("date","State","Unemployment")], 
				by.x=c("Monthly.Rpt.Prd","STATE"), by.y=c("date","State"), all.x=TRUE )
  Combined_Data		<- rbindlist( list( Combined_Data_MSA, Combined_Data_State ), use.names=TRUE, fill=TRUE )
  rm(Combined_Data_MSA, Combined_Data_State); gc()

  # Define judicious states (according to Freddie Mac)
  judicious_states	<- c( "DE", "FL", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
			      "ND", "NE", "NJ", "NM", "NY", "OH", "OK", "PA", "SC", "SD" )
  Combined_Data[, Judicious:= ifelse( STATE %chin% judicious_states, 1, 0 ) ]

  # Compute the percentage of the loan paid
  Combined_Data[,Per_Paid:=LAST_UPB/ORIG_AMT]

  # Count the number of months a loan has been considered delinquent
  setorderv(Combined_Data, c("LOAN_ID", "Monthly.Rpt.Prd"))
  Combined_Data[, Cum_Delinquent:= cumsum( !(Delq.Status %in% c('0','X','')) ), by=LOAN_ID ]

  # Drop unneccessary columns
  Combined_Data[,c("MSA","ORIG_DTE","LOAN_ID","LAST_UPB","Delq.Status","ORIG_TRM"):=NULL]

  # Create buckets for continuous attributes
  bucket_labels	<- list( OrigRtBkt=	     c('NA', paste("[", seq(from=0, to=10.5, by=0.5), "%,", seq(from=0.5, to=11, by=0.5), "%)", sep=""), '[11%,+)'),
			 OrigAmtBkt=	     c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-150k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'),
			 OltvBkt=	     c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'),
			 OcltvBkt=	     c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'),
			 NumBoBkt=	     c('1', '2', '3+', 'Missing'),
			 DtiBkt=	     c('NA', '[0-10)', '[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70+)'),
			 CScoreMinBkt=	     c('NA', paste("[", seq(from=350, to=800, by=50), ",", seq(from=400, to=850, by=50), ")", sep=""), '[850+)'),
			 SatoBkt=	     c('NA', paste("[", seq(from=-5, to=4.5, by=0.5), "%,", seq(from=-4.5, to=5, by=0.5), "%)", sep=""), '[5%,+)'),
			 OrigValBkt=	     c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-150k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'),
			 UnempBkt=           c('NA', paste( "[", seq(from=0, to=19.5, by=0.5), "%,", seq(from=0.5, to=20, by=0.5), "%)", sep=""), '[20%+)'),
			 OrigValRatioBkt=    c('NA', paste("[", seq(from=0, to=4.9, by=0.1), ",", seq(from=0.1, to=5, by=0.1), ")", sep=""), '[5+)'),
			 OrigRtRatioBkt=     c('NA', paste("[", seq(from=0, to=1.95, by=0.05), ",", seq(from=0.05, to=2, by=0.05), ")", sep=""), '[2+)'),
			 OrigCScoreRatioBkt= c('NA', paste("[", seq(from=0, to=1.95, by=0.05), ",", seq(from=0.05, to=2, by=0.05), ")", sep=""), '[2+)'),
			 OrigDtiRatioBkt=    c('NA', paste("[", seq(from=0, to=1.95, by=0.05), ",", seq(from=0.05, to=2, by=0.05), ")", sep=""), '[2+)'),
			 CurValBkt=	     c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-150k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'),
			 CurRtRatioBkt=      c('NA', paste("[", seq(from=0, to=1.95, by=0.05), ",", seq(from=0.05, to=2, by=0.05), ")", sep=""), '[2+)'),
			 CurCScoreBkt=       c('NA', paste("[", seq(from=350, to=800, by=50), ",", seq(from=400, to=850, by=50), ")", sep=""), '[850+)'),
			 CurDtiBkt=          c('NA', '[0-10)', '[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70+)'),
			 CumDelinquentBkt=   c(seq(from=0, to=19, by=1), '20+', 'Missing'),
			 PerPaidBkt=	     c('NA', paste("[", seq(from=0, to=1.95, by=0.05), ",", seq(from=0.05, to=2, by=0.05), ")", sep=""), '[2+)')
			)
			 
  Combined_Data[,c("OrigRtBkt", "OrigAmtBkt", "OltvBkt", "OcltvBkt", "NumBoBkt", "DtiBkt", "CScoreMinBkt", "SatoBkt", "OrigValBkt",
		   "UnempBkt", "OrigValRatioBkt", "OrigRtRatioBkt", "OrigCScoreRatioBkt", "OrigDtiRatioBkt", "CurValBkt", "CurRtRatioBkt",
		   "CurCScoreBkt", "CurDtiBkt", "CumDelinquentBkt", "PerPaidBkt")
              :=list(as.character(cut(ORIG_RT, breaks = c(-Inf, seq(from=0, to=11, by=0.5), Inf), 
				labels = bucket_labels[["OrigRtBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_AMT, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf),
                     		labels = bucket_labels[["OrigAmtBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(OLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),
				labels = bucket_labels[["OltvBkt"]], right = TRUE, ordered = TRUE)),
                     as.character(cut(OCLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),
				labels = bucket_labels[["OcltvBkt"]], right = TRUE, ordered = TRUE)),                                                                 
                     as.character(as.character(ifelse(NUM_BO=="","Missing", ifelse(!(NUM_BO %chin% c("1","2")), "3+", NUM_BO)))),
                     as.character(cut(DTI, breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, Inf), 
				labels = bucket_labels[["DtiBkt"]], right = FALSE, ordered = TRUE)),                                    
                     as.character(cut(CSCORE_MN, breaks = c(-Inf, seq(from=350, to=850, by=50), Inf),
				labels = bucket_labels[["CScoreMinBkt"]], right = FALSE, ordered = TRUE)),
                     as.character(cut(SATO, breaks = c(-Inf, seq(from=-5, to=5, by=0.5), Inf), 
				labels = bucket_labels[["SatoBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_VAL, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf),
                     		labels = bucket_labels[["OrigValBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(Unemployment, breaks = c(-Inf, seq(from=0, to=20, by=0.5), Inf), 
				labels = bucket_labels[["UnempBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_VAL_Ratio, breaks = c(-Inf, seq(from=0, to=5, by=0.1), Inf), 
				labels = bucket_labels[["OrigValRatioBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_RT_Ratio, breaks = c(-Inf, seq(from=0, to=2, by=0.05), Inf), 
				labels = bucket_labels[["OrigRtRatioBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_CSCORE_Ratio, breaks = c(-Inf, seq(from=0, to=2, by=0.05), Inf), 
				labels = bucket_labels[["OrigCScoreRatioBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(ORIG_DTI_Ratio, breaks = c(-Inf, seq(from=0, to=2, by=0.05), Inf), 
				labels = bucket_labels[["OrigDtiRatioBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(CUR_VAL, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf), 
				labels = bucket_labels[["CurValBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(CUR_RT_Ratio, breaks = c(-Inf, seq(from=0, to=2, by=0.05), Inf), 
				labels = bucket_labels[["CurRtRatioBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(CUR_CSCORE, breaks = c(-Inf, seq(from=350, to=850, by=50), Inf), 
				labels = bucket_labels[["CurCScoreBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(cut(CUR_DTI, breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, Inf), 
				labels = bucket_labels[["CurDtiBkt"]], right = FALSE, ordered = TRUE)),
		     as.character(as.character(ifelse(Cum_Delinquent=="","Missing", 
				ifelse( Cum_Delinquent > 19, "20+", Cum_Delinquent ) ))),
		     as.character(cut(Per_Paid, breaks = c(-Inf, seq(from=0, to=2, by=0.05), Inf), 
				labels = bucket_labels[["PerPaidBkt"]], right = FALSE, ordered = TRUE))
		)]
		     

  # Create 'Missing' buckets for continuous attributes
  Combined_Data[, OrigRtBkt:=          ifelse( is.na(OrigRtBkt),          'MissingOrigRt',    OrigRtBkt ) ]
  Combined_Data[, OrigAmtBkt:=         ifelse( is.na(OrigAmtBkt),         'MissingOrigAmt',   OrigAmtBkt ) ]
  Combined_Data[, OltvBkt:=            ifelse( is.na(OltvBkt),            'MissingOltv',      OltvBkt ) ]
  Combined_Data[, OcltvBkt:=           ifelse( is.na(OcltvBkt),           'MissingOcltv',     OcltvBkt ) ]
  Combined_Data[, NumBoBkt:=           ifelse( is.na(NumBoBkt),           'Missing',          NumBoBkt ) ]
  Combined_Data[, DtiBkt:=             ifelse( is.na(DtiBkt),             'MissingDti',       DtiBkt ) ]
  Combined_Data[, CScoreMinBkt:=       ifelse( is.na(CScoreMinBkt),       'MissingCScoreMin', CScoreMinBkt ) ]
  Combined_Data[, SatoBkt:=            ifelse( is.na(SatoBkt),            'MissingSato',      SatoBkt ) ]
  Combined_Data[, OrigValBkt:=         ifelse( is.na(OrigValBkt),         'MissingOrigVal',   OrigValBkt ) ]
  Combined_Data[, UnempBkt:=           ifelse( is.na(UnempBkt),           'MissingUnemp',     UnempBkt ) ]
  Combined_Data[, OrigValRatioBkt:=    ifelse( is.na(OrigValRatioBkt),    'Missing',          OrigValRatioBkt ) ]
  Combined_Data[, OrigRtRatioBkt:=     ifelse( is.na(OrigRtRatioBkt),     'Missing',          OrigRtRatioBkt ) ]
  Combined_Data[, OrigCScoreRatioBkt:= ifelse( is.na(OrigCScoreRatioBkt), 'Missing',          OrigCScoreRatioBkt ) ]
  Combined_Data[, OrigDtiRatioBkt:=    ifelse( is.na(OrigDtiRatioBkt),    'Missing',          OrigDtiRatioBkt ) ]
  Combined_Data[, CurValBkt:=          ifelse( is.na(CurValBkt),          'Missing',          CurValBkt ) ]
  Combined_Data[, CurRtRatioBkt:=      ifelse( is.na(CurRtRatioBkt),      'Missing',          CurRtRatioBkt ) ]
  Combined_Data[, CurCScoreBkt:=       ifelse( is.na(CurCScoreBkt),       'Missing',          CurCScoreBkt ) ]
  Combined_Data[, CurDtiBkt:=          ifelse( is.na(CurDtiBkt),          'Missing',          CurDtiBkt ) ]
  Combined_Data[, CumDelinquentBkt:=   ifelse( is.na(CumDelinquentBkt),   'Missing',          CumDelinquentBkt ) ]
  Combined_Data[, PerPaidBkt:=         ifelse( is.na(PerPaidBkt),         'Missing',          PerPaidBkt ) ]

  # Delete unnecessary columns
  Combined_Data[,c("ORIG_RT","ORIG_AMT","OLTV","OCLTV","NUM_BO","DTI","CSCORE_MN","SATO","ORIG_VAL","Unemployment",
		   "ORIG_VAL_Ratio","ORIG_RT_Ratio","ORIG_CSCORE_Ratio","ORIG_DTI_Ratio","CUR_VAL",
		   "CUR_RT_Ratio","CUR_CSCORE","CUR_DTI","Cum_Delinquent","Per_Paid"):=NULL]


  # Subset to the loans which prepaid (or defaulted) and were NOT modified
  Combined_def 	<- subset( Combined_Data, (Zero.Bal.Code %chin% c('03','09') ) & (MOD_FLAG == 'N') )
  Combined_pre 	<- subset( Combined_Data, (Zero.Bal.Code %chin% '01' ) & (MOD_FLAG == 'N') )

  # Delete unnecessary columns
  Combined_Data[,c("Zero.Bal.Code","MOD_FLAG"):=NULL]
  Combined_def[, c("Zero.Bal.Code","MOD_FLAG"):=NULL]
  Combined_pre[, c("Zero.Bal.Code","MOD_FLAG"):=NULL]

  # Make the count summary tables
  table_cols			<- colnames(Combined_Data)
  all_table[[split_num]]	<- lapply( table_cols, function(x) table( Combined_Data[,get(x)] ) )
  def_table[[split_num]]	<- lapply( table_cols, function(x) table( Combined_def[,get(x)] ) )
  pre_table[[split_num]]	<- lapply( table_cols, function(x) table( Combined_pre[,get(x)] ) )
  names(all_table[[split_num]]) <- gsub( "\\.", "_", table_cols )
  names(def_table[[split_num]]) <- gsub( "\\.", "_", table_cols )
  names(pre_table[[split_num]]) <- gsub( "\\.", "_", table_cols )
  
  # Delete the combined data from memory
  rm(Combined_Data, Combined_pre, Combined_def); gc()

} # end for

  # Save the count table data
  table_count		<- list( all_table=all_table, def_table=def_table, pre_table=pre_table )
  save(table_count, file=file.path( temp_location, paste( "FNMA_All_Def_Pre_Count_Table_", k, ".RData", sep="" ) ) )

  # Return the count table data
  return(table_count)

} # end for each


# Save the count table data to file
save(table_list, file=file.path( Result_location, "FNMA_All_Def_Pre_Count_Tables.RData" ) )

####################################################################
# End of Frequency Table Creation
####################################################################

organize_count_table <- function(dat)
{
  # Order the data by variable name
  dat		<- unlist(dat)
  dat		<- dat[ order(names(dat)) ]

  # Get the variable name and groups
  dat_name	<- strsplit( names(dat), split="[.]" )
  dat_name.1	<- sapply( dat_name, function(x) x[1] )
  dat_name.2	<- sapply( dat_name, function(x) paste( x[-1], collapse="." ) )
  
  # Sum the data counts by group and split by variable name
  dat		<- as.data.frame(dat)
  names(dat)	<- "val"
  dat$name	<- dat_name.1
  dat$group	<- dat_name.2
  dat_sum	<- aggregate( val ~ name + group, sum, data=dat )
  dat_sum	<- split( dat_sum, dat_sum$name )
  # Note: all of the variables should have the same number of observations
  # sapply( dat_sum, function(x) sum(x$val) )

  # Return the summary
  return( dat_sum )

} # end function


# Organize the count summary tables by variable
all_table	<- lapply( table_list, function(x) x$all_table )
def_table 	<- lapply( table_list, function(x) x$def_table )
pre_table 	<- lapply( table_list, function(x) x$pre_table )
all_table_sum	<- organize_count_table( all_table )
def_table_sum	<- organize_count_table( def_table )
pre_table_sum	<- organize_count_table( pre_table )


	###############################################################################################

output_table_results <- function( all_table_sum, pre_table_sum, analysis_var )
{ # Output the table results: pdf of barplots and an Excel workbook

  table_sum	 <- lapply( names(all_table_sum), function(x) rbindlist( list( all_table_sum[[x]], pre_table_sum[[x]] ), 
				use.names=TRUE, fill=TRUE, idcol=TRUE ) )
  names(table_sum) <- names(all_table_sum)
  table_sum	 <- lapply( table_sum, function(x) x[, .id:= ifelse( .id==1, 'All', analysis_var ) ] )

  # Compute the fraction of prepaid transitions by grouping variable and display barplots
  table_summary	<- vector( mode="list", length=length(table_sum) )
  pdf( file.path( Result_location, paste( analysis_var, "Transition Barplots.pdf" ) ) )
  for( i in 1:length(table_summary) )
  {
    # Make into a table
    names(table_summary)[i] <- table_sum[[i]][1,name]
    table_summary[[i]] 	  <- dcast( table_sum[[i]], .id ~ group, value.var="val" )
    table_summary[[i]][is.na(table_summary[[i]])] <- 0
  
    # Order the columns
    col_names		  <- colnames(table_summary[[i]])
    if( names(table_summary)[i] %in% names(bucket_labels) )
    {
      col_order		  <- match(bucket_labels[[ names(table_summary)[i] ]], col_names)
      col_order		  <- col_order[!is.na(col_order)]
      temp_col_names	  <- col_names[col_order]
      col_order		  <- c( which( !(col_names %in% temp_col_names) ), col_order )
      table_summary[[i]]  <- table_summary[[i]][, .SD, .SDcols=col_names[col_order] ]
    } else if( names(table_summary)[i] == "Loan_Age_Yr" )
    {
      col_names		  <- c( col_names[1], sort(as.numeric(col_names[2:length(col_names)])) )
      table_summary[[i]]  <- table_summary[[i]][, .SD, .SDcols=col_names ]
    }

    # Compute the fraction prepaid (or defaulted)
    ncols		  <- ncol(table_summary[[i]])
    cols	    	  <- colnames(table_summary[[i]])[2:ncols]
    percent_prepaid	  <- table_summary[[i]][.id==analysis_var, .SD, .SDcols=cols] / table_summary[[i]][.id=="All", .SD, .SDcols=cols]

    # Create the barplot
    if( !(names(table_summary)[i] %in% c("Seller_Name","Servicer_Name","Servicer_Name1")) )
    {
      barplot( height=as.numeric(percent_prepaid), names.arg=cols, xlab=names(table_summary)[i], ylab=paste( "Fraction of Transitions", analysis_var ) )
    }

    # Add the fraction prepaid to the table
    percent_prepaid[,.id:='Fraction']
    table_summary[[i]]	  <- rbindlist( list(table_summary[[i]], percent_prepaid), use.names=TRUE, fill=TRUE )
   
  } # end for
  dev.off()


  # Save the tables to an Excel workbook
  # Create the output file
  Tables	<- loadWorkbook( file.path( Result_location, paste( "FNMA_", analysis_var, "_Transitions.xlsx", sep="") ), create = TRUE)
  n_row		<- nrow(table_summary[[1]])

  # Write out the Transition Tables by Acquisition Quarter
  for( i in 1:length(table_summary) )
  {
    createSheet( Tables, name = names(table_summary)[i] )
    writeWorksheet(Tables, table_summary[[i]], sheet = names(table_summary)[i], startRow = 1, startCol = 1)
  
  } # end for

  # Write the workbook to file
  saveWorkbook(Tables)
  return(NULL)

} # end function


# Output the table results: pdf of barplots and an Excel workbook
output_table_results( all_table_sum, def_table_sum, analysis_var="Default" )
output_table_results( all_table_sum, pre_table_sum, analysis_var="Prepay" )
