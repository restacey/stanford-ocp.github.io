# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
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
if (!(require(XLConnect))) install.packages ("XLConnect")

# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website at https://loanperformancedata.fanniemae.com/lppub/index.html.
# For more detail please refer to the accompanied presentation. After downloading the files you will need to unzip the files. 
# Though read.table function in R can read zipped files, we have used the "fread" function from data.table package 
# to read these files for efficiency and speed. Unfortunately, fread cannot read zipped files.
# While this program will run with any number of pairs of files, we encourage users to download the entire set of Acquisition and Performance 
# files. The naming of the files should remain the same after download and unzipping process so that the files are saved in order. 
# This program will compute the transition frequency table for each of the acquisition months. It will save the monthly acquisition tables into the "Trans" folder.
# The folder will be created if it does not already exists. The program will will compute the overall transition frequency and probability tables and write
# the overall as well as monthly tables to an Excel workbook named "FNMA_Transition_Matrices.xlsx" in the "Results" folder. The folder will be created if it does not already exists.

# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
data_location	<- file.path( file_location, "Data" )
trans_location	<- file.path( file_location, "Trans" )
Result_location	<- file.path( file_location, "Results" )

# Create the output folder if it does not already exist
dir.create( trans_location, showWarnings = FALSE )
dir.create( Result_location, showWarnings = FALSE )

# Check the number of files downloaded (should be even, equal number of Acquisition and Performance Files).
numberoffiles	<- length( list.files( data_location, pattern = glob2rx("*txt"), full.names=TRUE) )

# The "foreach" package contructs a loop so that R can iterate through all pairs of related Acquisition and Performance files.
# Calculate the number of iterations/cores in parallel processing allowing each pair to be processed simultaneously.
numberofloops	<- (numberoffiles/2)


####################################################################
# Compute the Frequency Transition Matrix for Each File
####################################################################

# After defining the Acquisition and Performance variables and their classes, the files are read into R and then data manipulation is carried out. 
# Acquisition and Performance files (from one or many quarters) will be merged into an R dataframe called "Combined_Data."
trans_count_list <- foreach(k=1:numberofloops, .inorder=FALSE,
           		.packages=c("data.table")) %do% {

# Define Performance variables and classes, and read the files into R.
Performance_Variables = c("LOAN_ID", "Monthly.Rpt.Prd", "Servicer.Name", "LAST_RT", "LAST_UPB", "Loan.Age", "Months.To.Legal.Mat"
                        , "Adj.Month.To.Mat", "Maturity.Date", "MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code", 
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
num_batches		<- 2		#### NOTE: CAN INCREASE THIS IF MEMORY IS A PROBLEM ####
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
temp_trans	<- vector(mode="list", length=num_batches)
for( split_num in 1:num_batches )
{

  # Read and Process the Performance loan id data (1st column)
  col_names_keep	<- c( "LOAN_ID", "Loan.Age", "Months.To.Legal.Mat", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code" )
  col_ind		<- which( Performance_Variables %in% col_names_keep )
  Data_P 		<- fread( Performance[k], sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE, select=col_ind,
				  nrows=split_id_rows[split_num,nrows], skip=(split_id_rows[split_num,start]-1) )
  setnames(Data_P, Performance_Variables[col_ind] )

  # Sort data by Loan ID and Monthly Reporting Period
  setorderv(Data_P, c("LOAN_ID", "Loan.Age"))
  setkey(Data_P, "LOAN_ID")

  # Standardize Delinquency Status Codes
  Data_P[, "Delq.Status":= as.numeric( ifelse(Delq.Status=="X", "999", Delq.Status) ) ]

  # Define the last status of a loan (current, months delinquent, paid-off, foreclosure, etc.)
  # Note: Treats matured loans separately from pre-paid loans (Matured=M) and also considers modified loans (Modified=Y)
  # Note: Modified loans are considered an absorbing state
  Data_P[, LOAN_STATUS:= 
           ifelse(MOD_FLAG=='Y','Y',ifelse(Months.To.Legal.Mat==0,'M',ifelse(Zero.Bal.Code=='01','P',ifelse(Zero.Bal.Code=='03','S', ifelse(Zero.Bal.Code=='06', 'R', ifelse(Zero.Bal.Code=='09', 'F', ifelse(Delq.Status=='999','X',ifelse(Delq.Status >9, '9', ifelse(Delq.Status==0, 'C', as.character(Delq.Status)))))))))),
        ]

  # Check if there are any missing months
  # Data_P[,Max_month_diff:=max(diff(Loan.Age)),by=LOAN_ID]
  # range(Data_P$Max_month_diff)
  # table(Data_P$Max_month_diff)
  # View( subset( Data_P, Max_month_diff > 1 ) )
  # Note: there are missing months in the data

  # Add in the missing missing months
  temp		<- Data_P[, min(Loan.Age):max(Loan.Age), by=LOAN_ID]
  setnames(temp, c("LOAN_ID","Loan.Age") )
  setkeyv(temp,  c("LOAN_ID","Loan.Age") )
  setkeyv(Data_P, c("LOAN_ID","Loan.Age") )
  Data_P	<- merge( x=Data_P, y=temp, all=TRUE )
  setorderv(Data_P, c("LOAN_ID", "Loan.Age"))

  # Confirm all of the missing months are added
  # Note: should return c(1,1) (it does)
  # range( Data_P[,max(diff(Loan.Age)),by=LOAN_ID]$V1 ) 

  # Define the state variables
  states	<- c( 'C', 1:9, 'M', 'Y', 'P', 'S', 'R', 'F', 'X', NA )
  Data_P[, LOAN_STATUS:=factor(LOAN_STATUS, levels=states, exclude=NULL) ]
  Data_P[, N:=.N, by=LOAN_ID]

  # Count the number of transitions per CUSIP ID
  trans_count	<- Data_P[, table( LOAN_STATUS[-N], LOAN_STATUS[-1] ), by=LOAN_ID ]

  # Delete the performance data from memory
  rm(Data_P)
  gc()

  # Sum the transitions across the CUSIP IDs
  loan_ids	<- unique( trans_count$LOAN_ID )
  trans_count	<- matrix( trans_count$V1, nrow=length(loan_ids), byrow=TRUE )		# rows are CUSIP IDs, columns are states
  trans_count	<- colSums(trans_count)							# vector of transitions by state

  # Convert into a transition frequency table
  trans_count		<- matrix( trans_count, nrow=length(states) )
  rownames(trans_count) <- states
  colnames(trans_count)	<- states
  # Note: the sum of the transition frequency table should equal: 
  #	sum(trans_count) == nrow(Data_P) - length(loan_ids)
  temp_trans[[split_num]] <- trans_count

} # end for

  # Save the transition frequency table (for the acquisition quarter) to file
  trans_count	<- Reduce('+', temp_trans)
  write.csv(trans_count, file.path( trans_location, paste( "FNMA_Trans_Frequency_", k, ".csv", sep="" ) ) )

  # Return the transition frequency table
  return(trans_count)

} # end for each

# Save the transition frequency tables
fnames 	<- list.files(data_location, pattern = glob2rx("*Performance*txt"), full.names=FALSE)
fnames	<- gsub( ".txt", "", gsub( "Performance_", "", fnames ) )
names(trans_count_list)	<- fnames
save(trans_count_list, file=file.path( Result_location, "FNMA_Transition_Matrices.RData" ) )

####################################################################
# End of Transition Matrix Computation
####################################################################

# Save the transition frequency and probability tables into an Excel workbook

# Make the state labels more descriptive
row_names	<- rownames(trans_count_list[[1]])
row_names[is.na(row_names)] <- "Missing"
row_names[which(row_names == 'C')] <- "Current"
row_names[which(row_names == 'M')] <- "Matured"
row_names[which(row_names == 'Y')] <- "Modified"
row_names[which(row_names == 'P')] <- "Pre-Paid"
row_names[which(row_names == 'S')] <- "Short-Sale"
row_names[which(row_names == 'R')] <- "Repurchased"
row_names[which(row_names == 'F')] <- "REO"
row_names[which(row_names == 'X')] <- "Unknown"
first_ind	  	<- row_names==1
num_ind		   	<- suppressWarnings( !is.na( as.numeric(row_names) ) & !(row_names==1) )
row_names[first_ind] 	<- paste( row_names[first_ind], "Month Delinquent" )
row_names[num_ind] 	<- paste( row_names[num_ind], "Months Delinquent" )
row_names		<- data.frame(row_names)

# Create the output file
Tables		<- loadWorkbook( file.path( Result_location, "FNMA_Transition_Matrices.xlsx" ), create = TRUE)
n_row		<- nrow(trans_count_list[[1]])

# Write out the Overall Transition Table
overall_table	<- Reduce( '+', trans_count_list )
  createSheet( Tables, name = "All" )
  writeWorksheet(Tables, t(row_names), sheet = "All", startRow = 1, startCol = 2, header=FALSE)
  writeWorksheet(Tables, row_names, sheet = "All", startRow = 2, startCol = 1, header=FALSE)
  writeWorksheet(Tables, overall_table, sheet = "All", startRow = 2, startCol = 2, header=FALSE)
  writeWorksheet(Tables, "Number of Observations:", sheet = "All", startRow = n_row+2, startCol = 1, header=FALSE)
  writeWorksheet(Tables, sum(overall_table), sheet = "All", startRow = n_row+2, startCol = 2, header=FALSE)

  temp		<- overall_table / rowSums(overall_table)
  writeWorksheet(Tables, t(row_names), sheet = "All", startRow = n_row+5, startCol = 2, header=FALSE)
  writeWorksheet(Tables, row_names, sheet = "All", startRow = n_row+6, startCol = 1, header=FALSE)
  writeWorksheet(Tables, temp, sheet = "All", startRow = n_row+6, startCol = 2, header=FALSE)

# Write out the Transition Tables by Acquisition Quarter
for( i in 1:length(trans_count_list) )
{
  createSheet( Tables, name = names(trans_count_list)[i] )
  writeWorksheet(Tables, t(row_names), sheet = names(trans_count_list)[i], startRow = 1, startCol = 2, header=FALSE)
  writeWorksheet(Tables, row_names, sheet = names(trans_count_list)[i], startRow = 2, startCol = 1, header=FALSE)
  writeWorksheet(Tables, trans_count_list[[i]], sheet = names(trans_count_list)[i], startRow = 2, startCol = 2, header=FALSE)
  writeWorksheet(Tables, "Number of Observations:", sheet = names(trans_count_list)[i], startRow = n_row+2, startCol = 1, header=FALSE)
  writeWorksheet(Tables, sum(trans_count_list[[i]]), sheet = names(trans_count_list)[i], startRow = n_row+2, startCol = 2, header=FALSE)

  temp		<- trans_count_list[[i]] / rowSums(trans_count_list[[i]])
  writeWorksheet(Tables, t(row_names), sheet = names(trans_count_list)[i], startRow = n_row+5, startCol = 2, header=FALSE)
  writeWorksheet(Tables, row_names, sheet = names(trans_count_list)[i], startRow = n_row+6, startCol = 1, header=FALSE)
  writeWorksheet(Tables, temp, sheet = names(trans_count_list)[i], startRow = n_row+6, startCol = 2, header=FALSE)

} # end for

# Write the workbook to file
saveWorkbook(Tables)
