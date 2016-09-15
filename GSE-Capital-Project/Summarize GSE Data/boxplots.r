# Make boxplots for the Fannie Mae Acquisition Data
# NOTE: RUN "fannie_mae_code - Acquisition Data.r" before running this code

# Load the required libraries
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")

file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
Result_location <- file.path( file_location, "Results" )

# Load the Fannie Mae Acquisition Data
load( file.path( Result_location, "FANNIEMAE_Acquisitions_Data.RData" ) )

# Convert from character to date
Acquisitions_Data[, ORIG_DTE:= as.yearmon(ORIG_DTE, "%m/%Y")]

# Make the boxplots
png( file=file.path( Result_location, "Fannie Mae Boxplot - Origination Amount.png" ) )
  boxplot( ORIG_AMT ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Origination Amount" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Origination Rate.png" ) )
  boxplot( ORIG_RT ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Origination Rate" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Origination Term.png" ) )
  boxplot( ORIG_TRM ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Origination Term" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Origination LTV.png" ) )
  boxplot( OLTV ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Origination LTV" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Origination CLTV.png" ) )
  boxplot( OCLTV ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Origination CLTV" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Debt to Income.png" ) )
  boxplot( DTI ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Debt to Income" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Credit Score Borrower.png" ) )
  boxplot( CSCORE_B ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Credit Score Borrower" )
dev.off()

png( file=file.path( Result_location, "Fannie Mae Boxplot - Credit Score Co-Signer.png" ) )
  boxplot( CSCORE_C ~ ORIG_DTE, data=Acquisitions_Data, xlab="Origination Date", ylab="Credit Score Co-Signer" )
dev.off()
