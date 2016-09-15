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


calc_1yr_appreciation <- function( summary_table, type )
{ # Calculate the 1yr home appreciation
  # Type:	"MSA" or "STATE"

  suppressWarnings( summary_table[, W_1Yr_Appreciation:=NULL ] )
  wide_table	<- dcast( summary_table, get(type) ~ ORIG_DTE, value.var="W_ORIG_VAL" )
  colnames(wide_table)[which( colnames(wide_table) == "type" )] 	<- type
  appreciation 	<- ddply( wide_table, .(get(type)), function(x) x[14:ncol(wide_table)] / x[2:(ncol(wide_table)-12)] )
  colnames(appreciation)[which( colnames(appreciation) == "get(type)" )] <- type
  long_table	<- melt( appreciation, id.vars=type )
  colnames(long_table)[which( colnames(long_table) == "variable" )] 	<- "ORIG_DTE"
  colnames(long_table)[which( colnames(long_table) == "value" )] 	<- "W_1Yr_Appreciation"
  long_table$ORIG_DTE							<- as.Date( as.character( long_table$ORIG_DTE ) )
  summary_table	<- merge( x=summary_table, y=long_table, by.x=c("ORIG_DTE",type), by.y=c("ORIG_DTE",type), all.x=TRUE )
  return(summary_table)
 
} # end function


fix_loan_status <- function( LOAN_STATUS )
{ # Correct the loan delinquency status
  # Ex: If goes from current to 2 months delinquent and set to 1 month delinquent
  
  # Count the number of incorrect loan delinquency statuses
  LOAN_STATUS	<- as.data.table(LOAN_STATUS)
  LOAN_STATUS[, LOAN_STATUS_NEXT:= shift(LOAN_STATUS, n=1, type="lead") ]
  LOAN_STATUS[, Check:= ifelse( LOAN_STATUS == 'C' & LOAN_STATUS_NEXT %in% c('2', '3', '4'), 1, 
			ifelse( LOAN_STATUS == '1' & LOAN_STATUS_NEXT %in% c('3', '4'),      1,
			ifelse( LOAN_STATUS == '2' & LOAN_STATUS_NEXT == '4',      	     1,
				       0 ) ) ) ]
  check		<- sum( LOAN_STATUS$Check, na.rm=TRUE )
  LOAN_STATUS	<- as.character( LOAN_STATUS$LOAN_STATUS )

  # If any incorrect, then fix them
  if( check > 0 )
  {
    for( i in 1:( length(LOAN_STATUS) - 1 ) )
    {
      if( !is.na(LOAN_STATUS[i]) & !is.na(LOAN_STATUS[i+1]) )
      {
        if( LOAN_STATUS[i] == 'C' & LOAN_STATUS[i+1] %in% c('2', '3', '4') )
        {
          LOAN_STATUS[i+1]	<- '1'
        } else if( LOAN_STATUS[i] == '1' & LOAN_STATUS[i+1] %in% c('3', '4') )
        {
	  LOAN_STATUS[i+1]	<- '2'
        } else if( LOAN_STATUS[i] == '2' & LOAN_STATUS[i+1] == '4')
        {
	  LOAN_STATUS[i+1]	<- '3'
        } # end inner if
      } # end if
    } # end for
  } # end outer if

  # Return the correct loan status
  return( LOAN_STATUS )

} # end function

