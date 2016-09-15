plot_LGD_data <- function( Loss_Data, LGD_name="NET_SEV", model_x, Result_location )
{ # Plot the historical LGD data

  # Plot the LGD boxplot by categorical variables: 
  cat_variables		<- colnames(Loss_Data)[ which( sapply(Loss_Data, class) == "factor" ) ]
  cat_variables		<- setdiff( cat_variables, c("Monthly.Rpt.Prd","LOAN_ID","ZB_DTE","LPI_DTE","FCC_DTE","DISP_DT","LOAN_STATUS_HORIZON","LOAN_STATUS_1yr","LAST_DTE") )

  #mypdf( path=Result_location, foldername="LGD_Boxplots" )
  pdf( file.path( Result_location, "LGD Boxplots.pdf" ) )
    sapply( cat_variables, function(x) plot_LGD_boxplot( Loss_Data, xval=x, yval=LGD_name ) )
  dev.off()


  # Show the correlation matrix plot
  #mypdf( path=Result_location, foldername="LGD_Correlations" )
  pdf( file.path( Result_location, "LGD Correlation Plots.pdf" ) )
    plot_correlation_matrix( Loss_Data, LGD_name )
  dev.off()

  # Fit a classification tree to the LGD data and plot the results
  pdf( file.path( Result_location, "LGD CART.pdf" ), paper="USr" )
    show_LGD_CART( Loss_Data, yval=LGD_name, model_x )
  dev.off()

  # Plot the empirical LGD histogram and some example BEINF distributions
  pdf( file.path( Result_location, "LGD Histogram and Example BEINF.pdf" ) )
    # Plot the histogram of LGD to show it has positive mass at 0 and 1
    mass_0	<- sum( Loss_Data[,get(LGD_name)] == 0 ) / nrow(Loss_Data)
    mass_1	<- sum( Loss_Data[,get(LGD_name)] == 1 ) / nrow(Loss_Data)
    hist( Loss_Data[,get(LGD_name)], breaks="FD", xlab=LGD_name, main="Histogram of LGD" )
    mtext( side=3, paste( "Prob(LGD=0)=", round(mass_0,2)*100, "%", "    Prob(LGD=1)=", round(mass_1,2)*100, "%", sep="" ) )

    # Show some example Inflated Beta distribution plots
    plot_example_BEINF(n=100000)
  dev.off()

} # end function


plot_LGD_boxplot <- function( Loss_Data, xval, yval )
{ # Plot the LGD boxplot as a function of a categorical variable

    with( Loss_Data, plot( x=as.factor(get(xval)), y=get(yval), xlab=xval, ylab=yval ) )
    
} # end function


plot_correlation_matrix <- function( dat, LGD_name )
{ # Show the correlation matrix plot

  # Replace the missing values with NA for purposes of plotting the correlations
  Loss_Data		<- copy(dat)	# copy by value (do not want to change the original Loss_Data)
  numeric_variables	<- colnames(Loss_Data)[ which( sapply(Loss_Data, class) %in% c("numeric", "integer") ) ]
  NA_cols		<- grep( "_NA", colnames(Loss_Data), value=TRUE )
  for( k in numeric_variables )
  {
    k_NA		<- paste( k, "NA", sep="_" )
    if( k_NA %in% NA_cols )
    { 
      Loss_Data[, eval(k):=ifelse( get(k_NA) == 1, NA, get(k) ) ]
    } # end if
  } # end for

  # Only plot the variables with non-zero standard deviation
  numeric_variables	<- colnames(Loss_Data)[ which( sapply(Loss_Data, class) %in% c("numeric", "integer") ) ]
  sd_vals		<- sapply( Loss_Data[, .SD, .SDcols=numeric_variables ], function(x) sd(x, na.rm=TRUE) )
  numeric_variables	<- numeric_variables[ sd_vals != 0 ]

  # Show the correlation matrix plot: do with 7 variables at a time
  numeric_variables	<- setdiff( numeric_variables, LGD_name )
  breaks		<- seq(numeric_variables) %/% 7
  unique_breaks		<- unique(breaks)
  for( i in unique_breaks )
  {
    break_variables	<- c( LGD_name, numeric_variables[ breaks == i ] )	# Put the LGD as the first column
    corrgram(  Loss_Data[, .SD, .SDcols=break_variables ], order=FALSE, 
				lower.panel=panel.conf, upper.panel=panel.ellipse, 
				diag.panel=panel.density, main="Loss Data Correlation Matrix" ) 
   } # end for

} # end function


show_LGD_CART <- function( Loss_Data, yval, model_x )
{ # Fit a classification tree to the LGD data and plot the results

  # Fit a classification tree to the training set when considering the approval and default years
  training_tree		<- tree( as.formula(paste(yval, "~ 1 +", paste(model_x, collapse= "+"))), data=Loss_Data )
  predicted_LGD		<- predict( training_tree, newdata=Loss_Data )

  # Plot the tree results
  summary(training_tree)
  plot(training_tree)
  text(training_tree, pretty=0)
  training_tree

} # end function


plot_example_BEINF <- function(n)
{ # Show some example Inflated Beta distribution plots

  # As a function of mu
  op	<- par(mfrow=c(2,2))
  mu_5	<- rBEINF( n, mu=0.5, sigma=0.5, nu=0.15, tau=0.03 )
  mu_6	<- rBEINF( n, mu=0.6, sigma=0.5, nu=0.15, tau=0.03 )
  mu_7	<- rBEINF( n, mu=0.7, sigma=0.5, nu=0.15, tau=0.03 )
  mu_8	<- rBEINF( n, mu=0.8, sigma=0.5, nu=0.15, tau=0.03 )
  hist( mu_5, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: mu=0.5" )
  hist( mu_6, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: mu=0.6" )
  hist( mu_7, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: mu=0.7" )
  hist( mu_8, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: mu=0.8" )
  par(op)

  # As a function of sigma
  op	<- par(mfrow=c(2,2))
  s_2	<- rBEINF( n, mu=0.75, sigma=0.2, nu=0.15, tau=0.03 )
  s_4	<- rBEINF( n, mu=0.75, sigma=0.4, nu=0.15, tau=0.03 )
  s_6	<- rBEINF( n, mu=0.75, sigma=0.6, nu=0.15, tau=0.03 )
  s_8	<- rBEINF( n, mu=0.75, sigma=0.8, nu=0.15, tau=0.03 )
  hist( s_2, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: sigma=0.2" )
  hist( s_4, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: sigma=0.4" )
  hist( s_6, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: sigma=0.6" )
  hist( s_8, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: sigma=0.8" )
  par(op)


  # As a function of nu
  op	<- par(mfrow=c(2,2))
  nu_1	<- rBEINF( n, mu=0.75, sigma=0.2, nu=0.1, tau=0.03 )
  nu_2	<- rBEINF( n, mu=0.75, sigma=0.2, nu=0.2, tau=0.03 )
  nu_3	<- rBEINF( n, mu=0.75, sigma=0.2, nu=0.3, tau=0.03 )
  nu_4	<- rBEINF( n, mu=0.75, sigma=0.2, nu=0.4, tau=0.03 )
  hist( nu_1, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: nu=0.1" )
  hist( nu_2, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: nu=0.2" )
  hist( nu_3, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: nu=0.3" )
  hist( nu_4, breaks="FD", xlab="LGD", ylab="Density", main="Example BEINF: nu=0.4" )
  par(op)

} # end function


get_parameter_variables <- function(v0, coef_table, model_x, p_max)
{ # Get the best variables for one of the distribution parameters
  
  p_values		<- coef_table[,"Pr(>|t|)"]
  coefs			<- coef_table[,"Estimate"]
  coefs			<- coefs[ !is.na( coefs ) & ( coefs != 0 ) & ( p_values <= p_max ) ]
  coefs			<- gsub( "x_train", "", names(coefs) )
  coefs			<- unique( gsub( "\\[.*", "", coefs ) )
  coefs			<- coefs[ coefs != "(Intercept)" ]
  res			<- c()
  for( x in model_x )
  {
    temp 	<- grep( x, coefs )
    if( length(temp) > 0 ) { res <- c( res, x ) }
  }
  return(res)

} # end function


get_selected_LGD_variables <- function(v0, model_x, p_max)
{ # Get the best variables for each of the distribution parameters

  summary_fit 		<- summary( v0, save=TRUE )
  mu_coef		<- get_parameter_variables(v0, coef_table=summary_fit$mu.coef.table, 	model_x, p_max)
  sigma_coef		<- get_parameter_variables(v0, coef_table=summary_fit$sigma.coef.table, model_x, p_max)
  nu_coef		<- get_parameter_variables(v0, coef_table=summary_fit$nu.coef.table, 	model_x, p_max)
  tau_coef		<- get_parameter_variables(v0, coef_table=summary_fit$tau.coef.table, 	model_x, p_max)

  # Return the selected LGD coefficients
  return( list( mu_coef=mu_coef, sigma_coef=sigma_coef, nu_coef=nu_coef, tau_coef=tau_coef ) )

} # end function


get_coef_formula <- function( coefs, parm, y_val )
{ # Get the formula for the LGD distribution parameter

  if( length(coefs) >= 1 )
  {
    if( parm == "mu" )
    {
      coef_formula 	<- as.formula(paste(y_val,  "~ 1 +", paste(coefs, collapse= "+")))
    } else
    {
      coef_formula	<- as.formula(paste("~ 1 +", paste(coefs, collapse= "+")))
    }
  } else if( length(coefs) == 0 )
  {
    if( parm == "mu" )
    {
      coef_formula 	<- as.formula( paste( y_val, "~ 1" ) )
    } else
    {
      coef_formula	<- as.formula("~ 1")
    }
  }

  # return the coefficient formula
  return( coef_formula )

} # end function


get_selected_LGD_formula <- function( LGD_coefs, y_val )
{ # Get the formulas for all of the LGD parameters

  mu_formula		<- get_coef_formula( LGD_coefs$mu_coef,    parm="mu",	 y_val )
  sigma_formula		<- get_coef_formula( LGD_coefs$sigma_coef, parm="sigma", y_val )
  nu_formula		<- get_coef_formula( LGD_coefs$nu_coef,    parm="nu",	 y_val )
  tau_formula		<- get_coef_formula( LGD_coefs$tau_coef,   parm="tau",	 y_val )
  return( list( mu_formula=mu_formula, sigma_formula=sigma_formula,
		nu_formula=nu_formula, tau_formula=tau_formula ) )

} # end function


train_LGD_model <- function( train, train_validate, validate, fitted_mod, t, p_max=0.1, Result_location )
{ # Train the Beta Inflated Model
  # Define the complete model (allow all columns for all the distribution parameters)
  # and choose the model parameters to minimize the validation data global deviance
  # Select only the coefficients with a p-value <= 0.1

  model_x	<- setdiff( colnames(train), y_val )
  pdf( file.path( Result_location, paste( "LGD Fitting Plots - Start Yr ", t, ".pdf", sep="" ) ), height=8, width=11 )  
    # Get the best variables for each of the distribution parameters
    LGD_coefs			<- get_selected_LGD_variables( fitted_mod, model_x, p_max=1 )	# keep all the best variables

    # Fit the model on the combined training and validation datasets
    LGD_formulas		<- get_selected_LGD_formula( LGD_coefs, y_val )
    fitted_train_val		<- with( LGD_formulas, gamlss( mu_formula, sigma.fo=sigma_formula, 
					nu.fo=nu_formula, tau.fo=tau_formula, family=BEINF, data=na.omit(train_validate) ) )
    #print( plot(fitted_train_val) )
    model_summary 		<- summary(fitted_train_val)
    write.csv( model_summary, file.path( Result_location, paste( "Fitted Train Validate Model Summary - Start Yr ", t, ".csv", sep="" ) ) )
 

    # Refit using only the coefficients with a p-value <= 0.1
    LGD_coefs			<- get_selected_LGD_variables( fitted_train_val, model_x, p_max )
    LGD_formulas		<- get_selected_LGD_formula( LGD_coefs, y_val )
    fitted_train_val		<- with( LGD_formulas, gamlss( mu_formula, sigma.fo=sigma_formula, 
					nu.fo=nu_formula, tau.fo=tau_formula, family=BEINF, data=na.omit(train_validate) ) )
    #print( plot(fitted_train_val) )
    model_summary 		<- summary(fitted_train_val)
    write.csv( model_summary, file.path( Result_location, paste( "Fitted Train Validate Model Summary P-Value Subset - Start Yr ", t, ".csv", sep="" ) ) )

    # Plot some example distribution fits on the data
    pdf.plot( fitted_train_val, obs=sample(nrow(train_validate),6), min=0, max=1, step=0.001 )

    # Calculate the Validation data global deviance (based on model using only the training data)
    temp 			<- with( LGD_formulas, gamlssVGD( mu_formula, sigma.fo=sigma_formula, 
					nu.fo=nu_formula, tau.fo=tau_formula, family=BEINF, data=train, newdata=validate ) )
    val_GD			<- VGD(temp)
  dev.off()
  
  # Return the fitted model and deviance
  return( list( fitted_mod=fitted_mod, fitted_train_val=fitted_train_val, val_GD=val_GD ) )

} # end function


plot_parameter_deviance <- function(v0, train_validate)
{ # Plot the Global Deviance as a function of the distribution parameters

  op	<- par(mfrow=c(2,2))
  print( prof.dev(v0, data=train_validate, which="mu", min=0.0001, max=0.9999, length=20) )
  print( prof.dev(v0, data=train_validate, which="sigma", min=0.0001, max=0.9999, length=20) )
  print( prof.dev(v0, data=train_validate, which="nu", min=0.0001, max=0.9999, length=20) )
  print( prof.dev(v0, data=train_validate, which="tau", min=0.0001, max=0.9999, length=20) )
  par(op)

} # end function


inv_logit <- function(x)
{ # Return the inverse logit value

  return( exp(x) / (1+exp(x)) )
} # end function


calc_BEINF_quantile <- function( quantiles, mu, sigma, nu, tau )
{ # Calculate the probability of observing a loss in each of the buckets
  # Note: assumes quantiles start at zero and ends at 1

  prob_of_loss				<- diff( c( 0, pBEINF( quantiles, mu=mu, sigma=sigma, nu=nu, tau=tau ) ) )
  prob_of_loss[length(prob_of_loss)] 	<- tail(prob_of_loss,1) - dBEINF( 1, mu=mu, sigma=sigma, nu=nu, tau=tau )
  prob_of_loss 				<- c( prob_of_loss, dBEINF( 1, mu=mu, sigma=sigma, nu=nu, tau=tau ) )

  return( prob_of_loss )

} # end function


evaluate_LGD_model_performance <- function( fitted_model, train_data, test_data, y_val, name )
{ # Evaluate the LGD model performance on a new test dataset

  # Estimate the Beta Inflated distribution parameter values for the test data
  mu			<- inv_logit( predict(fitted_model, what="mu", data=train_data, newdata=test_data ) )		# mu uses logit link
  sigma			<- inv_logit( predict(fitted_model, what="sigma", data=train_data, newdata=test_data ) )	# sigma uses logit link
  nu			<- exp( predict(fitted_model, what="nu", data=train_data, newdata=test_data ) )			# nu uses log link
  tau			<- exp( predict(fitted_model, what="tau", data=train_data, newdata=test_data ) )		# tau uses log link

  # Make sure all of the parameters are in range
  eps			<- 1e-8
  mu			<- pmin( pmax( mu,    eps ), 1-eps )		# 0 < mu < 1
  sigma			<- pmin( pmax( sigma, eps ), 1-eps )		# 0 < sigma < 1
  nu			<- pmax( nu, eps )				# nu > 0
  tau			<- pmax( tau, eps )				# tau > 0

  # Summarize the empirical distribution
  q			<- length( test_data[,get(y_val)] )
  s1			<- q^2/(q-1)/(q-2)
  k1			<- (q+1)*q^2/(q-1)/(q-2)/(q-3)
  k2			<- (q-1)^2/(q-2)/(q-3)
  emp_mean		<- mean( test_data[,get(y_val)] )
  emp_sd		<- sqrt( sum( ( test_data[,get(y_val)] - emp_mean )^2 ) / (q-1) )
  emp_skew		<- s1 * mean( ( test_data[,get(y_val)] - emp_mean )^3 ) / (emp_sd^3)
  emp_exkurt		<- k1 * mean( ( test_data[,get(y_val)] - emp_mean )^4 ) / (emp_sd^4) - 3*k2
  emp_0_loss		<- sum( test_data[,get(y_val)] == 0 ) / q
  emp_1_loss		<- sum( test_data[,get(y_val)] == 1 ) / q

  # Summarize the predicted distribution
  quantiles		<- seq(from=0, to=1, by=0.001)
  loss_prob_quantiles 	<- sapply( 1:length(mu), function(i) 
				calc_BEINF_quantile( quantiles, mu=mu[i], sigma=sigma[i], nu=nu[i], tau=tau[i] ) )
  loss_prob_quantiles	<- t( loss_prob_quantiles )
  predicted_portfolio_lgd <- colSums(loss_prob_quantiles) / nrow(test_data)
  pred_CDF		<- cumsum(predicted_portfolio_lgd)
  pred_0_loss		<- head( predicted_portfolio_lgd, 1 )
  pred_1_loss		<- tail( predicted_portfolio_lgd, 1 )
  n_points		<- length(predicted_portfolio_lgd)
  predicted_portfolio_lgd[ n_points ] 	<- predicted_portfolio_lgd[ n_points-1 ] + predicted_portfolio_lgd[ n_points ]
  predicted_portfolio_lgd 		<- predicted_portfolio_lgd[ -(n_points-1) ]
  pred_mean		<- sum( quantiles * predicted_portfolio_lgd )
  pred_sd		<- sqrt( sum( ( quantiles - pred_mean )^2 * predicted_portfolio_lgd ) )
  pred_skew		<- sum( ( quantiles - pred_mean )^3 * predicted_portfolio_lgd ) / (pred_sd^3)
  pred_exkurt		<- sum( ( quantiles - pred_mean )^4 * predicted_portfolio_lgd ) / (pred_sd^4) - 3


  # Draw the QQ-Plot
  U			<- runif(10000)
  rand_points		<- findInterval( U, sort(pred_CDF), all.inside=TRUE )
  rand_LGD		<- quantiles[ rand_points ]
  qqplot( x=rand_LGD, y=test_data[,get(y_val)], xlab="Predicted LGD Quantiles", ylab="Empirical LGD Quantiles",
		main=paste( "QQ Plot on", name, "Data" ) )
  abline(a=0, b=1)


  # Calculate the probability of observing a loss in each of the buckets: 0%, (0%,1%], ..., (99%,100%), 100% for the transactions
  quantiles		<- seq(from=0, to=1, by=0.01)
  loss_prob_quantiles 	<- sapply( 1:length(mu), function(i) 
				calc_BEINF_quantile( quantiles, mu=mu[i], sigma=sigma[i], nu=nu[i], tau=tau[i] ) )
  loss_prob_quantiles	<- t( loss_prob_quantiles )
  colnames(loss_prob_quantiles) <- c( quantiles, "100%" )

  # Compute the expected loss (assuming all have EAD=1) for each of the loss buckets
  predicted_portfolio_lgd <- colSums(loss_prob_quantiles) / nrow(test_data)
  barplot( names.arg=colnames(loss_prob_quantiles), height=predicted_portfolio_lgd, ylim=c(0,0.5),
	xlab="Portfolio Loss Buckets", ylab="Loss Bucket Probability", 
	main=paste( "Predicted LGD (EAD=1) Bucket Probability on", name, "Data" ) )
  mtext( side=3, paste( "Mean: ", round(pred_mean*100,2), "%   ",
			"SD: ", round(pred_sd*100,2), "%   ",
			"Skew: ", round(pred_skew,2), "    ",
			"Ex Kurt: ", round(pred_exkurt,2), "    ",
			"P(LGD=0): ", round(pred_0_loss*100,2), "%   ",
			"P(LGD=1): ", round(pred_1_loss*100,2), "%",
		  sep="" ) )


  # Calculate the empirical LGD (EAD=1) bucket values
  quantiles		<- seq(from=0, to=1, by=0.01)
  LGD_ecdf 		<- ecdf( test_data[,get(y_val)] )
  empirical_loss_prob	<- diff( c( 0, LGD_ecdf(quantiles) ) )
  empirical_loss_prob[length(empirical_loss_prob)]	<- tail(empirical_loss_prob,1) - sum( test_data[,get(y_val)] == 1 ) / nrow(test_data)
  empirical_loss_prob	<- c( empirical_loss_prob, sum( test_data[,get(y_val)] == 1 ) / nrow(test_data) )
  names(empirical_loss_prob) <- c( quantiles, "100%" )
  barplot( names.arg=names(empirical_loss_prob), height=empirical_loss_prob,  ylim=c(0,0.5),
	xlab="Portfolio Loss Buckets", ylab="Loss Bucket Probability", 
	main=paste( "Empirical LGD (EAD=1) Bucket Probability on", name, "Data" ) )
  mtext( side=3, paste( "Mean: ", round(emp_mean*100,2), "%   ",
			"SD: ", round(emp_sd*100,2), "%   ",
			"Skew: ", round(emp_skew,2), "    ",
			"Ex Kurt: ", round(emp_exkurt,2), "    ",
			"P(LGD=0): ", round(emp_0_loss*100,2), "%   ",
			"P(LGD=1): ", round(emp_1_loss*100,2), "%",
		  sep="" ) )

  # Return the distribution summaries
  return( c( emp_mean=emp_mean, emp_sd=emp_sd, emp_skew=emp_skew, emp_exkurt=emp_exkurt,
		emp_0_loss=emp_0_loss, emp_1_loss=emp_1_loss,
	     pred_mean=pred_mean, pred_sd=pred_sd, pred_skew=pred_skew, pred_exkurt=pred_exkurt,
		pred_0_loss=pred_0_loss, pred_1_loss=pred_1_loss ) )

} # end function


analyze_BEINF_LGD <- function( defaulted_loans, y_val, model_x, max_train_yr, max_val_yr, p_max, Result_location )
{ # Train and analyze the Beta Inflated LGD model allowing the starting time window to vary
  # Uses stepTGDAll.A for selecting the model parameters
  # Uses Beta Inflated (BEINF) which puts positive mass at 0 and 1

  # Drop any factors with only 1 level
  drop_cols	<- sapply( defaulted_loans, function(x) length(levels(x)) )
  drop_cols	<- names( which( drop_cols == 1, useNames=TRUE ) )
  defaulted_loans[, which( colnames(defaulted_loans) %in% drop_cols ):=NULL ]

  # Split between in and out of sample (13% train, 9% validation, 78% test)
  yr_col			<- "Obs_Year"
  keep_cols			<- c( y_val, model_x, yr_col )
  keep_cols			<- intersect( keep_cols, colnames(defaulted_loans) )
  validate			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) > max_train_yr ) & ( get(yr_col) <= max_val_yr ), select=-get(yr_col) )
  test				<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) > max_val_yr, select=-get(yr_col) )
 
  test2006			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2006, select=-get(yr_col) )
  test2007			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2007, select=-get(yr_col) )
  test2008			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2008, select=-get(yr_col) )
  test2009			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2009, select=-get(yr_col) )
  test2010			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2010, select=-get(yr_col) )
  test2011			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2011, select=-get(yr_col) )
  test2012			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2012, select=-get(yr_col) )
  test2013			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2013, select=-get(yr_col) )
  test2014			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], get(yr_col) == 2014, select=-get(yr_col) )

  all_LGD			<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], select=-get(yr_col) )


  # Train the Beta Inflated Model allowing the start of the training data to vary
  # Select the training start year (time window) which minimizes the validation global deviance
  starting_yrs			<- min(defaulted_loans[,get(yr_col)]):(max_train_yr-1)
  fit_results			<- vector( mode="list", length=length(starting_yrs) )
  names(fit_results)		<- starting_yrs
  for( t in starting_yrs )
  {
    # Get the training and train-validate data based on the starting time period (window)
    train		        <- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) >= t ) & ( get(yr_col) <= max_train_yr ), select=-get(yr_col) )
    train_validate		<- subset( defaulted_loans[,.SD, .SDcols=keep_cols], ( get(yr_col) >= t + (max_val_yr - max_train_yr) ) & ( get(yr_col) <= max_val_yr ), select=-get(yr_col) )

    # Train the LGD model
    v0 				<- gamlss( as.formula( paste( y_val, "~ ." ) ), sigma.fo=~., nu.fo=~1, tau.fo=~., family=BEINF, data=train )
    fitted_mod			<- stepTGDAll.A(v0, scope=~., sigma.scope=~., nu.scope=~., tau.scope=~., newdata=na.omit(validate))
    save( fitted_mod, file=file.path( Result_location, paste( "BEINF Fitted Model - Start Yr ", t, ".RData", sep="" ) ) )
    fit_results[[as.character(t)]] 	<- train_LGD_model( train, train_validate, validate, fitted_mod, t, p_max, Result_location )

    # Store the results
    fitted_mod			<- fit_results[[as.character(t)]]$fitted_mod
    fitted_train_val		<- fit_results[[as.character(t)]]$fitted_train_val

    # Plot the Global Deviance as a function of the distribution parameters for the intercept only model
    #pdf( file.path( Result_location, paste( "LGD Intercept Model Parameter Deviance - Start Yr ", t, ".pdf", sep="" ) ) )
    #  v_intercept		<-  gamlss( as.formula( paste( y_val, "~ 1" ) ), family=BEINF, data=train_validate )
    #  plot_parameter_deviance(v_intercept, train_validate)
    #dev.off()


    # Show the predicted vs empirical probability of losses in each quantile
    pdf( file.path( Result_location, paste( "LGD Model Performance - Start Yr ", t, ".pdf", sep="" ) ), height=8, width=11 )
      dist_summary_trainIn	<- evaluate_LGD_model_performance( fitted_mod, train, train, y_val, name="Train (In-Sample)" )
      dist_summary_Val		<- evaluate_LGD_model_performance( fitted_mod, train, validate, y_val, name="Validation" )
      dist_summary_trainVal	<- evaluate_LGD_model_performance( fitted_mod, train, train_validate, y_val, name="Train-Validate" )
      dist_summary_trainValIn	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, train_validate, y_val, name="Train-Validate (In-Sample)" )
      dist_summary_test		<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test, y_val, name="Test" )

      dist_summary_test2006	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2006, y_val, name="Test2006" )
      dist_summary_test2007	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2007, y_val, name="Test2007" )
      dist_summary_test2008	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2008, y_val, name="Test2008" )
      dist_summary_test2009	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2009, y_val, name="Test2009" )
      dist_summary_test2010	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2010, y_val, name="Test2010" )
      dist_summary_test2011	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2011, y_val, name="Test2011" )
      dist_summary_test2012	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2012, y_val, name="Test2012" )
      dist_summary_test2013	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2013, y_val, name="Test2013" ) 
      dist_summary_test2014	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, test2014, y_val, name="Test2014" )

      dist_summary_allData	<- evaluate_LGD_model_performance( fitted_train_val, train_validate, all_LGD, y_val, name="All Data" )
    dev.off()
    dist_summary		<- rbind( dist_summary_trainIn, dist_summary_Val, dist_summary_trainVal, dist_summary_trainValIn, dist_summary_test,
					  dist_summary_test2006, dist_summary_test2007, dist_summary_test2008, dist_summary_test2009,
					  dist_summary_test2010, dist_summary_test2011, dist_summary_test2012, dist_summary_test2013,
					  dist_summary_test2014, dist_summary_allData )
    write.csv( dist_summary, file.path( Result_location, paste( "LGD Model Performance Dist Summary - Start Yr ", t, ".csv", sep="" ) ) )

  } # end for

  # Write out the results
  fit_results_VGD		<- sapply( fit_results, function(x) x$val_GD )
  fit_results_VGD		<- unlist(fit_results_VGD)
  min_VGD			<- min(fit_results_VGD)
  t				<- as.numeric( names(fit_results_VGD)[ which.min(fit_results_VGD) ] )
  write.csv( fit_results_VGD, file.path( Result_location, "BEINF VGD Fit Results by Training Window.csv" ) )
  save( fit_results, file=file.path( Result_location, "BEINF Fit Results by Training Window.RData" ) )

  # Return the fitted models and data needed for the simulation model
  return( list( fit_results=fit_results, defaulted_loans=defaulted_loans, keep_cols=keep_cols ) )

} # end function


mypdf = function( path, foldername, ...) 
{ # Since some of the PDFs become too large, store the files separately in a folder

    dir.create( file.path( path, foldername ), showWarnings = FALSE )
    fname = paste0(foldername, "%05d.svg")
    pngname = file.path(path, foldername, fname)
    svg(pngname, ...)
    invisible(NULL)
} # end function

