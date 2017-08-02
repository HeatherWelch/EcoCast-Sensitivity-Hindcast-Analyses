# EcoCast-Sensitivity-Hindcast-Analyses
Code to iteratively run EcoCast with optional date, weighting, variable presence and variable date parameters

EcoCast_hindcast.R :
      Function to run Ecocast for multiple dates/weightings
      i.e. Lagged official output sensitivity (OO)- official output persistence across days

EcoCast_hindcast_DEMO.R :
      Example code for running EcoCast_hindcast()
      
EcoCast_leave_one_out_Sensitivity.R:
      Function to run Ecocast with missing variables
      i.e. Leave on out sensitivity (LOO)- output persistence across missing variables
      
EcoCast_LOO_DEMO.R
      Example code for running EcoCast_leave_one_out_Sensitivity.R
      
EcoCast_lagged_Sensitivity.R
      Function to fun Ecocast with lagged variables
      i.e. Ladded variable sensitivity (lagged) - output persistence across non-real time variables (lags= 1d, 7d, 14d, 21d, 28d & 30d)
      
EcoCast_lagged_DEMO.R
      Example code for running EcoCast_lagged_sensitivity.R
      
EcoCast_Sensitivity_Master.R
      Set up to run OO, LOO and lagged all at once

quantify_OO.R
      Code to quantify sensitivity within OO analysis
