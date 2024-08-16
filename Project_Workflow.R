

DiagrammeR::grViz("digraph {

graph [layout = dot]

# define the global styles of the nodes. We can override these in box if we wish

data1 [label = 'Tree Ring Data', shape = folder, style = filled, fillcolor = Beige]
data2 [label = 'FIA diameters (cored trees)', style = filled, shape = folder, fillcolor = Beige]
data3 [label = 'FIA diameter (tally trees)', style = filled, shape = folder, fillcolor = Beige]
data4 [label = 'FIA annual data', style = filled, shape = folder, fillcolor = Beige]
KayeData [label = 'Kaye et al \n Allometries', style = filled, shape = folder, fillcolor = Beige]
CMIP5 [label = 'CMIP5 climate \n projections', style = filled, shape = folder, fillcolor = Beige]
population [label = 'FIA population tables', style = filled, shape = folder, fillcolor = Beige]
# set up the nodes for the stan code
modelcode [shape = folder, label = 'STAN model code', style = filled, fillcolor=PeachPuff]


# set up nodes
node [shape = box, style = filled, fillcolor = grey]
process [label =  'Prep \n Data']
statistical [label = 'Fit State-Space \n Models \n (1965-2001)', shape = box]
modelvalidation [label = 'Validation', shape = box]
results [label= 'Posterior \n Estimates', shape = folder, fillcolor = beige]
predicttally [label = 'Predict tally trees \n last DBH to 2001', shape = box]
predicttally2100 [label = 'Predict tally and cored tree growth \n 2001 to 2100', shape = box]
predicttallyAGB [label = 'Predict tree level AGB \n 2001 to 2100', shape = box]
predictplotAGB [label = 'plot AGB = sum(tree level AGB)', shape = box]
predictregionAGB [label = 'regional AGB = sum(plot level AGB*Area represented)', shape = box]

SDIMortality [label = 'Predict SDI and MSB \n mortality \n FVS SDI mortality', shape = box]
GrowthMortality [label = 'Predict Growth-Dependent \n mortality \n pmort = f(growth, DBH)', shape = box]


# edge definitions with the node IDs
{data1 data2}  -> process -> statistical -> modelvalidation -> results -> predicttally
data3 -> predicttally

node [shape = oval, style = filled, fillcolor = lightblue]
full_ssm_stan_model;
Model_summary; 
model_comparison_table;
forecast_future_plot_biomass;
biomass_sensitivity_periodic;
generate_forecast;
plot2AGB_kayePIPO;
climate_change_effects_figures;

# the stancode
node [shape = oval, style = filled, fillcolor = PeachPuff]
model_1;
model_2;
model_3;
model_4;
model_5;
model_6;



# edge definitions with for state-space model scripts
model_1 -> modelcode
model_2 -> modelcode
model_3 -> modelcode
model_4 -> modelcode
model_5 -> modelcode
model_6-> modelcode
modelcode -> full_ssm_stan_model
full_ssm_stan_model -> statistical 
Model_summary -> modelvalidation 
model_comparison_table -> modelvalidation 


# edge definitions for predicting tally trees
forecast_future_plot_biomass -> predicttally
forecast_future_plot_biomass -> predicttally2100
forecast_future_plot_biomass -> biomass_sensitivity_periodic
biomass_sensitivity_periodic -> predicttally2100
generate_forecast -> biomass_sensitivity_periodic 
plot2AGB_kayePIPO -> generate_forecast 
KayeData -> plot2AGB_kayePIPO
CMIP5 -> generate_forecast
predicttally -> predicttally2100
SDIMortality -> generate_forecast
GrowthMortality -> generate_forecast
data4 -> GrowthMortality
plot2AGB_kayePIPO-> predicttallyAGB
predicttally2100 -> predicttallyAGB
predicttallyAGB -> predictplotAGB 
predictplotAGB -> predictregionAGB
population -> predictregionAGB
climate_change_effects_figures -> predictregionAGB

}")
