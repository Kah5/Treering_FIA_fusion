require(magrittr)
require(DiagrammeR)
require(DiagrammeRsvg)
require(xml2)
require(rsvg)


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

}") %>% export_svg %>% charToRaw %>% rsvg_png("project_workflow.png")

######################################################################
# make a diagrammR workflow for the prediction Code
######################################################################
DiagrammeR::grViz("digraph {

graph [layout = dot]

# define the global styles of the nodes. We can override these in box if we wish
data4 [label = 'FIA annual data', style = filled, shape = folder, fillcolor = Beige]

KayeData [label = 'Kaye et al \n Allometries', style = filled, shape = folder, fillcolor = Beige]
CMIP5 [label = 'CMIP5 climate \n projections', style = filled, shape = folder, fillcolor = Beige]

subgraph cluster_0 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Gold
    
        label = 'Mortality Scenarios'
      node[shape = rectangle, fillcolor = pink, margin = 0.25]
        F[label = 'Full: Density Dep. increase & \n Growth Dependent x2']
        G[label = 'Density Dep. increase over time']
        H[label = 'Growth Dependent x2']
        I[label = 'Growth Dependent x1']
        J[label = 'no climate change']
      }

# set up nodes
node [shape = box, style = filled, fillcolor = grey]
predicttally [label = 'Predict tally trees \n last DBH to 2001', shape = box]
predicttally2100 [label = 'Predict tally and cored tree growth \n 2001 to 2100', shape = box]
predicttallyAGB [label = 'Predict tree level AGB \n 2001 to 2100', shape = box]
predictplotAGB [label = 'plot AGB = sum(tree level AGB)', shape = box]

SDIMortality [label = 'Predict SDI and MSB \n mortality \n FVS SDI mortality', shape = box]
GrowthMortality [label = 'Predict Growth-Dependent \n mortality \n pmort = f(growth, DBH)', shape = box]


# edge definitions with the node IDs

#data3 -> predicttally

node [shape = rectangle, style = filled, fillcolor = lightblue]

forecast_future_plot_biomass;
biomass_sensitivity_periodic;
generate_forecast;
plot2AGB_kayePIPO;
#climate_change_effects_figures;




# edge definitions for predicting tally trees
forecast_future_plot_biomass -> predicttally
forecast_future_plot_biomass -> predicttally2100
forecast_future_plot_biomass -> biomass_sensitivity_periodic
biomass_sensitivity_periodic -> predicttally2100

#subgraph cluster_0 -> biomass_sensitivity_periodic 
generate_forecast-> F -> biomass_sensitivity_periodic
generate_forecast-> G -> biomass_sensitivity_periodic
generate_forecast-> H -> biomass_sensitivity_periodic
generate_forecast-> I -> biomass_sensitivity_periodic
generate_forecast-> J -> biomass_sensitivity_periodic
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


}") %>% export_svg %>% charToRaw %>% rsvg_png("predict_using_posteriors_workflow.png")



DiagrammeR::grViz("digraph {

  node [shape = diamong, style = filled, fillcolor = forestgreen]
  fortree [label = 'For each tree i', shape = diamond]
  foryear [label = 'For each year t', shape = diamond]
  predictssm [label = 'Predict yearly growth', shape = rectangle, fillcolor = green]
  calculateSDI [label = 'Plot and Subplot mortality', shape = rectangle, fillcolor = green]

predictssm -> fortree


# sdi and MSB dependent mortality 
subgraph cluster_0 {  
      ifMSB [label = 'If QMD > MSB line \n & TPA.stand > 0 \n or QMD > 0 &  maxDBH > 30', shape = diamond]
      calculateMSB [label = 'calculate mortality
            pMSBmort = 1/ (1+ exp(-0.1*(DBH_avg - 35)))', shape = rectangle, fillcolor = green]
    calculateMSB [label = 'calculate MSB mortality
            pMSBmort = 1/ (1+ exp(-0.1*(DBH_avg - 35)))', shape = rectangle, fillcolor = green]
    calculateTPAMSB [label = 'TPA_MSB_dead[i,t] = TPAall[i,t] - pMSBmort', shape = rectangle, fillcolor = '#fff7bc']
    
    
    else [label = 'calculate SDI and TPA', shape = rectangle, fillcolor = green]
    
    SDImax [label = 'if DD mort ramps up n\ 
    SDImax decreases each \n decade from 0.6 to 0.4', shape = rectangle, fillcolor = grey]
    calcSDI [label = 'if SDI > subplot SDI Max', shape = rectangle, fillcolor = green]
    calcSDImort [label = ' 
              PCT <- apply(BA, 2, percent_rank) \n
              MR <- (0.84525-(0.01074*PCT)+(0.0000002*PCT^3)) \n
              ', shape = rectangle, fillcolor = green]
    calcTPASDImort [label = 'TPA_dead_DD[i,t] = TPAall[i,t-1] -  MR*TPAall[i,t-1]', shape = rectangle, fillcolor = '#fff7bc']

}

foryear -> fortree -> predictssm -> calculateSDI 
calculateSDI -> ifMSB -> calculateMSB -> calculateTPAMSB 
ifMSB ->  else -> calcSDI -> calcSDImort -> calcTPASDImort
SDImax -> calcSDI 

GDmort[label = 'Tree-level mortality', shape = rectangle, fillcolor = green]

# growth dependent mortality 
subgraph cluster_1 {

    predictGDmort[label = 'If t > 2', shape = diamond]
    fortreegd [label = 'For each tree i', shape = diamond]
    ifscaleDImort [label = 'scaled to historic rate: scale.DImort = 10 \n
    increased growth depenent mortality: scale.DI.mort = 20', shape = rectangle, fillcolor = grey]
    calcpmort [label = 'pmort = 1 - inv.logit(a + (b * increment[i,]) + (b*dbh.pred[i,]) ))
            \n
            mort.code <- rbinom(1,1, prob = min(1, pmort*scale.DImort))', shape = rectangle, 
            fillcolor = green]
              
    reduceTPA [label = 'TPA_dead_DI = TPAall[i,t] - TPAall[i,t]*pmort*scale.DImort', fillcolor = '#fff7bc']
}

predictssm -> GDmort -> predictGDmort -> fortreegd -> calcpmort -> reduceTPA
ifscaleDImort -> calcpmort

# recalculate values
subgraph cluster_2 {
  recalculateVALS[label = 'recalculate stand variables', shape = diamond, color = forestgreen]
  TPAall[label = 'TPAall[i,t+1] = \n TPAall[i,t] - \n (TPA_dead_DI[i,t] + TPA_dead_MSB[i,t] + TPA_dead_DD[i,t])', shape = rectangle, fillcolor = '#fff7bc']
  recalculateSDI[label = 'SDI[t+1] = SUM(TPAall*(DBH/10)^1.6))', shape = rectangle, fillcolor = '#fff7bc']
  
}
calculateTPAMSB -> recalculateVALS
reduceTPA -> recalculateVALS
calcTPASDImort -> recalculateVALS
recalculateVALS -> TPAall -> recalculateSDI
recalculateSDI -> foryear 

}") %>% export_svg %>% charToRaw %>% rsvg_png("generate_forecast_workflow.png")



