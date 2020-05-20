# GrassBlue
## Modelling leaf elongation rate in response to variations in blue light and VPD

### Model simulation
Model simulation is done as follows:
- make sure that all files are in de same folder
- in the [01_ModelSimulation.R](./01_ModelSimulation.R) file: 
  - modify the working directory to that folder
  - adapt the PLANTNo parameter under Settings if desired
- Run the entire [01_ModelSimulation.R](./01_ModelSimulation.R) script

You will have created an RDS file starting with "Simulation_Exp2_" which can be read in R by the `readRDS()` function


__RDS files__

- Input files Transpiration:
  - Time 	 - h (0-1 is stabilisation, 1-6 is actual data)
  - Transp - g m-2 h-1
- Calib files Leaf elongation:
  - Time   - h (0-1 is stabilisation, 1-6 is actual data)
  - LER    - m h-1
  - LERnorm (normalised versus the mean of the datafile)

__txt file__

[GrassMod_Exp2.txt](./GrassMod_Exp2.txt): txt file of the model used in Exp2

__R files__

- [Thetas_Exp2.R](./Thetas_Exp2.R): generation of the 'thetas_Exp2' named vector based on parameter settings
- [Inits_Exp2.R](./Inits_Exp2.R): generation of the 'inits_Exp2' named vector based on initial values of state variables
- [01_ModelSimulation.R](01_ModelSimulation.R): script for model simulation with reference parameters including: 
  - Loading input data
  - Loading Calibration data
  - Simulation settings
  - Parameter values + automatic calculation of derived parameters and initial conditions
  - Model compilation (if necessary)
  - Simulation runs for 4 different Transpiration inputs (similar treatment) + mean Transpiration
