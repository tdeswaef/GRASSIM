# rm(list=ls())
setwd("C:/Users/tdeswaef/Documents/GRASS_BLUE/Exp_2")
library(RxODE)
library(ggplot2)
##################
# Input Data
##################

PsiSoil <- readRDS(file = "Input_Exp2_PsiSoil")
Transp1 <- readRDS(file = "Input_Exp2_Transp1")
Transp2 <- readRDS(file = "Input_Exp2_Transp2")
Transp3 <- readRDS(file = "Input_Exp2_Transp3")
Transp4 <- readRDS(file = "Input_Exp2_Transp4")
Transpmean <- readRDS(file = "Input_Exp2_Transpmean")

PsiSoil_fun <- approxfun(PsiSoil[,1], PsiSoil[,2])
Transp1_fun <- approxfun(Transp1[,1], Transp1[,2])
Transp2_fun <- approxfun(Transp2[,1], Transp2[,2])
Transp3_fun <- approxfun(Transp3[,1], Transp3[,2])
Transp4_fun <- approxfun(Transp4[,1], Transp4[,2])
Transpmean_fun <- approxfun(Transpmean[,1], Transpmean[,2])

####################
# Calibration data
####################
LER1 <- readRDS(file = "Calib_Exp2_LER1")
LER2 <- readRDS(file = "Calib_Exp2_LER2")
LER3 <- readRDS(file = "Calib_Exp2_LER3")
LER4 <- readRDS(file = "Calib_Exp2_LER4")
LER5 <- readRDS(file = "Calib_Exp2_LER5")

##################
# Settings
##################
PLANTNo <- 1  #(1 to 4, 5 is the mean)


START <- 0; STOP <- 5.9
times <- seq(START, STOP, 0.01)



ev <- eventTable(time.units='hours') %>%
  add.sampling(times)


inputs_1 <- data.frame(Psi_Soil_input = PsiSoil_fun(times), 
                      Transp_input = Transp1_fun(times))
inputs_2 <- data.frame(Psi_Soil_input = PsiSoil_fun(times), 
                       Transp_input = Transp2_fun(times))
inputs_3 <- data.frame(Psi_Soil_input = PsiSoil_fun(times), 
                       Transp_input = Transp3_fun(times))
inputs_4 <- data.frame(Psi_Soil_input = PsiSoil_fun(times), 
                       Transp_input = Transp4_fun(times))
inputs_mean <- data.frame(Psi_Soil_input = PsiSoil_fun(times), 
                       Transp_input = Transpmean_fun(times))

INPUTLIST <- list(inputs_1,inputs_2, inputs_3, inputs_4, inputs_mean)


###################################
# Parameters
###################################
CSuc.GZ <- 0.193;           CSuc.HZ <- 0.23;           CSuc.Blade <- 0.23
Temp <- 297;               Xyl_prop <- 0.005;         Le.Sheath <- 0.1
Peri_PX <- 0.0008 # Martre 2000: protoxylem perimeter
Volume.Root_init <- 1e-03; 
Width.Blade_init <- 0.009; Thick.Blade_init <- 0.00036
Le.Bladem_1_init <- 0.22; Le.Bladem_2_init <- 0.30; Le.Bladem_3_init <- 0.40 
Le.Bladem_4_init <- 0.10; Le.Bladem_5_init <- 0
Le.GZm_1_init <- 0; Le.GZm_2_init <- 0; Le.GZm_3_init <- 0
Le.GZm_4_init <- 0.04; Le.GZm_5_init <- 0.04; Le.HZm_5_init <- 0.01

Cond.GZx <- 0.0003;          Cond.HZx_G <- 0.005;          Cond.Bladex_G <- 0.0035
Cond.HZx_M <- 0.012;          Cond.Bladex_M <- 0.012    #(mmol s-1 m MPa-1)
Cond.Root <- 12.3 #(mmol s-1 m-2 MPa-1; Martre 2001) ! on a total blade area basis

Cond.GZm_G <- 6.5  #(mmol s-1 m-2 MPa-1; Martre 2000) ! on a protoxylem interface area basis
Cond.HZm_G <- 21; Cond.Bladem_G <- 21 #(mmol s-1 m-2 MPa-1; Martre 2001)
Cond.HZm_M <- 7.2; Cond.Bladem_M <- 7.2 #(mmol s-1 m-2 MPa-1; Martre 2001)

epsilon.Root <- 10;         Gamma <- 0.3
fi.GZm <- 0.1;             epsilon.GZx <- 1;         epsilon.MZx <- 3 
epsilon.GZm_l <- 0.15;       epsilon.GZm_w <- 0.15;       epsilon.GZm_th <- 0.15
epsilon.MZm_l <- 3;       epsilon.MZm_w <- 3;       epsilon.MZm_th <- 0.15



epsilon.GZm <- (epsilon.GZm_l * epsilon.GZm_w * epsilon.GZm_th )/ 
  (epsilon.GZm_l * epsilon.GZm_w + epsilon.GZm_l * epsilon.GZm_th + epsilon.GZm_th * epsilon.GZm_w)
epsilon.MZm <- (epsilon.MZm_l * epsilon.MZm_w * epsilon.MZm_th )/ 
  (epsilon.MZm_l * epsilon.MZm_w + epsilon.MZm_l * epsilon.MZm_th + epsilon.MZm_th * epsilon.MZm_w)



Load_4 <- 5600; Load_5 <- 5600




### Section below is not to be modified: setting units properly for the model

s_to_h <- 3600; M_Water <- 18; M_Suc <- 342; R <- 8.31; ro_Water <- 1e06

R.Root <- 1/(Cond.Root*M_Water/1000*s_to_h*Width.Blade_init*
              (Le.Bladem_1_init + Le.Bladem_2_init + Le.Bladem_3_init + Le.Bladem_4_init + Le.Bladem_5_init))

R.HZx_1 <- 1/(Cond.HZx_M*M_Water/1000*s_to_h/(Le.Sheath))
R.Bladex_1 <- 1/(Cond.Bladex_M*M_Water/1000*s_to_h/(Le.Bladem_1_init))

R.HZm_1 <- 1/(Cond.HZm_M*M_Water/1000*s_to_h*(Le.Sheath*Width.Blade_init))
R.Bladem_1 <- 1/(Cond.Bladem_M*M_Water/1000*s_to_h*(Le.Bladem_1_init*Width.Blade_init))

R.HZx_2 <- 1/(Cond.HZx_M*M_Water/1000*s_to_h/(Le.Sheath))
R.Bladex_2 <- 1/(Cond.Bladex_M*M_Water/1000*s_to_h/(Le.Bladem_2_init))

R.HZm_2 <- 1/(Cond.HZm_M*M_Water/1000*s_to_h*(Le.Sheath*Width.Blade_init))
R.Bladem_2 <- 1/(Cond.Bladem_M*M_Water/1000*s_to_h*(Le.Bladem_2_init*Width.Blade_init))

R.HZx_3 <- 1/(Cond.HZx_M*M_Water/1000*s_to_h/(Le.Sheath))
R.Bladex_3 <- 1/(Cond.Bladex_M*M_Water/1000*s_to_h/(Le.Bladem_3_init))

R.HZm_3 <- 1/(Cond.HZm_M*M_Water/1000*s_to_h*(Le.Sheath*Width.Blade_init))
R.Bladem_3 <- 1/(Cond.Bladem_M*M_Water/1000*s_to_h*(Le.Bladem_3_init*Width.Blade_init))

R.GZx_4 <- 1/(Cond.GZx*M_Water/1000*s_to_h/(Le.GZm_4_init))
R.HZx_4 <- 1/(Cond.HZx_G*M_Water/1000*s_to_h/(Le.Sheath - Le.GZm_4_init))
R.Bladex_4 <- 1/(Cond.Bladex_G*M_Water/1000*s_to_h/(Le.Bladem_4_init))

R.GZm_4 <- 1/(Cond.GZm_G*M_Water/1000*s_to_h*(Le.GZm_4_init)*Peri_PX)
R.HZm_4 <- 1/(Cond.HZm_G*M_Water/1000*s_to_h*(Le.Sheath - Le.GZm_4_init)*Width.Blade_init)
R.Bladem_4 <- 1/(Cond.Bladem_G*M_Water/1000*s_to_h*(Le.Bladem_4_init*Width.Blade_init))

R.GZm_4 <- 1/(Cond.GZm_G*M_Water/1000*s_to_h*(Le.GZm_5_init)*Peri_PX)
R.GZx_5 <- 1/(Cond.GZx*M_Water/1000*s_to_h/(Le.GZm_5_init))
R.HZx_5 <- 1/(Cond.HZx_G*M_Water/1000*s_to_h/(Le.HZm_5_init))
R.HZm_5 <- 1/(Cond.HZm_G*M_Water/1000*s_to_h*(Le.HZm_5_init*Width.Blade_init)) 

#########################
# Initial Conditions
#########################

F.GZm4_init <- Load_4*(Width.Blade_init * Thick.Blade_init * (Le.GZm_4_init) * (1-Xyl_prop)) / CSuc.GZ
F.GZm5_init <- Load_5*(Width.Blade_init * Thick.Blade_init * (Le.GZm_5_init) * (1-Xyl_prop)) / CSuc.GZ
Flow_Total  <- F.GZm4_init + F.GZm5_init
Transp_init <- INPUTLIST[[PLANTNo]]$Transp_input[1]
Psi.Soil_init <- INPUTLIST[[PLANTNo]]$Psi_Soil_input[1]

Volume.GZ_4_init <- Width.Blade_init * Thick.Blade_init * (Le.GZm_4_init)
Volume.GZ_5_init <- Width.Blade_init * Thick.Blade_init * (Le.GZm_5_init)

Volume.HZ_1_init <- Width.Blade_init * Thick.Blade_init * Le.Sheath
Volume.HZ_2_init <- Width.Blade_init * Thick.Blade_init * Le.Sheath
Volume.HZ_3_init <- Width.Blade_init * Thick.Blade_init * Le.Sheath
Volume.HZ_4_init <- Width.Blade_init * Thick.Blade_init * (Le.Sheath-Le.GZm_4_init)
Volume.HZ_5_init <- Width.Blade_init * Thick.Blade_init * (Le.HZm_5_init)

Volume.Blade_1_init <- Width.Blade_init * Thick.Blade_init * Le.Bladem_1_init
Volume.Blade_2_init <- Width.Blade_init * Thick.Blade_init * Le.Bladem_2_init
Volume.Blade_3_init <- Width.Blade_init * Thick.Blade_init * Le.Bladem_3_init
Volume.Blade_4_init <- Width.Blade_init * Thick.Blade_init * Le.Bladem_4_init

Tr.1_init <- Transp_init*Width.Blade_init *Le.Bladem_1_init
Tr.2_init <- Transp_init*Width.Blade_init *Le.Bladem_2_init
Tr.3_init <- Transp_init*Width.Blade_init *Le.Bladem_3_init
Tr.4_init <- Transp_init*Width.Blade_init *Le.Bladem_4_init

Fupt_init <- Transp_init* Width.Blade_init *
  (Le.Bladem_1_init+Le.Bladem_2_init+Le.Bladem_3_init+Le.Bladem_4_init) + Flow_Total

Psi.Root_init <- Psi.Soil_init - Fupt_init*R.Root/2

F.GZx4_init <- Tr.4_init + F.GZm4_init
F.GZx5_init <- F.GZm5_init

F.HZx1_init <- Tr.1_init; F.Bladex1_init <- Tr.1_init; F.Bladem1_init <- Tr.1_init
F.HZx2_init <- Tr.2_init; F.Bladex2_init <- Tr.2_init; F.Bladem2_init <- Tr.2_init
F.HZx3_init <- Tr.3_init; F.Bladex3_init <- Tr.3_init; F.Bladem3_init <- Tr.3_init
F.HZx4_init <- Tr.4_init; F.Bladex4_init <- Tr.4_init; F.Bladem4_init <- Tr.4_init
F.HZx5_init <- 0

P.HZx_1_init <-  Psi.Root_init - F.HZx1_init*(R.Root/2+R.HZx_1/2)
P.Bladex_1_init <- P.HZx_1_init - Tr.1_init*(R.Bladex_1/2+R.HZx_1/2)
P.HZm_1_init <- P.HZx_1_init + R*Temp*CSuc.HZ/M_Suc
P.Bladem_1_init <- P.Bladex_1_init - Tr.1_init*(R.Bladem_1) + R*Temp*CSuc.Blade/M_Suc
M.HZm_1_init <- CSuc.HZ*(1-Xyl_prop)*Volume.HZ_1_init*ro_Water
M.Bladem_1_init <- CSuc.Blade*(1-Xyl_prop)*Volume.Blade_1_init*ro_Water

P.HZx_2_init <-  Psi.Root_init - F.HZx2_init*(R.Root/2+R.HZx_2/2)
P.Bladex_2_init <- P.HZx_2_init - Tr.2_init*(R.Bladex_2/2+R.HZx_2/2)
P.HZm_2_init <- P.HZx_2_init + R*Temp*CSuc.HZ/M_Suc
P.Bladem_2_init <- P.Bladex_2_init - Tr.2_init*(R.Bladem_2) + R*Temp*CSuc.Blade/M_Suc
M.HZm_2_init <- CSuc.HZ*(1-Xyl_prop)*Volume.HZ_2_init*ro_Water
M.Bladem_2_init <- CSuc.Blade*(1-Xyl_prop)*Volume.Blade_2_init*ro_Water

P.HZx_3_init <-  Psi.Root_init - F.HZx3_init*(R.Root/2+R.HZx_3/2)
P.Bladex_3_init <- P.HZx_3_init - Tr.3_init*(R.Bladex_3/2+R.HZx_3/2)
P.HZm_3_init <- P.HZx_3_init + R*Temp*CSuc.HZ/M_Suc
P.Bladem_3_init <- P.Bladex_3_init - Tr.3_init*(R.Bladem_3) + R*Temp*CSuc.Blade/M_Suc
M.HZm_3_init <- CSuc.HZ*(1-Xyl_prop)*Volume.HZ_3_init*ro_Water
M.Bladem_3_init <- CSuc.Blade*(1-Xyl_prop)*Volume.Blade_3_init*ro_Water

P.GZx_4_init <-  Psi.Root_init - F.GZx4_init*(R.Root/2+R.GZx_4/2)
P.HZx_4_init <-  P.GZx_4_init - Tr.4_init*(R.GZx_4/2+R.HZx_4/2)
P.Bladex_4_init <- P.HZx_4_init - Tr.4_init*(R.Bladex_4/2+R.HZx_4/2)
P.GZm_4_init <- P.GZx_4_init - F.GZm4_init*(R.GZm_4) + R*Temp*CSuc.GZ/M_Suc
P.HZm_4_init <- P.HZx_4_init + R*Temp*CSuc.HZ/M_Suc
P.Bladem_4_init <- P.Bladex_4_init - Tr.4_init*(R.Bladem_4) + R*Temp*CSuc.Blade/M_Suc
M.GZm_4_init <- CSuc.GZ*(1-Xyl_prop)*Volume.GZ_4_init*ro_Water
M.HZm_4_init <- CSuc.HZ*(1-Xyl_prop)*Volume.HZ_4_init*ro_Water
M.Bladem_4_init <- CSuc.Blade*(1-Xyl_prop)*Volume.Blade_4_init*ro_Water

P.GZx_5_init <- Psi.Root_init - F.GZx5_init*(R.Root/2+R.GZx_5/2)
P.HZx_5_init <- P.GZx_5_init
P.GZm_5_init <- P.GZx_5_init - F.GZm5_init*(R.GZm_5) + R*Temp*CSuc.GZ/M_Suc
P.HZm_5_init <- P.HZx_5_init + R*Temp*CSuc.HZ/M_Suc
M.GZm_5_init <- CSuc.GZ*(1-Xyl_prop)*Volume.GZ_5_init*ro_Water
M.HZm_5_init <- CSuc.HZ*(1-Xyl_prop)*Volume.HZ_5_init*ro_Water


source("Thetas_Exp2.R")
source("Inits_Exp2.R")

##########################
# Model compiling
##########################
Model_GRASS <- RxODE(filename="GrassMod_Exp2.txt", modName = "GrassMod_Exp2", wd=".")
#Model_GRASS
# saveRDS(Model_GRASS, "Model_GRASS")


###########################
# Simulation

#Simulation mean
ptm <- Sys.time()
out_Exp2 <- as.data.frame(Model_GRASS$solve(thetas_Exp2, ev, inits_Exp2, 
                                              covs=INPUTLIST[[PLANTNo]], covs_interpolation = "linear"))
Sys.time()-ptm


ggplot() + geom_line(data=out_Exp2, aes(x=time, y=Psi.GZx_4)) +
  geom_line(data=out_Exp2, aes(x=time, y=Psi.HZx_4)) +
  geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladex_4)) +
  geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladem_4)) +
  geom_line(data=out_Exp2, aes(x=time, y=Psi.GZm_4), color='blue')
# 
ggplot() + geom_line(data=out_Exp2, aes(x=time, y=P.GZm_4))
# 
# ggplot() +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.HZx_1)) +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladex_1)) +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladem_1), color='blue') 
# 
# ggplot() +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.HZx_2)) +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladex_2)) +
#   geom_line(data=out_Exp2, aes(x=time, y=Psi.Bladem_2), color='blue') 
# 
# ggplot() +
#   geom_line(data=out_Exp2, aes(x=time, y=F.Root)) +
#   geom_line(data=out_Exp2, aes(x=time, y=Transp.Blade_1), color='red') +
#   geom_line(data=out_Exp2, aes(x=time, y=Transp.Blade_2), color='blue') +
#   geom_line(data=out_Exp2, aes(x=time, y=Transp.Blade_1+Transp.Blade_2+Transp.Blade_3+Transp.Blade_4), color='orange') 


# out_ref <- out_Exp2
saveRDS(out_Exp2, paste0("Simulation_Exp2_",PLANTNo))

# print(epsilon.MZm)
# 
ggplot() +
  geom_line(data = out_ref, aes(x=time, y=dLe.Leaf_4), size = 1) +
  geom_line(data = out_Exp2, aes(x=time, y=dLe.Leaf_4), color= 'red', size = 2)

# ggplot() +
#   geom_line(data = out_ref, aes(x=time, y=P.Bladem_2), size = 1) + 
#   geom_line(data = out_Exp2, aes(x=time, y=P.Bladem_2), color= 'red', size = 2) 
