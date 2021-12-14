run_over_scen_end_game = function(ve=0.75, vp=0.9, rapid=FALSE, bc_scen=TRUE, reopenR, ramp_T, days)
{
  #Initial stage
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=1.0, in_school=FALSE)
  H = c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.12,0.0,0.0,0.0,0.0,0.0,0.0)*N_i 
  V0 <- N_i-H
  # Just run for one day to get df set up...
  df0 <- run_sim_basic(C, I0, percent_vax=0.12, strategy=list(1:15), num_perday=1.0, 
                       v_e=rep(ve, num_groups), v_p=rep(vp,num_groups),
                       u = u_var, num_days=2,H=H, 
                       with_essential=TRUE)
  
  vax <- get_num_vax(df0)
  
  df0$R1 = initRecovered[1]
  df0$R2 = initRecovered[2]
  df0$R3 = initRecovered[3]
  df0$R4 = initRecovered[4]
  df0$R5 = initRecovered[5]
  df0$R6 = initRecovered[6]
  df0$R7 = initRecovered[7]
  df0$R8 = initRecovered[8]
  df0$R9 = initRecovered[9]
  df0$Re3 = initRecovered[10]
  df0$Re4 = initRecovered[11]
  df0$Re5 = initRecovered[12]
  df0$Re6 = initRecovered[13]
  df0$Re7 = initRecovered[14]
  df0$Re8 = initRecovered[15]
  
  # next stages : no more vax until may, 565 cases on Jan 1 2021, slightly decreased 
  # to around 400 on Feb 15 2021, then slightly increase to 575 Mar 21 2021, rapid 
  # increase to 1250 on Apr 15 2021, rapid decrease to 45 on Jul 22 2021, then due to 
  # reopening stages increase to 500 on Sep 01 2021, fluctuated now. 
  
  # vax, nothing changes until may 8, MAY 8 - MAY 31 increase 80+ from 7% to 14%
  # 80+ increase to 90% on mid July (july 15)
  # 70-79 increase to 90% on July 29 (May 29 - July 29)
  # 60-69 increase to 75% on July 15 (June 5 - July 15), slightly increase to 89% (Jult 15 - Oct)
  # 50-59 increase to 65% on July 15, slightly to 81%
  # 40-49 increase to 50% ... 82%
  # 30-39 40% ... 82%
  # 18-29 28% ... 75%
  # 0-17 0% ... 26%
  # resources: https://covid19tracker.ca/provincevac.html?p=BC
  
  
  ############################################################################
  
  # stage 1, 565 to 400 from Jan 1 to Feb 15, 45 days, only change R0
  alpha=0.0
  T1 <- 45
  n <- 0.0 
  #####change R from 0.9 to 0.95
  #0.95 to 1.0
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=1.0, in_school=FALSE, alpha_factor=alpha)
  C[1,1] = C[1,1]*2.5
  C[2,2] = C[2,2]*2.5
  C[9,9] = C[9,9]*0.2
  
  
  df1 <- run_sim_restart(C, df_0=tail(df0, n=1), percent_vax = 1.0, strategy=list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T1, with_essential=TRUE, H=H)
  
  #############################################################################
  
  # stage 2 400 to 575, around 40 days, only change R0
  
  #H1 = c(0.25,0.15,0.17,0.15,0.14,0.16,0.10,0.05,0.05,0.17,0.15,0.14,0.16,0.10,0.10)*N_i 
  #n <- 0.0005
  alpha=0.0
  ###change T2 40 to 30
  #30 to 25
  T2 <- 25
  n <- 0.0 
  
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=1.1, in_school=FALSE, alpha_factor=alpha)
  C[1,1] = C[1,1]*2.5
  C[2,2] = C[2,2]*2.5
  C[9,9] = C[9,9]*0.2
  df2 <- run_sim_restart(C, df_0=tail(df1, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T2, with_essential=TRUE, H=H)
  
  
  #############################################################################
  
  # stage 3 575 to 1250, around 35 days, only change R0
  
  alpha=0.0
  T3 <- 30
  n <- 0.0 
  
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=1.4, in_school=FALSE, alpha_factor=alpha)

  C[,1] = C[,1]*1.5
  C[,2] = C[,2]*1.5
  C[,6] = C[,6]*0.5
  C[4,4] = C[4,4]*0.2
  C[5,5] = C[5,5]*0.2
  C[6,6] = C[6,6]*0.2
  C[,13] = C[,13]*0.5
  C[,8] = C[,8]*0.4
  C[,15] = C[,15]*0.4
  C[,9] = C[,9]*0.1
  C[1,1] = C[1,1]*2.5
  C[2,2] = C[2,2]*1.5
  C[9,9] = C[9,9]*0.05
  df3 <- run_sim_restart(C, df_0=tail(df2, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T3, with_essential=TRUE, H=H)
  
  
  
  #############################################################################
  # stage 4 1250 to 45, around 90 days, need to consider vax now, vax start on 
  # May 22, therefore 35 days no vax, 55 days vax, really high restriction.
  #############################################################################
  
  # stage 4-1 no vax for 35 days.
  alpha=0.0
  T4 <- 35
  n <- 0.0 
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=0.85, in_school=FALSE, alpha_factor=alpha)
  
  C[1,1] = C[1,1]*4.5
  C[2,2] = C[2,2]*3.0
  C[9,9] = C[9,9]*0.1
  C[8,8] = C[8,8]*0.5
  C[4,4] = C[4,4]*0.5
  C[5,5] = C[5,5]*0.5
  C[6,6] = C[6,6]*0.5
  df4 <- run_sim_restart(C, df_0=tail(df3, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T4, with_essential=TRUE, H=H)
  
  # stage 4-2 start vax
  
  #90% vax, BC
  if(bc_scen == TRUE){
    H1 = c(0.99,0.99,0.7,0.6,0.5,0.4,0.25,0.15,0.10,0.7,0.6,0.5,0.4,0.25,0.15)*N_i}
  
  #70% vax, non-BC
  else{
    H1 = c(0.99,0.99,0.8,0.7,0.6,0.5,0.35,0.25,0.20,0.8,0.7,0.6,0.5,0.35,0.25)*N_i}
  
  alpha=0.0
  T5 <- 45
  n <- 0.05
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=1.1, in_school=FALSE, alpha_factor=alpha)
  C[1,1] = C[1,1]*0.5
  C[2,2] = C[2,2]*0.5
  C[9,9] = C[9,9]*0.1
  C[8,8] = C[8,8]*0.5
  C[4,4] = C[4,4]*0.5
  C[5,5] = C[5,5]*0.5
  C[6,6] = C[6,6]*0.5
  df5 <- run_sim_restart(C, df_0=tail(df4, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T5, with_essential=TRUE, H=H1)
  
  
  #############################################################################
  
  # stage 5, reopening and vax
  
  #90% vax, BC
  if(bc_scen == TRUE){
    H2 = c(0.99,0.8,0.17,0.15,0.14,0.16,0.10,0.05,0.05,0.17,0.15,0.14,0.16,0.10,0.10)*N_i}

  #70% vax, non-BC
  else{
    H2 = c(0.99,0.85,0.37,0.35,0.34,0.36,0.30,0.35,0.35,0.37,0.35,0.34,0.36,0.30,0.30)*N_i}
  
  alpha=0.0
  T6 <- 50
  n <- 0.002

  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=2.1, in_school=TRUE, alpha_factor=alpha)
  
  
  df6 <- run_sim_restart(C, df_0=tail(df5, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T6, with_essential=TRUE, H=H2)
  
  
  #############################################################################
  
  # stage 6, reopening and vax, no more vax (almost)
  
  #90% vax, BC
  if(bc_scen == TRUE){
  H3 = c(0.99,0.8,0.17,0.15,0.14,0.16,0.10,0.05,0.05,0.17,0.15,0.14,0.16,0.10,0.10)*N_i}
  
  #70% vax, non-BC
  else{
  H3 = c(0.99,0.85,0.37,0.35,0.34,0.36,0.30,0.35,0.35,0.37,0.35,0.34,0.36,0.30,0.30)*N_i}
  
  alpha=0.0

  T7 <- 120
  n <- 0.0022
  
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=2.2, in_school=TRUE, alpha_factor=alpha)

  C[1,1] = C[1,1]*1.3
  C[,2] = C[,2]*0.9
  C[,3] = C[,3]*0.9
  C[,4] = C[,4]*0.9
  C[,5] = C[,5]*0.9
  C[,6] = C[,6]*0.9
  C[7,7] = C[7,7]*3
  C[8,8] = C[8,8]*2
  C[2,2] = C[2,2]*0.5
  
  
  df7 <- run_sim_restart(C, df_0=tail(df6, n=1), percent_vax =1.0, strategy= list(1:15), num_perday=n,
                         v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                         u = u_var, num_days=T7, with_essential=TRUE, H=H3)
  
  
  
  #############################################################################
  # stage 7, gradual reopening and rapid reopening. Gradual reopen linearly 
  # over 300 day window, run for 900 days.
  
  #############################################################################
  if(rapid == FALSE){
  R_vec = get_R_vec(R1=2.2,R2=reopenR,start_ramp = 1,end_ramp = ramp_T,ndays = days)
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=reopenR, in_school=TRUE, alpha_factor=alpha)
  
  alpha=0.0
  T8 <- days
  n <- 0.0
  
  
  df <- run_sim_restart_ramp_R(C=C, df_0=tail(df7, n=1),  R_vec=R_vec, percent_vax =1.0, strategy= list(1:15), num_perday=n,
                               v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                               u = u_var, num_days=T8, with_essential=TRUE, H=H)}
  
  else{  
  alpha=0.0
  T8 <- days
  n <- 0
  C <- construct_C_from_prem(home=mu_home, work=mu_work, school=mu_school, other=mu_other, u=u_var,
                             target_R0=reopenR, in_school=TRUE, alpha_factor=alpha)
  
  df <- run_sim_restart(C, df_0=tail(df7, n=1), percent_vax = 1.0, strategy=list(1:15), num_perday=n,
                        v_e = rep(ve, num_groups), v_p=rep(vp, num_groups),
                        u = u_var, num_days=T8, with_essential=TRUE, H=H)}
  

  #############################################################################
  #combine
  
  df1$time = df1$time+2+1
  df2$time = df2$time+T1+2+1
  df3$time = df3$time+T1+T2+2+1
  df4$time = df4$time+T1+T2+T3+2+1
  df5$time = df5$time+T1+T2+T3+T4+2+1
  df6$time = df6$time+T1+T2+T3+T4+T5+2+1
  df7$time = df7$time+T1+T2+T3+T4+T5+T6+2+1
  df$time = df$time+T1+T2+T3+T4+T5+T6+T7+2+1
  
  df <- combine_age_groups(rbind(df0,df1[2:T1-1,],df2[2:T2-1,],
                                 df3[2:T3-1,],df4[2:T4-1,],df5[2:T5-1,],
                                 df6[2:T6-1,],df7[2:T7-1,],
                                 df[2:T8-1,]))
  
  # add pars
  df$ve <- ve
  df$vp <- vp
  df$alpha <- alpha
  return(df)
}


