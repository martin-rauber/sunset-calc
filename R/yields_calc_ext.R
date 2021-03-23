####################################################################################
#yields_calc_ext
#this section of code was written by Gary Salazar: gary.salazar@dcb.unibe.ch
####################################################################################

#Load libraries----------------------------------------------------------------------
library(pracma)
#library(readxl)
#excel_title_row<-28
choose<-c(1,2,3); total_fractions<-3; split_point_algoritm<-"temperature"
n_avg<-50                   #total points for moving average
yield_points<-22            #total points for yield average measurement
average_type<-"forward"     #forward, centered or golay 
#fitting_type<-"poly"      #poly, expo or manual
Golay_p.order<-3
#manual.coef<-c(8562,-0.0235,-5.00E-4)
#manual.coef<-c(7645, -0.371, 5.32E-4)
error_message<-""

#-------------- strong exponential fitting --------------
super_fitting_expo<-function(ms,ls) {
  minimization<-data.frame(ls0=single(),K=single(),K1=single(),ms0=single(),K2=single(),res=single())
  ms_mid<-mean(c(max(ms),min(ms))); ls_mid<-mean(c(max(ls),min(ls)))
  cuenta<-0; K_initial<-(max(ls)-min(ls));ls0_initial<-max(ls)+(5.5*K_initial); ls0_final<-max(ls)
  
  for (ls0 in seq(from=ls0_initial,to=ls0_final,length.out=25)) {
    for (ms0 in seq(from=max(ms),to=1.2*max(ms),length.out=15)) { 
      q<-c()
      try(q<-nls(ls ~ ls0 + K*(exp(K1*(ms0-ms))/(K2+exp(K1*(ms0-ms)))), data = ds,start = list(K=K_initial, K1=4E-3,K2=1),
                 upper=list(K=abs(1.8*K_initial),K1=1,K2=2),lower=list(K=-abs(1.8*K_initial),K1=-1,K2=1),algorithm="port"),silent=TRUE) 
      if (length(coef(q))>0) {
        cuenta<-cuenta+1   
        r<-sum(resid(q)^2)
        p<-as.list(coef(q))
        minimization[cuenta,1]<-ls0
        minimization[cuenta,2]<-p[1]
        minimization[cuenta,3]<-p[2]
        minimization[cuenta,4]<-ms0
        minimization[cuenta,5]<-p[3]
        minimization[cuenta,6]<-r
      } 
    }
  }
  parameters<-as.numeric(minimization[which.min(minimization[,6]),c(1:5)])
  return(parameters)
}

#---------------- moving average function  ----------------
fwd_moving_avg<-function(data_y,n_avg){
  out_data_y<-c()
  n_data<-length(data_y)
  for (ciclo_cuenta in c(1:n_data)) {
    start<-ciclo_cuenta; end<-ciclo_cuenta+n_avg-1
    if (end>n_data) {
      end<-n_data
    }
    suma<-sum(data_y[start:end]); n<-end-start+1; promedio<-suma/n
    out_data_y[ciclo_cuenta]<-promedio
  }
  return(out_data_y)
}
#------------- recalc ATN from polynomial function  ------------
ATN_poly_calc<-function(guess_poly) {
  laser_corr<-(guess_poly[3]*temp_lag^2)+(guess_poly[2]*temp_lag)+guess_poly[1]
  ATN_new<- -100*log(laser_smooth/laser_corr)
  return(ATN_new)
}

#------------ calc yields and charring function ----------------

ATN_to_yields_all<-function(ATN) {
  EC_yield_calc<-c();charring_calc<-c();ATNmax_xlag1<-c();ATNt_xlag1<-c()
  count<-1;ATN_t1<-c();ATN_max1<-c();count_max<-1
  ATN0_range<-which(temp_lag[1:data_search_x[2]]>240 & temp_lag[1:data_search_x[2]]<280)
  ATN0<-mean(ATN[ATN0_range])-min(ATN)
  data_search_y1<-temp_lag[data_search_x]/max(temp_lag)
  
  while (count<=length(choose)) {
    low<-0;high<-0
    low<-data_search_x[2*count]-as.integer(yield_points/2)
    high<-data_search_x[2*count]+as.integer(yield_points/2)
    range<-data_search_x[count_max]
    range_end<-data_search_x[count_max+1]
    ATN_t1[count]<-mean(ATN[low:high])-min(ATN)    
    ATN_max1[count]<-max(ATN[range:range_end])-min(ATN) 
    ATNt_xlag1[count]<-paste(toString(low),"-",toString(high))
    ATNmax_xlag1[count]<-paste(toString(range),"-",toString(range_end))
    count<-count+1;count_max<-count_max+2
  }
  
  EC_yield_calc[1]<-ATN_t1[1]/ATN_max1[1]
  EC_yield_calc[2]<-EC_yield_calc[1]*ATN_t1[2]/ATN_max1[2]
  EC_yield_calc[3]<-EC_yield_calc[2]*ATN_t1[3]/ATN_max1[3]
  charring_calc[1]<-(ATN_max1[1]-ATN0)/ATN0
  charring_calc[2]<-(ATN_max1[2]-ATN_t1[1])/ATN0
  charring_calc[3]<-(ATN_max1[3]-ATN_t1[2])/ATN0
  return(c(EC_yield_calc,charring_calc,ATN_t1,ATN_max1,ATNt_xlag1,ATNmax_xlag1))
}

ATN_to_yields_calc<-function(ATN) {
  EC_yield_calc<-c();charring_calc<-c();ATNmax_xlag1<-c();ATNt_xlag1<-c()
  count<-1;ATN_t1<-c();ATN_max1<-c();count_max<-1
  ATN0_range<-which(temp_lag[1:data_search_x[2]]>240 & temp_lag[1:data_search_x[2]]<280)
  ATN0<-mean(ATN[ATN0_range])-min(ATN)
  data_search_y1<-temp_lag[data_search_x]/max(temp_lag)
  
  while (count<=length(choose)) {
    low<-0;high<-0
    low<-data_search_x[2*count]-as.integer(yield_points/2)
    high<-data_search_x[2*count]+as.integer(yield_points/2)
    range<-data_search_x[count_max]
    range_end<-data_search_x[count_max+1]
    ATN_t1[count]<-mean(ATN[low:high])-min(ATN)    
    ATN_max1[count]<-max(ATN[range:range_end])-min(ATN) 
    ATNt_xlag1[count]<-paste(toString(low),"-",toString(high))
    ATNmax_xlag1[count]<-paste(toString(range),"-",toString(range_end))
    count<-count+1;count_max<-count_max+2
  }
  
  EC_yield_calc[1]<-ATN_t1[1]/ATN_max1[1]
  EC_yield_calc[2]<-EC_yield_calc[1]*ATN_t1[2]/ATN_max1[2]
  EC_yield_calc[3]<-EC_yield_calc[2]*ATN_t1[3]/ATN_max1[3]
  charring_calc[1]<-(ATN_max1[1]-ATN0)/ATN0
  charring_calc[2]<-(ATN_max1[2]-ATN_t1[1])/ATN0
  charring_calc[3]<-(ATN_max1[3]-ATN_t1[2])/ATN0
  return(c(EC_yield_calc,charring_calc))
}

poly_to_Q.charr<-function(K) {  #K1 first order, K2 second order coef.
  guess_poly<-c(K0,K[1],K[2])
  ATN<-ATN_poly_calc(guess_poly)
  yields_calc<-ATN_to_yields_calc(ATN)
  charring1<-abs(yields_calc[c(4:6)])
  total_charr<-sum(charring1) 
  return(total_charr)
}
poly_to_Q.yield<-function(K) {  #K1 first order, K2 second order coef.
  guess_poly<-c(K0,K[1],K[2])
  ATN<-ATN_poly_calc(guess_poly)
  yields_calc<-ATN_to_yields_calc(ATN)
  EC_yield1<-yields_calc[c(1:3)]
  Q.yield<-3-sum(EC_yield1)   #Q.yield is minimum when sum(EC_yields is near 3)              
  return(Q.yield)
}
poly_to_Q.laser_ATN<-function(K) { #K1 first order, K2 second order coef.
  guess_poly<-c(K0,K[1],K[2])
  ATN<-ATN_poly_calc(guess_poly)
  yields_calc<-ATN_to_yields_calc(ATN)
  ATN_mid<-mean(c(max(ATN),min(ATN)))
  ATN_norm<-(ATN-ATN_mid)/(abs(max(ATN-ATN_mid)))
  laser_ATN_Q<-abs(sum(laser_smooth_norm+ATN_norm))
  return(laser_ATN_Q)
}

#----------Select sample and take raw data -------------------------
bracket<-5    # if search by Temp activated, set bracket
x<-c();laser<-c();temp<-c();request<-c();largo<-0
selected_sample<-2
laser_col<-3
temp_col<-2
x<-c(1:nrow(tabla_complete))
laser<-as.vector(tabla_complete[,4])
temp<-as.vector(tabla_complete[,3])
request<-as.vector(tabla_complete[,10])
co2<-as.vector(tabla_complete[,16])
largo<-length(x)

par(mfrow=c(3,1))  #print raw data
plot(x,laser,type="l")
plot(x,co2,type="l")
plot(x,temp,type="l")
par(mfrow=c(1,1))

#---------------------------smooth moving average of 20 back ---raw to lag ---------------------------
if (average_type=="centered") {
  a<-as.integer(n_avg/2);b<-largo-a
  f20<-rep(1/n_avg,n_avg)
  y_lag<-filter(laser,f20, sides=2)
  t_lag<-filter(temp,f20,sides=2)
  co2_lag<-filter(co2,f20,sides=2)
  request_lag<-request[a:b]
  x_lag<-x[a:b]
  largo_lag<-length(x_lag)
  laser_smooth<-y_lag[a:b]
  temp_lag<-t_lag[a:b]
}
if (average_type=="forward") {
  f20<-rep(1/n_avg,n_avg)
  #y_lag<-na.omit(rev(filter(rev(laser),f20, method=c("convolution"), sides=1)))
  y_lag<-fwd_moving_avg(laser,n_avg)
  #t_lag<-na.omit(rev(filter(rev(temp),f20, method=c("convolution"), sides=1)))
  t_lag<-fwd_moving_avg(temp,n_avg)
  #co2_lag<-na.omit(rev(filter(rev(co2),f20, method=c("convolution"), sides=1)))
  co2_lag<-fwd_moving_avg(co2,n_avg)
  b<-length(y_lag)
  request_lag<-request[1:b]
  x_lag<-x[1:b]
  largo_lag<-length(x_lag)
  laser_smooth<-y_lag[1:b]
  temp_lag<-t_lag[1:b]
}
if (average_type=="golay") {
  library(signal)
  if (((n_avg/2)-trunc(n_avg/2))==0) {n_avg<-n_avg+1}
  b<-length(laser)-(n_avg)
  laser_smooth<-sgolayfilt(laser,Golay_p.order,n_avg)[1:b]
  temp_lag<-sgolayfilt(temp,Golay_p.order,n_avg)[1:b]
  co2_lag<-sgolayfilt(co2,Golay_p.order,n_avg)[1:b]
  x_lag<-x[1:b]
  request_lag<-request[1:b]
  
}
#-------------laser vs temperature --exponential fit of laser during cooling down from max temp (last fraction)---------------------
request_diff<-temp_lag-request_lag

# L_start<-which.max(request_diff)  #algorithm difference between requested and real temperature
# MR v. 1.0.5 L_start defined as value with max temperature: max temp. will always be the last step
L_start<-which.max(temp_lag)
L_end<-length(x_lag)
L1<-laser_smooth[L_start]
L2<-laser_smooth[L_end]
T1<-temp_lag[L_start]
T2<-temp_lag[L_end]
ms<-temp_lag[L_start:L_end]
ls<-laser_smooth[L_start:L_end]
ds <- data.frame(ms = ms, ls = ls)
max_laser<-1.1*max(ls); half_laser<-(max_laser+(min(ls)))/2   #maximum laser plus 10%
fitting_expo<-c(); fitting_poly<-c()

#first approx. simulatneous equations

Kpoly<-function(L1,L2,T1,T2,max_laser) {
  result2<-((T2*(L1-max_laser)/T1)-(T1*(L2-max_laser)/T2))/(T2-T1)
  result3a<-(((L1-max_laser)/T1)-result2)/T1
  result3b<-(((L2-max_laser)/T2)-result2)/T2
  result3<-mean(c(result3a,result3b))
  return(c(result2,result3))
}

#fitting model  
fitting_poly.nls<-c()
Kcoe<-Kpoly(L1,L2,T1,T2,max_laser)
K2_initial<-0.1*Kcoe[1]; K3_initial<--0.1*Kcoe[2]
K2_min<--abs(2*K2_initial); K3_min<--abs(3*K3_initial)
K2_max<-abs(2*K2_initial); K3_max<-0

if (fitting_type=="poly") {
  fitting_poly.nls<-tryCatch({
    nls(ls ~ (K3*ms^2+K2*ms+K),data=ds, start=list(K=1.0*max_laser,K2=K2_initial,K3=K3_initial),
        lower=list(K=0.7*max_laser,K2=K2_min,K3=K3_min), upper=list(K=1.5*max_laser,K2=K2_max,K3=K3_max),algorithm="port")
  })
  tryCatch({r.sqr<-max(cor(ls,predict(fitting_poly.nls)),0)^2; fitting_poly.coef<-coef(fitting_poly.nls)})
  
  if (length(fitting_poly.nls)==0) {
    fitting_poly.nls<-tryCatch({
      nls(ls ~ (K3*ms^2+K2*ms+K),data=ds, start=list(K=max_laser,K2=K2_initial,K3=K3_initial),
          algorithm="port")
    })
    tryCatch({r.sqr<-max(cor(ls,predict(fitting_poly.nls)),0)^2; fitting_poly.coef<-coef(fitting_poly)})
  }
  if (length(fitting_poly.nls)==0) {
    error_message<-paste(error_message," ,bad fitting for laser correction"); print(error_message)
  }
}

if (fitting_type=="expo") {
  last_expo<-as.numeric(super_fitting_expo(ms,ls))
  ls0<-last_expo[1];ms0<-last_expo[4];K_initial<-last_expo[2];K1_initial<-last_expo[3]; K2_initial<-last_expo[5]
  fitting_expo<-nls(ls ~ ls0 + K*(exp(K1*(ms0-ms))/(K2+exp(K1*(ms0-ms)))), data = ds,start = list(K=K_initial, K1=4E-3,K2=1),
                    upper=list(K=abs(1.8*K_initial),K1=2,K2=2),lower=list(K=-abs(1.8*K_initial),K1=-1,K2=1),algorithm="port")
  fitting_expo.coef<-as.numeric(coef(fitting_expo))
  tryCatch({r.sqr<-max(cor(ls,predict(fitting_expo)),0)^2})
  
  if (length(fitting_expo)==0) {
    error_message<-paste(error_message," ,bad fitting for laser correction"); print(error_message)
  }
}

if (fitting_type=="manual") {
  fitting_poly.coef<-manual.coef; fitting.coef<-manual.coef
}

# -----------------------correct the laser L0 and calc ATN-----------------------------
if (fitting_type=="expo") {
  laser_corr<-ls0 + fitting_expo.coef[1]*(exp(fitting_expo.coef[2]*(ms0-temp_lag))/(fitting_expo.coef[3]+exp(fitting_expo.coef[2]*(ms0-temp_lag)))) #exponential fitting
  fitting.coef<-fitting_expo.coef
  plot(ms,ls,col="red",main=paste("K0=",as.integer(fitting_expo.coef[1])," K1=",signif(fitting_expo.coef[2],3)," K2=",signif(fitting_expo.coef[3],3)," r^2=",signif(r.sqr,3)))
  lines(ms,predict(fitting_expo,data.frame(x=ms)),col="blue")
}
if (fitting_type=="poly" || fitting_type=="manual") {
  laser_corr<-(fitting_poly.coef[3]*temp_lag^2)+(fitting_poly.coef[2]*temp_lag)+fitting_poly.coef[1]
  fitting.coef<-fitting_poly.coef
  
  ATN<--100*log(laser_smooth/laser_corr)
  
  #title for plot_fit
  plot_fit_title <- bquote(K[0] == .(as.integer(fitting_poly.coef[1])) ~ " " ~ K[1] == .(signif(fitting_poly.coef[2],3))~ " " ~ K[2] == .(signif(fitting_poly.coef[3],3))~ " " ~r^2 == .(signif(r.sqr,3)))
#plot_fit
  temp_laser_data = as.data.frame(cbind(temp_lag,laser_corr))
  theme_set(theme_classic(base_size = 13,base_family = "Helvetica"))
  plot_fit = ggplot(ds, aes(x=ms, y=ls)) +
    geom_point(shape=20, color ="red")+
    geom_line(temp_laser_data, mapping = aes(temp_lag,laser_corr),col="blue")+
    xlab("Temperature (°C)")+
    ylab("Laser signal (a.u.)")+
    labs(title = plot_fit_title)
  plot_fit = plot_fit+ theme( plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
  plot_fit
}

#black is uncorrected smoothed laser, red is corrected smoothed laser, blue is ATN of corrected smoothed laser
#data is normalised 

laser_data <- as.data.frame(cbind(x_lag,laser_smooth, laser_corr, ATN))
laser_data_normalised <- as.data.frame(cbind(x_lag,laser_smooth/max(laser_smooth)*100, laser_corr/max(laser_smooth)*100, ATN/max(ATN)*100))
colnames(laser_data_normalised) <- c("x_lag","laser_smooth", "laser_corr", "ATN")
theme_set(theme_classic(base_size = 13,base_family = "Helvetica"))
plot_TwoSide <- ggplot(laser_data_normalised) +
  geom_line(aes(x_lag,laser_smooth),color = "black")+
  geom_line(aes(x_lag,laser_corr),color = "red")+
  geom_line(aes(x_lag,ATN),color = "blue")+
  xlab("Time (s)")+
  ylab("Normalised (%)")+
  labs(title = "black: unc laser, red: corr laser, blue: ATN" )
  plot_TwoSide <- plot_TwoSide + theme( plot.margin = margin(0.5, 0.5, 0.5, 0.5, "in"))
  plot_TwoSide

# par(mfrow=c(3,1))
# plot(x_lag,ATN,type="l")
# plot(x_lag,co2_lag,type="l")
# plot(x_lag,temp_lag,type="l")
# par(mfrow=c(1,1))


#---------------------- find time points by requested T bracket search ----------------
if (split_point_algoritm=="temperature") {
  
  data_search_x<-c(); data_search_y<-c() 
  request_jumps2_index<-c(); request_jumps2_T<-c()
  plot(x_lag,request_lag, type="l")
  lines(x_lag,temp_lag, type="l")
  request_jumps<-diff(request_lag)   #the derivative of the temp request gives accurately the temperature jumps
  request_jumps2_index<-which(request_jumps!=0)    #position or index of the temp jumps
  request_jumps2_T<-request_jumps[request_jumps2_index]  #magnitude of the temp jumps
  look<-1;data_search_jump<-c();jump_i<--1;jump_p<-0; jump_n<-0         #look is the counter for thermogram fractions     
  
  while (look<length(request_jumps2_T)) {
    jump1<-request_jumps2_T[look];jump2<-request_jumps2_T[look+1]
    #DEFINITION of thermogram fraction: region bracketed by a positive and negative jump of requested temperature
    if (jump1<0 & jump2<0) {
      if (request_jumps2_T[look+1]<request_jumps2_T[look]) {jump_n<-look+1}
      if (request_jumps2_T[look+1]>=request_jumps2_T[look]) {jump_n<-look}
      data_search_jump[jump_i+1]<-request_jumps2_index[jump_n]
      jump_n<-0
    } 
    if (jump1>0 & jump2>0) {
      if (request_jumps2_T[look+1]>request_jumps2_T[look]) {jump_p<-look+1}
      if (request_jumps2_T[look+1]<=request_jumps2_T[look]) {jump_p<-look}
    } 
    if (jump1>0 & jump2<0) {
      jump_i<-jump_i+2
      if (jump_p==0) {jump_p<-look}
      if (jump_n==0) {jump_n<-look+1}
      data_search_jump[jump_i]<-request_jumps2_index[jump_p]
      data_search_jump[jump_i+1]<-request_jumps2_index[jump_n]
      initial_t<-temp_lag[request_jumps2_index[look]-1]
      max_temp<-max(temp_lag[request_jumps2_index[look]:request_jumps2_index[look+1]])
      max_temp_index<-request_jumps2_index[look]+which.max(temp_lag[request_jumps2_index[look]:request_jumps2_index[look+1]])
      jump_p<-0;jump_n<-0
      
    } 
    print(data_search_jump)
    look<-look+1
  }
  
  
  if (2*total_fractions>length(data_search_jump)) {
    error_message<-paste(error_message," ,not enough split points were found using defined temperature algorithm")
  }
}
#--------------- find time points by temp request diff and temp. inflexions
main_inflections_diff<-c(); main_inflections<-c();all_inflections<-c()
derivada<-diff(temp_lag)
all_inflections<-findpeaks(abs(derivada),minpeakheight=0.5)

for (counter in c(1:(length(data_search_jump)-1))) {
  inflection_within_range<-which(all_inflections[,2]>data_search_jump[counter] & all_inflections[,2]<data_search_jump[counter+1])
  main_inflections_diff[counter]<-max(all_inflections[inflection_within_range,1])
  main_inflections[counter]<-all_inflections[which(all_inflections[,1]==main_inflections_diff[counter]),2]
}

inflection_within_range<-which(all_inflections[,2]>data_search_jump[length(data_search_jump)])
main_inflections_diff[length(data_search_jump)]<-max(all_inflections[inflection_within_range,1])
main_inflections[length(data_search_jump)]<-all_inflections[which(all_inflections[,1]==main_inflections_diff[length(data_search_jump)]),2]
print(main_inflections)

if (2*length(choose)>length(main_inflections)) {
  error_message<-paste(error_message," ,not enough split points were found using T steps algorithm")
}  

#------------------------ Calc Yields -----------------------
data_search_x<-data_search_jump
yields_calc_vector<-ATN_to_yields_all(ATN)
EC_yield1<-as.numeric(yields_calc_vector[c(1:3)]); charring1<-as.numeric(yields_calc_vector[c(4:6)])
ATN_t1<-as.numeric(yields_calc_vector[c(7:9)]); ATN_max1<-as.numeric(yields_calc_vector[c(10:12)])
ATNt_xlag1<-yields_calc_vector[c(13:15)]; ATNmax_xlag1<-yields_calc_vector[c(16:18)]
data_search_y1<-temp_lag[data_search_x]/max(temp_lag)

data_search_x<-main_inflections
yields_calc_vector<-ATN_to_yields_all(ATN)
EC_yield2<-as.numeric(yields_calc_vector[c(1:3)]); charring2<-as.numeric(yields_calc_vector[c(4:6)])
ATN_t2<-as.numeric(yields_calc_vector[c(7:9)]); ATN_max2<-as.numeric(yields_calc_vector[c(10:12)])
ATNt_xlag2<-yields_calc_vector[c(13:15)]; ATNmax_xlag2<-yields_calc_vector[c(16:18)]
data_search_y2<-temp_lag[data_search_x]/max(temp_lag)

if (fitting_type=="poly") {
  K0<-fitting_poly.coef[1]
  laser_smooth_mid<-mean(c(max(laser_smooth),min(laser_smooth)))
  laser_smooth_norm<-(laser_smooth-laser_smooth_mid)/(abs(max(laser_smooth-laser_smooth_mid)))
  Q_laser_ATN<-poly_to_Q.laser_ATN(fitting_poly.coef[c(2:3)])
  print(Q_laser_ATN)
}


tabla_resultados<-data.frame("EC_yield"=c(EC_yield1,EC_yield2), "charring"=c(charring1,charring2), "ATN_t"=c(ATN_t1,ATN_t2), "ATN_max"=c(ATN_max1,ATN_max2), 
                             "ATN_t range"=c(ATNt_xlag1,ATNt_xlag2),"ATNmax range"=c(ATNmax_xlag1,ATNmax_xlag2), "fitting coef"=c(fitting.coef,fitting.coef))
print(tabla_resultados)
#write.table(tabla_resultados,"clipboard",sep="\t")

tabla_resultados2<-data.frame("EC_yield_S1"=c(tabla_resultados[1,1]), "EC_yield_S2"=c(tabla_resultados[2,1]), "EC_yield_S3"=c(tabla_resultados[3,1]), "charring_S1"=c(tabla_resultados[1,2]), "charring_S2"=c(tabla_resultados[2,2]), "charring_S3"=c(tabla_resultados[3,2]), "charring_total"=c(tabla_resultados[1,2])+c(tabla_resultados[2,2])+c(tabla_resultados[3,2]))
#tabla_resultados2<-data.frame(tabla_resultados[3,1], tabla_resultados[1,2], tabla_resultados[2,2], tabla_resultados[3,2])
print(tabla_resultados2)

ATN_rango<-max(ATN)-min(ATN); ATN_medio<-ATN_rango/2

###ggplot plot_Sunset
plot_Sunset_data <- as.data.frame(cbind(x_lag,temp_lag/max(temp_lag),ATN))
colnames(plot_Sunset_data) <- c("x_lag","temp_lag_norm", "ATN")
theme_set(theme_classic(base_size = 13,base_family = "Helvetica"))
plot_Sunset <- ggplot(plot_Sunset_data) +
  geom_line(aes(x_lag,temp_lag_norm),color = "black")+
  geom_line(aes(x_lag,request_lag/max(temp_lag)),color = "green")+
  geom_line(aes(x_lag,(ATN)/(max(ATN))),color = "blue")+
  geom_vline(xintercept=x_lag[data_search_jump], color = "red", linetype = "dashed")+
  xlab("Time (s)")+
  ylab("Normalised")+
  labs(title = paste("ATNmax=",signif(max(ATN),3)," ATNmin=",signif(min(ATN),3), " Lasermax=",signif(max(laser_smooth))))
plot_Sunset <- plot_Sunset + theme( plot.margin = margin(0.5, 0.5, 0.5, 0.5, "in"))
plot_Sunset

laser_data <- as.data.frame(cbind(x_lag,laser_smooth, laser_corr, ATN))
laser_data_normalised <- as.data.frame(cbind(x_lag,laser_smooth/max(laser_smooth)*100, laser_corr/max(laser_smooth)*100, ATN/max(ATN)*100))
colnames(laser_data_normalised) <- c("x_lag","laser_smooth", "laser_corr", "ATN")
theme_set(theme_classic(base_size = 13,base_family = "Helvetica"))
plot_TwoSide <- ggplot(laser_data_normalised) +
  geom_line(aes(x_lag,laser_smooth),color = "black")+
  geom_line(aes(x_lag,laser_corr),color = "red")+
  geom_line(aes(x_lag,ATN),color = "blue")+
  xlab("Time (s)")+
  ylab("Normalised (%)")+
  labs(title = "black: unc laser, red: corr laser, blue: ATN" )
plot_TwoSide <- plot_TwoSide + theme( plot.margin = margin(0.5, 0.5, 1, 0.5, "in"))
plot_TwoSide

###


if (error_message=="") {print("no errors")}
print(error_message)

# -------------------------------------- FINISH --------------------------------------------------------------
# result quality factors
if (fitting_type=="manual") {
  laser_smooth_mid<-mean(c(max(laser_smooth),min(laser_smooth)))
  laser_smooth_norm<-(laser_smooth-laser_smooth_mid)/(abs(max(laser_smooth-laser_smooth_mid)))
  data_search_x<-data_search_jump
  initial_coe<--1*abs(c(manual.coef[2],manual.coef[3]))  #make sure coef. are negative
  yields_calc<-data.frame(K0=single(),Q.laser_ATN=single(),K1=double(),K2=double(),EC1=double(),EC2=double(),EC3=double(),char1=double(),char2=double(),char3=double())
  n_K0<-0
  
  for (K0 in seq(from=0.85*max(laser_smooth),to=1.5*max(laser_smooth),length.out = 30)) {
    n_K0<-n_K0+1; Q.charr_optim<-c(); K<-c()
    initial_coe<-c(-0.01,-0.001) ; low_limit<-c(-0.3,-0.005); up_limit<-c(0.3,-0.00001) #K1 first order, K2 second order coef. K0 constant 
    
    try(Q.yield_optim<-optim(initial_coe, fn=poly_to_Q.yield, method="L-BFGS-B", lower=low_limit, upper=up_limit),silent=TRUE)
    if (is.null(Q.yield_optim)==FALSE) {initial_coe<-Q.yield_optim$par; K<-initial_coe} else {initial_coe<-c(-0.05,-0.001)}
    
    try(Q.ATN_laser_optim<-optim(initial_coe, fn=poly_to_Q.laser_ATN, method="L-BFGS-B", lower=low_limit, upper=up_limit),silent=TRUE)
    if (is.null(Q.ATN_laser_optim)==FALSE) {initial_coe<-Q.ATN_laser_optim$par; K<-initial_coe} else {initial_coe<-c(-0.005,-0.001)}
    
    try(Q.charr_optim<-optim(initial_coe, fn=poly_to_Q.charr, method="L-BFGS-B", lower=low_limit, upper=up_limit),silent=TRUE)
    if (is.null(Q.charr_optim)==FALSE) {initial_coe<-Q.charr_optim$par; K<-initial_coe} else {initial_coe<-c(-0.005,-0.001)}
    
    if (length(K)==0) {next}
    Q.laser_ATN.value<-poly_to_Q.laser_ATN(K) 
    guess_poly<-c(K0,K[1],K[2])
    ATN<-ATN_poly_calc(guess_poly); found_yields<-ATN_to_yields_calc(ATN)
    yields_calc[n_K0,]<-c(K0,Q.laser_ATN.value,K,found_yields)
  }
  K0_best<-0; n_candidates<-0
  for (a in c(5:10)) {
    derivative2<-abs(diff(yields_calc[,a],differences=1))
    K0_candidates<-which(derivative2<0.0020)
    K0_sel<-K0_candidates[which(diff(K0_candidates)==1)][1]
    if (is.na(K0_sel)){next}
    n_candidates<-n_candidates+1; K0_best<-K0_best+K0_sel
  }
  K0_best<-as.integer(K0_best/n_candidates)
  print(yields_calc)
  print(max(laser_smooth))
  print(K0_best)
  
  plot(yields_calc$K0,yields_calc$EC1,type="b",ylim=c(0,1.1))
  for (a in c(6:7)) {points(yields_calc[,1],yields_calc[,a],type="b")}
  lines(c(0,max(yields_calc$K0)),c(1,1))
  
  plot(yields_calc[,1],yields_calc$char1,type="b",ylim=c(-0.5,0.5))
  for (a in c(9:10)) {points(yields_calc[,1],yields_calc[,a],type="b")}
  lines(c(0,max(yields_calc$K0)),c(0,0))
  
  plot(yields_calc[,1],yields_calc$Q.laser_ATN,type="b")
}

#end

