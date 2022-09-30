

#' Plot the covariate estimates
#'
#' @param rep input the report object from the model
#' @param sdrep input the sdreport object from the model
#' @param output input the output object from the model_prep function
#' @param n_covar input the number of covariates used in the model
#' @param gl_switch input whether the model had separate slopes estimated for Great Lakes (1) or not (0)
#' @param covar_name input a vector of the names of covariates
#'
#' @return plot
#' @export
#'
#' @examples plot_covars(rep, sdrep, n_covar=2, gl_switch=0, covar_name=c("Area","Distance"))
plot_covars<-function(rep, sdrep, output, n_covar=1, gl_switch=0, covar_name=c("Covar1")){
val_names<-names(sdrep$value)

if(n_covar==1 & gl_switch==0){
  
  ind<-val_names=="tau"
  
  covar_gl_dat<-data.frame(first_covar=output$tmb.data$first_covar, effort=output$tmb.data$log_E) 
  first_covar<-seq(from=range(covar_gl_dat$first_covar)[1], to=range(covar_gl_dat$first_covar)[2], length=length(covar_gl_dat$first_covar))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar 
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar
  
  covar_df<-data.frame(pred_E=pred_E, 
                       covar_name=rep(c(covar_name),length(pred_E)),
                       covars=c(first_covar),
                       taus = c(first_tau),
                       tau_high=c(first_tau_high),
                       tau_low = c(first_tau_low),
                       E=c(covar_gl_dat$effort),
                       covar_dat=c(covar_gl_dat$first_covar))
p1<-ggplot()+
  geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
  geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
  geom_line(data=covar_df, aes(x=covars, y=taus))+
  facet_wrap(~covar_name, scales="free", nrow=2)+
  theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
  theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
}

if(n_covar==2 & gl_switch==0){
  
  ind<-val_names=="tau"
  
  covar_gl_dat<-data.frame(first_covar=output$tmb.data$first_covar, second_covar=output$tmb.data$second_covar, effort=output$tmb.data$log_E) 
  first_covar<-seq(from=range(covar_gl_dat$first_covar)[1], to=range(covar_gl_dat$first_covar)[2], length=length(covar_gl_dat$first_covar))
  second_covar<-seq(from=range(covar_gl_dat$second_covar)[1], to=range(covar_gl_dat$second_covar)[2], length=length(covar_gl_dat$second_covar))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar + mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar + mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  
  second_tau<- mean(rep$tau_int)+sdrep$value[ind][2]*second_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]
  second_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*second_covar + mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]
  second_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*second_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]
  
  
  covar_df<-data.frame(pred_E=c(pred_E,pred_E), 
                       covar_name=rep(c(covar_name),each=length(pred_E)),
                       covars=c(first_covar, second_covar),
                       taus = c(first_tau, second_tau),
                       tau_high=c(first_tau_high, second_tau_high),
                       tau_low = c(first_tau_low, second_tau_low),
                       E=c(covar_gl_dat$effort,covar_gl_dat$effort),
                       covar_dat=c(covar_gl_dat$first_covar,covar_gl_dat$second_covar))
p1<-  ggplot()+
    geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
    geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
    geom_line(data=covar_df, aes(x=covars, y=taus))+
    facet_wrap(~covar_name, scales="free", nrow=2)+
    theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
}


if(n_covar==3 & gl_switch==0){
  
  ind<-val_names=="tau"
  
  covar_gl_dat<-data.frame(first_covar=output$tmb.data$first_covar, second_covar=output$tmb.data$second_covar,
                           third_covar=output$tmb.data$third_covar, effort=output$tmb.data$log_E) 
  first_covar<-seq(from=range(covar_gl_dat$first_covar)[1], to=range(covar_gl_dat$first_covar)[2], length=length(covar_gl_dat$first_covar))
  second_covar<-seq(from=range(covar_gl_dat$second_covar)[1], to=range(covar_gl_dat$second_covar)[2], length=length(covar_gl_dat$second_covar))
  third_covar<-seq(from=range(covar_gl_dat$third_covar)[1], to=range(covar_gl_dat$third_covar)[2], length=length(covar_gl_dat$third_covar))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar + mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar + mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  
  second_tau<- mean(rep$tau_int)+sdrep$value[ind][2]*second_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  second_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*second_covar + mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  second_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*second_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$third_covar)*sdrep$value[ind][3]
  
  third_tau<- mean(rep$tau_int)+sdrep$value[ind][3]*third_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  third_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][3]+qnorm(0.975)*sdrep$sd[ind][3])*third_covar + mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  third_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][3]-qnorm(0.975)*sdrep$sd[ind][3])*third_covar+ mean(covar_gl_dat$first_covar)*sdrep$value[ind][1]+ mean(covar_gl_dat$second_covar)*sdrep$value[ind][2]
  
  
  covar_df<-data.frame(pred_E=c(pred_E,pred_E,pred_E), 
                       covar_name=rep(c(covar_name),each=length(pred_E)),
                       covars=c(first_covar, second_covar, third_covar),
                       taus = c(first_tau, second_tau, third_tau),
                       tau_high=c(first_tau_high, second_tau_high, third_tau_high),
                       tau_low = c(first_tau_low, second_tau_low, third_tau_low),
                       E=c(covar_gl_dat$effort,covar_gl_dat$effort,covar_gl_dat$effort),
                       covar_dat=c(covar_gl_dat$first_covar,covar_gl_dat$second_covar, covar_gl_dat$third_covar))
 p1<- ggplot()+
    geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
    geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
    geom_line(data=covar_df, aes(x=covars, y=taus))+
    facet_wrap(~covar_name, scales="free", nrow=2)+
    theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
}


if(n_covar==1 & gl_switch==1){
  
  ind<-val_names=="tau_gl"
  
  
  covar_gl_dat<-data.frame(iGL=output$tmb.data$iGL, first_covar=output$tmb.data$gl_area_covar, effort=output$tmb.data$log_E)
  
  noGL<-subset(covar_gl_dat, iGL==0)
  GL_dat<-subset(covar_gl_dat, iGL==1)
  
  first_covar<-seq(from=range(noGL$first_covar)[1], to=range(noGL$first_covar)[2], length=length(noGL$iGL))
  first_covar_GL<-seq(from=range(GL_dat$first_covar)[1], to=range(GL_dat$first_covar)[2], length=length(GL_dat$iGL))
  
  pred_EnoGL<-seq(from=-1, to=11, length=length(noGL$iGL))
  pred_EGL<-seq(from=-1, to=11, length=length(GL_dat$iGL))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar
  
  second_tau<- mean(rep$tau_int)+sdrep$value[ind][2]*first_covar_GL 
  second_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL
  second_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL
  
  covar_df<-data.frame(pred_E=c(pred_EnoGL, pred_EGL), 
                       covar_name=rep(c("Waterbody log Area","Waterbody log Area (Great Lakes)"),
                                      c(length(pred_EnoGL),length(pred_EGL))),
                       covars=c(first_covar, first_covar_GL),
                       taus = c(first_tau, second_tau),
                       tau_high=c(first_tau_high, second_tau_high),
                       tau_low = c(first_tau_low, second_tau_low),
                       E=c(noGL$effort, GL_dat$effort),
                       covar_dat=c(noGL$first_covar, GL_dat$first_covar))
  
p1<-  ggplot()+
    geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
    geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
    geom_line(data=covar_df, aes(x=covars, y=taus))+
    facet_wrap(~covar_name, scales="free", nrow=2)+
    theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
  
}

if(n_covar==2 & gl_switch==1){
  
  ind<-val_names=="tau_gl"
  
  
  covar_gl_dat<-data.frame(iGL=output$tmb.data$iGL, first_covar=output$tmb.data$gl_area_covar,
                           second_covar=output$tmb.data$first_covar,
                           effort=output$tmb.data$log_E)
  
  noGL<-subset(covar_gl_dat, iGL==0)
  GL_dat<-subset(covar_gl_dat, iGL==1)
  
  first_covar<-seq(from=range(noGL$first_covar)[1], to=range(noGL$first_covar)[2], length=length(noGL$iGL))
  first_covar_GL<-seq(from=range(GL_dat$first_covar)[1], to=range(GL_dat$first_covar)[2], length=length(GL_dat$iGL))
  second_covar<-seq(from=range(noGL$second_covar)[1], to=range(noGL$second_covar)[2], length=length(noGL$iGL))
    
  pred_EnoGL<-seq(from=-1, to=11, length=length(noGL$iGL))
  pred_EGL<-seq(from=-1, to=11, length=length(GL_dat$iGL))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar+ mean(noGL$second_covar)*rep$tau[1]
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(noGL$second_covar)*rep$tau[1]
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(noGL$second_covar)*rep$tau[1]
  
  second_tau<- mean(rep$tau_int)+sdrep$value[ind][2]*first_covar_GL + mean(noGL$second_covar)*rep$tau[1]
  second_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL+ mean(noGL$second_covar)*rep$tau[1]
  second_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL+ mean(noGL$second_covar)*rep$tau[1]
  
  ind<-val_names=="tau"
  
  third_tau<-mean(rep$tau_int)+sdrep$value[ind][1]*second_covar + mean(noGL$first_covar)*rep$tau_gl[1]
  third_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*second_covar  + mean(noGL$first_covar)*rep$tau_gl[1]
  third_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*second_covar + mean(noGL$first_covar)*rep$tau_gl[1]
  
  covar_df<-data.frame(pred_E=c(pred_EnoGL, pred_EGL, pred_EnoGL), 
                       covar_name=rep(c("Waterbody log Area","Waterbody log Area (Great Lakes)", covar_name),
                                      c(length(pred_EnoGL),length(pred_EGL),length(pred_EnoGL))),
                       covars=c(first_covar, first_covar_GL, second_covar),
                       taus = c(first_tau, second_tau, third_tau),
                       tau_high=c(first_tau_high, second_tau_high, third_tau_high),
                       tau_low = c(first_tau_low, second_tau_low, third_tau_low),
                       E=c(noGL$effort, GL_dat$effort, noGL$effort),
                       covar_dat=c(noGL$first_covar, GL_dat$first_covar, noGL$second_covar))
  
  p1<- ggplot()+
    geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
    geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
    geom_line(data=covar_df, aes(x=covars, y=taus))+
    facet_wrap(~covar_name, scales="free", nrow=2)+
    theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
  
}

if(n_covar==3 & gl_switch==1){
  
  ind<-val_names=="tau_gl"
  
  
  covar_gl_dat<-data.frame(iGL=output$tmb.data$iGL, first_covar=output$tmb.data$gl_area_covar,
                           second_covar=output$tmb.data$first_covar,third_covar=output$tmb.data$second_covar,
                           effort=output$tmb.data$log_E)
  
  noGL<-subset(covar_gl_dat, iGL==0)
  GL_dat<-subset(covar_gl_dat, iGL==1)
  
  first_covar<-seq(from=range(noGL$first_covar)[1], to=range(noGL$first_covar)[2], length=length(noGL$iGL))
  first_covar_GL<-seq(from=range(GL_dat$first_covar)[1], to=range(GL_dat$first_covar)[2], length=length(GL_dat$iGL))
  second_covar<-seq(from=range(noGL$second_covar)[1], to=range(noGL$second_covar)[2], length=length(noGL$iGL))
  third_covar<-seq(from=range(noGL$third_covar)[1], to=range(noGL$third_covar)[2], length=length(noGL$iGL))
  
  pred_EnoGL<-seq(from=-1, to=11, length=length(noGL$iGL))
  pred_EGL<-seq(from=-1, to=11, length=length(GL_dat$iGL))
  pred_E<-seq(from=-1, to=11, length=length(output$tmb.data$log_C))
  
  first_tau<- mean(rep$tau_int)+sdrep$value[ind][1]*first_covar+ mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  first_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  first_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*first_covar+ mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  
  second_tau<- mean(rep$tau_int)+sdrep$value[ind][2]*first_covar_GL + mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  second_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL+ mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  second_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*first_covar_GL+ mean(noGL$second_covar)*rep$tau[1]+ mean(noGL$third_covar)*rep$tau[2]
  
  ind<-val_names=="tau"
  
  third_tau<-mean(rep$tau_int)+sdrep$value[ind][1]*second_covar + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$third_covar)*rep$tau[2]
  third_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][1]+qnorm(0.975)*sdrep$sd[ind][1])*second_covar  + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$third_covar)*rep$tau[2]
  third_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][1]-qnorm(0.975)*sdrep$sd[ind][1])*second_covar + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$third_covar)*rep$tau[2]
  
  fourth_tau<-mean(rep$tau_int)+sdrep$value[ind][2]*third_covar + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$second_covar)*rep$tau[1]
  fourth_tau_high<-mean(rep$tau_int)+(sdrep$value[ind][2]+qnorm(0.975)*sdrep$sd[ind][2])*third_covar  + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$second_covar)*rep$tau[1]
  fourth_tau_low<-mean(rep$tau_int)+(sdrep$value[ind][2]-qnorm(0.975)*sdrep$sd[ind][2])*third_covar + mean(noGL$first_covar)*rep$tau_gl[1]+ mean(noGL$second_covar)*rep$tau[1]
  
  
  covar_df<-data.frame(pred_E=c(pred_EnoGL, pred_EGL, pred_EnoGL, pred_EnoGL), 
                       covar_name=rep(c("Waterbody log Area","Waterbody log Area (Great Lakes)", covar_name[1],covar_name[2]),
                                      c(length(pred_EnoGL),length(pred_EGL),length(pred_EnoGL),length(pred_EnoGL))),
                       covars=c(first_covar, first_covar_GL, second_covar, third_covar),
                       taus = c(first_tau, second_tau, third_tau, fourth_tau),
                       tau_high=c(first_tau_high, second_tau_high, third_tau_high, fourth_tau_high),
                       tau_low = c(first_tau_low, second_tau_low, third_tau_low, fourth_tau_low),
                       E=c(noGL$effort, GL_dat$effort, noGL$effort, noGL$effort),
                       covar_dat=c(noGL$first_covar, GL_dat$first_covar, noGL$second_covar, noGL$third_covar))
  
  p1<-  ggplot()+
    geom_point(data=covar_df, aes(x=covar_dat, y=E), colour="darkgrey")+
    geom_ribbon(data=covar_df, aes(ymin=tau_low, ymax=tau_high, x=covars,y=pred_E), fill="lightblue", alpha=0.5)+
    geom_line(data=covar_df, aes(x=covars, y=taus))+
    facet_wrap(~covar_name, scales="free", nrow=2)+
    theme_bw()+xlab("Covariate")+ylab("log(Effort Hours per Day)")+
    theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
  
}
print(p1)
}

