

#' Plot the relationship between catch and effort
#'
#' @param rep input the report object from the model
#' @param sdrep input the sdreport object from the model
#' @param tmb.data input the tmb.data object that was used to create the model (e.g. output$tmb.data)
#' @param xlim decide what the xlim of the figure will be (min, max). (optional)
#' @param ylim decide what the ylim of the figure will be (min, max). (optional)
#'
#' @return plot
#' @export
#'
#' @examples plot_cpue(rep, sdrep, tmb.data, xlim=c(-1,5), ylim=c(-1,5))
plot_cpue<-function(rep, sdrep, tmb.data, xlim=c(-1,11), ylim=c(-1,11)){
tmb.data<-output$tmb.data

val_names<-names(sdrep$value)

ind<-val_names=="q_dev"
ind2<-val_names=="intercept"

seasons<-sort(unique(tmb.data$random_effect))

pred_E<-seq(from=-1, to=11, by=0.01)

pred_C<- rep$q_dev[1]*pred_E
pred_C_low<-(rep$q_dev[1]-qnorm(0.975)*sdrep$sd[ind][1])*pred_E
pred_C_high<-(rep$q_dev[1]+qnorm(0.975)*sdrep$sd[ind][1])*pred_E

#pred_CE_df<-pred_CE_df[2:12011,]

pred_c_e<-data.frame(pred_E=pred_E, pred_C=pred_C, pred_C_low=pred_C_low, pred_C_high=pred_C_high)
ce_dat<-data.frame(log_E=tmb.data$log_E, log_C=tmb.data$log_C)

#setwd("Figs")

ggplot2::ggplot()+
  geom_point(data=ce_dat, aes(x=log_E, y=log_C), col="darkgrey")+
  geom_ribbon(data=pred_c_e, aes(ymin=pred_C_low, ymax=pred_C_high, x=pred_E,y=pred_C), fill="lightblue", alpha=0.5)+
  geom_line(data=pred_c_e, aes(x=pred_E, y=pred_C), size=1.5)+
  xlim(xlim[1],xlim[2])+
  ylim(ylim[1],ylim[2])+
  xlab("log(Effort Hours per Day)")+ylab("log(Catch per Day)")+
  theme_bw()+
  theme(axis.text= element_text(size=14),axis.title=element_text(size=16,face="bold"))
}
