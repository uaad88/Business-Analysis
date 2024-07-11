library(readxl)
library(writexl)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(funModeling)
library(zoo)
setwd("D:/R_code for 2023/(3) dudoo/secondfloor_掃碼點餐_導入後/ledledor/")
#-------------(1.input data_1 for single .csv file)---------------------------------------------------
#(1) 
re_1<-read.csv("daily_sales_performance_2023_1.csv",
               header = TRUE, sep = ";", quote = "\"", dec = ",", 
               fill = TRUE, comment.char = "",
               encoding="UTF-8")

#-------------(2.input data_2  for multiply .csv files )------------------------
# List of file paths for multiple CSV files
file_paths<-c("daily_sales_performance_2023_1.csv",
              "daily_sales_performance_2023_2.csv",
              "daily_sales_performance_2023_3.csv",
              "daily_sales_performance_2023_4.csv",
              "daily_sales_performance_2023_5.csv",
              "daily_sales_performance_2023_6.csv",
              "daily_sales_performance_2023_7.csv",
              "daily_sales_performance_2023_8.csv",
              "daily_sales_performance_2023_9.csv",
              "daily_sales_performance_2022_5.csv",
              "daily_sales_performance_2022_6.csv",
              "daily_sales_performance_2022_7.csv",
              "daily_sales_performance_2022_8.csv",
              "daily_sales_performance_2022_9.csv",
              "daily_sales_performance_2022_10.csv",
              "daily_sales_performance_2022_11.csv",
              "daily_sales_performance_2022_12.csv")


# Use lapply to read each CSV file with semicolon as separator
data_list <- lapply(file_paths, function(file_path) {
  read.csv(file_path, sep=';')
})
#-------------(3. extract the individual file)--------------------------------------------
# Access the data frames using names or indices
#2023
month_23_1<-data_list[[1]]           
month_23_2<-data_list[[2]] 
month_23_3<-data_list[[3]]
month_23_4<-data_list[[4]]
month_23_5<-data_list[[5]]
month_23_6<-data_list[[6]]
month_23_7<-data_list[[7]]
month_23_8<-data_list[[8]]
month_23_9<-data_list[[9]]

#2022
month_22_5<-data_list[[10]]
month_22_6<-data_list[[11]]
month_22_7<-data_list[[12]]
month_22_8<-data_list[[13]]
month_22_9<-data_list[[14]]
month_22_10<-data_list[[15]]
month_22_11<-data_list[[16]]
month_22_12<-data_list[[17]]

#-------------(4.Average price per customer )------------------------------------------------
# establish the empty dataframe
sum_all<-data.frame()

#(testing.1) 
#(2023)平均客單價(onsite_amount/avg_person_amount)
sum_all[1,c("平均客單價")]<-sum(month_23_1$onsite_amount)/sum(month_23_1$persons)
sum_all[2,c("平均客單價")]<-sum(month_23_2$onsite_amount)/sum(month_23_2$persons)
sum_all[3,c("平均客單價")]<-sum(month_23_3$onsite_amount)/sum(month_23_3$persons)
sum_all[4,c("平均客單價")]<-sum(month_23_4$onsite_amount)/sum(month_23_4$persons)
sum_all[5,c("平均客單價")]<-sum(month_23_5$onsite_amount)/sum(month_23_5$persons)
sum_all[6,c("平均客單價")]<-sum(month_23_6$onsite_amount)/sum(month_23_6$persons)
sum_all[7,c("平均客單價")]<-sum(month_23_7$onsite_amount)/sum(month_23_7$persons)
sum_all[8,c("平均客單價")]<-sum(month_23_8$onsite_amount)/sum(month_23_8$persons)
sum_all[9,c("平均客單價")]<-sum(month_23_9$onsite_amount)/sum(month_23_9$persons)

#(2022)平均客單價
sum_all[10,c("平均客單價")]<-sum(month_22_5$onsite_amount)/sum(month_22_5$persons)
sum_all[11,c("平均客單價")]<-sum(month_22_6$onsite_amount)/sum(month_22_6$persons)
sum_all[12,c("平均客單價")]<-sum(month_22_7$onsite_amount)/sum(month_22_7$persons)
sum_all[13,c("平均客單價")]<-sum(month_22_8$onsite_amount)/sum(month_22_8$persons)
sum_all[14,c("平均客單價")]<-sum(month_22_9$onsite_amount)/sum(month_22_9$persons)
sum_all[15,c("平均客單價")]<-sum(month_22_10$onsite_amount)/sum(month_22_10$persons)
sum_all[16,c("平均客單價")]<-sum(month_22_11$onsite_amount)/sum(month_22_11$persons)
sum_all[17,c("平均客單價")]<-sum(month_22_12$onsite_amount)/sum(month_22_12$persons)
sum_all$月份<-c(seq(from = 1, to = 17, by = 1))

#modify
sum_all$group<-c(rep(2023, times = 9),rep(2022, times = 8))
sum_all$月份_改<-c(seq(from = 01, to =09 , by = 1),c(seq(from = 05, to = 12, by = 1)))
sum_all$月份_合<-format(as.Date(as.yearmon(paste(sum_all$group,sum_all$月份_改,sep = '-'))), "%Y-%m")
sum_all$平均客單價_改<-as.numeric(round(sum_all$平均客單價,digits = 0))

#plots
ggplot(sum_all, aes(x = 月份_合,y=平均客單價_改))+
  geom_segment(aes(x =月份_合, xend =月份_合, y =0, yend =平均客單價_改))+
  geom_point(y=sum_all$平均客單價_改,
             size=15, color="red", fill=alpha("orange", 0.3),
             alpha=0.7, shape=21, stroke=2)+
  geom_label(aes(月份_合, 平均客單價_改 , label = signif(平均客單價_改)),  
             colour = "darkred", nudge_x = 0.35, size = 8)+
  
  
  
  ggtitle("2022-2023年台灣A店導入掃碼點餐後的平均內用客單價")+
  xlab("2022-2023年度") + ylab("平均內用客單價")+
  ylim(0, 800)+
  
  
 theme(axis.text = element_text(size = 16,face = "bold"),
        axis.title = element_text(size = 25,face = "bold"),
        legend.title = element_text(size = 10,face = "bold"),
        legend.text = element_text(size = ,face = "bold"),
        plot.title = element_text(size = 40,face = "bold",hjust = 0.1))

#(testing.2) 
ggplot(sum_all, aes(x=sum_all$月份_合, y=sum_all$平均客單價_改,group=group,color=group))+
geom_point(size=10)+
geom_line(size=2)+
geom_text(aes(label=sum_all$平均客單價_改,fontface ="plain", color = "black", size = 8,vjust=0))



#-------------(5. sale prediction model)----------------------------------------
#Topic: what kind of variables are suitable for sale prediction beneath the 
#condition that 
#data cleaning 
combined_row <- rbind(month_22_5[1:nrow(month_22_5),],
                      month_22_6[1:nrow(month_22_6),],
                      month_22_7[1:nrow(month_22_7),],
                      month_22_8[1:nrow(month_22_8),],
                      month_22_9[1:nrow(month_22_9),],
                      month_22_10[1:nrow(month_22_10),],
                      month_22_11[1:nrow(month_22_11),],
                      month_22_12[1:nrow(month_22_12),],
                      month_23_1[1:nrow(month_23_1),],
                      month_23_2[1:nrow(month_23_2),],
                      month_23_3[1:nrow(month_23_3),],
                      month_23_4[1:nrow(month_23_4),],
                      month_23_5[1:nrow(month_23_5),],
                      month_23_6[1:nrow(month_23_6),],
                      month_23_7[1:nrow(month_23_7),],
                      month_23_8[1:nrow(month_23_8),],
                      month_23_9[1:nrow(month_23_9),])
combined_row_01<-combined_row[,c(5,9,11:14,16:19,26:29)]
combined_row_01$avg_onsitesales_person<-(combined_row_01$onsite_amount)/(combined_row_01$persons)
combined_row_02<-combined_row_01[-1,]


#univariate variable
U_results<-as.data.frame(apply(combined_row_02[,c(5,7,8,9,10)],2,function(x){
  y<-lm(avg_onsitesales_person~ x, data=combined_row_02)
  output<-round(summary(y)$coefficients[2,c(1,4)],digits =6)
  return(output)}))
U_results_01<-as.data.frame(t(U_results))
U_results_01$varialbe<-row.names(U_results_01)
colnames(U_results_01)<-c("Estimate.U","P-value.U","varialbe")


#multivariable variables
M_reaults<-lm(avg_onsitesales_person~service_charge_amount+
                                     discount_amount+
                                     cancel_amount
                                    ,data=combined_row_02)

M_results_01<-as.data.frame(round(summary(M_reaults)$coefficients[2:4,c(1,4)],digits =6))

M_results_01$varialbe<-row.names(M_results_01)
colnames(M_results_01)<-c("Estimate.M","P-value.M","varialbe")


#conclusion
merge(U_results_01,M_results_01)



























