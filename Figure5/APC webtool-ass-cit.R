library(data.table)
library(parallel)
library(R.utils)  # 用于处理zip文件
setwd("F:/BaiduSyncdisk/AMRGBD/第一篇/图表改2019/图5/代码及数据ASS/data")
# 设置线程数
setDTthreads(parallel::detectCores() - 1)

# 获取所有zip文件
dir <- "F:/BaiduSyncdisk/AMRGBD/第一篇/YUANSHI/xiaoming/2.APC网页版/Population"
zip_files <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)

# 创建并行处理集群
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(data.table)
  library(R.utils)
})

# 导出必要的变量到集群
clusterExport(cl, "zip_files")

# 并行读取数据
GBD_population <- parLapply(cl, zip_files, function(zip_file) {
  # 创建临时目录
  temp_dir <- tempdir()
  temp_file <- tempfile(tmpdir = temp_dir, fileext = ".csv")
  
  tryCatch({
    # 解压到临时文件
    unzip(zip_file, exdir = temp_dir)
    
    # 获取解压后的csv文件
    csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
    
    # 读取数据
    dt <- fread(csv_file,
                nThread = 2,
                showProgress = FALSE,
                integer64 = "numeric")
    
    # 清理临时文件
    unlink(csv_file)
    return(dt)
  }, 
  error = function(e) {
    message("Error processing file: ", zip_file, "\n", e$message)
    return(NULL)
  },
  finally = {
    # 确保清理临时文件
    unlink(temp_file)
  })
})

# 关闭集群
stopCluster(cl)

# 移除NULL结果（如果有的话）
GBD_population <- GBD_population[!sapply(GBD_population, is.null)]

# 合并数据
GBD_population <- rbindlist(GBD_population, use.names = TRUE, fill = TRUE)

# 清理内存
gc()

# 显示结果概要
print(dim(GBD_population))
print(head(GBD_population))


unique(GBD_population$year)
unique(GBD_population$age_name)

names(GBD_population)
## 对人口学数据进行整理
## 取我们需要的列名
GBD_population <- GBD_population %>% 
  select(location_name,sex_name,age_name,year,metric_name,val) 

unique(GBD_population$location_name)


## 对疾病数据进行整理
case <- read.csv('dataasscar.csv')
# 按照下面条件筛选数据
CDI <- case %>%
  dplyr::filter(measure_name=="Deaths") %>% 
  dplyr::filter(metric_name=="Number") %>% 
  #filter(cause!="Diabetes mellitus") %>%
  # filter(rei!="Blindness and vision loss") %>% 
  dplyr::filter(age_name!="All ages") %>%
  dplyr::filter(age_name!="Age-standardized")

## 合并疾病和人口学数据
df <- left_join(CDI,GBD_population,by=c("location_name","sex_name","age_name","metric_name","year"))
unique(GBD_population$metric_name)
colnames(CDI)
colnames(df)[17] <- "Deaths"
colnames(df)[20] <- "population"


df <- df %>% 
  mutate(population=round(population,digits = 0)) %>% 
  mutate(Deaths=round(Deaths,digits = 0)) %>% 
  select(location_name,pathogen,sex_name,age_name,cause_name,rei_name,year,Deaths,population) %>% 
  arrange(location_name,pathogen,sex_name,age_name,cause_name,rei_name,year) 
# 删除 'year' 列为1990和1991的行
df <- df %>% dplyr::filter(!(year %in% c(2020, 2021)))
unique(df$age_name)
# 筛选不同年龄组的数据
ages <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", 
          "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", 
          "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", 
          "90-94 years", "95-99 years") 

df <- df %>% dplyr::filter(Deaths > 0) %>% #删除缺失值
  mutate(age_name = sub('95\\+ years', '95-99 years', age_name),
         age_name = sub('<5 years', '0-4 years', age_name)) %>% 
  mutate(age_name=factor(age_name,levels = ages,ordered = T)) %>% 
  arrange(age_name)
unique(df$age_name)
## 对year重新命名
df <- df %>% mutate(year=case_when(
  year<1995 ~ "1990~1994",
  year<2000 & year>=1995 ~ "1995~1999",
  year<2005 & year>=2000 ~ "2000~2004",
  year<2010 & year>=2005 ~ "2005~2009",
  year<2015 & year>=2010 ~ "2010~2014",
  year<2020 & year>=2015 ~ "2015~2019"))

df2 <- df %>%
  dplyr::filter(pathogen=="Citrobacter spp." & sex_name== "Both") %>% 
  group_by(location_name,sex_name,cause_name,age_name,year) %>% 
  mutate(case=round(mean(Deaths),digits = 0)) %>% 
  mutate(population2=round(mean(population),digits = 0)) %>% 
  select(location_name,pathogen,sex_name,age_name, cause_name,year,case,population2) %>% 
  unique()
unique(df2$age_name)
## 挑选除全球、男性、不同年龄段的、不同年份的数据
## 将数据转换成行为年龄，列为年的数据
apc_case <- df2 %>% dplyr::filter(pathogen=="Citrobacter spp." & sex_name== "Both")
apc_case_death <- reshape2::dcast(apc_case,age_name~year,value.var = 'case') %>% 
  arrange(age_name) %>% 
  select(-1)
## 将数据转换成行为年龄，列为年的数据
apc_case_pop <- reshape2::dcast(apc_case,age_name~year,value.var = 'population2') %>% 
  arrange(age_name) %>% 
  select(-1)

## 将数据整理成 APC网页版需要的数据格式:疾病和人口数据交替排列
for (i in 1:(ncol(apc_case_death)+ncol(apc_case_pop))){
  if(i == 1){
    result <-apc_case_death[,i] %>% as.data.frame()}
  else{
    if(i%%2==0){
      result <- cbind(result,apc_case_pop[,ceiling(i/2)])}
    else{
      result <- cbind(result,apc_case_death[,ceiling(i/2)])}
  }
}
write.csv(result,'apc_web_data_ass_cit.csv')



## long age curve
long_age <- APC_analysis_global_output[["LongAge"]] %>% as.data.frame()
ggplot(data=long_age,aes(x=Age,y=Rate)) +
  geom_line(color='Red') +
  geom_ribbon(aes(Age,ymin=CILo,ymax=CIHi,fill='Red'),alpha=0.1) +
  theme_bw()


## CohortRR
CohortRR <- APC_analysis_global_output[["CohortRR"]] %>% as.data.frame()
ggplot(data=CohortRR,aes(x=Cohort,y=`Rate Ratio`)) +
  geom_line(color='Red') +
  geom_ribbon(aes(Cohort,ymin=CILo,ymax=CIHi,fill='Red'),alpha=0.1) +
  theme_bw() +
  geom_hline(yintercept = 1,linetype = 2, color= 'black') +
  geom_vline(xintercept = CohortRR[which(CohortRR$`Rate Ratio`==1),1],
             linetype = 2, color= 'black')

## periodRR
PeriodRR <- APC_analysis_global_output[["PeriodRR"]] %>% as.data.frame()
ggplot(data=PeriodRR,aes(x=Period,y=`Rate Ratio`)) +
  geom_line(color='Red') +
  geom_ribbon(aes(Period,ymin=CILo,ymax=CIHi,fill='Red'),alpha=0.1) +
  theme_bw() +
  geom_hline(yintercept = 1,linetype = 2, color= 'black') +
  geom_vline(xintercept = PeriodRR[which(PeriodRR$`Rate Ratio`==1),1],
             linetype = 2, color= 'black')


### Local drift and net drift
Local <- APC_analysis_global_output[["LocalDrifts"]] %>% as.data.frame()
Net <- APC_analysis_global_output[["NetDrift"]] %>% as.data.frame()
ggplot(data=Local,aes(x=Age,y=`Percent per Year`)) +
  geom_line(color='Red') + geom_point(color='Red') +
  geom_ribbon(aes(Age,ymin=CILo,ymax=CIHi,fill='Red'),alpha=0.1) +
  theme_bw() +
  geom_hline(yintercept = 0,linetype = 2, color= 'black') +
  geom_hline(data= Net,aes(yintercept = `Net Drift (%/year)`),color='Blue',
             linetype = 1) +
  geom_hline(data= Net,aes(yintercept = `CI Lo`),color='Blue',
             linetype = 2) +
  geom_hline(data= Net,aes(yintercept = `CI Hi`),color='Blue',
             linetype = 2)
