#updated blog post can be found here https://scweiss.blogspot.com/2020/03/can-trade-with-china-predict-covid-19.html


library(dplyr)
library(reshape2)
library(glmnet)
library(ranger)
library(ggplot2)
library(ggrepel)

#cand find data here https://oec.world/en/resources/data/
data = read.table('/users/sweiss/downloads/year_origin_destination_hs07_4.tsv',
                  sep = '\t', header = TRUE, quote="")
country_names = read.table('/users/sweiss/downloads/country_names.tsv',
                           sep = '\t', header = TRUE)
product_names = read.csv('/users/sweiss/downloads/products_hs_07.csv')

#subset to China exports for 2017
data = subset(data, origin == 'chn' & year == 2017)
data[,'export_val'] = as.numeric(as.character(data[,'export_val']))
data[,'import_val'] = as.numeric(as.character(data[,'import_val']))
data[is.na(data)] = 0


#merge with other country names and product category names
data = merge(data,country_names[,2:3], by.x = 'dest', by.y = 'id_3char', all.x = TRUE)
data = merge(data,product_names[,2:3], by.x = 'hs07', by.y = 'hs07', all.x = TRUE)
colnames(data)[7:8] = c('country','product')

#turn to wide data
data_wide = dcast(data[,c('product','country','export_val')], country~product)
data_wide[is.na(data_wide)] = 0

#load / clean up covid 19 data
covid_19 = read.csv('/users/sweiss/downloads/time_series_19-covid-Confirmed.txt')
covid_19 = covid_19[,c('Country.Region','X3.9.20')]
covid_19 = aggregate( . ~ Country.Region, data = covid_19, FUN = sum)
colnames(covid_19) = c('name','covid_19')
covid_19[,'name'] = as.character(covid_19[,'name'])
covid_19[which(covid_19[,'name'] == 'US'),'name'] = 'United States'
covid_19[which(covid_19[,'name'] == 'UK'),'name'] = 'United Kingdom'

#merge with trade data
data_wide_2 = merge(data_wide,covid_19, by.x = 'country', by.y = 'name')


#population found here https://data.worldbank.org/indicator/SP.POP.TOTL
population = read.csv('/users/sweiss/downloads/pop_country.csv')
population = population[c('Type.of.aggregate..group..and.constituents..','X2020')]
population[,'X2020'] = gsub(' ','',population[,'X2020'])
colnames(population) = c('country','population')

population = (unique(population))

adtl_pop= data.frame(country= c('South Korea','macao', 'vietnam'), population = c(51470,  622, 95540))
population[,'country'] = as.character(population[,'country'])
population[grep('Brunei',population[,'country']),'country'] = 'Brunei'
population[grep('Czech',population[,'country']),'country'] = 'Czech Republic'
population[grep('Hong Kong',population[,'country']),'country'] = 'Hong Kong'
population[grep('Iran',population[,'country']),'country'] = 'Iran'
population[grep('Macau',population[,'country']),'country'] = 'Macau'
population[grep('Moldova',population[,'country']),'country'] = 'Moldova'
population[grep('Palestine',population[,'country']),'country'] = 'Palestine'
population[grep('Russia',population[,'country']),'country'] = 'Russia'
population[grep('United States',population[,'country'])[1],'country'] = 'United States'

population = rbind(population, adtl_pop)

data_wide_3 = merge(data_wide_2, unique(unique(population)), by = 'country')

data_wide_3[,'population'] = as.numeric(data_wide_3[,'population'])
hist(data_wide_3[,'covid_19']/data_wide_3[,'population'])

covid_cap = data_wide_3[,'covid_19']/data_wide_3[,'population']
exports_cap = rowSums(data_wide_3[,-c(which(colnames(data_wide_3) %in% c('covid_19','population','country')) )])/data_wide_3[,'population']

eda_df = data.frame(covid_cap, exports_cap, covid = data_wide_3[,'covid_19'],
                    exports = rowSums(data_wide_3[,-c(which(colnames(data_wide_3) %in% c('covid_19','population','country')) )]),
                    country = data_wide_3[,'country'],
                    population = data_wide_3[,'population'])
ggplot(eda_df, aes(x = log(exports_cap), y= log(covid_cap), label = country))+geom_label()

ggplot(eda_df, aes(x = log(exports), y= log(covid), label = country))+geom_label()

summary(lm(log(covid) ~ log(exports), data = eda_df))
summary(lm(log(covid) ~ log(covid_cap), data = eda_df))

data_mat_x = data_wide_3[,-c(which(colnames(data_wide_3) %in% c('covid_19','population','country')) )] / data_wide_3[,'population']
data_mat_x = data_mat_x[,-which(apply(data_mat_x,2,sd) == 0)]

scaled_log_data_mat_x = apply(log(1+as.matrix(data_mat_x)), 2, scale)
lasso_reg = cv.glmnet(scaled_log_data_mat_x, log(1+data_wide_3[,'covid_19']))


rf_df = data.frame(log(1+as.matrix(data_mat_x)))
rf_df$covid_19 = log(covid_cap)
rf_1 = ranger(covid_19~., data = rf_df, importance = 'impurity')
rf_1
#how much prediction power from just total_exports - .5

sort(rf_1$variable.importance, decreasing = TRUE)[1:10]

preds = predict(lasso_reg, newx = log(1+as.matrix(data_mat_x)), s = 'lambda.min')
plot_data = data.frame(ln_covid = log(covid_cap), 
                       preds = rf_1$predictions, 
                       country = data_wide_3[,'country'],
                       ln_total_exports = log(rowSums(data_mat_x)),
                       lasso_preds = preds[,1])

summary(lm(ln_covid ~ ln_total_exports, data = plot_data))
# .24 r^2


ggplot(plot_data, aes(x = ln_covid))+geom_histogram() + theme_minimal() + 
  ggtitle("Histogram of log Per Capita COVID-19 in Each Country") + ylab('Count')+
  xlab('Log(Number of Covid-19 Instances / Population)')


ggplot(plot_data, aes(x = ln_total_exports, y = ln_covid, label = country)) + geom_text_repel() + theme_minimal()+
  ggtitle("log Covid-19 Per Capita by Chinese Exports") + ylab('log(Covid-19 / population)')+
  xlab('Log($ Exports)')

  
ggplot(plot_data, aes(x = preds, y = ln_covid, label = country)) +
  geom_text_repel() + theme_minimal()+
  ggtitle("Covid-19/Population by OOB Predictions") + ylab('log(Covid-19 / Population)')+
  xlab('Predictions')






coefs = as.matrix(coef(lasso_reg, s= 'lambda.min'))
coefs = data.frame(coefs[which(coefs[,1]!=0),])
coefs$ids = rownames(coefs)
colnames(coefs) = c('coef','id')
coefs[order(coefs[,'coef'],decreasing = TRUE),][1:10,]
coefs[,'id'] = substr(coefs[,'id'], 1, 30)
coefs[,'id'] = factor(coefs[,'id'], coefs[order(coefs[,'coef']),'id'])
ggplot(coefs[-1,], aes(x = id, y= coef))+geom_point() + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),axis.text=element_text(size=12))+
  ggtitle('Coefficients of GLMNET Model')+xlab('Trade Industry') + ylab('Scaled Coefficient')


