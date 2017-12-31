
library(XML)

url <- "http://www.poliscirumors.com/journals.php"
parse_stats <- htmlParse(file=url)
tables <- readHTMLTable(parse_stats, stringsAsFactors=FALSE)
tabs <- as.data.frame(tables[2]) # save table as dataframe
tabs <- tabs[,-6] # remove 'notes' column
colnames(tabs) <- c("journal","result","first_response","av_time_btwn_RR","ref_reports","year_sub","year_added")
tabs$accept <- ifelse(tabs$result=="Accepted",1,0)
tabs$first_response <- as.numeric(tabs$first_response) # time to first response
tabs$ref_reports <- as.numeric(tabs$ref_reports) # number of referee reports

# Subset data to three major political science journals
data_sub <- subset(tabs, journal=="Journal Of Politics"|journal=="American Journal Of Political Science"|journal=="American Political Science Review")

summary(data_sub$accept) # Note very few are accepted  

# Just pool data and put into logit (very simple model)
mod <- glm(accept~ref_reports+first_response, data=data_sub,family="binomial")

summary(mod)

pred_min <- min(data_sub$first_response) # Predictions go from min to max
pred_max <- max(data_sub$first_response)
pred_range <- pred_max - pred_min
pred_med <- median(data_sub$ref_reports) # hold ref reports at median

pred <- predict(mod, newdata=data.frame(first_response=seq(pred_min,pred_max,by=1),ref_reports=rep(pred_med,by=pred_range)),type="response")
plot(seq(0,13,by=1),pred,xlab="Months",ylab="Predicted Accept",ylim=c(min(pred),max(pred)),type="l",lty="solid")
pred

# Let ref reports vary; don't store results before putting them into predictions
min(data_sub$ref_reports) # Predictions go from min to max
max(data_sub$ref_reports)
median(data_sub$first_response) # hold time at median

pred <- predict(mod, newdata=data.frame(ref_reports=seq(0,5,by=1),first_response=rep(2,6)),type="response")
plot(seq(0,5,by=1),pred,xlab="reports",ylab="Predicted Accept",ylim=c(min(pred),max(pred)),type="l",lty="solid")
pred




