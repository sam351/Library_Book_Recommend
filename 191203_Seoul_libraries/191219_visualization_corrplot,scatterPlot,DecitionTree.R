setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
rm(list = ls())

# 데이터 로드 및 확인
book_loan_df = read.csv( '191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수 (인구통계 추가).csv')
book_loan_df = book_loan_df[,-1]
book_loan_df[,'기간'] = as.factor(book_loan_df[,'기간'])
str(book_loan_df)
colnames(book_loan_df)
table(is.na(book_loan_df))


# 상관계수 플롯 - book_loan_df
library(corrplot)
colnames(book_loan_df)
tmp_df = book_loan_df[c(3, 4:7, 11:14, 17:25, 27:33)]  # 대출건수, 아동 연령, 부모연령, 조부모연령, 전세대

cor_values = cor( tmp_df )[1,]
sort(cor_values[ cor_values>0.3 ], decreasing = T)

cor_matrix = cor(tmp_df)[ cor_values>0.3, cor_values>0.3 ]
corrplot(cor_matrix)

colnames(book_loan_df)
tmp_df2 = book_loan_df %>% select( -기간, -자치구 )

colnames(tmp_df2)
tmp_df2['under_9'] = tmp_df2['X0.4세'] + tmp_df2['X5.9세']
tmp_df2['under_14'] = tmp_df2['X0.4세'] + tmp_df2['X5.9세'] + tmp_df2['X10.14세']
tmp_df2['btw_4050'] = tmp_df['X40.44세'] + tmp_df['X45.49세']
tmp_df2['over_90'] = tmp_df2['X90.94세'] + tmp_df2['X95.99세'] + tmp_df2['X100세.이상.']
tmp_df2['hold_under2'] = tmp_df2['X1인세대'] + tmp_df2['X2인세대']
tmp_df2['hold_over3'] = tmp_df2['전체세대'] - tmp_df2['hold_under2']
tmp_df2 = tmp_df2[c(1, 35:39)]

cor(tmp_df2)[1,]
corrplot(cor(tmp_df2))

plot(tmp_df2$under_14, tmp_df2$loan_count,
     main = "(16~18년 구별) 14세 이하 인구 x 아동도서 대출건수",
     xlab = "14세 이하 인구",
     ylab = "인기 아동도서 대출건수")

plot( log(tmp_df2$btw_4050), log(tmp_df2$loan_count),
     main = "(16~18년 구별) 40대 인구 x 아동도서 대출건수",
     xlab = "40대 인구(로그값)",
     ylab = "인기 아동도서 대출건수(로그값)")


# 의사 결정 트리 플롯
library(party)
book_loan_ctree <- ctree(loan_count ~ 
                           under_9+btw_4050+over_90+hold_under2+hold_over3,
                         data = tmp_df2)
book_loan_ctree
plot(book_loan_ctree)


View(cbind(book_loan_df$자치구, tmp_df2) %>% arrange(desc(under_14)))
View(cbind(book_loan_df$자치구, tmp_df2))



# ======================================



# 데이터 로드 및 확인
book_loan_df_exp = read.csv('191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수 (인구통계 추가) (ISBN전체 추가).csv')
book_loan_df_exp = book_loan_df_exp[,-1]
book_loan_df_exp[,'기간'] = as.factor(book_loan_df_exp[,'기간'])
str(book_loan_df_exp)
colnames(book_loan_df_exp)
table(is.na(book_loan_df_exp))


# 상관계수 플롯 - book_loan_df
book_loan_df_exp = book_loan_df_exp %>% 
  mutate( loan_ratio = loan_count/loan_count_all )
colnames(book_loan_df_exp)

tmp_df2 = book_loan_df_exp = book_loan_df_exp %>% 
  mutate( loan_ratio = loan_count/loan_count_all )
tmp_df2['under_9'] = ( tmp_df2['X0.4세'] + tmp_df2['X5.9세'] ) / tmp_df2['인구합계']
tmp_df2['under_14'] = ( tmp_df2['X0.4세'] + tmp_df2['X5.9세'] + tmp_df2['X10.14세'] ) / tmp_df2['인구합계']
tmp_df2['btw_4050'] = ( tmp_df['X40.44세'] + tmp_df['X45.49세'] ) / tmp_df2['인구합계']
tmp_df2['over_90'] = ( tmp_df2['X90.94세'] + tmp_df2['X95.99세'] + tmp_df2['X100세.이상.'] ) / tmp_df2['인구합계']

tmp_df2['hold_under2'] = ( tmp_df2['X1인세대'] + tmp_df2['X2인세대'] ) / tmp_df2['전체세대']
tmp_df2['hold_over3'] = ( tmp_df2['전체세대'] - tmp_df2['hold_under2'] ) / tmp_df2['전체세대']
colnames(tmp_df2)
# grep("loan_ratio", colnames(tmp_df2))

ratio_df = tmp_df2[c(2,4, 38:44)]
colnames(ratio_df)

cor(ratio_df[-c(1,3)])[1,]
corrplot(cor(ratio_df[-c(1,3)]))

plot(ratio_df$hold_under2, log(ratio_df$loan_count),
     main = "(16~18년 구별) 2인 이하 가구 수 x 아동도서 대출건수",
     xlab = "2인 이하 가구 수",
     ylab = "인기 아동도서 대출건수")
plot(ratio_df$hold_over3, log(ratio_df$loan_count),
     main = "(16~18년 구별) 3인 이상 가구 수 x 아동도서 대출건수",
     xlab = "3인 이상 가구 수",
     ylab = "인기 아동도서 대출건수")


lm_model = lm( loan_count ~ under_14+btw_4050+hold_under2+hold_over3 , data = ratio_df )
summary(lm_model)
step(lm_model, direction = 'both')

lm_model2 = lm(formula = loan_count ~ btw_4050 + hold_over3, data = ratio_df)
summary(lm_model2)

lm_model3 = lm(formula = loan_count ~ log(btw_4050) + hold_over3, data = ratio_df)
summary(lm_model3)


plot(ratio_df$btw_4050, ratio_df$loan_count)
plot(ratio_df$hold_over3, ratio_df$loan_count)
plot(ratio_df$hold_under2, ratio_df$loan_count)