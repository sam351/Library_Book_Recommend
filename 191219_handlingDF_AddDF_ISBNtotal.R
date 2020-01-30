setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
rm(list = ls())

library(dplyr)

# 1. 데이터 프레임 불러오기
book_loan_df = read.csv('191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수 (인구통계 추가).csv', stringsAsFactors=F)
book_loan_df = book_loan_df[,-1]
str(book_loan_df)

total_loan_df = read.csv('191219_2016~2018년 서울시 구별 ISBN전체 인기대출도서 대출건수.csv', stringsAsFactors=F)
total_loan_df = total_loan_df[-1]
str(total_loan_df)


# 2. 두 데이터 프레임 합치기
final_df = left_join(total_loan_df, book_loan_df, by=c('기간', '자치구'))
str(final_df)
View(final_df)


# 5. 통합 DF 저장
write.csv(final_df, "191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수 (인구통계 추가) (ISBN전체 추가).csv")
