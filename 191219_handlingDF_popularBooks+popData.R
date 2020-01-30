setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
rm(list = ls())

library(dplyr)

# 1. 인구 통계 파일목록 확보
all_files = list.files(getwd())
pop_files = all_files[grepl('서울시' , all_files ) & grepl('통계' , all_files )]
pop_files


# 2. 파일 불러와서 인구 통계 df로 통합
households_df = data.frame()
populations_df = data.frame()

for (idx in 1:length(pop_files)) {
  tmp_file = pop_files[idx]
  tmp_df = read.csv(tmp_file, stringsAsFactors = F)
  
  if (idx <= 3) {
    households_df = rbind( households_df, tmp_df )
  } else {
    populations_df = rbind( populations_df, tmp_df )
  }
}


# 3. 인구 통계 df 별 전처리 & 하나의 df로 통합
## househods_df - 세대원수별 세대수
nrow(households_df)  # 78
households_df = households_df %>% filter(자치구 != '합계')
nrow(households_df)  # 75

## populations_df - 연령대별 인구수
populations_df = populations_df %>%
  filter(구분 != '합계' & 구분.1 == '계') %>% 
  select(-구분.1) %>% 
  rename(인구합계 = 계, 자치구 = 구분)

# View(households_df)
# View(populations_df)

pop_df = left_join(populations_df, households_df, by=c('기간','자치구'))
# View(pop_df)

for (col in colnames(pop_df[-c(1,2)])) {
  pop_df[, col] = gsub(',', '', pop_df[, col])
  pop_df[, col] = as.numeric(pop_df[, col])
}
# View(pop_df)
str(pop_df)


# 4. 인기도서 대출건수와 통합한 df 생성
book_loan_df = read.csv('191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수.csv', stringsAsFactors=F)
book_loan_df = book_loan_df[,-1] %>% 
  rename(기간 = year, 자치구 = gu)
head(book_loan_df)
str(book_loan_df)

final_df = left_join(book_loan_df, pop_df, by=c('기간','자치구'))
str(final_df)
# View(final_df)


# 5. 통합 DF 저장
colnames(final_df) = gsub('X','',colnames(final_df))
write.csv(final_df, "191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수 (인구통계 추가).csv")
