setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
rm(list = ls())

library(XML)
api_key = "인증키"

# 1. 16~18년_서울시구별 인기대출도서 데이터 로드
## columns : 연도, 구, 대출건수

gu_names = c('종로구', '중구', '용산구', '성동구', '광진구', '동대문구', '중랑구', '성북구', '강북구', '도봉구', '노원구', '은평구', '서대문구', '마포구', '양천구', '강서구', '구로구', '금천구', '영등포구', '동작구', '관악구', '서초구', '강남구', '송파구', '강동구')

year_vec = c()
gu_vec = c()
loan_cnt_vec = c()
for (tmp_year in c(2016:2018)) {
  for (idx in 1:25) {
    if (idx<10) {
      gu_code = paste0('110', idx*10)
    } else {
      gu_code = paste0('11', idx*10)
    }
    
    fileUrl = paste0('http://data4library.kr/api/loanItemSrch?authKey=', api_key,
                     '&startDt=', tmp_year, '-01-01&endDt=', 
                     tmp_year, '-12-31&region=11&dtl_region=', gu_code)
    xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)
    Sys.sleep(2)
    loan_counts = xpathSApply( xmlRoot(xml_doc), "//loan_count", xmlValue)
    tmp_loan_sum = sum(as.numeric(loan_counts))
    
    year_vec = c(year_vec, tmp_year)
    gu_vec = c(gu_vec, gu_names[idx])
    loan_cnt_vec = c(loan_cnt_vec, tmp_loan_sum)
  }
}

res_df = data.frame( 기간 = year_vec, 자치구 = gu_vec, 
                     loan_count_all = loan_cnt_vec, stringsAsFactors = F)
View(res_df)


# 2. 결측치 직접 채워넣기
res_df[ is.na(res_df$loan_count), ]  # 광진구 16, 17년, 강북구 16년, 마포구 16년

## 하나씩 로드
# tmp_year = 2017
# tmp_gu = '광진구'
# idx = grep(tmp_gu, gu_names)
# if (idx<10) {
#   gu_code = paste0('110', idx*10)
# } else {
#   gu_code = paste0('11', idx*10)
# }
# 
# fileUrl = paste0('http://data4library.kr/api/loanItemSrch?authKey=', api_key,
#                  '&startDt=', tmp_year, '-01-01&endDt=', 
#                  tmp_year, '-12-31&region=11&dtl_region=', gu_code)
# xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)
# loan_counts = xpathSApply( xmlRoot(xml_doc), "//loan_count", xmlValue)
# sum(as.numeric(gsub(',','',loan_counts)))

res_df[5,3] = 56030  # 광진구 16년 56030
res_df[9,3] = 45516  # 강북구 16년 45516
res_df[14,3] = 35459  # 마포구 16년 35459
res_df[30,3] = 48284  # 광진구 17년 48284



# 3. 데이터 저장
write.csv(res_df, '191219_2016~2018년 서울시 구별 ISBN전체 인기대출도서 대출건수.csv')

