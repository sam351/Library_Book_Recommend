setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')
rm(list = ls())

library(XML)
api_key = "인증키"

# 0. sampling - ISBN아동_2018년_강남구 인기대출도서 데이터 로드
## API 호출해 응답받은 xml 문서 저장
fileUrl = paste0('http://data4library.kr/api/loanItemSrch?authKey=', api_key, '&startDt=2018-01-01&endDt=2018-12-31&region=11&dtl_region=11230&addCode=7')
xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)

## 한 개의 구 불러와서 전체 대출 건수 sum하기
loan_counts = xpathSApply( xmlRoot(xml_doc), "//loan_count", xmlValue)
sum(as.numeric(loan_counts))

## 기타 파싱 연습
rootNode <- xmlRoot(xml_doc)  # 루트노드 따로 저장
xmlName(rootNode)  # 루트노드의 이름 확인
names(rootNode)  # 루트노드의 하위노드 확인

docs_node = rootNode[[3]]  # doc들을 가지고 있는 docs 노드 따로 저장
doc1_node = docs_node[[1]]  # docs 노드의 첫 번째 doc 노드 따로 저장
length(names(doc1_node))  # doc 노드의 하위노드 개수 확인
doc1_node[[11]]  # doc 노드의 11번째 값이 대출건수 값


# 1. Scale-up - ISBN아동_16~18년_서울시구별 인기대출도서 데이터 로드
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
                     tmp_year, '-12-31&region=11&dtl_region=', gu_code, '&addCode=7')
    xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)
    Sys.sleep(2)
    loan_counts = xpathSApply( xmlRoot(xml_doc), "//loan_count", xmlValue)
    tmp_loan_sum = sum(as.numeric(loan_counts))
    
    year_vec = c(year_vec, tmp_year)
    gu_vec = c(gu_vec, gu_names[idx])
    loan_cnt_vec = c(loan_cnt_vec, tmp_loan_sum)
  }
}

res_df = data.frame( year = year_vec, gu = gu_vec, 
                     loan_count = loan_cnt_vec, stringsAsFactors = F)
View(res_df)


# 2. 결측치 직접 채워넣기
res_df[ is.na(res_df$loan_count), ]  # 광진구 16, 17년
res_df[5,3] = 44103  # 광진구 16년 44103건
res_df[30,3] = 36382  # 광진구 17년 36382건


# 3. 데이터 저장
write.csv(res_df, '191219_2016~2018년 서울시 구별 ISBN아동 인기대출도서 대출건수.csv')

