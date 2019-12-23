# setwd()
rm(list = ls())

library(XML)
api_key = "인증키"


# 0. 샘플 인기도서 목록 생성 - 서명, ISBN, ISBN 부가기호, KDC 열 저장
options(scipen = 99)
pop_books = read.csv('2018년_성북구_아동_인기대출도서.csv', stringsAsFactors=F, skip=13, header=T)[c(2, 7:9)]
pop_books$서명 = trimws(pop_books$서명)
str(pop_books)
head(pop_books)



# 0. sampling - 한 도서에 대해 도서 키워드&가중치 목록 수집 (도서 키워드 목록 API 활용)
tmp_ISBN = 9788986621136

tmp_url = paste0('http://data4library.kr/api/keywordList?authKey=', api_key,'&isbn13=', tmp_ISBN)
xml_doc = xmlTreeParse(tmp_url, useInternal=TRUE)

tmp_kewords = trimws(xpathSApply( xmlRoot(xml_doc), "//word", xmlValue))
tmp_weights = as.numeric(trimws(xpathSApply( xmlRoot(xml_doc), "//weight", xmlValue)))

tmp_res_list = list(bookname = pop_books$서명[1], ISBN = pop_books$ISBN[1], 
                    ISBN_add = pop_books$ISBN부가기호[1], KDC = pop_books$KDC[1],
                    keywords_df = data.frame(keyword = tmp_kewords, weight = tmp_weights))
str(tmp_res_list)



# 1. scale-up - 200개 도서에 대해 키워드&가중치 목록 수집
res_list = list()

# for (idx in 1:nrow(pop_books)) {

for (idx in 10:20) {
  tmp_ISBN = pop_books$ISBN[idx]
  tmp_bookname = pop_books$서명[idx]
  
  tmp_url = paste0('http://data4library.kr/api/keywordList?authKey=', api_key,'&isbn13=', tmp_ISBN)
  xml_doc = xmlTreeParse(tmp_url, useInternal=TRUE)
  Sys.sleep(1)
  
  tmp_kewords = trimws(xpathSApply( xmlRoot(xml_doc), "//word", xmlValue))
  tmp_weights = as.numeric(trimws(xpathSApply( xmlRoot(xml_doc), "//weight", xmlValue)))
  
  tmp_res_list = list(bookname = tmp_bookname, ISBN = tmp_ISBN,
                      ISBN_add = pop_books$ISBN부가기호[idx], KDC = pop_books$KDC[idx],
                      keywords_df = data.frame(keyword = tmp_kewords, weight = tmp_weights))
  
  res_list[[tmp_bookname]] = tmp_res_list
  
  # check
  cat(tmp_bookname, any(is.na(tmp_res_list)), 
      any(is.na(tmp_res_list$keywords_df)), '\n')  # 정상이면 F, F (KDC에는 일부 결측치 존재)
  
  if (idx%%50==0) {
    print('======= 50번째 완료 =======')
  }
  # check
  
}

View(res_list)



# 3. 데이터 저장 & 다시 불러와서 확인
saveRDS(res_list, '2018_seongbook_popbook200_kewords.rds')

res_list_check = readRDS('2018_seongbook_popbook200_kewords.rds')
identical(res_list, res_list_check)
