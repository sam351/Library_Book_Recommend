# < 도서 상세 조회 API 활용, 성별+연령대별 순위 및 대출건수 수집 >
# 사용 중 발견 문제점1 : 도서 상세 조회 API는 최근 90일 정보만 제공
# 사용 중 발견 문제점2 : 도서 상세 조회 API는 각 조건별 상위 1,000위까지 데이터만 제공

setwd('E:/Rstudy/191203~191220 서울시 도서관 데이터 분석 프로젝트')
library(XML)
api_key = '인증키'


# unused_books 벡터 생성
unused_books = c('신기한 스쿨 버스:키즈','책청소부 소소',
                 '황 반장 똥 반장 연애 반장','집안 치우기',
                 '윔피 키드','(앤서니 브라운의) 거울 속으로',
                 '(설민석의) 한국사 대모험','초정리 편지:배유안 장편동화',
                 '(어진이의)농장 일기','개구리와 두꺼비는 친구')

unused_books_ISBN = c(9788949150246, 9788954613521, 9788954619370,
                      9788955821055, 9788965590972, 9788984883727,
                      9791195794799, 9788936442293, 9788936445270,
                      9788949160016)


# 도서 상세 조회 API 활용해서 성별+연령대별 순위 및 대출건수 수집
# group_loan_cnt
group_loan_cnt = matrix(0, ncol = 14)
group_rank = matrix(0, ncol = 14)

for (tmp_ISBN in unused_books_ISBN) {
  tmp_rank_vec = rep(0, 14)
  tmp_cnt_vec = rep(0, 14)
  
  cols = c('초등(남)','초등(여)','청소년(남)','청소년(여)',
           "20대(남)","20대(여)","30대(남)","30대(여)",
           "40대(남)","40대(여)","50대(남)","50대(여)",
           "60대이상(남)","60대이상(여)")
  names(tmp_rank_vec) = cols
  names(tmp_cnt_vec) = cols
  
  # 도서 상세 조회 API 응답 수집 -> genderResult 노드 탐색
  fileUrl = paste0('http://data4library.kr/api/srchDtlList?authKey=',api_key,
                   '&isbn13=',tmp_ISBN,'&loaninfoYN=Y&displayInfo=gender')
  xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)
  
  loanInfo_node = xmlRoot(xml_doc)[["loanInfo"]]
  result_node = loanInfo_node[["genderResult"]]
  
  # 각 성별+연령대별 순위 및 대출건수 수집
  if (xmlSize(result_node) != 0) {
    for (idx in 1:xmlSize(result_node)) {
      tmp_idx = xpathSApply(result_node[[idx]], 'name', xmlValue)
      tmp_rank = xpathSApply(result_node[[idx]], 'ranking', xmlValue)
      tmp_cnt = xpathSApply(result_node[[idx]], 'loanCnt', xmlValue)
      
      tmp_rank_vec[tmp_idx] = tmp_rank
      tmp_cnt_vec[tmp_idx] = tmp_cnt
    }
  }
  
  group_rank = rbind(group_rank, tmp_rank_vec)
  group_loan_cnt = rbind(group_loan_cnt, tmp_cnt_vec)
}
group_rank = group_rank[-1,]
group_loan_cnt = group_loan_cnt[-1,]

rownames(group_rank) = NULL
rownames(group_loan_cnt) = NULL

group_rank
group_loan_cnt

