setwd('E:/Rstudy/191203~191220 서울시 도서관 데이터 분석 프로젝트')
library(XML)
api_key = '99b1685572eb0c82ab29e82749a1392ff4468117090465fd5c9a3ffe9b96f603'


# unused_books 벡터 생성
unused_books = c('이세돌의 어린이 바둑 교과서 :동화로 쉽게, 재밌게 바둑의 원리를 배운다!',
                 'Olivia Forms a Band', '(아름다운 우리 전통)매듭 만들기',
                 '탐나는 케이크', '탐나는 케이크')
unused_books_ISBN = c(9788993968439,9781847386045,9788940803714,9788991310605,9788991310612)


# 도서 상세 조회 API로 성별+연령별 순위 및 대출건수 수집
# group_loan_cnt
group_loan_cnt = matrix(0, ncol = 14)

for (tmp_ISBN in unused_books_ISBN) {
  tmp_cnt_vec = rep(0, 14)
  names(tmp_cnt_vec) = c('초등(남)','초등(여)','청소년(남)','청소년(여)',
                      "20대(남)","20대(여)","30대(남)","30대(여)",
                      "40대(남)","40대(여)","50대(남)","50대(여)",
                      "60대이상(남)","60대이상(여)")
  
  fileUrl = paste0('http://data4library.kr/api/srchDtlList?authKey=',api_key,
                   '&isbn13=',tmp_ISBN,'&loaninfoYN=Y&displayInfo=gender')
  
  xml_doc = xmlTreeParse(fileUrl,useInternal=TRUE)
  xpathSApply( xmlRoot(xml_doc), "//genderResult", xmlValue)
  
  
}

tmp_loan_sum = sum(as.numeric(loan_counts))
xml_doc

rootNode <- xmlRoot(xml_doc)  # 루트노드 따로 저장
xmlName(rootNode)  # 루트노드의 이름 확인
names(rootNode)  # 루트노드의 하위노드 확인

loanInfo_node = rootNode[[3]]
names(loanInfo_node)
result_node = loanInfo_node[[2]]
names(result_node)
xmlSize(result_node)


for (idx in 1:xmlSize(result_node)) {
  tmp_idx = xpathSApply(result_node[[idx]], 'name', xmlValue)
  tmp_cnt = xpathSApply(result_node[[idx]], 'loanCnt', xmlValue)
  tmp_cnt_vec[tmp_idx] = tmp_cnt
}
tmp_cnt_vec


docs_node = rootNode[[3]]  # doc들을 가지고 있는 docs 노드 따로 저장
doc1_node = docs_node[[1]]  # docs 노드의 첫 번째 doc 노드 따로 저장
length(names(doc1_node))  # doc 노드의 하위노드 개수 확인
doc1_node[[11]]  # doc 노드의 11번째 값이 대출건수 값
