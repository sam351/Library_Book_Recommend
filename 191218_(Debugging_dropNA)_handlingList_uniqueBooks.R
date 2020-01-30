# < 각 df에서 중복 도서들의 대출건수를 통합해 하나의 레코드로 만든 df들의 리스트 생성 >
# < 위 작업에서 성북구 디버깅&데이터 업데이트 - 도서명이 NA인 행 제외 >

setwd('C:/Users/student/Desktop/sam/팀 프로젝트 (메인)')

rm(list = ls())
load('2018년 성북구 도서관별 대출목록.RData')  # 변수명 = sb_lib_list
sb_cumsum_list = readRDS('2018 seongbook library cumsum data.rds')  # 수정할 데이터


# 성북정보도서관, 아리랑정보도서관에서 도서명 NA값 제외하고 작업 실행해 sb_cumsum_list 수정

for (tmp_lib in c("성북정보도서관", "아리랑정보도서관")) {
  tmp_lib_list = sb_lib_list[[tmp_lib]]  # 도서관 A의 리스트
  tmp_res_list = list()  # 도서관 A에서 중복을 제거해 새로 만들 리스트
  
  list_idx = 0  # 원래 리스트의 이름목록을 그대로 가져오기 위해, 인덱스 사용
  
  for (tmp_df in tmp_lib_list) {
    tmp_df = na.omit(tmp_df)
    
    uniq_books = unique(tmp_df$도서명)
    uniq_cnt = c()
    
    for (idx in 1:length(uniq_books)) {
      tmp_book = uniq_books[idx]
      loan_num_vec = tmp_df$대출건수[tmp_df$도서명 == tmp_book]
      uniq_cnt[idx] = sum( loan_num_vec )
      
      # # check
      # if (idx %% 10000 == 0) {
      #   cat(idx, '번째 도서 완료')
      # }
      # # check
    }
    
    tmp_res_df = data.frame( 도서명 = uniq_books, 대출건수 = uniq_cnt, stringsAsFactors=F )
    
    list_idx = list_idx+1
    tmp_month = names(tmp_lib_list)[list_idx]
    tmp_res_list[[ tmp_month ]] = tmp_res_df
    
    # check
    cat(tmp_month, '수정 완료\n')
    # check
  }
  
  sb_cumsum_list[[tmp_lib]] = tmp_res_list
  cat(tmp_lib, '통합 완료\n')
}

View(sb_cumsum_list)

table(is.na(sb_cumsum_list$성북정보도서관[[1]])); table(is.na(sb_cumsum_list$성북정보도서관[[12]])); table(is.na(sb_cumsum_list$아리랑정보도서관[[1]])); table(is.na(sb_cumsum_list$아리랑정보도서관[[12]]))  # 정상이면 all FALSE


# 2. RDS 파일로 저장 & 불러와서 확인
file_name = '2018 seongbook library cumsum data (updated 191218).rds'
saveRDS(sb_cumsum_list, file = file_name)

sb_cumsum_list_check = readRDS(file_name)
identical(sb_cumsum_list, sb_cumsum_list_check)  # 정상이면 TRUE
View(sb_cumsum_list_check)

