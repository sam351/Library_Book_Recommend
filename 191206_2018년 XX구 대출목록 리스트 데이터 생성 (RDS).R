library(dplyr)
library(readxl)

# XX구 파일목록 확보
gu <- '도봉구'
src_dir <- paste(gu ,'대출목록')
src_file_vec <- list.files(src_dir)
src_file_cnt <- length(src_file_vec)

# XX구 파일목록에서, 2018년 파일만 담은 목록 생성
file_Y2018_vec <- c()
for (item in src_file_vec) {
  if (grepl('2018', item)) {
    file_Y2018_vec <- c(file_Y2018_vec, item)
  }
}
file_Y2018_cnt <- length(file_Y2018_vec)


# 도서관 이름 목록 벡터 생성
gu_lib_vec <- c()
for (item in file_Y2018_vec) {
  if ( grepl(' 도서 ', item) ) {
    end_p <- regexpr(' 도서 ', item)
    lib_name <- substr(item, 1,end_p - 1)
  }
  else if ( grepl(' 장서', item) ) {
    end_p <- regexpr(' 장서 ', item)
    lib_name <- substr(item, 1,end_p - 1)
  }
  else {
    print('====exception!!!====')
    print(item)
    lib_name <- item
  }
  gu_lib_vec <- c(gu_lib_vec, lib_name)
}
gu_lib_vec <- unique(gu_lib_vec)
gu_lib_vec


# 최종 - 성북구 도서관별 대출 데이터 리스트 생성
print('generating list...')
gu_lib_list <- list()
for (tmp_lib in gu_lib_vec) {
  
  tmp_list <- list()
  for (tmp_file in file_Y2018_vec) {
    tmp_file_head <- substr(tmp_file, 1, nchar(tmp_lib))
    
    if (tmp_file_head == tmp_lib) {
      temp_df <- 
        read_excel(paste0(src_dir, "/", tmp_file)) %>% 
        select(도서명,대출건수)
      temp_df$대출건수 <- as.numeric(temp_df$대출건수)
      
      tmp_name <- substr( tmp_file,
                          regexpr('[(]', tmp_file)+1, 
                          regexpr('[)]', tmp_file)-1 )
      tmp_list[[tmp_name]] <- temp_df
    }
  }
  
  gu_lib_list[[tmp_lib]] <- tmp_list
}

View(gu_lib_list)

file_name <- paste('2018년', gu, '도서관별 대출목록.rds')
saveRDS(gu_lib_list, file = file_name)
rm(gu_lib_list)
readRDS(file_name)
str(gu_lib_list)
