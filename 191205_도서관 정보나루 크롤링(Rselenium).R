# 패키지 로드, 기본 변수 생성
library(rvest)
library(RSelenium)
library(httr)

gu_vec <- c('강북구', '도봉구')
remDr <- remoteDriver(port=4445, browserName='chrome')

# 사용할 함수 3개 정의
## 함수(1) '장서/대출 데이터' 이동 > '지역' 서울 선택
reset_page <- function() {
  url_main <- 'https://www.data4library.kr/openDataL'
  remDr$navigate(url_main)
  Sys.sleep(1)
  
  tmp_options <- remDr$findElement('css', '#p_region')
  tmp_options$sendKeysToElement(list('서울'))
  Sys.sleep(1)
}

## 함수(2) '장서/대출 데이터' 이동 > '지역' 서울, '세부지역(구)' gu_name 선택
reset_page_gu <- function(gu_name='강남구') {
  url_main <- 'https://www.data4library.kr/openDataL'
  remDr$navigate(url_main)
  Sys.sleep(1)
  
  tmp_options <- remDr$findElement('css', '#p_region')
  tmp_options$sendKeysToElement(list('서울'))
  Sys.sleep(1)
  
  tmp_options <- remDr$findElement('css', '#dtl_region')
  tmp_options$sendKeysToElement(list(gu_name))
  Sys.sleep(1)
}

## 함수(3) 도서관 대출 데이터 최대 30건 다운로드
download_files <- function() {
  tmp_elem <- remDr$findElement('css','#sb-site > section > div.sub_container > div.paging_nav > span')
  tmp_pg_num <- as.numeric(tmp_elem$getElementText())
  tmp_pg_num <- tmp_pg_num %% 10
  
  if (tmp_pg_num > 3) {
    tmp_pg_num <- 3
  }
  
  for (i in 1:tmp_pg_num) {
    remDr$executeScript(script = "window.scrollTo(0, 350);")
    Sys.sleep(1)
    
    tmp_list <- remDr$findElements('css', '#sb-site > section > div.sub_container > div.notice_wrap.mgb_20 > table > tbody > tr')
    rep_num <- length(tmp_list)
    
    for (j in 1:rep_num) {
      try({
        tmp_selector <- paste0('#sb-site > section > div.sub_container > div.notice_wrap.mgb_20 > table > tbody > tr:nth-child(', j, ') > td.data_type.br_none > a.download_link.excel_type')
        tmp_file <- remDr$findElement('css', tmp_selector)
        tmp_file$clickElement()
      })
    }
    
    tmp_btn <- tryCatch(
      {
        tmp_selector <- paste0('#sb-site > section > div.sub_container > div.paging_nav > span > a:nth-child(', i+1, ')')
        remDr$findElement('css',tmp_selector)
      },
      error = function(e) {
        remDr$findElement('css','#sb-site > section > div.sub_container > div.paging_nav > a.page_select.next_page')
      }
    )
    tmp_btn$clickElement()
    Sys.sleep(1)
  }
}

# 1. 크롤링 할 페이지 로드 (서울시 도서관 대출 데이터 목록)
remDr$open()
Sys.sleep(1)
url_main <- 'https://www.data4library.kr/openDataL'


# 2. '장서/대출 데이터' > 지역, 구 선택(반복문) > 각 도서관 페이지 방문(반복문) >  파일 30개 다운로드(반복문 함수)
for (gu in gu_vec) {
  reset_page_gu(gu)
  
  ## 해당 세부지역(구)의 도서관 대출 데이터 페이지수 수집
  tmp_el <- remDr$findElement('css', '#pagef > div.paging_nav > span')
  tmp_page_nums <- as.numeric(tmp_el$getElementText())
  tmp_last_pg <- tmp_page_nums %% 10
  
  ## 해당 세부지역(구)의 모든 도서관 대출 데이터 수집
  pg_cnt <- 1
  while (T) {
    tmp_list <- remDr$findElements('css', '#pagef > div.tbl_scroll_box.mgb_20 > table > tbody a')
    rep_num <- length(tmp_list)
    
    
    for (j in 1:rep_num) {
      
    # # 테스트용 코드 (접근 수 줄이기)
    #   if ((pg_cnt==1 & j<19) | (pg_cnt==2 & j<6)) {
    #     next
    #   }
      
      if (j>10) {
        remDr$executeScript(script = "window.scrollTo(0, 300);")
        Sys.sleep(1)
      }
      
      error_obj <- tryCatch(
        {
          tmp_selector <- paste0('#pagef > div.tbl_scroll_box.mgb_20 > table > tbody > tr:nth-child(', j, ') > td.link_td > a')
          tmp_btn <- remDr$findElement('css', tmp_selector)
          tmp_btn$clickElement()
          Sys.sleep(1)
          
          download_files()
          print(paste(gu, pg_cnt, '번째 페이지의', j, '번째 도서관 방문 완료'))  # 추후 수정 → download_files()
        },
        error = function(e) {e}
      )
      if(inherits(error_obj, "error")) {
        print(error_obj)
        break
      }
      
      reset_page_gu(gu)
      if (pg_cnt>1) {
        for (i in 1:pg_cnt-1) {
          remDr$executeScript(script = "window.scrollTo(0, 500);")
          tmp_btn <- remDr$findElement('css', '#pagef > div.paging_nav > a.page_select.next_page')
          tmp_btn$clickElement()
          Sys.sleep(1)
        }
      }
    }
    
    pg_cnt <- pg_cnt + 1
    if (pg_cnt > tmp_last_pg) { break }
    
    reset_page_gu(gu)
    for (i in 1:pg_cnt-1) {
      remDr$executeScript(script = "window.scrollTo(0, 500);")
      tmp_btn <- remDr$findElement('css', '#pagef > div.paging_nav > a.page_select.next_page')
      tmp_btn$clickElement()
      Sys.sleep(1)
    }
  }
}





