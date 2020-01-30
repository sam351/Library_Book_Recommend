# 패키지 로드, 기본 변수 생성
library(rvest)
library(RSelenium)
library(httr)

gu_vec <- c('송파구')
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

## 함수(3) 도서관 대출 데이터 전체 다운로드
download_files <- function() {
  # 마지막 페이지로 이동 & 총 페이지 수 저장 (tmp_page_len-1만큼 반복)
  remDr$executeScript(script = "window.scrollTo(0, 350);")
  tmp_btn <- remDr$findElement('css','#sb-site a.page_select.last_page')
  tmp_btn$clickElement()
  
  remDr$executeScript(script = "window.scrollTo(0, 350);")
  tmp_elem <- remDr$findElement('css','#sb-site > section > div.sub_container > div.paging_nav > span > strong')
  tmp_page_len <- as.numeric(tmp_elem$getElementText())
  
  for (i in 1 : tmp_page_len-1) {
    # 파일 개수 확인 & 하나씩 클릭해 다운로드
    tmp_list <- remDr$findElements('css', '#sb-site > section > div.sub_container > div.notice_wrap.mgb_20 > table > tbody > tr')
    rep_num <- length(tmp_list)
    
    for (j in 1:rep_num) {
      try({
        tmp_selector <- paste0('#sb-site > section > div.sub_container > div.notice_wrap.mgb_20 > table > tbody > tr:nth-child(', j, ') > td.data_type.br_none > a.download_link.excel_type')
        tmp_file <- remDr$findElement('css', tmp_selector)
        tmp_file$clickElement()
        Sys.sleep(2)
        
      })
    }
    
    # 한 페이지 앞으로 이동
    tmp_btn <- remDr$findElement('css','#sb-site > section > div.sub_container > div.paging_nav > a.page_select.prev_page')
    tmp_btn$clickElement()
    remDr$executeScript(script = "window.scrollTo(0, 350);")
    Sys.sleep(3)
    
    # 에러 발생 시 예외처리용 코드
    # tmp_btn <- tryCatch(
    #   {
    #     
    #   },
    #   error = function(e) {
    #     
    #   }
    # )
  }
}





# 1. 크롤링 할 페이지 로드 (서울시 도서관 대출 데이터 목록)
remDr$open()
Sys.sleep(1)
url_main <- 'https://www.data4library.kr/openDataL'


# 2. '장서/대출 데이터' > 지역, 구 선택(반복문) > 각 도서관 페이지 방문(반복문) >  파일 전체 다운로드(반복문 함수)
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
          print(paste(gu, pg_cnt, '번째 페이지의', j, '번째 도서관 방문 완료'))
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



# 3. 결측 데이터 직접 채우기 - 한 번에 크기가 큰 여러 파일을 클릭하다 보니 스킵되는 파일이 있음
reset_page_gu(gu)
# (여기서 도서관 직접 선택 후 진행)
download_files()


