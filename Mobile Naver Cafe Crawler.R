
# load packages
library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(magrittr)

# sample url 
# 'https://m.cafe.naver.com/SectionArticleSearch.nhn?query=%22%EA%B0%95%EB%82%A8%EC%B4%88%EB%93%B1%ED%95%99%EA%B5%90+%EB%B0%B0%EC%A0%95%22'

# Set user-agent
ua <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36'

# --------------------------------------------------------------------------------

# css로 텍스트만 수집하는 함수 생성
getHtmlText <- function(x, css) {
  result <- x %>% html_nodes(css = css) %>% html_text()
  return(result)
}

# css로 링크 수집하는 함수 생성
getHtmlAttr <- function(x, css, name) {
  result <- x %>% html_nodes(css = css) %>% html_attr(name = name)
  return(result)
}


# --------------------------------------------------------------------------------

# 카페 메인 페이지에서 키워드로 링크 수집하는 함수 생성
getCafeLink <- function(keyword, maxPage) {
  
  # Set url elements
  mainUrl <- 'https://m.cafe.naver.com/SectionArticleSearch.nhn'
  
  # url 조립 : 키워드에 따옴표 처리
  url <- paste0(mainUrl,
                paste0('?query=', paste0('%27', URLencode(keyword), '%27'))
  )
  
  # 에러 발생 시 무시
  tryCatch({
    
    # html request
    resp <- GET(url, user_agent(ua))
    
    # 건수 확인
    count <- resp %>% 
      read_html() %>% 
      html_node(css = 'span.num') %>% 
      html_text() %>% 
      as.numeric()
    
    # 페이지수 계산 : 1페이지에 20건 기준
    pages <- ceiling(count / 20)
    
    # 페이지 최대값 계산
    pages <- min(pages, maxPage, 100)
    
    # 결과 객체 생성
    articles <- data.frame()
    
    # 순환 실행
    for (i in 1:pages) {
      
      # url 조립
      url <- paste0(mainUrl,
                    paste0('?query=', paste0('%27', URLencode(keyword), '%27')),
                    paste0('&page=', i)
      )
      
      # html request
      resp <- GET(url = url, user_agent(ua))
      
      # 본문 수집
      items <- resp %>% 
        read_html() %>% 
        html_nodes(css = 'ul#searchList li')
      
      # 카페 링크, 제목, 요약, 카페명, 게시일자 수집
      link <- getHtmlAttr(x = items, css = 'a', name = 'href')
      head <- getHtmlText(x = items, css = 'div.article_wrap strong')
      text <- getHtmlText(x = items, css = 'div.article_wrap p.desc.ellip') %>% str_trim()
      cafe <- getHtmlText(x = items, css = 'div.article_wrap div.info span.cafe_name')
      date <- getHtmlText(x = items, css = 'div.article_wrap div.info span.date')
      
      # 데이터 프레임으로 저장
      df <- data.frame(keyword, link, head, date, text, cafe)
      
      # 결과 객체에 행 기준 추가
      articles <- rbind(articles, df)
      
      # set time sleep
      Sys.sleep(3)
    }
    
  }, error = function(e) e)
  
  return(articles)
}


# --------------------------------------------------------------------------------

# 카페 링크로 본문과 댓글 수집하는 함수 생성
getTextReples <- function(url) {
  
  # reference 설정
  ref <- 'https://m.cafe.naver.com'
  
  # 에러 발생 시 중단 
  tryCatch({
    
    # HTML request
    resp <- GET(url, user_agent(ua), add_headers(referer = ref))
    
    # 본문 수집
    fulltext <- resp %>% 
      read_html() %>% 
      html_node(css = 'div#postContent') %>% 
      html_text() %>% 
      str_replace_all(pattern = '[\n\r\t ]+', replacement = ' ') %>% 
      str_trim()
    
    # 댓글 갯수 확인
    reNum <- resp %>% 
      read_html() %>% 
      html_node(css = 'div.section_comment a em') %>% 
      html_text() %>% 
      as.numeric()
    
    # 댓글 단 아이디 수집
    reIds <- resp %>% 
      read_html() %>% 
      html_nodes(css = 'li.comment span.name.ellip') %>% 
      html_text()
    
    # 댓글 수집
    reple <- resp %>% 
      read_html() %>% 
      html_nodes(css = 'li.comment p') %>% 
      html_text() %>% 
      str_replace_all(pattern = '[\n\r\t ]+', replacement = ' ') %>% 
      str_trim()
    
    # 댓글 등록일자 수집
    rdate <- resp %>% 
      read_html() %>% 
      html_nodes(css = 'li.comment div.date_area span') %>% 
      html_text()
    
    # 데이터 프레임으로 저장
    df <- data.frame(url, fulltext, reNum, reIds, reple, rdate)
    
    # set time sleep
    Sys.sleep(3)
    
  }, error = function(e) e)
  
  return(df)
}


# --------------------------------------------------------------------------------

# 키워드 벡터 설정
keywords <- c('문재인', '김정은')

# 링크 저장할 데이터 프레임 생성
cafeLinks <- data.frame()

# 순환 실행
# maxPage는 100까지 설정할 수 있습니다! 
for (keyword in keywords) {
  df <- getCafeLink(keyword = keyword, maxPage = 10)
  cafeLinks <- rbind(cafeLinks, df)
}

# check frequency
table(cafeLinks$keyword)

# 카페 링크 건수 지정
n <- nrow(cafeLinks)

# 댓글 저장할 데이터 프레임 생성
cafeReples <- data.frame()

# 순환 실행
for (i in 1:n) {
  url <- cafeLinks$link[i]
  df <- getTextReples(url)
  cafeReples <- rbind(cafeReples, df)
}

# 결과 미리보기 
head(x = cafeReples, n = 20L)


# save RDS
saveRDS(object = cafeLinks, file = 'Naver_cafe_links_for_keywords.RDS')
saveRDS(object = cafeReples, file = 'Naver_cafe_reples_for_keywords.RDS')
