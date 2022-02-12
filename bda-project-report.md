\[경영데이터분석\] 기말 프로젝트
================
이은서, 송지영
2021 6 13

<style type="text/css">
  body{
  font-size: 15pt;
  font-family: Calibri
}
</style>

## 프로젝트 주제

로튼 토마토 내 평론가들의 영화 리뷰의 정서 분석 : 영화 <라라랜드>,
<주토피아>, <부산행>을 중심으로

## 프로젝트 동기/이유

로튼 토마토에서 TOP 100 MOVIES OF 2016 안에서 중 화제성 높았던 세 영화
<주토피아>, <부산행>, <라라랜드>의 토마토미터는 각각 98%, 94%, 91%로
좋은 평가를 받은 영화들이다. 토마토미터는 평론가들의 긍/부정 리뷰를
비율로 나타낸 것으로 60%를 기준으로 60% 이상이면 Fresh, 미만은
Rotten으로 나뉜다. 우리는 이 영화들의 평론가 리뷰를 정서 분석 후 긍/부정
점수를 산정하여 높은 토마토미터와 리뷰의 정서에 연관성을 밝히고자 한다.

## 데이터 수집 계획

로튼 토마토 내 각 영화 리뷰 페이지 내 평론가 리뷰 섹션 에서 웹
스크래핑을 통하여 리뷰 데이터를 수집한다.

(<https://www.rottentomatoes.com/m/la_la_land/reviews>)
(<https://www.rottentomatoes.com/m/zootopia/reviews>)
(<https://www.rottentomatoes.com/m/train_to_busan/reviews>)

## 데이터 수집 방법 및 결과

1.  우선 rvest와 SelectorGadget을 이용하여 html 노드를 가져와 리뷰
    데이터프레임을 생성한다.

2.  영화별 리뷰페이지가 각각 존재하기 때문에 function()을 이용하여
    페이지별로 리뷰를 수집할 수 있는 함수를 만들어 영화별 리뷰
    데이터프레임을 형성한다.

다음은 영화 <라라랜드> 리뷰 데이터프레임을 형성한 것이다.

``` r
# 페이지별 리뷰를 가져오는 함수를 fetch()로 설정한다.
fetch <- function(p) {
  reviews <- "https://www.rottentomatoes.com/m/la_la_land/reviews"
  html <- read_html(paste0(reviews,"?type=&sort=&page=", p))
  critic_name <- html %>% html_nodes("#content .articleLink") %>% html_text(); critic_name
  critic_publication <- html %>% html_nodes(".critic-publication") %>% html_text(); critic_publication
  review <- html %>% html_nodes(".the_review") %>% html_text(trim = TRUE); reviews
  date <- html %>% html_nodes(".review-date") %>% html_text(trim = TRUE); date
  tomatometer <- html %>% html_nodes("#content .icon") %>% html_attr("class")
  tomatometer <- ifelse(str_detect(tomatometer, pattern = "fresh"), "fresh", "rotten"); tomatometer
  lalaland <- tibble(critic_name, critic_publication, review, date, tomatometer); lalaland
}

# 직접 만든 함수 fetch()를 이용하여 전체 리뷰에 대한 데이터프레임을 형성한다.
lalaland <- lapply(1:24, function(p) fetch(p)) %>%
  bind_rows()

# 개봉연도에 작성된 리뷰와 그 이후에 작성된 리뷰를 나누기 위하여 열을 새로 설정한다.
lalaland$year <- strsplit(lalaland$date, split =", ")
lalaland$year <- ifelse(str_detect(lalaland$year, pattern = "2016"), "in 2016", "after 2016")

lalaland
```

    # A tibble: 0 x 6
    # ... with 6 variables: critic_name <chr>, critic_publication <chr>,
    #   review <chr>, date <chr>, tomatometer <lgl>, year <lgl>

위와 같은 방법으로 영화 <주토피아>, <부산행>의 리뷰 데이터프레임을
형성한다.

## 텍스트 기본통계분석

형성한 리뷰 데이터프레임들을 가지고 토큰화, 불용어 처리, 그리고 단어빈도
통계(그래프, 워드클라우드)를 실시하였다.

### 영화 <라라랜드>의 기본통계분석

``` r
tidy_lll <- lalaland %>% group_by(tomatometer, year) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>% ungroup(); tidy_lll
```

    # A tibble: 5,171 x 3
       tomatometer year       word         
       <chr>       <chr>      <chr>        
     1 fresh       after 2016 simple       
     2 fresh       after 2016 tale         
     3 fresh       after 2016 told         
     4 fresh       after 2016 visual       
     5 fresh       after 2016 inventiveness
     6 fresh       after 2016 reminds      
     7 fresh       after 2016 audience     
     8 fresh       after 2016 power        
     9 fresh       after 2016 inherent     
    10 fresh       after 2016 cinema       
    # ... with 5,161 more rows

``` r
tidy_lll %>% count(word, sort = T)
```

    # A tibble: 2,104 x 2
       word          n
       <chr>     <int>
     1 la          364
     2 land        173
     3 musical     100
     4 film         87
     5 movie        76
     6 love         68
     7 hollywood    49
     8 chazelle     48
     9 review       47
    10 spanish      44
    # ... with 2,094 more rows

``` r
freq_lll <- tidy_lll %>% count(word, sort = T) %>%
  mutate(percent = n / sum(n) * 100);freq_lll
```

    # A tibble: 2,104 x 3
       word          n percent
       <chr>     <int>   <dbl>
     1 la          364   7.04 
     2 land        173   3.35 
     3 musical     100   1.93 
     4 film         87   1.68 
     5 movie        76   1.47 
     6 love         68   1.32 
     7 hollywood    49   0.948
     8 chazelle     48   0.928
     9 review       47   0.909
    10 spanish      44   0.851
    # ... with 2,094 more rows

``` r
freq_lll %>% filter(percent > 0.3) %>%
  mutate(word = reorder(word, percent)) %>%
  ggplot(aes(word, percent)) + geom_col() + coord_flip()
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
freq_lll %>%
  with(wordcloud(word, n, max.words = 80, colors = brewer.pal(8, "Paired")))
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

### 영화 <주토피아>의 기본통계분석

``` r
tidy_zoo <- zootopia %>% group_by(tomatometer, year) %>%
  unnest_tokens(output = word, input = review) %>%
  anti_join(stop_words, by = "word") %>% ungroup(); tidy_zoo
```

    # A tibble: 3,115 x 3
       tomatometer year       word      
       <chr>       <chr>      <chr>     
     1 fresh       after 2016 provoking 
     2 fresh       after 2016 funny     
     3 fresh       after 2016 time      
     4 fresh       after 2016 reveal    
     5 fresh       after 2016 genius    
     6 fresh       after 2016 abundance 
     7 fresh       after 2016 humor     
     8 fresh       after 2016 action    
     9 fresh       after 2016 mask      
    10 fresh       after 2016 underlying
    # ... with 3,105 more rows

``` r
tidy_zoo %>% count(word, sort = T)
```

    # A tibble: 1,515 x 2
       word         n
       <chr>    <int>
     1 zootopia   100
     2 film        57
     3 disney      50
     4 animated    46
     5 movie       33
     6 message     32
     7 kids        25
     8 review      25
     9 world       24
    10 fun         22
    # ... with 1,505 more rows

``` r
freq_zoo <- tidy_zoo %>% count(word, sort = T) %>%
  mutate(percent = n / sum(n) * 100); freq_zoo
```

    # A tibble: 1,515 x 3
       word         n percent
       <chr>    <int>   <dbl>
     1 zootopia   100   3.21 
     2 film        57   1.83 
     3 disney      50   1.61 
     4 animated    46   1.48 
     5 movie       33   1.06 
     6 message     32   1.03 
     7 kids        25   0.803
     8 review      25   0.803
     9 world       24   0.770
    10 fun         22   0.706
    # ... with 1,505 more rows

``` r
freq_zoo %>% filter(percent > 0.3) %>%
  mutate(word = reorder(word, percent)) %>%
  ggplot(aes(word, percent)) + geom_col() + coord_flip()
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
freq_zoo %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Paired")))
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### 영화 <부산행>의 기본통계분석

``` r
tidy_busan <- busantrain %>% group_by(tomatometer, year) %>%
  unnest_tokens(output = word, input = review) %>%
  anti_join(stop_words, by = "word") %>% ungroup(); tidy_busan
```

    # A tibble: 1,355 x 3
       tomatometer year       word         
       <chr>       <chr>      <chr>        
     1 fresh       after 2016 sound        
     2 fresh       after 2016 design       
     3 fresh       after 2016 extra        
     4 fresh       after 2016 level        
     5 fresh       after 2016 predominantly
     6 fresh       after 2016 horror       
     7 fresh       after 2016 concepts     
     8 fresh       after 2016 wisely       
     9 fresh       after 2016 double       
    10 fresh       after 2016 opportunities
    # ... with 1,345 more rows

``` r
tidy_busan %>% count(word, sort = T)
```

    # A tibble: 791 x 2
       word        n
       <chr>   <int>
     1 train      48
     2 busan      36
     3 zombie     33
     4 film       27
     5 yeon       22
     6 review     21
     7 spanish    20
     8 action     16
     9 sang       16
    10 ho         13
    # ... with 781 more rows

``` r
freq_busan <- tidy_busan %>% count(word, sort = T) %>%
  mutate(percent = n / sum(n) * 100); freq_busan
```

    # A tibble: 791 x 3
       word        n percent
       <chr>   <int>   <dbl>
     1 train      48   3.54 
     2 busan      36   2.66 
     3 zombie     33   2.44 
     4 film       27   1.99 
     5 yeon       22   1.62 
     6 review     21   1.55 
     7 spanish    20   1.48 
     8 action     16   1.18 
     9 sang       16   1.18 
    10 ho         13   0.959
    # ... with 781 more rows

``` r
freq_busan %>% filter(percent > 0.3) %>%
  mutate(word = reorder(word, percent)) %>%
  ggplot(aes(word, percent)) + geom_col() + coord_flip()
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
freq_busan %>%
  with(wordcloud(word, n, max.words = 80, colors = brewer.pal(8, "Paired")))
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

1.  위 결과 중 단어빈도 통계 그래프를 보면 각 영화명이 리뷰에 많이 쓰인
    만큼 그래프에서 상위에 위치한 단어들이 영화명에 쓰인 단어들(la,
    land, zootopia, train, busan)임을 볼 수 있다.

2.  또한 영화의 감독명(chazelle, damien, chazelle’s, yeon, sang, ho)와
    출연배우의 이름(gosling, stone)들이 불용어처리가 되지 않아
    단어통계에 포함돼있음을 볼 수 있다.

## 텍스트 심화분석(정서분석/ 토픽모델링 등)

### 토마토미터Tomatometer별 리뷰 분석

리뷰 분석에 토마토미터와 리뷰 작성연도 두가지 지표를 가지고 정서분석을
실시한다. 우선 감성 어휘사전인 Bing, NRC, Affin을 이용하여
토마토미터(fresh/rotten)별 기본 정서분석을 실시한다.

### 영화 <라라랜드>의 토마토미터별 기본정서분석 (Bing, NRC, Affin 순)

``` r
sent_lala <- tidy_lll %>% inner_join(sentiments, by = "word")
sent_lalaland <- sent_lala %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative)
sent_lalaland
```

    # A tibble: 2 x 4
      tomatometer negative positive  sent
      <chr>          <int>    <int> <int>
    1 fresh            230      775   545
    2 rotten            40       39    -1

``` r
sent_ll_nrc <- tidy_lll %>% inner_join(get_sentiments("nrc"), by = "word"); sent_ll_nrc
```

    # A tibble: 4,002 x 4
       tomatometer year       word     sentiment   
       <chr>       <chr>      <chr>    <chr>       
     1 fresh       after 2016 tale     positive    
     2 fresh       after 2016 audience anticipation
     3 rotten      after 2016 team     trust       
     4 rotten      after 2016 sizzle   anger       
     5 rotten      after 2016 crazy    anger       
     6 rotten      after 2016 crazy    fear        
     7 rotten      after 2016 crazy    negative    
     8 rotten      after 2016 crazy    sadness     
     9 rotten      after 2016 stupid   negative    
    10 rotten      after 2016 love     joy         
    # ... with 3,992 more rows

``` r
sent_lala_nrc <- sent_ll_nrc %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% mutate(sent = positive - negative)
sent_lala_nrc
```

    # A tibble: 2 x 12
      tomatometer anger anticipation disgust  fear   joy negative positive sadness
      <chr>       <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 fresh         203          441      56   125   594      230     1043     260
    2 rotten         17           36       6    11    44       21       96      22
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_lala_afinn <- tidy_lll %>%
inner_join(get_sentiments("afinn"), by = "word") %>%
  count(tomatometer, word, wt = value, sort = T)
sent_lala_afinn
```

    # A tibble: 354 x 3
       tomatometer word          n
       <chr>       <chr>     <dbl>
     1 fresh       love        192
     2 fresh       romance      38
     3 fresh       beautiful    36
     4 fresh       fun          36
     5 fresh       perfect      33
     6 fresh       wonderful    32
     7 fresh       joy          27
     8 fresh       joyous       24
     9 fresh       vibrant      24
    10 fresh       charm        21
    # ... with 344 more rows

sent\_lalaland를 살펴보면 Bing 사전을 이용하여 정서분석한 총 단어의
개수는 1,084개이다. ‘fresh’ 리뷰는 긍정 단어가 부정 단어보다 545개 많고,
‘rotten’ 리뷰는 부정 단어가 긍정 단어보다 1개 많다.

sent\_lala\_nrc를 살펴보면 NRC 사전을 이용하여 정서 분석한 총 단어의
개수는 4,890개이며, 단어를 총 11개로 분류하여 나타냈다. 긍정과 부정
단어를 살펴보면 ‘fresh’ 리뷰는 긍 정단어가 부정 단어보다 813개 많고,
‘rotten’ 리뷰도 긍정단어가 부정단어보다 75개 많다.

sent\_lala\_affin를 살펴보면 Affin 사전을 이용하여 정서 분석한 총 단어의
개수는 354개이다. 영화 <라라랜드>의 장르가 로맨스, 멜로드라마인 만큼
’love’가 가장 많이 언급되었음을 알 수 있다.

### 영화 <주토피아>의 토마토미터별 기본정서분석 (Bing, NRC, Affin 순)

``` r
sent_zoo <- tidy_zoo %>% inner_join(sentiments, by = "word")
sent_zootopia <- sent_zoo %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% mutate(sent = positive - negative)
sent_zootopia
```

    # A tibble: 2 x 4
      tomatometer negative positive  sent
      <chr>          <int>    <int> <int>
    1 fresh            193      483   290
    2 rotten             7        5    -2

``` r
sent_zoo_nrc <- tidy_zoo %>% inner_join(get_sentiments("nrc"), by = "word")
sent_zooto_nrc <- sent_zoo_nrc %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% mutate(sent = positive - negative)
sent_zooto_nrc
```

    # A tibble: 2 x 12
      tomatometer anger anticipation disgust  fear   joy negative positive sadness
      <chr>       <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 fresh          68          193      38    81   260      126      547      41
    2 rotten          1            5       3     3     4        3        6       2
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_zooto_afinn <- tidy_zoo %>%
inner_join(get_sentiments("afinn"), by = "word") %>%
  count(tomatometer, word, wt = value, sort = T)
sent_zooto_afinn
```

    # A tibble: 203 x 3
       tomatometer word             n
       <chr>       <chr>        <dbl>
     1 fresh       fun             84
     2 fresh       funny           80
     3 fresh       clever          32
     4 fresh       entertaining    26
     5 fresh       charming        21
     6 fresh       perfect         18
     7 fresh       wonderful       16
     8 fresh       humor           14
     9 fresh       comedy          13
    10 fresh       adorable        12
    # ... with 193 more rows

sent\_zootopia를 살펴보면 Bing 사전을 이용하여 정서분석한 총 단어의
개수는 688개이다. ‘fresh’ 리뷰는 긍정 단어가 부정 단어보다 290개 많고,
‘rotten’ 리뷰는 부정 단어가 긍정 단어보다 2개 많다.

sent\_zooto\_nrc를 살펴보면 NRC 사전을 이용하여 정서분석한 총 단어의
개수는 2,125개이며, 단어를 총 11개로 분류하여 나타냈다. 긍정과 부정
단어를 살펴보면 ’fresh’리뷰는 긍정 단어가 부정 단어보다 421개 많고,
’rotten’리뷰도 긍정단어가 부정단어보다 3개 많다.

sent\_zooto\_affin를 살펴보면 Affin 사전을 이용하여 정서 분석한 총
단어의 개수는 203개이다. 영화 <주토피아>의 장르가 애니메이션, 코미디인
만큼 ’fun’이 가장 많이 언급되었음을 알 수 있다.

### 영화 <부산행>의 토마토미터별 기본정서분석 (Bing, NRC, Affin 순)

``` r
sent_busan <- tidy_busan %>% inner_join(sentiments, by = "word")
sent_busantrain <- sent_busan %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% mutate(sent = positive - negative)
sent_busantrain
```

    # A tibble: 2 x 4
      tomatometer negative positive  sent
      <chr>          <int>    <int> <int>
    1 fresh            148      119   -29
    2 rotten             7        3    -4

``` r
sent_bu_nrc <- tidy_busan %>% inner_join(get_sentiments("nrc"), by = "word")
sent_busan_nrc <- sent_bu_nrc %>% count(tomatometer, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% mutate(sent = positive - negative)
sent_busan_nrc
```

    # A tibble: 2 x 12
      tomatometer anger anticipation disgust  fear   joy negative positive sadness
      <chr>       <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 fresh          72           60      59    81    56      116      164      61
    2 rotten          1            1       0     1     4        2        8       0
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_busan_afinn <- tidy_busan %>%
inner_join(get_sentiments("afinn"), by = "word") %>%
  count(tomatometer, word, wt = value, sort = T)
sent_busan_afinn
```

    # A tibble: 104 x 3
       tomatometer word             n
       <chr>       <chr>        <dbl>
     1 fresh       fun             20
     2 fresh       entertaining    12
     3 fresh       exciting        12
     4 fresh       fantastic       12
     5 fresh       funny            8
     6 fresh       blockbuster      6
     7 fresh       effective        6
     8 fresh       top              6
     9 fresh       care             4
    10 fresh       celebrating      3
    # ... with 94 more rows

sent\_busantrain를 살펴보면 Bing 사전을 이용하여 정서 분석한 총 단어의
개수는 277개이다. ‘fresh’ 리뷰는 부정 단어가 긍정 단어보다 29개 많고,
‘rotten’ 리뷰는 부정 단어가 긍정단어보다 4개 많다. 앞서 살펴 본 영화와는
달리 ‘fresh’와 ’rotten’ 리뷰 모두 부정 단어가 많은데, 그 이유는 부산행의
장르가 공포 영화, 좀비, 재난 영화이기 때문이다.

sent\_busan\_nrc를 살펴보면 NRC 사전을 이용하여 정서분석한 총 단어의
개수는 871개이며, 단어를 총 11개로 분류하여 나타냈다. 긍정과 부정단어를
살펴보면 ‘fresh’ 리뷰는 긍정 단어가 부정 단어보다 48개많고, ‘rotten’
리뷰도 긍정단어가 부정단어보다 6개 많다.

sent\_busan\_affin를 살펴보면 Affin 사전을 이용하여 정서분석한 총 단어의
개수는 104개이다. 하지만, 영화 <부산행>의 장르는 공포 영화, 좀비, 재난
영화인데 ’fun’이 가장 많이 언급되었음을 알 수 있다.

### 연도별 리뷰 정서분석

분석에 이용하는 리뷰들은 2016년 개봉 당시부터 현재 2021년까지의 리뷰가
포함되어있다.영화 리뷰별로 3가지 정서사전(Bing, NRC, Affin)을 이용하여
정서분석을 실시한 후 2016년에 작성된 리뷰와 그 이후 연도 리뷰의 정서
차이를 분석해본다.

### 영화 <라라랜드>의 연도별 리뷰 정서분석 (Bing, NRC, Affin 순)

``` r
sent_lll_bing <- tidy_lll %>% inner_join(sentiments, by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_lll_bing #Bing 사전을 이용한 정서분석
```

    # A tibble: 2 x 4
      year       negative positive  sent
      <chr>         <int>    <int> <int>
    1 after 2016      131      393   262
    2 in 2016         139      421   282

``` r
sent_lll_nrc <- tidy_lll %>% inner_join(get_sentiments("nrc"), by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_lll_nrc #NRC 사전을 이용한 정서분석
```

    # A tibble: 2 x 12
      year       anger anticipation disgust  fear   joy negative positive sadness
      <chr>      <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 after 2016    99          218      29    71   307      123      558     133
    2 in 2016      121          259      33    65   331      128      581     149
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_lll_afinn <- tidy_lll %>% inner_join(get_sentiments("afinn"), by= "word") %>%
  count(year, wt = value); sent_lll_afinn #Afinn 사전을 이용한 정서분석
```

    # A tibble: 2 x 2
      year           n
      <chr>      <dbl>
    1 after 2016   570
    2 in 2016      554

위에 정서분석한 내용 중 긍정/부정(positive/negative)만을 가지고 시각화
처리하였다.
![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

우선 위 그래프에서 눈에 띄는 점은 2016년 개봉연도에 작성된 리뷰와 그
이후에 작성된 리뷰의 정서점수 차이가 그렇게 크지 않다는 점이다. Bing을
이용한 분석에서는 2016년 긍정 단어는 421개, 부정 단어는 139개로 긍부정
점수는 282이다. 2016년 이후의 긍정 단어는 393개, 부정 단어는 131개로
262이다. NRC을 이용한 분석에서는 2016년 긍정 단어 581개, 부정 단어
128개로 긍부정 점수가 453이고, 2016년 이후의 긍정 단어 558개, 부정 단어
123개로 긍부정 점수가 435이다. Afinn을 이용한 분석에서는 2016년 긍부정
점수가 554, 2016년 이후 긍부정점수가 570으로 앞에 두 분석과 다르게
2016년 이후의 점수가 더 높게 나온 것을 알 수 있다.

### 영화 <주토피아>의 연도별 리뷰 정서분석 (Bing, NRC, Affin 순)

``` r
sent_zoo_bing <- tidy_zoo %>% inner_join(sentiments, by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_zoo_bing #Bing 사전을 이용한 정서분석
```

    # A tibble: 2 x 4
      year       negative positive  sent
      <chr>         <int>    <int> <int>
    1 after 2016       28       89    61
    2 in 2016         172      399   227

``` r
sent_zoo_nrc <- tidy_zoo %>% inner_join(get_sentiments("nrc"), by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_zoo_nrc #NRC 사전을 이용한 정서분석
```

    # A tibble: 2 x 12
      year       anger anticipation disgust  fear   joy negative positive sadness
      <chr>      <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 after 2016    10           36       7    18    44       17       89       7
    2 in 2016       59          162      34    66   220      112      464      36
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_zoo_afinn <- tidy_zoo %>% inner_join(get_sentiments("afinn"), by= "word") %>%
  count(year, wt = value); sent_zoo_afinn #Afinn 사전을 이용한 정서분석
```

    # A tibble: 2 x 2
      year           n
      <chr>      <dbl>
    1 after 2016   120
    2 in 2016      540

위에 정서분석한 내용 중 긍정/부정(positive/negative)만을 가지고 시각화
처리하였다.
![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

영화 <주토피아>는 개봉월이 3월이었던 점으로 보아 작성된 리뷰가 대부분
개봉년도인 2016년에 작성되어서 연도별 비교가 큰 의미를 가질 것 같지는
않지만 비교해보자면 우선 연도별로 긍부정 점수 그래프들은 서로 꽤 비슷한
모양을 가지고 있다. Bing을 이용한 분석에서는 2016년 긍정 단어는 399개,
부정 단어는 172개로 긍부정 점수는 227이다. 2016년 이후의 긍정 단어는
89개, 부정 단어는 28개로 61이다. NRC을 이용한 분석에서는 2016년 긍정
단어 464개, 부정 단어 112개로 긍부정 점수가 352이고, 2016년 이후의 긍정
단어 89개, 부정 단어 17개로 긍부정 점수가 72이다. Afinn을 이용한
분석에서는 2016년 긍부정 점수가 540, 2016년 이후 긍부정점수가 120으로
확인할 수 있다.

### 영화 <부산행>의 연도별 리뷰 정서분석(Bing, NRC, Affin 순)

``` r
sent_busan_bing <- tidy_busan %>% inner_join(sentiments, by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_busan_bing #Bing 사전을 이용한 정서분석
```

    # A tibble: 2 x 4
      year       negative positive  sent
      <chr>         <int>    <int> <int>
    1 after 2016       65       62    -3
    2 in 2016          90       60   -30

``` r
sent_busan_nrc <- tidy_busan %>% inner_join(get_sentiments("nrc"), by= "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sent = positive - negative); sent_busan_nrc #NRC 사전을 이용한 정서분석
```

    # A tibble: 2 x 12
      year       anger anticipation disgust  fear   joy negative positive sadness
      <chr>      <int>        <int>   <int> <int> <int>    <int>    <int>   <int>
    1 after 2016    31           28      29    36    28       52       79      26
    2 in 2016       42           33      30    46    32       66       93      35
    # ... with 3 more variables: surprise <int>, trust <int>, sent <int>

``` r
sent_busan_afinn <- tidy_busan %>% inner_join(get_sentiments("afinn"), by= "word") %>%
  count(year, wt = value); sent_busan_afinn #Afinn 사전을 이용한 정서분석
```

    # A tibble: 2 x 2
      year           n
      <chr>      <dbl>
    1 after 2016     2
    2 in 2016       -2

위에 정서분석한 내용 중 긍정/부정(positive/negative)만을 가지고 시각화
처리하였다.

``` r
busan_bing <- tidy_busan %>% inner_join(sentiments, by= "word") %>%
  count(year, sentiment)
busan_nrc <- tidy_busan %>% inner_join(get_sentiments("nrc"), by= "word") %>%
  count(year, sentiment)
busan_afinn <- tidy_busan %>% inner_join(get_sentiments("afinn"), by= "word") %>%
  count(year, word, wt = value)
plot_busan_bing<- ggplot(busan_bing, aes(year, n, fill = sentiment)) + 
  geom_col(position = "dodge") + labs(fill="sentiment", title = "Bing 사전을 이용한 정서분석")+ theme_light()
plot_busan_nrc <- ggplot(subset(busan_nrc, sentiment== c("positive", "negative")), aes(year, n, fill = sentiment)) + geom_col(position = "dodge") + 
  labs(fill="sentiment", title = "NRC 사전을 이용한 정서분석") + theme_light()
plot_busan_afinn <- ggplot(busan_afinn, aes(year, n, fill=ifelse(n >0, "positive", "negative"))) + 
  geom_col(position = "dodge") + labs(fill="sentiment", title = "Afinn 사전을 이용한 정서분석") + theme_light()

grid.arrange(plot_busan_bing, plot_busan_nrc, plot_busan_afinn)
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

앞서 토마토미터별 분석에서도 알 수 있듯이 영화 <부산행>은 장르 특성상
리뷰에 부정 단어가 많이 쓰여 긍부정 점수 산정에 있어 다른 영화들과 다른
점수 분포를 보일듯 한다. Bing을 이용한 분석에서는 2016년 긍정 단어는
60개, 부정 단어는 90개로 긍부정 점수는 -30이다. 2016년 이후의 긍정
단어는 62개, 부정 단어는 65개로 -3이다. NRC을 이용한 분석에서는 2016년
긍정 단어 93개, 부정 단어 66개로 긍부정 점수가 27이고, 2016년 이후의
긍정 단어 79개, 부정 단어 52개로 긍부정 점수가 동일하게 27이다. Afinn을
이용한 분석에서는 2016년 긍부정 점수가 -2, 2016년 이후 긍부정점수가
2으로 알 수 있다.

### 영화별 리뷰에 사용된 긍/부정 단어의 상대빈도 (Bing 사전)

``` r
# 라라랜드
lll_word_per_ty <- tidy_lll %>% count(tomatometer, year)
lll_ratio <- sent_lalaland %>% left_join(lll_word_per_ty) %>% 
  mutate(neg_ratio = negative/n, pos_ratio = positive/n, sent_ratio = (positive - negative) / n); lll_ratio
```

    # A tibble: 4 x 9
      tomatometer negative positive  sent year      n neg_ratio pos_ratio sent_ratio
      <chr>          <int>    <int> <int> <chr> <int>     <dbl>     <dbl>      <dbl>
    1 fresh            230      775   545 afte~  2139    0.108      0.362    0.255  
    2 fresh            230      775   545 in 2~  2598    0.0885     0.298    0.210  
    3 rotten            40       39    -1 afte~   301    0.133      0.130   -0.00332
    4 rotten            40       39    -1 in 2~   133    0.301      0.293   -0.00752

``` r
# 주토피아
zoo_word_per_ty <- tidy_zoo %>% count(tomatometer, year)
zoo_ratio <- sent_zootopia %>% left_join(zoo_word_per_ty) %>% 
  mutate(neg_ratio = negative/n, pos_ratio = positive/n, sent_ratio = (positive - negative) / n); zoo_ratio
```

    # A tibble: 4 x 9
      tomatometer negative positive  sent year      n neg_ratio pos_ratio sent_ratio
      <chr>          <int>    <int> <int> <chr> <int>     <dbl>     <dbl>      <dbl>
    1 fresh            193      483   290 afte~   482    0.400     1.00       0.602 
    2 fresh            193      483   290 in 2~  2570    0.0751    0.188      0.113 
    3 rotten             7        5    -2 afte~     4    1.75      1.25      -0.5   
    4 rotten             7        5    -2 in 2~    59    0.119     0.0847    -0.0339

``` r
# 부산행
busan_word_per_ty <- tidy_busan %>% count(tomatometer, year)
busan_ratio <- sent_busantrain %>% left_join(busan_word_per_ty) %>% 
  mutate(neg_ratio = negative/n, pos_ratio = positive/n, sent_ratio = (positive - negative) / n); busan_ratio
```

    # A tibble: 4 x 9
      tomatometer negative positive  sent year      n neg_ratio pos_ratio sent_ratio
      <chr>          <int>    <int> <int> <chr> <int>     <dbl>     <dbl>      <dbl>
    1 fresh            148      119   -29 afte~   577     0.256    0.206     -0.0503
    2 fresh            148      119   -29 in 2~   706     0.210    0.169     -0.0411
    3 rotten             7        3    -4 afte~    10     0.7      0.3       -0.4   
    4 rotten             7        3    -4 in 2~    62     0.113    0.0484    -0.0645

### 긍/부정 단어 간 독립성 검정 (Bing 사전)

``` r
# 라라랜드
lllsent <- xtabs(~ sentiment, sent_lala)
r_lllsent <- chisq.test(lllsent); r_lllsent
```


        Chi-squared test for given probabilities

    data:  lllsent
    X-squared = 273, df = 1, p-value < 2.2e-16

``` r
r_lllsent$stdres
```

    sentiment
     negative  positive 
    -16.52282  16.52282 

``` r
# 주토피아
zoosent <- xtabs(~ sentiment, sent_zoo)
r_zoosent <- chisq.test(zoosent); r_zoosent
```


        Chi-squared test for given probabilities

    data:  zoosent
    X-squared = 120.56, df = 1, p-value < 2.2e-16

``` r
r_zoosent$stdres
```

    sentiment
    negative positive 
    -10.9799  10.9799 

``` r
# 부산행
busansent <- xtabs(~ sentiment, sent_busan)
r_busansent <- chisq.test(busansent); r_busansent
```


        Chi-squared test for given probabilities

    data:  busansent
    X-squared = 3.9314, df = 1, p-value = 0.04739

``` r
r_busansent$stdres
```

    sentiment
     negative  positive 
     1.982778 -1.982778 

### 가장 빈번한 긍/부정 단어 파악 (Bing 사전)

``` r
# <라라랜드>
sent_lala %>% count(word, sentiment, sort=T)
```

    # A tibble: 519 x 3
       word      sentiment     n
       <chr>     <chr>     <int>
     1 love      positive     68
     2 classic   positive     21
     3 magic     positive     18
     4 modern    positive     18
     5 romantic  positive     18
     6 magical   positive     14
     7 beautiful positive     12
     8 perfect   positive     11
     9 gorgeous  positive     10
    10 homage    positive     10
    # ... with 509 more rows

``` r
sent_lala %>% count(word, sentiment, sort=T) %>%
  filter(n > 3) %>% mutate(word = reorder(word, n)) %>%
  ggplot() + geom_col(aes(word, n)) + coord_flip() +
  facet_wrap(~ sentiment, scale = "free_y")
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# <주토피아>
sent_zoo %>% count(word, sentiment, sort=T)
```

    # A tibble: 349 x 3
       word         sentiment     n
       <chr>        <chr>     <int>
     1 fun          positive     22
     2 funny        negative     20
     3 clever       positive     16
     4 classic      positive     14
     5 entertaining positive     13
     6 prejudice    negative     11
     7 issues       negative     10
     8 timely       positive     10
     9 smart        positive      9
    10 charming     positive      7
    # ... with 339 more rows

``` r
sent_zoo %>% count(word, sentiment, sort=T) %>%
  filter(n > 3) %>% mutate(word = reorder(word, n)) %>%
  ggplot() + geom_col(aes(word, n)) + coord_flip() +
  facet_wrap(~ sentiment, scale = "free_y")
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
# <부산행>
sent_busan %>% count(word, sentiment, sort=T)
```

    # A tibble: 188 x 3
       word         sentiment     n
       <chr>        <chr>     <int>
     1 zombie       negative     33
     2 entertaining positive      6
     3 bloody       negative      5
     4 fun          positive      5
     5 exciting     positive      4
     6 tension      negative      4
     7 apocalypse   negative      3
     8 dead         negative      3
     9 effective    positive      3
    10 fans         positive      3
    # ... with 178 more rows

``` r
sent_busan %>% count(word, sentiment, sort=T) %>%
  filter(n > 2) %>% mutate(word = reorder(word, n)) %>%
  ggplot() + geom_col(aes(word, n)) + coord_flip() +
  facet_wrap(~ sentiment, scale = "free_y")
```

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

### 워드클라우드로 긍/부정 단어 파악

![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->![](%5B최종%5D경영데이터분석기말프로젝트_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

## 시사점

이 분석을 실시하기 전 우리는 당연히 ’평점이 좋은 영화들이니 리뷰
정서분석 또한 긍정적으로 나올 것이다’라고 생각하였다. 또한 같은
영화더라도 영화가 리뷰된 시기에 따라 리뷰의 정서가 달라질 수 있을 것이라
예상하였다. 하지만 정서분석 결과와 우리의 예상은 매우 달랐고, 우리는 이
분석을 통해 크게 3가지를 깨달을 수 있었다.

1.  좋은 평가를 받은 영화임에도 불구하고 영화 장르에 따라 정서 분석이
    완전히 부정적인 방향으로 진행될 수 있다.
2.  좋은 평가의 리뷰가 많다고 그에 따른 긍정 단어가 많은 것이 아니다.
3.  영화 리뷰 작성 시기에 따른 정서 차이는 크게 발생하지 않는다.

그리고 분석을 실시하면서 느낀 고충도 있었다. 바로 감성 어휘 사전에 따라
분석의 무게가 달라진다는 점이다 감성 어휘 사전을 살펴보면 Bing 사전은
Affin과 NRC 사전에 비해 단어 수가 너무 적다. 하지만 Affin과 NRC는 Bing에
비해 단어 수가 너무 방대하다. 실제로도 5천개 이상의 불용어 처리를 완료한
분석 가능한 단어가 있음에도 불구하고 Bing 사전을 이용해 정서분석을
실시하면 분석에 이용된 단어는 100여개 밖에 없는 것을 확인할 수 있었고,
이는 우리가 알고 싶어한 범위보다 더 적은 범위만 알게된 느낌이라 매우
당황스러웠었다.

또한 우리가 수집한 텍스트 데이터가 짧은 문장으로 이루어져있어 TF-IDF와
토픽 모델링를 적용하는 것이 쉽지 않아 감성 어휘 사전으로만 텍스트 분석을
실시했다는 점이 조금은 아쉬웠다. 텍스트 분석에 있어서 어느 정도 문장이
긴 텍스트를 가지고 분석을 진행해야한다는 것을 깨달았다.
