# ---- Project Genshin 圣遗物狗粮路线效率优化---- #
#                              Code by Yifan.LIU  #

# Env. Settings----
library(tidyverse)
library(readxl)
library(writexl)
library(gurobi)

simpledate <- \() format(Sys.Date(),"%y%m%d")

hex_encode <- \(x) x %>% str_replace_all("1[0-5]",\(num) as.integer(num) %>% as.hexmode)

hex_decode <- \(x) x %>% str_replace_all('[a-f]',\(hex) as.hexmode(hex) %>% as.integer)

Route_simplify <- \(Route) Route %>% strsplit("—") %>% map_chr( 
  ~ str_match(.x, '([\u4e00-\u9fa5]+[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
    mutate(V4 = if_else(V2 == lag(V2, 1) %>% replace_na(""), "", V2)) %>% 
    str_glue_data("{V4}{V3}") %>% str_c(collapse = "—"))

# Data load----

ToT <- 5 # Time(s) of Teleport

Route_design <- read_xlsx('./1.Data/路线设计.xlsx',range = cell_cols("A:B")) %>% 
  rename(time = `耗时(s)`) %>% mutate(`路线` = `路线` %>% hex_encode)

PointData <- read_xlsx('./1.Data/点位描述.xlsx', range = cell_cols("A:J")) %>%
  rename(`点位` = `编号`) %>%
  filter(`刷新周期` != '刷新周期') %>% 
  mutate(`点位` = `点位` %>% str_remove_all("（.+）") %>% hex_encode) %>% 
  mutate(`刷新周期` = if_else(`刷新周期` == "每日4点", "0", `刷新周期`)) %>% 
  mutate(across(c(`数量`, `刷新周期`,`掉落期望`,`无摩拉`, `有摩拉`), as.numeric))

RouteData <- Route_design %>% mutate(
  `路线` %>% strsplit("—") %>% map_dfr(
    ~ data.frame(`点位` = .x[-1]) %>% left_join(PointData, by = "点位") %>%
      mutate(mora_drop = `有摩拉` * 150) %>%
      summarise(
        `点数` = sum(`数量`),
        `期望摩拉` = sum(mora_drop),
        `期望经验` = sum(`数量` * `掉落期望` * 420)
      )),
  timeTP = time + ToT,
  `狗粮效率(经验/s)` = `期望经验` / timeTP,
  `综合资源掉落` = (期望摩拉 + `期望经验`),
  `综合资源效率` = `综合资源掉落`/timeTP
)


PointData %>% select(`点位`,`数量`,`刷新周期`,`掉落期望`) %>% 
   write_xlsx('./2.Result/点位经验.xlsx')

RouteData %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
  write_xlsx('./2.Result/路线数据.xlsx')

# N+1 Gurobi ----

gurobi_solve_N1 <- function(n_point = 124, USE.MOUNT = T, must_P, dup_P, write_route = F) {
  
  cons.mx <- c(must_P,'锚点',dup_P) %>% map_dfr(
    ~ str_detect(RouteData$`路线`,.x) %>% set_names(1:nrow(RouteData))
  ) %>% as.matrix
  
  modelN <- list(
    A = rbind(RouteData$点数, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteData$timeTP, weight = 1, priority = 10),
      `2` = list(objn = RouteData$`期望经验`,weight = -1)),
    rhs = c(n_point, str_count(must_P, "\\|") + 1, USE.MOUNT, rep(1, nrow(cons.mx) - 2)),
    sense = c('=', '=', '=', rep('<=',nrow(cons.mx) - 2)),
    vtype  = 'B'
  )
  
  solveN <- gurobi(modelN)
  
  RouteN <- RouteData[as.logical(solveN$x),] 
  
  RouteSupp <- RouteN %>% pull(`路线`) %>% strsplit('—') %>% 
    map_dfr(~ data.frame(`点位` = .x[-1])) %>% left_join(PointData, by = '点位') %>% 
    filter(`刷新周期` == 24) %>% pull(`点位`) %>% str_c('—', ., collapse = "|") %>% 
    (\(x) RouteData %>% filter(str_detect(`路线`,str_c(x,'|锚点'),negate = T)))
  
  cons.mx <- RouteSupp %>% pull(`路线`) %>% strsplit("—") %>% 
    map(~.x[-1]) %>% unlist %>% unique %>% str_c("—",.) %>% 
    map(~ str_detect(RouteSupp$`路线`,.x)) %>% do.call(rbind,.)
  
  modelSupp <- list(
    A = rbind(RouteSupp$`点数`,cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteSupp$timeTP,weight = 1,priority = 100),
      `2` = list(objn = RouteSupp$`期望经验`,weight = -1)),
    rhs = c(99 + max(RouteSupp$`点数`),rep(1,nrow(cons.mx))),
    sense = c('=', rep('<=',nrow(cons.mx))),
    vtype  = 'B'
  )
  
  solveSupp <- gurobi(modelSupp)
  
  Route1 <- RouteSupp[as.logical(solveSupp$x), ]
  
  cat(str_glue("N路线耗时 {RouteN %>% summarise(time = sum(timeTP)/60) %>% round(2)}min\n\n"))
  cat(str_glue("1路线耗时 {Route1 %>% summarise(time = sum(timeTP)/60) %>% round(2)}min"))
  
  ls <- list(
    the_N = RouteN %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify),
    the_1 = Route1 %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) 
  )
  
  if (write_route) {
    write_xlsx(ls, str_glue('./2.Result/Gurobi_Route_Nplus1_ver124p_{simpledate()}.xlsx'))
  }
  
  return(ls)
  
}

# N+1 124 ver----
# 这一步需要手动筛选重复的点位，并精简重复点

RouteData$`路线` %>% enframe %>% 
  mutate(R_point = strsplit(value,'—') %>% map(~ .x[-1])) %>% 
  unnest(R_point) %>% pull(R_point) %>% (\(x) x[duplicated(x)]) %>% unique

duplicate.points <- c(
  '嘉铭1', '盗宝1', '矿架2', '吊机1', '观测1', '愚人1', '石厅路1', '奥藏山3',
  '华光林1', '绝云间2', '碧水原2', '碧水原4', '客栈2', '石像1', '藏宝洞1',
  '水帘洞1', '笈名1', '知比1', '清籁丸1', '营地2', '木船1', '火1', '狸猫5',
  '炮1', '雷1', '熔炉1', '炉心1', '镀金C', '兰房2', '镀金H1', '据点A1',
  '据点B1', '蘑菇顶1', '盗宝5', '镀金J', '瞭望3', '维摩1'
)

Route124 <-
  gurobi_solve_N1(
    write_route = T,
    USE.MOUNT = T,
    n_point = 124,
    must_P = "—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$",
    dup_P = duplicate.points
  )

# N+1 119 ver ----

Route119 <-
  gurobi_solve_N1(
    write_route = T,
    USE.MOUNT = F,
    n_point = 119,
    must_P = "清籁丸1",
    dup_P = duplicate.points
  )

# AB Gurobi  ----

gurobi_solve_AB <- function(n_point, custome_route = "清籁丸1", write_route = F) {
    
  FastFresh <- PointData %>% filter(`刷新周期` != 24) %>% pull(`点位`)
  
  RouteAB <- bind_rows(
    RouteData,
    RouteData[RouteData %>% pull(`路线`) %>% strsplit("—") %>% 
                map_dbl(~ prod(.x[-1] %in% FastFresh)) %>% as.logical, ]
  )
  
  cons.mx <- PointData %>% pull(`点位`) %>% str_c('—',.) %>% c(custome_route, '锚点', .) %>% 
    set_names %>% map(~ str_detect(RouteAB$路线, .x)) %>% do.call(rbind, .)
  
  cons.rhs <- PointData %>% mutate(rhs = if_else(`刷新周期` == 24, 1, 2)) %>%
    pull(rhs) %>% c(str_count(custome_route, "\\|") + 1, 0, .)
  
  modelAB <- list(
    A = rbind(RouteAB$点数, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteAB$timeTP,weight = 1,priority = 10),
      `2` = list(objn = RouteAB$期望经验, weight = -1)
    ),
    rhs = c(n_point, cons.rhs),
    sense = c('=', "=", rep('<=', length(cons.rhs) - 1)), 
    vtype  = 'B'
  )
  
  solveAB <- gurobi(modelAB)
  
  df <- RouteAB[solveAB$x %>% as.logical, ] 
  
  if (write_route) {
    df %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
      write_xlsx(str_glue('./2.Result/Gurobi_Route_AandB_ver{n_point}p_{simpledate()}.xlsx'))
  }
  
  return(df)
  
}

# 120 AB----
gurobi_solve_AB(
  n_point = 124 + 119,
  custome_route = "—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$",
  write_route = T
)

# 100 AB ----

gurobi_solve_AB(112 + 112, write_route = T)

