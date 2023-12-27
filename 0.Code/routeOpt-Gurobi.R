# ---- Project Genshin 圣遗物狗粮路线效率优化---- #
#                              Code by Yifan.LIU  #

# Env. Settings----
library(tidyverse)
library(LYFtools)
library(readxl)
library(writexl)
library(gurobi)

hex_encode <- \(x) x %>% str_replace_all("1[0-5]",\(num) as.integer(num) %>% as.hexmode)

hex_decode <- \(x) x %>% str_replace_all('[a-f]',\(hex) as.hexmode(hex) %>% as.integer)

Route_simplify <- \(Route) Route %>% strsplit("—") %>% map_chr(~{
  .x %>% str_match('([\u4e00-\u9fa5]+[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
    mutate(V4 = if_else(V2 == lag(V2, 1) %>% replace_na(""), "", V2)) %>% 
    str_glue_data("{V4}{V3}") %>% str_c(collapse = "—")
})

Route_restitute <- \(Route) Route %>% strsplit("—") %>% map_chr(~{
  .x %>% str_match('([\u4e00-\u9fa5]*[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
    mutate(V2 = na_if(V2, "")) %>% fill(V2) %>% 
    str_glue_data("{V2}{V3}") %>% str_c(collapse = "—")
})

# Data load----

PointData <- read_xlsx('./1.Data/点位描述.xlsx', range = cell_cols("A:J")) %>%
  rename(`点位` = `编号`) %>% filter(`刷新周期` != '刷新周期') %>% 
  mutate(`点位` = `点位` %>% str_remove_all("（.+）") %>% hex_encode) %>% 
  mutate(`刷新周期` = if_else(`刷新周期` == "每日4点", "0", `刷新周期`)) %>% 
  mutate(across(c(`数量`, `刷新周期`,`掉落期望`,`无摩拉`, `有摩拉`), as.numeric))


# TOT = Time of Teleport
getRouteData <- function(hardcore = T, TOT = 6) {
  
  Route_design <- './1.Data/路线设计.xlsx' %>% 
    read_xlsx(range = cell_cols("A:D")) %>% 
    mutate(rand = runif(nrow(.), 0, .1)) %>% 
    mutate(time = `基础耗时(s)` + rand,
           `路线` = hex_encode(`路线`))
  
  Route_design <- if (hardcore) {
    Route_design %>% 
      mutate(
        timeReal = time + TOT + replace_na(`地图惩罚`, 0), 
      ) %>% select(`路线`, time, timeReal)
  } else {
    Route_design %>% mutate(
      timeReal = time + TOT + replace_na(`地图惩罚`, 0) +
        replace_na(`舒适度惩罚`, 0) * 3, 
      ) %>% select(`路线`, time, timeReal)
  }
  
  Route_design %>% 
    mutate(points = `路线`, `路线` = as_factor(`路线`)) %>% 
    separate_rows(points, sep = "—") %>% 
    left_join(PointData %>% rename(points = `点位`)) %>% 
    mutate(mora_drop = `有摩拉` * 200) %>% 
    group_by(`路线`) %>% 
    summarise(
      `time` = mean(time, na.rm = T),
      `timeReal` = mean(timeReal, na.rm = T),
      `点数` = sum(`数量`, na.rm = T), 
      `期望摩拉` = sum(mora_drop, na.rm = T),
      `期望经验` = sum(`数量` * `掉落期望` * 420, na.rm = T)
    ) %>% 
    mutate(
      `路线` = as.character(`路线`), 
      `狗粮效率(经验/s)` = `期望经验` / (time + TOT),
      `综合资源掉落` = (期望摩拉 + `期望经验`),
      `综合资源效率` = `综合资源掉落` / (time + TOT)
    ) %>% 
    return()
  
}

getRouteData(hardcore = T) %>% left_join(
  read_xlsx('./1.Data/路线设计.xlsx', range = cell_cols("A:D")) %>% 
    mutate(舒适度惩罚 = 舒适度惩罚 * 3) %>% select(-`基础耗时(s)`)
) %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
  write_xlsx('./2.Result/路线数据.xlsx')

# Detect Duplicated Routes ----

getRouteData() %>% filter(`狗粮效率(经验/s)` > 40) %>% 
  pull(`路线`) %>% enframe %>% separate_rows(value, sep = '—') %>% 
  filter(!str_detect(value, "传送点|神像|副本|锚点")) %>% add_count(value) %>% 
  filter(n > 1, !duplicated(value)) %>% pull(value) %>% str_c(collapse = "|")

duplicate.points <- str_glue("嘉铭1|观测1|\\
  奥藏山3|绝云间2|碧水原4|客栈2|荻花州1|\\
  火1|炮1|雷1|熔炉1|高台1|炉心6|\\
  石像1|石像5|藏宝洞1|水帘洞1|\\
  清籁丸1|营地2|箱子2|夜雾1|兰房2|\\
  镀金A1|镀金C1|镀金D1|据点A1|镀金H1|\\
  据点B4|蘑菇顶1|阿如仓库1|阿如西1|\\
  赤王1|祝祭3|鳄王窟3|圣显1|巨人3|\\
  居尔中3|居尔东1|居尔西3|神棋2|\\
  精石1|会堂1|梅舍1|梅管1|艾伊古3|\\
  科院北1|北水文2|工坊中6|科院南1|\\
  佩妮北1|记忆1|灵魂1|人格1") %>% str_split("\\|") %>% unlist

# N+1 ----

gurobi_solve_N1 <- function(
    n,
    use.mount,
    hard = T,
    ban_P = "",
    pick_P1 = "",
    pick_P2 = "",
    dup_P,
    write_route
) {

  PreRoute <- if (nchar(ban_P) == 0) {
    getRouteData(hardcore = hard) %>%  filter(`狗粮效率(经验/s)` > 40)
  } else {
    getRouteData(hardcore = hard) %>% 
      filter(`狗粮效率(经验/s)` > 40, !str_detect(`路线`,ban_P))
  }
  
  cons.mx <- c(pick_P1, "锚点", dup_P) %>% map(
    ~ str_detect(PreRoute$`路线`, .x)
  ) %>% do.call(rbind, .)
  
  modelN <- list(
    A = rbind(PreRoute$点数, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = PreRoute$timeReal, weight = 1, priority = 10),
      `2` = list(objn = PreRoute$`综合资源掉落`,weight = -1)),
    rhs = c(n, str_count(pick_P1, "\\|") + 1, use.mount, rep(1, nrow(cons.mx) - 2)),
    sense = c('=', '=', '=', rep('<=',nrow(cons.mx) - 2)),
    vtype  = 'B'
  )
  
  solveN <- gurobi(modelN)
  
  RouteN <- PreRoute[as.logical(solveN$x), ]
  
  unuseable_P <- RouteN %>% 
    pull(`路线`) %>% str_split("—") %>% 
    reduce(c) %>% enframe(value = "点位") %>% 
    left_join(PointData, by = '点位') %>% 
    filter(`刷新周期` == 24) %>% pull(`点位`)
  
  RouteRelay <- PreRoute %>% 
    filter(!str_detect(`路线`, str_c('—', unuseable_P, collapse = "|")),
           !str_detect(`路线`, "锚点"))
  
  if (all(!str_detect(unuseable_P, "雷1|炉心1|熔炉1|洞1"),
          !str_detect(ban_P, "炉心|熔炉"))) {
    pick_P2 <- "—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$"
    n_relay <- 99 + 25
  } else if (all(!str_detect(unuseable_P, "—北水文4—工坊上1"),
                 !str_detect(ban_P, "北水文|工坊"))) {
    pick_P2 <- "—北水文4—工坊上1"
    n_relay <- 99 + 22
  } else if (all(!str_detect(unuseable_P, "—兰房2—石笋1"),
                 !str_detect(ban_P, "兰房|石笋"))) {
    pick_P2 <- "—兰房2—石笋1"
    n_relay <- 99 + 14
  }
  
  cons.mx_relay <- RouteRelay %>% pull(`路线`) %>% str_split('—') %>% 
    map(~ .x[-1]) %>% reduce(c) %>% unique %>% (\(x) c(pick_P2, x)) %>% 
    map(~ str_detect(RouteRelay$`路线`, .x)) %>% do.call(rbind, .)
  
  modelSupp <- list(
    A = rbind(RouteRelay$`点数`, cons.mx_relay * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteRelay$timeReal,weight = 1,priority = 10),
      `2` = list(objn = RouteRelay$`综合资源掉落`,weight = -1)),
    rhs = c(n_relay, str_count(pick_P2, "\\|") + 1, rep(1, nrow(cons.mx_relay) - 1)),
    sense = c('=', "=", rep('<=', nrow(cons.mx_relay) - 1)),
    vtype  = 'B'
  )
  
  solveSupp <- gurobi(modelSupp)
  
  Route1 <- RouteRelay[as.logical(solveSupp$x), ]
  
  cat(str_glue("N路线耗时 {RouteN %>% summarise(time = sum(timeReal)/60) %>% round(2)}min\n\n"))
  
  cat(str_glue("1路线耗时 {Route1 %>% summarise(time = sum(timeReal)/60) %>% round(2)}min"))
  
  ls <- list(
    the_N = RouteN %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify),
    the_1 = Route1 %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) 
  )
  
  if (write_route) {
    write_xlsx(ls, str_glue('./2.Result/Gurobi_Route_Nplus1_ver{n}p_{simple_date()}.xlsx'))
  }
  
  return(ls)
  
}


## N+1 127 ver ----

FastestN_127 <- gurobi_solve_N1(
  n = 127,
  use.mount = F,
  write_route = F,
  hard = F,
  dup_P = duplicate.points,
  ban_P = "格式塔|灵魂5|炉心",
  pick_P1 = "—秘仪阁1|—秘仪北C|—秘仪北B"
)


BalancedN1_127 <- gurobi_solve_N1(
  n = 127,
  use.mount = F,
  write_route = F,
  dup_P = duplicate.points,
  ban_P = "炉心1",
  pick_P1 = "—秘仪阁1|—秘仪北C|—秘仪北B"
)

### multi p----
gurobi_solve_N1(
  n = 99,
  use.mount = F,
  write_route = F,
  dup_P = duplicate.points,
  ban_P = str_glue("—越石村1|—清籁丸1|\\
  —炮1|—雷1|—熔炉1|—炉心1|—洞1|\\
  —权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —北水文2|—工坊中1"),
  pick_P1 = "渌华池",
)

# AB ----

gurobi_solve_AB <- function(n_point = 200, 
                            use.mount = F,
                            dup_P = duplicate.points, 
                            ban_P = "", 
                            hard = T,
                            pick_P, 
                            write_route = F) {
    
  PreRoute <- if (ban_P == "") {
    getRouteData(hardcore = hard) %>% 
      filter(`狗粮效率(经验/s)` > 40)
  } else {
    getRouteData(hardcore = hard) %>% 
      filter(`狗粮效率(经验/s)` > 40, !str_detect(`路线`, ban_P))
  }
  
  FastFresh <- PointData %>% filter(`刷新周期` != 24) %>% pull(`点位`)
  
  RouteAB <- bind_rows(
    PreRoute,
    PreRoute[PreRoute %>% pull(`路线`) %>% strsplit("—") %>% 
                map_dbl(~ prod(.x[-1] %in% FastFresh)) %>% as.logical, ]
  )
  
  cons.mx <- dup_P %>% str_c('—', .) %>% c(pick_P, '锚点', .) %>% set_names %>% 
    map(~ str_detect(RouteAB$`路线`, .x)) %>% do.call(rbind, .)
  
  cons.rhs <- dup_P %in% FastFresh %>% +1 %>% 
    c(str_count(pick_P, "\\|") + 1, use.mount, .)
  
  modelAB <- list(
    A = rbind(RouteAB$`点数`, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteAB$timeReal, weight = 1, priority = 10),
      `2` = list(objn = RouteAB$`狗粮效率(经验/s)`, weight = -1)
    ),
    rhs = c(n_point, cons.rhs),
    sense = c('=', "=", rep('<=', length(cons.rhs) - 1)), 
    vtype  = 'B'
  )
  
  solveAB <- gurobi(modelAB)
  
  df <- RouteAB[solveAB$x %>% as.logical, ] 
  
  if (write_route) {
    df %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
      write_xlsx(str_glue('./2.Result/Gurobi_Route_AandB_ver{n_point}p_{simple_date()}.xlsx'))
  }
  
  return(df)
  
}

## 127 +124 AB ----

RouteAB253 <- gurobi_solve_AB(
  n_point = 198 + 28 + 20 + 7,
  ban_P = "",
  pick_P = "—清籁丸1|—泡泡1-7|—秘仪阁1|—秘仪北C|—秘仪北B",
  write_route = T
)

RouteAB251 <- gurobi_solve_AB(
  use.mount = F,
  n_point = 198 + 28 + 25,
  ban_P = "镀金C2",
  pick_P = "—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$|—秘仪阁1|—秘仪北C|—秘仪北B",
  write_route = T
)

## Newbie ----

gurobi_solve_AB(
  n_point = 220,
  hard = F,
  ban_P = "权杖|据点A|据点B|镀金A|镀金C|石笋|炉心",
  pick_P = "—泡泡",
  write_route = F
)

## MultiPlayer----

### 4P ----
ban.4p <- str_glue(
  "—兰房3|\\
  —越石村1|—清籁丸1|\\
  —炮1|—雷1|—熔炉1|—炉心1|—洞1|\\
  —权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —北水文2|—工坊中1|\\
  —居尔东2|—居尔西1|\\
  —鳄王窟4|—祝祭3|—赤王1|\\
  —据点A3|—记忆2"
)

ban.4p <- str_glue(
  "—兰房3|—越石村1|—清籁丸1|—炮1|—雷1|—熔炉1|—炉心1|—洞1|—权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —北水文2|—工坊中1|—居尔东2|—居尔西1|—鳄王窟4|—祝祭3|—赤王1|—据点A3|—记忆2"
)

RouteAB.4p <- gurobi_solve_AB(
  use.mount = F,
  n_point = 198,
  ban_P = ban.4p,
  pick_P = "渌华池1",
  write_route = F
)
RouteAB.4p %>% mutate(路线 = Route_simplify(`路线`)) %>% 
  clipr::write_clip()

### 3P ----
ban.3p <- str_glue(
  "—雷1|—熔炉1|—炉心1|—洞1|\\
  —越石村1|—清籁丸1|\\
  —权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —北水文2|—工坊中1|\\
  —居尔东2|—居尔西1|\\
  —鳄王窟4|—祝祭3|—赤王1|\\
  —据点A3|—记忆2"
)

RouteAB.3p <- gurobi_solve_AB(
  use.mount = F,
  n_point = 198,
  ban_P = ban.3p,
  pick_P = "渌华池1",
  write_route = F
)

RouteAB.3p %>% mutate(路线 = Route_simplify(`路线`)) %>% 
  clipr::write_clip()

### 2P----
ban.2p <- str_glue(
  "—炮1|—雷1|—熔炉1|—炉心1|—洞1|\\
  —权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —越石村1|—清籁丸1|\\
  —北水文2|—工坊中1|\\
  —据点A3|—记忆2"
)

RouteAB.2p <- gurobi_solve_AB(
  use.mount = F,
  n_point = 198,
  ban_P = ban.2p,
  pick_P = "渌华池1",
  write_route = F
)


RouteAB.Multi %>% mutate(路线 = Route_simplify(`路线`)) %>% 
  clipr::write_clip()

# Fast99 ----

gurobi_solve_fast <- function(ban_P = "", hard = T, write_route = F){
  
  FastFresh <-  PointData %>% filter(`刷新周期` != 24) %>% pull(`点位`)
  
  FastRoutes <- getRouteData(hardcore = hard) %>% 
    pull(`路线`) %>% strsplit("—") %>% 
    map_dbl(~ prod(.x[-1] %in% FastFresh)) %>% 
    as.logical %>% (\(x) getRouteData(hardcore = hard)[x,]) %>% 
    (\(x) if (ban_P == "") x else x %>% filter(!str_detect(`路线`, ban_P)))
  
  cons.mx <- FastFresh %>% map(~ str_detect(FastRoutes$`路线`, .x)) %>% 
    do.call(rbind, .)
  
  modelN <- list(
    A = rbind(FastRoutes$点数, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = FastRoutes$timeReal, weight = 1, priority = 10),
      `2` = list(objn = FastRoutes$`期望经验`, weight = -1)),
    rhs = c(99, rep(1, nrow(cons.mx))), 
    sense = c('=', rep('<=',nrow(cons.mx))),
    vtype  = 'B'
  )
  
  solveN <- gurobi(modelN)
  
  Fast99 <- FastRoutes[solveN$x %>% as.logical(),]
  
  cat(str_glue("路线耗时 {Fast99 %>% summarise(time = sum(timeReal)/60) %>% round(2)}min\n\n"))
  
  if (write_route) {
    write_xlsx(Fast99, str_glue('./2.Result/Gurobi_Route_FastFresh99_{simple_date()}.xlsx'))
  }
  
  Fast99 %>% return()
}

gurobi_solve_fast(ban_P = "离岛", hard = T, write_route = F) %>% 
  mutate(`路线` = Route_simplify(`路线`)) %>% 
  clipr::write_clip()

# independent AB -----
gurobi_independent_AB <- function(n_point = 200, 
                                  use.mount = F,
                                  hard = T,
                                  dup_P = duplicate.points, 
                                  ban_P = "", 
                                  pick_P, 
                                  write_route = F) {
  
  PreRoute <- if (ban_P == "") {
    getRouteData(hardcore = hard) %>% 
      filter(`狗粮效率(经验/s)` > 40)
  } else {
    getRouteData(hardcore = hard) %>% 
      filter(`狗粮效率(经验/s)` > 40, !str_detect(`路线`, ban_P))
  }
  
  cons.mx <- dup_P %>% str_c('—', .) %>% c(pick_P, '锚点', .) %>% set_names %>% 
    map(~ str_detect(PreRoute$`路线`, .x)) %>% do.call(rbind, .)
  
  cons.rhs <- c(str_count(pick_P, "\\|") + 1, use.mount, rep(1, nrow(cons.mx) - 2))
  
  modelAB <- list(
    A = rbind(PreRoute$`点数`, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = PreRoute$timeReal, weight = 1, priority = 10),
      `2` = list(objn = PreRoute$期望经验, weight = -1)
    ),
    rhs = c(n_point, cons.rhs),
    sense = c('=', "=", rep('<=', length(cons.rhs) - 1)), 
    vtype  = 'B'
  )
  
  solveAB <- gurobi(modelAB)
  
  df <- PreRoute[solveAB$x %>% as.logical, ] 
  
  if (write_route) df %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
    write_xlsx(str_glue('./2.Result/Gurobi_Route_AB.ind_ver{n_point}p_{simple_date()}.xlsx'))
  
  return(df)
  
}

independent_AB <- gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 25,
  hard = F,
  use.mount = F,
  write_route = T,
  ban_P = "—镀金C2|—灵魂5|—记忆2",
  pick_P = "—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$|—秘仪阁1|—秘仪北C|—秘仪北B"
)

ind_AB <- gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 20,
  use.mount = F,
  write_route = T,
  ban_P = "—镀金C2|—据点A3|—灵魂5|—炉心1|—记忆2",
  pick_P = "—清籁丸1|—秘仪阁1|—秘仪北C|—秘仪北B"
)

# Consider Mora ----

gurobi_solve_AB_with_mora <- function(n_point = 200,
                                      use.mount = F,
                                      ban_P = "",
                                      hard = T,
                                      pick_P,
                                      dup_P = duplicate.points,
                                      write_route = F)
{
  
  PreRoute <- if (ban_P == "") {
    getRouteData(hardcore = hard)
  } else {
    getRouteData(hardcore = hard) %>%
      filter(!str_detect(`路线`, ban_P))
  }
  
  FastFresh <- PointData %>% filter(`刷新周期` != 24) %>% pull(`点位`)
  
  RouteAB <- bind_rows(
    PreRoute,
    PreRoute[PreRoute %>% pull(`路线`) %>% strsplit("—") %>% 
               map_dbl(~ prod(.x[-1] %in% FastFresh)) %>% as.logical, ]
  )
  
  cons.mx <- dup_P %>% str_c('—', .) %>% c(pick_P, '锚点', .) %>% set_names %>% 
    map(~ str_detect(RouteAB$`路线`, .x)) %>% do.call(rbind, .)
  
  cons.rhs <- dup_P %in% FastFresh %>% +1 %>% 
    c(str_count(pick_P, "\\|") + 1, use.mount, .)
  
  modelAB <- list(
    A = rbind(RouteAB$`点数`, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = RouteAB$timeReal, weight = 1, priority = 10),
      `2` = list(objn = RouteAB$`综合资源掉落`, weight = -1)
    ),
    rhs = c(n_point, cons.rhs),
    sense = c('=', "=", rep('<=', length(cons.rhs) - 1)), 
    vtype  = 'B'
  )
  
  solveAB <- gurobi(modelAB)
  
  df <- RouteAB[solveAB$x %>% as.logical, ] 
  
  if (write_route) {
    df %>% mutate(`路线` = `路线` %>% hex_decode %>% Route_simplify) %>% 
      write_xlsx(str_glue('./2.Result/Gurobi_Route_AandB_ver{n_point}p_{simple_date()}.xlsx'))
  }
  
  return(df)
  
}


## Max Loot ----
gurobi_solve_AB_with_mora(
  n = 198 + 28 + 25,
  use.mount = F,
  ban_P = "",
  dup_P = duplicate.points,
  pick_P = "—秘仪阁1|—秘仪北C|—秘仪北B|—狸猫3—雷1|—熔炉1|—高台1—炉心6|—洞1$"
) %>% view

# theory test----

gurobi_solve_fast <- function(n, use.mount, ban_P = "", pick_P, dup_P, write_route) {
  
  PreRoute <- if (ban_P == "") {
    getRouteData(hardcore = hard)
  } else {
    getRouteData(hardcore = hard) %>% filter(!str_detect(`路线`, ban_P))
  }
  
  cons.mx <- c(pick_P, '锚点', dup_P) %>% 
    map(~ str_detect(PreRoute$`路线`, .x)) %>% 
    do.call(rbind, .)
  
  modelN <- list(
    A = rbind(PreRoute$点数, cons.mx * 1) %>% as.matrix,
    multiobj = list(
      `1` = list(objn = PreRoute$timeReal, weight = 1, priority = 10),
      `2` = list(objn = PreRoute$`期望经验`,weight = -1)),
    rhs = c(n, str_count(pick_P, "\\|") + 1, use.mount, rep(1, nrow(cons.mx) - 2)),
    sense = c('=', '=', '=', rep('<=',nrow(cons.mx) - 2)),
    vtype  = 'B'
  )
  
  solveN <- gurobi(modelN)
  
  PreRoute[as.logical(solveN$x), ]
}

df1 <- 40:127 %>% map_dfr(
  ~ gurobi_solve_fast(
    n = .x,
    use.mount = F,
    ban_P = "",
    pick_P = "炮1",
    dup_P = duplicate.points,
    write_route = F
  ) %>% summarise(
    ExpGain = sum(`期望经验`),
    TimeConsume = sum(timeReal)
  ) %>% mutate(n_p = .x)
) 

df1 %>% mutate(Efficiency = ExpGain / TimeConsume) %>%
  ggplot(aes(x = n_p, y = Efficiency)) + geom_line()


nomora_P <- PointData %>% filter(`掉落期望` == 1.35) %>% pull(`点位`) %>% str_c(collapse = "|")

hasmora_P <- PointData %>% filter(`掉落期望` > 1.35) %>% pull(`点位`) %>% str_c(collapse = "|")

getRouteData(hardcore = T) %>% filter(!str_detect(`路线`, nomora_P)) %>% pull(`路线`) %>% 
  enframe %>% separate_rows(value, sep = "—") %>% 
  count(value) %>% arrange(desc(n)) %>% filter(n > 1) %>% pull(value)

duplicated.points <-
  c("嘉铭1",
    '碧水原2',
    "碧水原4",
    "绝云间2",
    "客栈2",
    "奥藏山3",
    "华光林1",
    "赤王1",
    "赤王4",
    "祝祭3")


gurobi_solve_fast(
  n = 99,
  use.mount = F,
  ban_P = nomora_P,
  pick_P = "黑泥1",
  dup_P = duplicated.points,
  write_route = F
) %>% clipr::write_clip()

# random consult----

RouteAB <- gurobi_solve_AB(
  n_point = 198 + 28 + 20 + 7,
  write_route = F,
  ban_P = "祝祭1|奥藏山4|据点A3|炉心1",
  pick_P = str_glue(
  "—嘉铭1|—观测1|—黑泥1|—倒吊2|\\
  —嘉铭1|—观测1|—黑泥1|—倒吊2|\\
  —奥藏山3|—采樵1|—镀金E|—镀金A|—镀金C|—镀金D|—镀金H|\\
  —奥藏山3|—采樵1|—镀金E|—镀金A|—镀金C|—镀金D|—镀金H|\\
  —旧桓那废墟|—据点A1|—祝祭3|—巨人1|\\
  —旧桓那废墟|—据点A1|—祝祭3|—巨人1|\\
  —清籁丸1|—泡泡1-7|—秘仪阁1|—秘仪北C|—秘仪北B"))



RouteAB %>% mutate(路线 = Route_simplify(路线)) %>% clipr::write_clip()

