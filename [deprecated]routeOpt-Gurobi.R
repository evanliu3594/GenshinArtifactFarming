# ---- Project Genshin 圣遗物狗粮路线效率优化---- #

# Env. Settings----
{
  library(tidyverse)
  library(readxl)
  library(LYFtools)
  library(gurobi)
  
  hex_encode <- \(x) x %>% str_replace_all("1[0-5]",\(num) as.integer(num) %>% as.hexmode)
  
  hex_decode <- \(x) x %>% str_replace_all('[a-f]',\(hex) as.hexmode(hex) %>% as.integer)
  
  Route_simplify <- \(Route) {
    Route %>% strsplit("——") %>% map_chr(~{
      .x %>% str_match('([\u4e00-\u9fa5]+[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
        mutate(V4 = if_else(V2 == lag(V2, 1) %>% replace_na(""), "", V2)) %>% 
        str_glue_data("{V4}{V3}") %>% str_c(collapse = "—")
    })
  }
  
  Route_restitute <- \(Route) {
    Route %>% strsplit("—") %>% map_chr(~{
      .x %>% str_match('([\u4e00-\u9fa5]*[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
        mutate(V2 = na_if(V2, "")) %>% fill(V2) %>% 
        str_glue_data("{V2}{V3}") %>% str_c(collapse = "———")
    })
  }
  
  getClosure <- function(end) {
    
    closures <- c(
      "秘仪圣殿" = "——秘仪阁1|——秘仪北C|——秘仪北B",
      "踏備砂" = "——狸猫3——雷1|——熔炉1|——高台1——炉心6|——洞1$",
      "水文塔" = "——北水文4——工坊上1",
      "清籁丸" = "——营地1——清籁丸1"
    )
    
    return(str_replace_all(end, closures))
  }
  
  Optim_core <- function(dummyRoute, n, pick_P, dup, use.mount = F, Time.weight = 100, target){
    
    mx <- c(getClosure(pick_P), "锚点", dup) %>% set_names() %>% 
      map(~str_detect(dummyRoute$路线, .x)) %>% 
      do.call(rbind, .) %>% rbind(P = dummyRoute$点数, .)
    
    rhs = c(n, c(str_count(getClosure(pick_P), "\\|") + 1, use.mount, rep(1, nrow(mx) - 3)))
    
    sense = c('=', "=", rep('<=', nrow(mx) - 2))
    
    model <- list(
      A = mx,
      multiobj = list(
        `1` = list(objn = dummyRoute %>% pull(timeReal), weight = Time.weight),
        `2` = list(objn = dummyRoute %>% pull(!!target), weight = -1)
      ),
      rhs = rhs,
      sense = sense,
      vtype  = "B"
    )
    
    gurobi_result <- gurobi(model)
    
    solveRoute <- dummyRoute[as.logical(gurobi_result$x),]
    
    return(solveRoute)
    
  }
}

# Data load----

PointData <- c("蒙德璃月","稻妻","须弥","枫丹","纳塔") %>% map(~ read_xlsx(
    "./data/点位描述.xlsx", range = cell_cols("A:K"), sheet = .x, col_types = "text"
  )) %>% list_rbind() %>% rename(点位 = 编号) %>% filter(!is.na(主地图)) %>% 
  mutate(点位 = 点位 %>% str_remove_all("（.+）") %>% hex_encode()) %>% 
  mutate(刷新周期 = if_else(刷新周期 == "每日4点", "0", 刷新周期)) %>% 
  mutate(across(c(数量, 刷新周期,掉落期望,无摩拉,有摩拉,封锁等级), as.numeric))

# TOT: Time of Teleport
getRouteData <- function(comfort_punish = 4, TOT = 6, lock_lvl = 10) {
  
  Route_design <- './data/路线设计.xlsx' %>% 
    read_xlsx(range = cell_cols("A:D")) %>% 
    mutate(
      路线 = hex_encode(路线),
      rand = runif(n(), 0, .01),
      time = `基础耗时(s)` + rand,
      time_tp = time + TOT,
      timeReal = time_tp + replace_na(地图惩罚, 0) + replace_na(n_不舒适, 0) * comfort_punish, 
    ) %>% 
    select(路线, time, time_tp, timeReal)
  
  Route_design %>% 
    mutate(points = 路线, 路线 = as_factor(路线)) %>% 
    separate_rows(points, sep = "——") %>% 
    left_join(PointData %>% rename(points = 点位)) %>% 
    filter(!is.na(主地图)) %>% 
    mutate(mora_drop = 有摩拉 * 200) %>% 
    group_by(路线) %>% 
    summarise(
      time = mean(time, na.rm = T),
      time_tp = mean(time_tp, na.rm = T),
      timeReal = mean(timeReal, na.rm = T),
      点数 = sum(数量, na.rm = T), 
      期望摩拉 = sum(mora_drop, na.rm = T),
      期望狗粮 = sum(数量 * 掉落期望 * 420, na.rm = T),
      封锁等级 = mean(封锁等级, na.rm = T)
    ) %>% 
    mutate(
      路线 = as.character(路线), 
      狗粮效率 = 期望狗粮 / time_tp,
      综合资源 = 期望摩拉 + 期望狗粮,
      综合效率 = 综合资源 / time_tp
    ) %>% 
    filter(封锁等级 < lock_lvl) %>% #view()
    select(-封锁等级) %>% 
    return()
}

getRouteData() %>% left_join(
  read_xlsx('./data/路线设计.xlsx', range = cell_cols("A:D")) %>% 
    mutate(舒适度惩罚 = n_不舒适 * 4) %>% select(-`基础耗时(s)`)
) %>% mutate(路线 = 路线 %>% hex_decode %>% Route_simplify) %>% 
  select(-综合资源) %>% 
  clipr::write_clip()

# Detect Duplicated Routes ----

getRouteData() %>% filter(狗粮效率 > 50) %>% 
  select(路线) %>% separate_rows(路线, sep = '——') %>% 
  filter(!str_detect(路线, "传送点|神像|副本|锚点")) %>%
  unlist %>% str_match("([一-龥]+[A-Za-z]?)([0-9]*)") %>% 
  as.data.frame() %>% mutate(across(c(V1,V2), ~fct_relevel(.x, unique(.x)))) %>% 
  count(V1) %>% filter(n>=2) %>% str_glue_data("{V1}×{n}") %>% paste(collapse = " ")

# PointData %>% filter(刷新周期 != 24) %>% pull(点位) %>% 
#   str_c(collapse = "—") %>% Route_simplify() %>% 
#   str_replace_all("(?<=\\d)—(?=[一-龥])","|")

duplicate.points <- str_glue("嘉铭1|观测1|\\
    奥藏东3|绝云间2|碧水原4|\\
    炮1|熔炉1|炉心1|\\
    石像5|藏宝洞1|水帘洞1|\\
    清籁丸1|营地2|箱子2|\\
    兰房2|镀金A1|镀金C1|镀金D1|据点A1|据点A2|镀金H1|据点B4|\\
    祝祭3|鳄王窟3|巨人3|居尔东8|居尔中3|居尔西3|神棋2|精石1|\\
    梅管1|梅舍1|艾伊古4|科院北1|北水文2|工坊中1|科院南1|\\
    佩妮北1|灵魂1|人格1|演奏厅1|\\
    恩加韦|游侠1|奥奇卡神像1|烬燃1|烬燃4|托佐兹2") %>% str_split("\\|") %>% unlist

# N+1 ----

gurobi_solve_N1 <- function(
    n,
    mount = F,
    comf = 4,
    tot = 6,
    tgt = "期望狗粮",
    lvl = 10,
    ban_P = "",
    pick_P1 = "",
    pick_P2 = "",
    dup_P = duplicate.points,
    write_route = F
) {
  
  PreRoute <- getRouteData(TOT = tot, comfort_punish = comf, lock_lvl = lvl) %>% 
    filter(狗粮效率 > 40, !str_detect(路线, if_else(nchar(ban_P) == 0, "全部路线", ban_P)))
  
  RouteN <- Optim_core(
    dummyRoute = PreRoute,
    n = n,
    pick_P = pick_P1,
    dup = dup_P,
    use.mount = mount,
    target = tgt
  )
  
  unuseable_P <- RouteN %>% 
    pull(路线) %>% str_split("—") %>% 
    reduce(c) %>% enframe(value = "点位") %>% 
    left_join(PointData, by = '点位') %>% 
    filter(刷新周期 == 24) %>% pull(点位) %>% 
    paste0("—",., collapse = "|")
  
  RouteRelay <- PreRoute %>% filter(!str_detect(路线, "锚点")) %>% 
    filter(!str_detect(路线, unuseable_P))
  
  if (pick_P2 == "") {
    pick_P2 <- if (!str_detect(paste0(unuseable_P,"|",ban_P), "雷1|熔炉1|炉心1|洞1")) {
      "踏備砂"
    } else if (!str_detect(paste0(unuseable_P,"|",ban_P), "北水文[234]|工坊上1")) {
      "水文塔"
    } else if (!str_detect(paste0(unuseable_P,"|",ban_P), "营地1|清籁丸1")) {
      "清籁丸"
    } else {
      RouteRelay$路线[which.max(RouteRelay$点数)]
    }
  }
  
  n_relay <- if (pick_P2 == "踏備砂") {
    99 + 25
  } else if (pick_P2 == "水文塔") {
    99 + 22
  } else if (pick_P2 == "清籁丸") {
    99 + 20
  } else {
    99 + max(RouteRelay$点数)
  }
  
  Route1 <- Optim_core(
    dummyRoute = RouteRelay,
    n = n_relay,
    pick_P = pick_P2,
    dup = dup_P,
    use.mount = F,
    target = tgt
  )
  
  cat(paste0("N路线耗时", RouteN %>% summarise(time = (sum(time_tp)/60) %>% round(2)), "min\n\n"))
  cat(paste0("1路线耗时", Route1 %>% summarise(time = (sum(time_tp)/60) %>% round(2)), "min"))
  
  ls <- list(
    the_N = RouteN %>% mutate(路线 = 路线 %>% hex_decode %>% Route_simplify),
    the_1 = Route1 %>% mutate(路线 = 路线 %>% hex_decode %>% Route_simplify)
  )
  
  if (write_route) {
    write_xlsx(ls, str_glue('./2.Result/Gurobi_Route_Nplus1_ver{n}p_{simple_date()}.xlsx'))
  }
  
  return(ls)
  
}


## N+1 127 ----

N_127 <- gurobi_solve_N1(
  n = 127, 
  comf = 6, tgt = "综合资源",
  dup_P = duplicate.points, 
  ban_P = "记忆2|北水文2",
  pick_P1 = "秘仪圣殿",
  pick_P2 = "踏備砂"
)

N_127$the_N %>% select(-time_tp,-timeReal,-综合资源,-狗粮效率) %>% clipr::write_clip()
N_127$the_1 %>% select(-time_tp,-timeReal,-综合资源,-狗粮效率) %>% clipr::write_clip()

# AB不重叠 -----
gurobi_independent_AB <- function(
    n_point = 200, 
    mount = F,
    comf = 4,
    tot = 6,
    lvl = 10,
    tgt = "期望狗粮",
    pick_P, 
    dup_P = duplicate.points, 
    ban_P = "", 
    write_route = F
  )
{
  PreRoute <- getRouteData(TOT = tot, comfort_punish = comf, lock_lvl = lvl) %>% 
    filter(狗粮效率 > 40, !str_detect(路线, if_else(nchar(ban_P) == 0, "全部路线", ban_P)))
  
  RouteAB <- Optim_core(
    dummyRoute = PreRoute,
    n = n_point,
    pick_P = pick_P,
    dup = dup_P,
    use.mount = mount,
    target = tgt
  ) %>% mutate(路线 = 路线 %>% hex_decode %>% Route_simplify)

  if (write_route) {
    write_xlsx(
      RouteAB,
      str_glue('./2.Result/Gurobi_Route_AB.ind_ver{n_point}p_{simple_date()}.xlsx')
    )
  }
  
  return(RouteAB)
  
}

## application ----

### 127+124 ----
#### normal situation
gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 25, 
  ban_P = "灵魂5|格式塔",
  pick_P = "踏備砂|秘仪圣殿"
) %>% select(-time_tp,-timeReal,-期望摩拉,-综合资源,-综合效率) %>% 
  clipr::write_clip()

#### more mora
gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 25, 
  tgt = "综合资源", comf = 6,
  ban_P = "记忆2|灵魂5|格式塔|观测1",
  pick_P = "踏備砂|秘仪圣殿"
) %>% select(-time_tp,-timeReal,-综合资源,-狗粮效率) %>% 
  clipr::write_clip()

### 127+119 ----
gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 20,
  ban_P = "—镀金C2|—据点A3|—灵魂5|—炉心1|—记忆2",
  pick_P = "清籁丸|秘仪圣殿"
)

### best Comfort ----
gurobi_independent_AB(
  n_point = 99 * 2 + 28 + 25, comf = 8, 
  ban_P = "记忆2|灵魂5|格式塔|北水文2",
  pick_P = "踏備砂|秘仪圣殿"
) %>% select(-timeReal,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

# 全12小时 ----

gurobi_solve_fast <- function(
    ban_P = "", 
    comf = 4, 
    tot = 6, 
    tgt = "期望狗粮",
    write_route = F
  )
  {
  
  FastFresh <-  PointData %>% filter(刷新周期 != 24) %>% pull(点位)
  
  FastRoutes <- getRouteData(comfort_punish = comf, TOT = tot) %>% 
    pull(路线) %>% strsplit("——") %>% 
    map_lgl(~ prod(.x[-1] %in% FastFresh)) %>% 
    getRouteData(comfort_punish = comf)[.,] %>% 
    filter(!str_detect(路线, ifelse(ban_P == "", "全部路线", ban_P)))
  
  cons.mx <- FastFresh %>% map(~ str_detect(FastRoutes$路线, .x)) %>% 
    do.call(rbind, .) %>% rbind(FastRoutes$点数, .)
  
  cons.rhs <- c(99, rep(1, nrow(cons.mx) - 1))
  
  cons.sense <- c('=', rep('<=', nrow(cons.mx) - 1))
  
  model99 <- list(
    A =  cons.mx,
    multiobj = list(
      `1` = list(objn = FastRoutes %>% pull(timeReal), weight = 77),
      `2` = list(objn = FastRoutes %>% pull(!!tgt), weight = -1)),
    rhs = cons.rhs, 
    sense = cons.sense,
    vtype  = 'B'
  )
  
  solve99 <- gurobi(model99)
  
  Fast99 <- FastRoutes[as.logical(solve99$x),] %>% 
    mutate(路线 = 路线 %>% hex_decode %>% Route_simplify)
  
  cat(str_glue("路线耗时 {Fast99 %>% summarise(time = sum(timeReal)/60) %>% round(2)}min\n\n"))
  
  if (write_route) {
    write_xlsx(Fast99, str_glue('./2.Result/Gurobi_Route_FastFresh99_{simple_date()}.xlsx'))
  }
  
  Fast99 %>% return()
}

## application ----
gurobi_solve_fast(
    ban_P = "离岛|镀金C2|鸡屁股|圣显1",
    tgt = "综合资源", comf = 6,
    write_route = F
  ) %>% 
  select(-timeReal,-time_tp,-综合资源,-狗粮效率) %>% 
  clipr::write_clip()

# 传统AB ----
gurobi_solve_AB <- function(
    n_point = 198, 
    use.mount = F,
    dup_P = duplicate.points, 
    ban_P = "", 
    pick_P,
    comf = 4,
    tot = 6,
    lvl = 10,
    tgt = "期望狗粮",
    no_2nd_use = "",
    write_route = F
  ) {
  
  PreRoute <- PreRoute <- getRouteData(TOT = tot, comfort_punish = comf, lock_lvl = lvl) %>% 
    filter(狗粮效率 > 40, !str_detect(路线, if_else(nchar(ban_P) == 0, "全部路线", ban_P)))
  
  FastFresh <- PointData %>% filter(刷新周期 != 24) %>% pull(点位) %>% 
    str_subset(no_2nd_use, negate = T)
  
  RouteFastFresh <- str_split(PreRoute$路线, "——") %>% 
    map_lgl(~ prod(.x[-1] %in% FastFresh)) %>% PreRoute[., ] %>% 
    filter(!str_detect(路线, if_else(no_2nd_use == "", "全部路线", no_2nd_use)))
  
  RouteAB <- bind_rows(PreRoute, RouteFastFresh) %>% arrange(路线) %>% 
    mutate(路线 = as.character(路线))
  
  cons.mx <- c(getClosure(pick_P), '锚点', dup_P) %>% set_names %>% 
    map(~str_detect(RouteAB$路线, .x)) %>% do.call(rbind, .) %>% 
    rbind(RouteAB$点数, .)
  
  cons.rhs <- c(n_point, str_count(getClosure(pick_P), "\\|") + 1, 
                use.mount, dup_P %in% FastFresh + 1)
  
  cons.sense <- c('=', "=", rep('<=', length(cons.rhs) - 2))
  
  modelAB <- list(
    A = cons.mx,
    multiobj = list(
      `1` = list(objn = RouteAB %>% pull(timeReal), weight = 100),
      `2` = list(objn = RouteAB %>% pull(!!tgt), weight = -1)
    ),
    rhs = cons.rhs,
    sense = cons.sense, 
    vtype  = 'B'
  )
  
  solveAB <- gurobi(modelAB)
  
  df <- RouteAB[as.logical(solveAB$x), ] %>% 
    mutate(路线 = 路线 %>% hex_decode %>% Route_simplify)
  
  if (write_route) {
    df %>% write_xlsx(str_glue(
      './2.Result/Gurobi_Route_AB_ver{n_point}p_{simple_date()}.xlsx'
    ))
  }
  
  return(df)
  
}

## Application ----
### 127 + 124 ----

gurobi_solve_AB(
  n_point = 198 + 28 + 25,
  ban_P = "镀金C2|灵魂5|格式塔|艾伊古4",
  pick_P = str_glue("——狸猫3——雷1|——熔炉1|——高台1——炉心6|——洞1$|\\
                    ——秘仪阁1|——秘仪北C|——秘仪北B"),
  no_2nd_use = "梅管1|托佐兹2",
) %>% select(-timeReal,-期望摩拉,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

### 127 + 126 ----

RouteAB253 <- gurobi_solve_AB(
  n_point = 198 + 28 + 20 + 7,
  ban_P = "",
  pick_P = "——清籁丸1|——泡泡1-7|——秘仪阁1|——秘仪北C|——秘仪北B"
) %>% select(-timeReal,-期望摩拉,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

### MultiPlayer----

#### 4P ----

ban.4p <- str_glue("
  镀金C2|巨人2|记忆2|灵魂5|\\
  兰房3|\\
  越石村1|清籁丸1|\\
  炮1|雷1|熔炉1|炉心1|洞1|\\
  权杖1|秘仪阁1|秘仪北C|秘仪北B|\\
  格式塔|\\
  北水文2|工坊中1|\\
  居尔东2|居尔西1|\\
  鳄王窟4|祝祭3|赤王1
  "
)

gurobi_solve_AB(
  n_point = 198,
  ban_P = ban.4p,
  pick_P = "渌华池1",
  no_2nd_use = "观测1|梅舍1|奥藏东3|采樵1|王山1|赛芭",
) %>% select(-timeReal,-期望摩拉,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

#### 3P ----
ban.3p <- str_glue(
  "
  雷1|熔炉1|炉心1|洞1|\\
  越石村1|清籁丸1|\\
  权杖1|秘仪阁1|秘仪北C|秘仪北B|\\
  北水文2|工坊中1|\\
  居尔东2|居尔西1|\\
  鳄王窟4|祝祭3|赤王1|\\
  记忆2|灵魂5
  "
)

gurobi_solve_AB(
  n_point = 198,
  ban_P = ban.3p,
  pick_P = "渌华池1"
) %>% select(-timeReal,-期望摩拉,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

#### 2P----
ban.2p <- str_glue(
  "—炮1|—雷1|—熔炉1|—炉心1|—洞1|\\
  —权杖1|—秘仪阁1|—秘仪北C|—秘仪北B|\\
  —越石村1|—清籁丸1|\\
  —北水文2|—工坊中1|\\
  —据点A3|—记忆2"
)

gurobi_solve_AB(
  n_point = 198,
  ban_P = ban.2p,
  pick_P = "渌华池1",
) %>% select(-timeReal,-期望摩拉,-综合资源,-综合资源效率) %>% 
  clipr::write_clip()

### BestComfort ----
### 127 + 124 ----

gurobi_solve_AB(
  n_point = 198 + 28 + 25,
  comf = 8,
  ban_P = "镀金C2|灵魂5|格式塔|艾伊古3",
  pick_P = str_glue("——狸猫3——雷1|——熔炉1|——高台1——炉心6|——洞1$|\\
                     ——秘仪阁1|——秘仪北C|——秘仪北B"),
  no_2nd_use = "梅管1|托佐兹2",
) %>% select(-timeReal,-期望摩拉,-综合资源,-狗粮效率) %>% 
  clipr::write_clip()

# test solution pool ----

gurobi_test <- function(n_point = 200,
                        use.mount = F,
                        comf = 4,
                        dup_P = duplicate.points,
                        ban_P = "",
                        pick_P,
                        tot = 6,
                        target = "综合资源")
{
  PreRoute <- if (ban_P == "") {
    getRouteData(comfort_punish = comf, TOT = tot) %>% 
      filter(狗粮效率 > 45)
  } else {
    getRouteData(comfort_punish = comf, TOT = tot) %>% 
      filter(狗粮效率 > 45, !str_detect(路线, ban_P))
  }
  
  cons.mx <- dup_P %>% str_c('—', .) %>% c(pick_P, '锚点', .) %>% set_names %>% 
    map(~ str_detect(PreRoute$路线, .x)) %>% do.call(rbind, .)
  
  cons.rhs <- c(str_count(pick_P, "\\|") + 1, use.mount, rep(1, nrow(cons.mx) - 2))
  
  model <- list(
    A = rbind(PreRoute$点数, cons.mx * 1) %>% as.matrix,
    obj = PreRoute$timeReal,
    rhs = c(n_point, cons.rhs),
    sense = c('=', "=", rep('<=', length(cons.rhs) - 1)), 
    vtype  = 'B'
  )
  
  gurobi_solve <- gurobi(model, list(PoolSearchMode = 2, PoolSolutions = 1E4))
  
  gradient <- gurobi_solve$pool %>% map_dbl(~{
    PreRoute[as.logical(.x[["xn"]]),] %>% 
      summarise(across(c(!!target, timeReal), sum)) %>% 
      unlist() %>% (\(x) x[1]/x[2])
  })
  
  this <- which.max(gradient) #;which.min(map_dbl(gurobi_solve$pool, ~.x[["objval"]]))
  
  result <- PreRoute[as.logical(gurobi_solve$pool[[this]][["xn"]]), ] %>% 
    mutate(路线 = 路线 %>% hex_decode %>% Route_simplify)

  return(result)
}

## application ----
gurobi_test(
  n_point = 99 * 2 + 28 + 25, comf = 8, target = "期望狗粮",
  ban_P = "记忆2|灵魂5|格式塔|北水文2",
  pick_P = "—狸猫3——雷1|—熔炉1|—高台1——炉心6|—洞1$|—秘仪阁1|—秘仪北C|—秘仪北B"
) %>% pull(期望摩拉) %>% sum

