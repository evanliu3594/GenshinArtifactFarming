# init ====
{
  library(tidyverse)
  library(readxl)
  library(ompr)
  library(ompr.roi)
  library(ROI.plugin.glpk)

  hex_encode <- \(x) {
    str_replace_all(x, "(1[0-5])(?=——)|(1[0-5])$", \(num) as.integer(num) %>% as.hexmode())
  }

  hex_decode <- \(x) {
    str_replace_all(x, "([a-f])(?=——)|([a-f])$", \(hex) as.hexmode(hex) %>% as.integer())
  }

  Route_simplify <- \(Route) {
    Route %>% strsplit("——") %>% map_chr(~{
      .x %>% str_match('([\u4e00-\u9fa5]+[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
        mutate(V4 = if_else(V2 == lag(V2, 1) %>% replace_na(""), "", V2)) %>% 
        str_glue_data("{V4}{V3}") %>% str_c(collapse = "—")
    })
  }

  Route_restore <- \(Route) {
    Route %>% strsplit("—") %>% map_chr(~{
      .x %>% str_match('([\u4e00-\u9fa5]*[A-Z]?)([0-9]?-?[0-9]?)') %>% as.data.frame %>% 
        mutate(V2 = na_if(V2, "")) %>% fill(V2) %>% 
        str_glue_data("{V2}{V3}") %>% str_c(collapse = "———")
    })
  }
}
# Data Load ====
{
  PointData <- c("蒙德璃月", "稻妻", "须弥", "枫丹", "纳塔") %>%
    map(~ read_xlsx(
      "./data/点位描述.xlsx",
      range = cell_cols("A:K"),
      sheet = .x,
      col_types = "text"
    )) %>% list_rbind() %>%
    rename(点位 = 编号) %>%
    filter(!is.na(主地图)) %>%
    mutate(
      点位 = str_remove_all(点位, "（.+）") %>% hex_encode(),
      across(c(数量, 掉落期望, 无摩拉, 有摩拉, 封锁等级), as.numeric),
    )

  PointData %>% filter(数量 != 有摩拉 + 无摩拉)

  PointData$数量 %>% sum()
}

getRouteData <- function(
  pun_comf = 6,
  pun_map = 6,
  t_teleport = 6,
  block_lvl = 10,
  reuse12 = FALSE
) {
  Route_design <- read_xlsx(
    '1.Data/路线设计.xlsx',
    range = cell_cols("A:D")
  ) %>%
    mutate(
      路线 = hex_encode(路线),
      rand = runif(n(), 0, .01),
      time = `基础耗时(s)` + rand,
      time_tp = time + t_teleport,
      time_pun = time_tp +
        replace_na(地图惩罚, 0) * pun_map +
        replace_na(舒适度惩罚, 0) * pun_comf,
    ) %>%
    select(路线, time, time_tp, time_pun)

  Route_data <- Route_design %>%
    mutate(points = 路线, 路线 = as_factor(路线)) %>%
    separate_longer_delim(points, delim = "——") %>%
    left_join(PointData, join_by(points == 点位)) %>%
    filter(!is.na(主地图)) %>%
    mutate(mora_drop = 有摩拉 * 200) %>%
    group_by(路线) %>%
    summarise(
      time = mean(time, na.rm = T),
      time_tp = mean(time_tp, na.rm = T),
      time_pun = mean(time_pun, na.rm = T),
      点数 = sum(数量, na.rm = T),
      期望摩拉 = sum(mora_drop, na.rm = T),
      期望狗粮 = sum(数量 * 掉落期望 * 420, na.rm = T),
      封锁等级 = mean(封锁等级, na.rm = T),
      allFast = all(刷新周期 != "24")
    ) %>%
    mutate(
      路线 = as.character(路线),
      狗粮效率 = 期望狗粮 / time_tp,
      综合资源 = 期望摩拉 + 期望狗粮,
      综合效率 = 综合资源 / time_tp
    ) %>%
    filter(封锁等级 < block_lvl) %>% #view()
    select(-封锁等级)

  Route_data_fast <- Route_data %>% filter(allFast) %>%
    mutate(路线 = str_replace_all(路线, "(——)([^——]+)", "\\1\\2rep"))

  if (reuse12) {
    bind_rows(Route_data, Route_data_fast) %>% select(-allFast)
  } else {
    Route_data %>% select(-allFast)
  }
}


# Optim Core ====

GeneralPlanning <- function(
  n_pts = 99,
  ...,
  use.mount = FALSE, 
  target = "期望狗粮",
  ban_P = "",
  pick_P = "",
  no_2nd_use = "",
  flex = .8,
  drop_least_eff = TRUE
) {

  banned_rep <- if (nchar(no_2nd_use) > 0) {
    no_2nd_use %>% str_replace_all("\\|", "rep|") %>% paste0("rep")
  } else {
    "全部点位"
  }
  
  PreRoute <- getRouteData(...) %>%
    filter(!str_detect(路线, if_else(ban_P == "", "全部路线", ban_P))) %>%
    (\(x) x %>% filter(!str_detect(路线, banned_rep)))
  
  if (nchar(pick_P) == 0) {
    pickedRoute <- data.frame()
    picked_pts <- "全部点位"
   } else {
    pickedRoute <- PreRoute %>% filter(str_detect(路线, pick_P))
    picked_pts <- pick_P %>% str_replace_all(c(
      "传送点|神像|^.+副本" = "",
      "—+" = "|",
      "\\|+" = "|",
      "^\\||\\$" = ""
    ))
  }
  
  pollingRoute <- PreRoute %>% filter(!str_detect(路线, picked_pts)) %>%
    (\(x) if (drop_least_eff) x %>% filter(狗粮效率 > quantile(狗粮效率, .3)) else x)

  pollingPoint <- pollingRoute$路线 %>% str_split("——") %>% 
    map(~tail(.x, -1)) %>% list_c() %>% unique()

  MountVec <- str_detect(pollingRoute$路线, "锚点") * 1L

  consMat <- pollingPoint %>% map(~ str_detect(pollingRoute$路线, .x) * 1L) %>% 
    do.call(cbind, .) %>% `dimnames<-`(list(pollingRoute$路线, pollingPoint))

  obj1 <- pollingRoute %>% pull(!!target)
  obj2 <- pollingRoute %>% pull(time_pun)
  pts <- pollingRoute %>% pull(点数)

  M_1 <- MIPModel() %>%
    add_variable(x[r], r = 1:nrow(consMat), type = "binary") %>%
    set_objective(sum_expr(obj1[r] * x[r], r = 1:nrow(consMat)), "max") %>%
    add_constraint(sum_expr(consMat[r, p] * x[r], r = 1:nrow(consMat)) <= 1, p = 1:ncol(consMat)) %>%
    add_constraint(sum_expr(pts[r] * x[r], r = 1:nrow(consMat)) == n_pts) %>% 
    add_constraint(sum_expr(MountVec[r] * x[r], r = 1:nrow(consMat)) == as.integer(use.mount))

  sol_1 <- solve_model(M_1, with_ROI("glpk"))

  best_gain <- objective_value(sol_1)

  M_2 <- M_1 %>%
    set_objective(sum_expr(obj2[r] * x[r], r = 1:nrow(consMat)), "min") %>%
    add_constraint(sum_expr(obj1[r] * x[r], r = 1:nrow(consMat)) >= best_gain * flex)

  sol_2 <- solve_model(M_2, with_ROI("glpk"))

  get_solution(sol_2, x[r]) %>% filter(value == 1) %>% pull(r) %>% 
    pollingRoute[.,] %>% bind_rows(pickedRoute)

}

# N+1 ----
## Regular ====
theN <- GeneralPlanning(
  target = "综合资源", flex = .7,
  ban_P = "记忆2|灵魂5|格式塔|集市顶8|马卡尼",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B"
)

theN %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

sum(theN$点数)

used_pts <- theN$路线 %>% str_split("——") %>% map(~.x[-1]) %>% list_c() %>% 
  paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

the1 <- GeneralPlanning(
  reuse12 = TRUE,  flex = .9,
  target = "综合资源",
  ban_P = sprintf("记忆2|灵魂5|格式塔|%s", used_pts),
  pick_P = "——集市西1——马卡尼|——集市顶8——集市顶7"
)

the1 %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = str_remove_all(路线,"rep") %>% hex_decode() %>% Route_simplify()) %>% 
  clipr::write_clip()

sum(the1$点数)

# AB不重叠 -----
## Regular ====
indeAB <- GeneralPlanning(
  n_pts = 198, flex = .9,
  target = "综合资源",
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B|——集市西1——马卡尼|——集市顶8——集市顶7"
)

indeAB %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

indeAB$点数 %>% sum()

## Comfort ----
indeAB_comfort <- GeneralPlanning(
  n_pts = 198,
  pun_comf = 10,
  target = "综合资源",
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B|——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$"
)

indeAB_comfort %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

# MultiPlayer----
## 4P ----
`4P` <- GeneralPlanning(
  n_pts = 99,
  reuse12 = TRUE,
  pun_comf = 4,
  pun_map = 4,
  ban_P = paste(
    c(
      "记忆2|灵魂5|格式塔",
      "集市|蛋壳屋|海景房",
      "权杖1|秘仪阁1|秘仪北C|秘仪北B|鳄王窟4|祝祭3|赤王1",
      "越石村|清籁丸"
    ),
    collapse = "|"
  ),
  flex = .996
) 

`4P` %>% 
  select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(str_remove_all(路线,"rep")))) %>% 
  clipr::write_clip()

## 4PAB ----
# 踏鞴沙25+桓纳兰那13+
pick.4p <- "兰房4|——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$"

#度假村48+赤王陵40+清籁丸30
#发条工坊25+奥奇卡20+居尔城18
ban.4p <- str_glue("
记忆2|灵魂5|格式塔|\\
集市|蛋壳屋|海景房|权杖1|秘仪阁1|秘仪北C|秘仪北B|鳄王窟4|祝祭3|赤王1|越石村|清籁丸|\\
岩道|北水文|工坊|奥奇卡神像|神像滩|丙壁|乙壁|居尔东2|居尔西1
")

AB_4P <- GeneralPlanning(
  n_pts = 198, 
  reuse12 = TRUE, pun_comf = 4, pun_map = 4,
  ban_P = ban.4p,
  pick_P = pick.4p,
  no_2nd_use = "",
) 

AB_4P %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(str_remove_all(路线,"rep")))) %>% 
  clipr::write_clip()

## 2PAB----
# 踏鞴沙25+清籁丸20
pick.2p <- "——营地1——清籁丸1|——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$"

#度假村48
#赤王陵40
ban.2p <- str_glue("
记忆2|灵魂5|格式塔|\\
集市|蛋壳屋|海景房|权杖1|秘仪阁1|秘仪北C|秘仪北B|鳄王窟4|祝祭3|赤王1
")

AB_2P <- GeneralPlanning(
  n_pts = 198, flex = .88,
  reuse12 = TRUE, pun_comf = 4, pun_map = 4,
  ban_P = ban.2p,
  pick_P = pick.2p,
  no_2nd_use = "",
) 

AB_2P %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(str_remove_all(路线,"rep")))) %>% 
  clipr::write_clip()

# MOREMORA ----
mora <- GeneralPlanning(
  target = "综合资源", flex = .999,
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——集市顶8|—集市西1——马卡尼"
)

mora %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

sum(mora$点数)

used_pts <- mora$路线 %>% str_split("——") %>% map(~.x[-1]) %>% 
  list_c() %>% paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

# 全12小时 ----
non_fast_pts <- PointData %>% filter(刷新周期=="24") %>% pull(点位) %>% 
  str_remove_all("rep$") %>% unique() %>% paste0(collapse = "|")

FastRefresh <- GeneralPlanning(
  target = "综合资源", flex = .8,
  ban_P = sprintf("鸡屁股|%s",non_fast_pts),
  drop_least_eff = FALSE
)

FastRefresh %>% 
  select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()
