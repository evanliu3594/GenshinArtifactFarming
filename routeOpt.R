# init ====
{
  rm(list = ls());gc();gc();gc()
  library(lpSolve)
  source("common_utils.R")
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

  # PreRoute <- getRouteData()
  PreRoute <- getRouteData(...) 

  if (nchar(no_2nd_use) > 0) {
    ban_2nd_use <- no_2nd_use %>% str_replace_all("([^\\|]+)(\\||$)", "\\1rep\\2")
    PreRoute <- PreRoute %>% filter(str_detect(路线, ban_2nd_use))
  }
  
  if (nchar(ban_P) > 0) {
    PreRoute <- PreRoute %>% filter(!str_detect(路线, ban_P))
  }

  if (nchar(pick_P) > 0) {
    pickedRoute <- PreRoute %>% filter(str_detect(路线, pick_P))
    picked_pts <- pickedRoute$路线 %>% str_split("——") %>% 
      map(~.x[-1]) %>% list_c() %>% paste(collapse = "|")
    pollingRoute <- PreRoute %>% filter(!str_detect(路线, picked_pts))
  } else {
    pollingRoute <- PreRoute
  }
  
  if (drop_least_eff) {
    pollingRoute <- pollingRoute %>% filter(狗粮效率 > quantile(狗粮效率, .3)) 
  }

  pollingPoint <- pollingRoute$路线 %>% str_split("——") %>% 
    map(~tail(.x, -1)) %>% list_c() %>% unique()

  MountVec <- str_detect(pollingRoute$路线, "锚点") * 1L

  constMat <- pollingPoint %>% map(~ str_detect(pollingRoute$路线, .x) * 1L) %>% 
    do.call(rbind, .) %>% `dimnames<-`(list(pollingPoint, pollingRoute$路线))

  obj1 <- pollingRoute %>% pull(!!target)
  obj2 <- pollingRoute %>% pull(time_pun)
  pts <- pollingRoute %>% pull(点数)

  lp_solv1 <- lp(
    direction = "max",
    objective.in = obj1,
    const.mat = rbind(pts, MountVec, constMat),
    const.dir = c("=", "<=", rep("<=", nrow(constMat))),
    const.rhs = c(n_pts, as.integer(use.mount), rep(1, nrow(constMat))),
    all.bin = TRUE
  )

  lp_solv2 <- lp(
    direction = "min",
    objective.in = obj2,
    const.mat = rbind(pts, obj1, MountVec, constMat),
    const.dir = c("=", ">=", "<=", rep("<=", nrow(constMat))),
    const.rhs = c(
      n_pts,
      lp_solv1$objval * flex,
      as.integer(use.mount),
      rep(1, nrow(constMat))
    ),
    all.bin = TRUE
  )

  bind_rows(
    pollingRoute[as.logical(lp_solv2$solution), ],
    pickedRoute
  )

}

# N+1 ----
## Regular ====
Regu_N <- GeneralPlanning(
  target = "综合资源", flex = .6, 
  t_teleport = 8,
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B"
)

Regu_N %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

used_pts <- Regu_N$路线 %>% str_split("——") %>% map(~.x[-1]) %>% list_c() %>% 
  paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

Regu_1 <- GeneralPlanning(
  reuse12 = TRUE,  flex = .88,
  target = "综合资源",
  ban_P = sprintf("记忆2|灵魂5|格式塔|%s", used_pts),
  pick_P = "——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$"
)

Regu_1 %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = str_remove_all(路线,"rep") %>% hex_decode() %>% Route_simplify()) %>% 
  clipr::write_clip()

## Comfort ====
Comf_N <- GeneralPlanning(
  target = "综合资源", Map = "no圣山",
  t_teleport = 8, pun_comf = 6,
  ban_P = "记忆2|灵魂5|格式塔|集市顶8|集市6",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B"
)

Comf_N %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

used_pts <- Comf_N$路线 %>% str_split("——") %>% map(~.x[-1]) %>% list_c() %>% 
  paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

Comf_1 <- GeneralPlanning(
  reuse12 = TRUE,  flex = .9,
  pun_map = 6, pun_comf = 6, 
  target = "综合资源",
  ban_P = sprintf("记忆2|灵魂5|格式塔|%s", used_pts),
  pick_P = "——集市西1——马卡尼|——集市顶8——集市顶7"
)

Comf_1 %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = str_remove_all(路线,"rep") %>% hex_decode() %>% Route_simplify()) %>% 
  clipr::write_clip()

sum(the1$点数)

# AB不重叠 -----
## Regular ====
Natla_A <- GeneralPlanning(
  n_pts = 99, flex = .999, Map = "纳塔",
  target = "综合资源",
  pick_P = "——集市西1——马卡尼|——集市顶8——集市顶7"
)

Natla_A %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

used_pts <- Natla_A$路线 %>% str_split("——") %>% map(~.x[-1]) %>% list_c() %>% 
  paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

noNatlan_B <- GeneralPlanning(
  n_pts = 99, flex = .86, Map = "no纳塔|圣山|层岩|旧日之海",
  target = "综合资源", t_teleport = 8,
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B"
)

noNatlan_B %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

## Comfort ----
Comf_indeAB <- GeneralPlanning(
  n_pts = 198, flex = .85,
  pun_comf = 8,
  target = "综合资源",
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B|——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$"
)

Comf_indeAB %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

# MultiPlayer----
## 4PMaxMora ----
`4P` <- GeneralPlanning(
  n_pts = 99,
  ban_P = "记忆2|灵魂5|格式塔|谐律院",
  pick_P = paste(c(
    "集市峡|集市顶8|集市西1|马卡尼|蛋壳屋|海景房",
    "权杖1|秘仪阁1|秘仪北C|秘仪北B|鳄王窟4|祝祭1|赤王1",
    "越石村|清籁丸"
  ), collapse = "|"),
  flex = .99
)

`4P` %>% 
  select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(str_remove_all(路线,"rep")))) %>% 
  clipr::write_clip()


## 2PAB----
# 秘仪圣殿40卡位 + 度假村48卡位
`2P` <- GeneralPlanning(
  n_pts = 198, 
  pun_map = 6, pun_comf = 6, 
  reuse12 = TRUE, 
  ban_P = "记忆2|灵魂5|格式塔",
  pick_P = str_glue("
  ——狸猫3——雷1|传送点——熔炉1|——高台1——炉心6——炉心3|——洞1$|营地1——清籁丸1|\\
  集市峡|集市顶8|集市西1|马卡尼|蛋壳屋|海景房|\\
  权杖1|秘仪阁1|秘仪北C|秘仪北B|鳄王窟4|祝祭3|赤王1
  ")
)

`2P` %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(str_remove_all(路线,"rep")))) %>% 
  clipr::write_clip()

# MOREMORA ----
mora <- GeneralPlanning(
  target = "综合资源", flex = .999,
  Map = "璃月|须弥|纳塔", ban_P = "精石",
  pick_P = "——集市顶8|—集市西1——马卡尼"
)

mora %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = Route_simplify(hex_decode(路线))) %>% clipr::write_clip()

used_pts <- mora$路线 %>% str_split("——") %>% map(~.x[-1]) %>% 
  list_c() %>% paste(collapse = "(?!rep)|") %>% paste0("(?!rep)")

mora_relay <- GeneralPlanning(
  reuse12 = TRUE, 
  Map = "no度假村|圣山|旧日之海",
  target = "综合资源", flex = .8,
  ban_P = sprintf("记忆2|灵魂5|格式塔|%s", used_pts),
  pick_P = "——秘仪阁1|——秘仪北C|——秘仪北B"
)

mora_relay %>% select(-time_tp,-time_pun,-综合资源,-狗粮效率) %>% 
  mutate(路线 = str_remove_all(路线,"rep") %>% hex_decode() %>% Route_simplify()) %>% 
  clipr::write_clip()

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
