library(tidyverse)
library(readxl)

hex_encode <- \(x) {
  str_replace_all(x, "(1[0-5])(?=—|\\||$)", \(num) as.hexmode(as.integer(num)))
}

hex_decode <- \(x) {
  str_replace_all(x, "([a-f])(?=—|\\||$)", \(hex) as.integer(as.hexmode(hex)))
}

Route_simplify <- \(Route) {
  Route %>%
    strsplit("——") %>%
    map_chr(
      ~ {
        .x %>%
          str_match('([\u4e00-\u9fa5]+[A-Z]?)([0-9]?-?[0-9]?)') %>%
          as.data.frame %>%
          mutate(
            V4 = if_else(V2 == lag(V2, 1) %>% replace_na(""), "", V2)
          ) %>%
          str_glue_data("{V4}{V3}") %>%
          str_c(collapse = "—")
      }
    )
}

Route_restore <- \(Route) {
  Route %>%
    strsplit("—") %>%
    map_chr(
      ~ {
        .x %>%
          str_match('([\u4e00-\u9fa5]*[A-Z]?)([0-9]?-?[0-9]?)') %>%
          as.data.frame %>%
          mutate(V2 = na_if(V2, "")) %>%
          fill(V2) %>%
          str_glue_data("{V2}{V3}") %>%
          str_c(collapse = "———")
      }
    )
}

getPointData <- function(Map = "") {
  PointData <- lapply(
    c("蒙德璃月", "稻妻", "须弥", "枫丹", "纳塔"),
    \(x) {
      read_xlsx(
        "data/artifact-drop-spots.xlsx",
        sheet = x,
        range = cell_cols("A:K"),
        col_types = "text"
      )
    }
  ) %>% 
    do.call(rbind, .) %>%
    rename(点位 = 编号) %>%
    filter(!is.na(主地图)) %>%
    mutate(
      点位 = str_remove_all(点位, "（.+）") %>% hex_encode(),
      across(c(数量, 掉落期望, 无摩拉, 有摩拉, 封锁等级), as.numeric),
    )
  
  if (nchar(Map) > 0 && Map == "全部地表") {
    PointData %>% filter(str_detect(主地图, "蒙德|璃月|稻妻|须弥|枫丹|纳塔"))
  } else if (nchar(Map) > 0 && str_detect(Map, "^no")) {
    PointData %>% filter(!str_detect(主地图, str_remove(Map, "^no")))
  } else if (nchar(Map) > 0) {
    PointData %>% filter(str_detect(主地图, Map))
  } else {
    PointData
  }
}

getRouteData <- function(
  ...,
  pun_comf = 4,
  pun_map = 4,
  t_teleport = 6,
  block_lvl = 10,
  reuse12 = FALSE,
  map_switch = ""
) {
  # PointData <- getPointData(Map = "no层岩")
  PointData <- getPointData(...)

  Route_design <- read_xlsx('data/artifact-farming-routes.xlsx', range = cell_cols("A:D")) %>%
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

  Route_data_fast <- Route_data %>%
    filter(allFast) %>%
    mutate(路线 = str_replace_all(路线, "(——)([^——]+)", "\\1\\2rep"))

  if (reuse12) {
    bind_rows(Route_data, Route_data_fast) %>% select(-allFast)
  } else {
    Route_data %>% select(-allFast)
  }

}

# testget <- \(...) {
#   getRouteData(...)
# }

# testget(Map = "no层岩")
