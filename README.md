# 原神圣遗物狗粮路线优化脚本
## 简介 | Introduction
本项目是一个用于优化原神圣遗物狗粮点采集路线的 R 语言脚本。通过混合整数规划（MIP）算法，结合自定义参数和地图点位数据，自动计算出最优的采集路线，最大化狗粮、摩拉等资源的获取效率。脚本支持多种自定义需求，如排除/指定点位、舒适度惩罚、多人协作等场景。

This project is an R script designed to optimize the farming routes for artifact materials in Genshin Impact. By using a mixed-integer programming (MIP) algorithm, combined with custom parameters and map location data, it automatically calculates the optimal collection route to maximize the efficiency of gathering materials like enhancement fodder and Mora. The script supports various customization options, such as excluding or specifying certain locations, applying comfort penalties, and accommodating multiplayer collaboration scenarios.

主要的数据收集工作由我独立完成，并由NGA众坛友补充，在此感谢！特别感谢 @彼音星垠 对本项目的大力支持！

Main data collections were done by myself, as well as some contributions from NGA user @彼音星垠.

基准速度测试基于全四星队伍，如罗风早亚，5.2以来改为罗欧早亚/罗欧早伊，记录了每一条圣遗物路线的跑图耗时（可在data文件夹中的路线设计文件查看）

Benchmark speed test was done through using all 4- star character team (e.g., Rosaria-Kazuha-Sayu-Kaeya), logged speed information for each point-route at `./data/路线设计.xlsx`

## 特性 | Features
支持自定义采集目标（狗粮经验、摩拉+狗粮经验）
支持排除/指定点位、路线
支持舒适度参数、地图惩罚参数调整
支持多种采集模式（单人、多人、AB路线不重叠等）
自动处理点位刷新周期（12小时/24小时）
结果可直接复制到剪贴板，便于分享和使用

Supports custom collection targets (experience materials, or Mora + experience materials)  
Allows exclusion or specification of points and routes  
Supports adjustment of comfort parameters and map penalty parameters  
Offers multiple collection modes (solo, multiplayer, non-overlapping AB routes, etc.)  
Automatically manages point refresh cycles (12 hours / 24 hours)  
Results can be copied directly to the clipboard for easy sharing and use

## 依赖 | Dependency
本程序基于R语言编写，请确保已安装以下 R 包：

This program is written in R. Please ensure that the following R packages are installed:

```R
install.packages(c(
  "tidyverse",
  "readxl",
  "ompr",
  "ompr.roi",
  "ROI.plugin.glpk",
  "clipr"
))
```
## 数据说明 | Data Description
- data/点位描述.xlsx：
    - 包含所有地图点位的详细信息
    - Includes detailed information for all map locations.
- data/路线设计.xlsx：
    - 包含预设的采集路线设计
    - Includes detailed information for all map locations.

## 核心程序说明 | Optim-func Usage

### `GeneralPlanning()`

#### n_pts:
> 指在ban/pick点位路线之外需要采集多少点，默认为99。

> Refers to how many points need to be collected outside the ban|pick location route, with a default value of 99.
#### ...:
> 额外传递给`getRouteData()`函数的参数，具体说明见下一节。

> Additional parameters passed to the `getRouteData()` function; detailed explanation can be found in the next section.
#### use.mount:
> 布尔值，是否使用口袋锚点，默认不使用。

> Boolean value indicating whether to use pocket anchors; default is not to use them.
#### target:
> 字符型，表明本次计算的优化目标，要么为"狗粮经验"、表明只在乎获取的圣遗物狗粮（默认），要么为"综合收益"、表示综合考虑圣遗物狗粮以及摩拉获取。

> Character type, indicating the optimization goal for this calculation: either "狗粮经验," meaning only the acquisition of artifact experience materials is considered (default), or "综合收益" which represents a comprehensive consideration of both artifact experience materials and Mora acquisition.
#### ban_P:
> 字符型，用于指定不想采用的圣遗物狗粮点位，不同点位之间用`|`分割，支持正则。圣遗物点位名称需要与`- data/点位描述.xlsx`中记录的一致。

> Character type, used to indicate artifact fodder positions to avoid; different positions are separated by `|` and support regular expressions. The artifact position names must match those recorded in `data/点位描述.xlsx`.
#### pick_P:
> 字符型，用于指定必选的圣遗物狗粮点位，一般用于指定收尾路线。不同点位之间用`|`分割，支持正则。圣遗物点位名称需要与`- data/点位描述.xlsx`中记录的一致。注意，如果你指定了收尾路线以外的点位，需要对应修改`n_pts`参数以免计算结果出错。

> Character type, used for mandatory artifact fodder positions, generally for specifying finishing routes. Different positions are separated by `|` and support regular expressions. The artifact position names must match those recorded in `data/点位描述.xlsx`. Note that if you specify positions outside the finishing route, you need to adjust the `n_pts` parameter accordingly to avoid calculation errors.
#### no_2nd_use:
> 字符型，用于指定不需要重复使用的圣遗物狗粮快速刷新点位，一般用于计算配套副路线时，过滤掉不想重复使用的段刷新周期点位。

> Character type, used to specify artifact fodder quick refresh points that do not need to be reused. It is generally used when calculating supporting sub-routes to filter out refresh cycle points that you do not want to reuse.
#### flex:
> 数值型，0.5~1之间，用于给出最高速度路线的收益约束。这涉及到本优化程序的逻辑：首先根据指定过滤条件计算出收益最大的跑法，然后根据不同flex下的收益宽容度（如80%最大收益），挑选对应收益目标下的最高速路线。默认flex水平为0.8。

> Numeric type, ranging from 0.5 to 1, used to set the profit constraint for the highest speed route. This relates to the logic of the optimization program: first, it calculates the route with the maximum profit based on specified filtering criteria; then, according to the profit tolerance under different flex settings (e.g., 80% of the maximum profit), it selects the highest speed route that meets the corresponding profit target. By default 0.8.
#### drop_least_eff:
> 布尔值，是否排除掉30%百分位以下效率的路线，默认过滤以精简计算。但当备选点数特别少的时候，需要改成`FALSE`

> Boolean value that determines whether to exclude routes with efficiency below the 30th percentile. By default, filtering is enabled to streamline calculations. However, when the number of candidate points is very limited, this should be set to `FALSE`.

### `getRouteData()`
#### pun_comf
> 舒适度惩罚，数值型，默认为4，将旁边有不舒适的要素（混杂点、怪物、读书点）的点位路线进行时间降权的参数，越高表明越不想碰上不舒适要素，越小表明不在乎这些不舒适点。

> Comfort penalty, numerical type, default value is 4. This parameter reduces the time weight of routes passing near uncomfortable elements (such as cluttered spots, monsters, or reading points). A higher value indicates a stronger aversion to encountering uncomfortable elements, while a lower value means less concern about these discomfort points.

#### pun_map
> 舒适度惩罚，数值型，默认为4，将处在独立地图中的点位路线进行时间降权的参数，越高表明越不想使用切换功能访问独立地图，越小表明不在乎。

> Comfort penalty, numeric type, default value is 4. This parameter applies a time penalty to routes passing through points located on separate maps. A higher value indicates a stronger preference to avoid using the switching feature to access separate maps, while a lower value means less concern about it.

#### t_teleport
> 预计传送时间，数值型，默认为6，为每一条路线进行效率计算时考虑的传送所花时间。越高表明传送时间越长，因此短链路线的效率会受影响，越小表明不在乎传送时间。

> Estimated transmission time, numeric type, default is 6. This value represents the time taken for transmission considered in the efficiency calculation of each route. A higher value indicates longer transmission time, which negatively impacts the efficiency of shorter routes, while a lower value means transmission time is less of a concern.
#### block_lvl
> 封锁水平，由我本人经验设定的访问这些圣遗物狗粮点的难易程度，比如说往昔的桓纳兰那这边的几个点都需要做完森林书才能够开启地图进入，因此封锁水平是10，其他没要求的露天点位封锁等级为0.

> The lockdown level is determined based on my personal experience regarding the difficulty of accessing these artifact farming spots. For example, several locations around the former Huannalan require completing the Forest Book quest before the map can be accessed, so the lockdown level is set to 10. Other open-air spots without such requirements have a lockdown level of 0.
#### reuse12
> 布尔值，反映了是否想重复使用短周期点位，仅当在配合计算副路线时有用。

> Boolean value indicating whether to reuse short-cycle points; only useful when calculating auxiliary routes.

## Supporting Information
具体点位名称、位置、刷新时间、掉落等信息请见[我的NGA版头贴](https://nga.178.com/read.php?tid=27875210)

See detailed point locations at (sry for only in Chinese): [My NGA Post](https://nga.178.com/read.php?tid=27875210)

一些现成的跑图路线攻略视频请见我的[Bilibili主页](https://space.bilibili.com/1897138)

See some prepared instruction videos for high-efficient farming route at my Blibili Spaces(sry for only in Chinese): [@游侠Evan](https://space.bilibili.com/1897138)

