# GIArtifactFarmingRoutePlanning
这是一个用来规划原神圣遗物狗粮路线的脚本工具。

This script is for planning your customized artifact farming route in Genshin Impact.

# Gener
基于整数线性规划求解器，我提供了一个用于规划最高效原神圣遗物跑图路线的工具。

Based on an integer linear programming solver, I provide a planning tool for predict the most high-efficient(i.e., least time and most loot) route to artifats farming in Genshin Impact game.

主要的数据收集工作由我独立完成，并由NGA众坛友补充，在此感谢！特别感谢 @彼音星垠 对本项目的大力支持！

Main data collections done by myself, as well some contributions form NGA user @彼音星垠.

基准速度测试基于罗万早亚配队，记录了每一条圣遗物路线的跑图耗时（可在Data文件夹中的路线设计文件查看）

Benchmark speed test was done through using team Rosaria-Kazuha-Sayu-Kaeya, logged speed information for each point-route at `./Data/路线设计.xlsx`

# Supporting Information
具体点位名称、位置、刷新时间、掉落等信息请见[我的NGA版头贴](https://nga.178.com/read.php?tid=27875210)

See detailed point locations at (sry for only in Chinese)：[My NGA Post](https://nga.178.com/read.php?tid=27875210)

一些现成的跑图路线攻略视频请见我的[Bilibili主页](https://space.bilibili.com/1897138)

See some prepared instruction videos for high-efficient farming route at my Blibili Spaces(sry for only in Chinese)：[@游侠Evan](https://space.bilibili.com/1897138)

## Requirements for this script:
- R
- Rstudio
- tidyverse package
- Gurobi Solver

## Usage:
1. Run `./圣遗物狗粮.Rproj`

2. Load data

3. Load utility functions

4. Run funtion with your customized parameters(numbers of points, prefered routes, wether uses a Portable Waypoint, etc.)

5. Use `write_route = TURE` in the planning functions to save the planned route to `./Results/`
