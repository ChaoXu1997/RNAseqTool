# RNAseqTool 重构 — PRD

## Problem Statement

RNAseqTool 当前是一个基于 R Shiny (golem + bs4Dash) 的 RNA-seq 数据分析可视化工具。用户（湿实验室研究人员）需要一个更现代化、易用的桌面应用，能够：
- 双击即用，无需配置 R 环境
- 每个分析模块可独立运行，也可串联成完整流程
- 修改绘图参数时不需要重新跑分析
- 保存/加载分析工作区

现有 Shiny 版本的痛点：
- 依赖 R 环境，湿实验室用户不会配置
- UI 不够现代化，交互体验差
- 修改绘图参数需要重新跑整个分析
- 模块之间没有数据传递，用户需要手动下载再上传

## Solution

重构为三层架构的桌面应用：
1. **React + Vite 前端** — 现代 SPA，后端出图 SVG 嵌入显示
2. **R plumber 后端** — REST API，承担所有计算和绘图
3. **Go launcher** — 启动器，检测/配置 R 环境，启动后端并打开浏览器

核心体验：用户双击 → 自动配置 → 浏览器打开 → 选择分析模块 → 上传数据 → 获得结果 → 保存工作区

## User Stories

1. 作为湿实验室研究人员，我希望双击应用图标就能启动工具，这样我不需要学习命令行或安装 R 环境
2. 作为湿实验室研究人员，我希望能加载示例数据快速体验所有分析模块，这样我能决定是否上传自己的数据
3. 作为湿实验室研究人员，我希望在首页看到所有分析模块（PCA、DESeq2、Volcano、Enrich、GSEA、GeneTrend、WGCNA），这样我能选择想做的分析
4. 作为湿实验室研究人员，我希望每个分析模块都能独立上传对应的数据，这样我只做富集分析时不需要从 raw counts 开始
5. 作为湿实验室研究人员，我希望 DESeq2 分析完成后能一键跳转到 Volcano 分析，这样我不需要手动下载再上传
6. 作为湿实验室研究人员，我希望下游模块（Volcano、Enrich、GSEA、GeneTrend、WGCNA）能从 workspace 自动加载 DESeq2 的结果，这样完整流程更顺畅
7. 作为湿实验室研究人员，我希望能修改绘图参数（颜色、阈值、标题等）而不需要重新跑分析，这样我能快速调整图表风格
8. 作为湿实验室研究人员，我希望绘图结果能保存为 RData 对象，这样我下次可以加载后直接修改参数重新出图
9. 作为湿实验室研究人员，我希望导出图表支持 SVG、PDF、TIF、PNG 格式，这样我能直接用于论文或 PPT
10. 作为湿实验室研究人员，我希望导出数据表格为 XLSX 格式，这样我能在 Excel 中进一步处理
11. 作为湿实验室研究人员，我希望能保存和加载整个工作区（RDS 文件），这样我能中断后继续分析
12. 作为湿实验室研究人员，我希望 WGCNA 支持从 DESeq2 筛选基因后分析，也支持上传完整 norm 矩阵，这样我有灵活的输入方式
13. 作为实验室管理员，我希望应用能自动检测已安装的 R 和 R 包，这样实验室电脑上已有的 R 环境能被复用
14. 作为湿实验室研究人员，我希望应用首次启动时自动配置所需 R 包，显示进度条，这样我不需要手动操作

## Implementation Decisions

### 前端模块

- **技术栈**: React 18 + Vite + TypeScript
- **状态管理**: Zustand
- **UI 库**: shadcn/ui + Tailwind CSS
- **路由**: react-router-dom
- **API 调用**: axios + react-query (异步任务轮询)

**页面结构**:
- Home: 模块选择入口 + 示例数据加载
- PCA: norm 矩阵 + sampleInfo → SVG 散点图
- DESeq2: raw counts + sampleInfo → 差异分析结果表格 → 下游跳转按钮
- Volcano: DESeq2 结果 → SVG 火山图
- Enrich: 基因列表/DESeq2 结果 → 富集分析 barplot/dotplot
- GSEA: geneList/DESeq2 结果 → GSEA enrichment plot
- GeneTrend: norm 矩阵 + sampleInfo + DESeq2 筛选 → Mfuzz 聚类趋势图
- WGCNA: norm 矩阵 + sampleInfo + trait + DESeq2 筛选(可选) → 热图 + 散点图

**每个页面通用组件**:
- DataUpload: 文件上传 + 从 workspace 加载
- ParamPanel: 分析/绘图参数配置
- PlotDisplay: SVG 图片显示 + loading 状态
- ResultTable: 数据表格 + XLSX 下载
- ExportButtons: SVG/PDF/TIF/PNG 下载

### 后端模块

- **框架**: R plumber
- **API 设计**: RESTful，每个分析模块对应一组 API
- **文件上传**: multipart/form-data → 临时目录
- **长时任务**: 异步轮询 (task_id → progress → result)
- **出图**: ggplot2 → SVG (默认) + PDF/TIF/PNG 可选

**API 路由**:
- `POST /api/data/upload` — 上传数据文件
- `POST /api/data/demo` — 加载示例数据
- `POST /api/analyze/pca` → task_id → `GET /api/task/:id`
- `POST /api/analyze/deseq2` → task_id
- `POST /api/analyze/enrich` → task_id
- `POST /api/analyze/gsea` → task_id
- `POST /api/analyze/genetrend` → task_id
- `POST /api/analyze/wgcna` → task_id
- `POST /api/plot/:type` — 根据参数生成/重新生成图表
- `POST /api/workspace/save` — 保存 workspace (RDS)
- `POST /api/workspace/load` — 加载 workspace (RDS)
- `GET /api/export/:format` — 导出图表/数据

**分析与绘图分离**:
- 分析函数(现有 fct_*.R)保存中间结果为 RData
- 绘图函数从 RData 加载结果 + 用户参数 → 生成 SVG
- 绘图对象(ggplot)连同参数保存为 RData，支持重新加载修改

### 启动器模块

- **语言**: Go
- **R 检测**: 检查系统 PATH → 版本验证 → 包完整性检查
- **依赖管理**: renv lockfile
- **行为**: 启动 plumber → 打开浏览器 → 托盘图标 → 关闭清理

### Workspace 模块

- **格式**: RDS (R list 对象)
- **内容**: 分析结果 + ggplot 绘图对象 + 所有参数
- **版本控制**: workspace 内存储版本号，加载时检查兼容性

### 数据流转

模块间数据传递通过 workspace 实现：
1. DESeq2 完成 → 结果自动存入 workspace
2. 下游模块(Volcano/Enrich/GSEA/GeneTrend/WGCNA)检测 workspace 中是否有 DESeq2 结果
3. 有则显示"从 DESeq2 结果加载"按钮，无则要求用户上传

## Testing Decisions

- **前端测试**: Vitest + React Testing Library，测试用户交互流程（上传 → 分析 → 出图 → 导出）
- **后端测试**: R testthat，测试分析函数的输入输出正确性
- **E2E 测试**: Playwright，测试完整用户流程（加载示例数据 → 跑 DESeq2 → 跳转 Volcano → 导出）
- **重点测试**:
  - 各分析模块的输入输出边界（文件格式校验）
  - workspace 保存/加载的一致性
  - 长时任务的超时和错误处理
  - 跨模块数据传递的正确性

## Out of Scope

- 多用户/协作功能
- 云端部署/远程访问
- 实时协作编辑
- 除 RNA-seq 外的其他组学分析
- 基因组浏览器/IGV 集成
- 自动化 pipeline 编排（只做手动分析）

## Further Notes

- 现有 R 绘图代码（fct_PCA_plot.R、fct_Volcano.R 等）基本可复用，主要修改点在输入输出接口
- 前端不绘制任何图表，所有图表由后端 ggplot2 生成
- 湿实验室用户的操作习惯：更习惯"点击 → 看到结果"，所以 loading 状态和进度条很重要
- R 包依赖的编译时间是首次启动的主要瓶颈，renv lockfile 需要仔细维护

## QA Strategy

- 每个分析模块完成后，由用户（湿实验室研究人员）进行实际数据测试
- 导出的 SVG/PDF 需要在 PPT/论文排版中验证质量
- 安装包在干净的 Windows/macOS 虚拟机上测试首次启动体验
- workspace 的保存/加载需要在不同版本间测试兼容性
