# RNAseqTool Refactor - Context

## 项目概述

RNAseqTool 是一个 RNA-seq 数据分析可视化工具，原为 R Shiny (golem + bs4Dash) 实现。
现重构为 React 前端 + R 后端(plumber API) + Go launcher 的桌面应用架构。

## 目标用户

湿实验室研究人员，零配置、双击即用。

## 核心架构

### 前端 (React + Vite)
- 纯 SPA，不做图表绘制
- 后端出图(SVG) → 前端 `<img>` 嵌入
- 表格用组件库渲染，下载用 xlsx
- 状态管理: Zustand
- UI 库: shadcn/ui + Tailwind CSS

### 后端 (R plumber)
- REST API，监听 localhost:随机端口
- 分析计算 + 出图(SVG/PNG/PDF/TIF)
- serve 前端静态文件

### 启动器 (Go)
- 检测系统 R → 有则用系统 R，无则用便携版 R
- 首次启动 renv restore 安装依赖
- 启动 plumber → 打开浏览器
- 关闭时清理 R 进程

## 模块依赖关系

```
PCA          ← norm矩阵 + sampleInfo（独立）
DESeq2       ← raw counts + sampleInfo（独立，核心枢纽）
  ├── Volcano   ← DESeq2 结果
  ├── Enrich    ← DESeq2 结果(Up/Down基因) 或 直接粘贴基因列表
  ├── GSEA      ← DESeq2 结果(geneList) 或 直接上传
  ├── GeneTrend ← norm矩阵 + sampleInfo + DESeq2 结果(筛选基因)
  └── WGCNA     ← norm矩阵 + sampleInfo + trait + DESeq2 结果(筛选基因)
```

## 数据格式

| 数据类型 | 格式 |
|---------|------|
| 表达矩阵(raw) | CSV/XLSX: Gene, Sample1, Sample2, ... (整数) |
| 表达矩阵(norm) | CSV/XLSX: Gene, Sample1, Sample2, ... (TPM/FPKM/CPM) |
| 样本信息 | CSV/XLSX: Sample, Group, ... |
| DESeq2结果 | Gene, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj, change |
| GSEA geneList | Gene, log2FoldChange (降序) |
| WGCNA trait | Sample, trait1, trait2, ... |

## Workspace 设计

- 每步分析独立，workspace 保存分析结果 + 绘图参数
- 存储格式：RDS (R原生序列化)
- 包含：分析结果对象 + ggplot绘图对象 + 所有参数
- 用户可保存/加载 workspace (.rds 文件)

## 可视化方案

- 后端 ggplot2 出图，输出 SVG（默认）
- 提供 PDF / TIF / PNG 下载
- 绘图对象保存为 RData，用户可加载后修改参数重新出图
- 修改参数 → 前端发请求 → 后端重新出图 → 前端刷新

## API 通信

- 文件上传：POST 文件到 /api/upload
- 长时任务：异步轮询 (POST task → 返回 task_id → 轮询 /api/status/:id)
- 绘图：POST /api/plot/:type + 参数 → 返回 SVG

## 项目目录结构

```
RNAseqTool/
├── frontend/           # React + Vite
│   ├── src/
│   │   ├── pages/      # 各分析模块页面
│   │   ├── components/ # 通用组件(文件上传、数据表格、参数面板)
│   │   ├── api/        # API 调用封装
│   │   └── stores/     # 状态管理(workspace、分析结果)
│   └── package.json
├── backend/            # R 后端
│   ├── R/              # 分析函数(复用现有 fct_*.R)
│   ├── api/            # plumber API 路由
│   ├── data/           # 示例数据
│   └── DESCRIPTION     # R 包依赖
├── launcher/           # Go 启动器
│   └── main.go
└── README.md
```

## 构建流程

```bash
# 开发
cd frontend && npm run dev
cd backend && Rscript run.R

# 构建
cd frontend && npm run build
cp -r frontend/dist backend/inst/www/
cd launcher && go build -o rnaseqtool
```

## R 依赖管理

- renv 管理包版本，lockfile 锁定依赖
- Launcher 启动时检测系统 R → 有则用 → 检查包完整性 → 缺则安装
- 无系统 R → 使用便携版 R + renv restore
- 安装包可选包含预装 R 环境（减少首次启动时间）
