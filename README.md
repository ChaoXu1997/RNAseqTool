# RNAseqTool

一个交互式 RNA-seq 数据分析可视化工具，面向湿实验室研究人员，零配置双击即用。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## 功能模块

| 模块 | 功能 | 输入 |
|------|------|------|
| **PCA** | 主成分分析，检测离群样本和批次效应 | norm 矩阵 + sampleInfo |
| **DESeq2** | 差异基因表达分析（核心枢纽） | raw counts + sampleInfo |
| **Volcano** | 火山图可视化差异表达结果 | DESeq2 结果 |
| **Enrich** | GO/KEGG 富集分析（ORA），支持 20 个物种、23 个数据库 | DESeq2 结果 或 基因列表 |
| **GSEA** | 基因集富集分析 | DESeq2 结果(geneList) 或 直接上传 |
| **GeneTrend** | 基因趋势分析（Mfuzz 模糊聚类） | norm 矩阵 + sampleInfo |
| **WGCNA** | 加权基因共表达网络分析 | norm 矩阵 + sampleInfo + trait |

## 模块依赖关系

```
PCA          ← norm矩阵 + sampleInfo（独立）
DESeq2       ← raw counts + sampleInfo（核心枢纽）
  ├── Volcano   ← DESeq2 结果
  ├── Enrich    ← DESeq2 结果(Up/Down 基因) 或直接粘贴基因列表
  ├── GSEA      ← DESeq2 结果(geneList) 或直接上传
  ├── GeneTrend ← norm矩阵 + sampleInfo + DESeq2 结果(筛选基因)
  └── WGCNA     ← norm矩阵 + sampleInfo + trait + DESeq2 结果(筛选基因)
```

## 架构

```
前端 (React + Vite)  ←→  后端 (R plumber API)  ←→  启动器 (Go)
     SPA 渲染              计算 + 出图(SVG)          检测 R + 启动
```

- 前端不做计算，后端 ggplot2 出 SVG → 前端 `<img>` 嵌入
- 长时任务异步轮询 (POST → taskId → 轮询 GET /api/task/:id)
- 绘图参数可实时调整，修改后重新出图
- Workspace 机制保存/加载分析状态（RDS 格式）
- 支持导出 SVG/PNG/PDF/TIF + ggplot RData

## 快速开始

### 一键启动（推荐）

```bash
chmod +x dev.sh
./dev.sh
```

浏览器打开 http://localhost:3000 即可使用。

其他命令：

```bash
./dev.sh --stop       # 停止全部服务
./dev.sh --backend    # 只启动后端
./dev.sh --frontend   # 只启动前端
./dev.sh --status     # 查看服务状态
./dev.sh --logs       # 实时查看日志
```

### 手动启动

```bash
# 后端（需要 R >= 4.1）
cd backend
Rscript run.R         # http://localhost:8000

# 前端（需要 Node.js >= 18）
cd frontend
npm install
npx vite --port 3000  # http://localhost:3000
```

## 数据格式

| 类型 | 格式 |
|------|------|
| 表达矩阵(raw) | CSV/XLSX: Gene, Sample1, Sample2, ... (整数) |
| 表达矩阵(norm) | CSV/XLSX: Gene, Sample1, Sample2, ... (TPM/FPKM/CPM) |
| 样本信息 | CSV/XLSX: Sample, Group, ... |
| DESeq2 结果 | Gene, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj, change |
| GSEA 基因列表 | Gene, log2FoldChange (降序) |
| WGCNA 性状数据 | Sample, trait1, trait2, ... |

## 示例数据

项目内置示例数据（`backend/data/` 目录），点击界面"加载示例数据"按钮即可加载测试。

## 构建桌面应用

```bash
chmod +x build.sh
./build.sh
```

产物在 `dist/` 目录下，包含 Windows/macOS/Linux 三平台安装包。

## 目录结构

```
RNAseqTool/
├── dev.sh              # 开发一键启动脚本
├── build.sh            # 多平台构建脚本
├── frontend/           # React + Vite + Tailwind CSS + shadcn/ui
│   ├── src/
│   │   ├── pages/      # 各分析模块页面
│   │   ├── components/ # 通用组件（文件上传、数据表格、参数面板）
│   │   ├── api/        # API 调用封装
│   │   └── stores/     # Zustand 状态管理
│   └── package.json
├── backend/            # R 后端
│   ├── R/              # 分析函数 (fct_*.R)
│   ├── api/            # plumber API 路由
│   ├── data/           # 示例数据 (.rda)
│   └── DESCRIPTION     # R 包依赖
├── launcher/           # Go 启动器
│   └── main.go
└── renv.lock           # R 依赖锁文件
```

## R 依赖

主要 R 包（通过 BiocManager 安装）：

| 包 | 用途 |
|----|------|
| plumber | REST API 框架 |
| DESeq2 | 差异表达分析 |
| clusterProfiler | 富集分析 |
| enrichplot | 富集分析可视化 |
| msigdbr | MSigDB 基因集 |
| Mfuzz | 模糊聚类趋势分析 |
| WGCNA | 共表达网络分析 |
| ggplot2, ggrepel | 绑图引擎 |

详见 [INSTALL.md](INSTALL.md)。

## 许可证

MIT License — 详见 [LICENSE](LICENSE)。
