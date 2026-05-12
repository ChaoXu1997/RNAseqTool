import { useState, useCallback, useEffect } from 'react'
import { useWorkspaceStore } from '../stores/workspace'
import { runWGCNA, getWGCNAPlot, getTaskStatus } from '../api/client'
import { PlotSaveLoad } from '../components/PlotSaveLoad'

interface WGCNAStep {
  name: string
  label: string
  status: 'pending' | 'running' | 'done' | 'error'
  svg?: string
  result?: Record<string, unknown>
}

const WGCNA_STEPS: Array<{ name: string; label: string }> = [
  { name: 'sampletree', label: '样本聚类' },
  { name: 'power', label: 'Power 选择' },
  { name: 'network', label: '网络构建' },
  { name: 'module_trait', label: 'Module-Trait' },
]

export default function WGCNA() {
  // Store state
  const exprNorm = useWorkspaceStore((s) => s.exprNorm)
  const sampleInfo = useWorkspaceStore((s) => s.sampleInfo)

  const hasNormData = exprNorm !== null

  // Data source
  const [dataSource, setDataSource] = useState<'workspace' | 'upload'>(
    hasNormData ? 'workspace' : 'upload'
  )

  // File upload state
  const [exprFile, setExprFile] = useState<File | null>(null)
  const [traitFile, setTraitFile] = useState<File | null>(null)

  // Analysis parameters
  const [cutHeight, setCutHeight] = useState(15)
  const [abline, setAbline] = useState(0.8)
  const [power, setPower] = useState<number | null>(null)
  const [mergeCutHeight, setMergeCutHeight] = useState(0.25)
  const [nSelect, setNSelect] = useState(400)

  // Module-trait parameters
  const [selectedModule, setSelectedModule] = useState('')
  const [selectedTrait, setSelectedTrait] = useState('')

  // Steps state
  const [steps, setSteps] = useState<WGCNAStep[]>(
    WGCNA_STEPS.map((s) => ({ ...s, status: 'pending' }))
  )
  const [currentStep, setCurrentStep] = useState(0)

  // Loading / error
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)

  // Results
  const [moduleColors, setModuleColors] = useState<string[]>([])
  const [traitNames, setTraitNames] = useState<string[]>([])

  // Auto-switch when workspace data changes
  useEffect(() => {
    if (!hasNormData && dataSource === 'workspace') {
      setDataSource('upload')
    }
  }, [hasNormData, dataSource])

  // Poll for task completion
  const pollTask = useCallback(async (taskId: string) => {
    let attempts = 0
    const maxAttempts = 120 // 10 minutes

    while (attempts < maxAttempts) {
      await new Promise((resolve) => setTimeout(resolve, 5000))
      const status = await getTaskStatus(taskId)

      if (status.status === 'done') {
        return status.result as Record<string, unknown> | undefined
      } else if (status.status === 'error') {
        throw new Error(status.error ?? 'Task failed')
      }
      attempts++
    }
    throw new Error('Task timed out')
  }, [])

  // Update step status
  const updateStep = useCallback(
    (stepName: string, update: Partial<WGCNAStep>) => {
      setSteps((prev) =>
        prev.map((s) => (s.name === stepName ? { ...s, ...update } : s))
      )
    },
    []
  )

  // Run a single WGCNA step
  const runStep = useCallback(
    async (stepName: string, extraParams: Record<string, unknown> = {}) => {
      updateStep(stepName, { status: 'running' })
      try {
        const params: Record<string, unknown> = { step: stepName, ...extraParams }

        // Add expression data for sampletree step
        if (stepName === 'sampletree' && dataSource === 'workspace') {
          params.expr = { placeholder: true }
        }

        const res = await runWGCNA(params as Parameters<typeof runWGCNA>[0])
        const result = await pollTask(res.taskId)

        updateStep(stepName, { status: 'done', svg: result?.svg as string | undefined, result })
        return result
      } catch (err: unknown) {
        const msg = (err as Error)?.message ?? 'Step failed'
        updateStep(stepName, { status: 'error' })
        throw new Error(msg)
      }
    },
    [dataSource, pollTask, updateStep]
  )

  // Run full pipeline
  const handleRunAll = useCallback(async () => {
    setError(null)
    setLoading(true)

    // Reset all steps
    setSteps(WGCNA_STEPS.map((s) => ({ ...s, status: 'pending', svg: undefined, result: undefined })))
    setCurrentStep(0)

    try {
      // Step 1: Sample tree
      setCurrentStep(0)
      await runStep('sampletree', { cutHeight })

      // Step 2: Power
      setCurrentStep(1)
      const powerResult = await runStep('power', { abline })
      if (powerResult && typeof powerResult.recommended_power === 'number') {
        setPower(powerResult.recommended_power)
      }

      // Step 3: Network
      setCurrentStep(2)
      const netResult = await runStep('network', {
        power: power ?? 6,
        mergeCutHeight,
      })
      if (netResult && netResult.module_summary) {
        const colors = Object.keys(netResult.module_summary as Record<string, number>)
        setModuleColors(colors.filter((c) => c !== 'grey'))
      }

      // Step 4: Module-trait (requires trait data)
      if (traitFile || sampleInfo) {
        setCurrentStep(3)
        const traitParams: Record<string, unknown> = {}
        if (sampleInfo) {
          traitParams.trait = { placeholder: true }
        }
        try {
          const mtResult = await runStep('module_trait', traitParams)
          if (mtResult) {
            setTraitNames(sampleInfo ? ['Group'] : [])
          }
        } catch {
          // Module-trait step is optional
          updateStep('module_trait', { status: 'error' })
        }
      }
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'WGCNA 分析失败'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [dataSource, cutHeight, abline, power, mergeCutHeight, traitFile, sampleInfo, runStep, updateStep])

  // Run individual step
  const handleRunStep = useCallback(
    async (stepName: string) => {
      setError(null)
      setLoading(true)
      try {
        const params: Record<string, unknown> = {}
        switch (stepName) {
          case 'sampletree':
            params.cutHeight = cutHeight
            break
          case 'power':
            params.abline = abline
            break
          case 'network':
            params.power = power ?? 6
            params.mergeCutHeight = mergeCutHeight
            break
          case 'module_trait':
            params.trait = { placeholder: true }
            break
        }
        const result = await runStep(stepName, params)
        if (stepName === 'power' && result && typeof result.recommended_power === 'number') {
          setPower(result.recommended_power)
        }
        if (stepName === 'network' && result && result.module_summary) {
          const colors = Object.keys(result.module_summary as Record<string, number>)
          setModuleColors(colors.filter((c) => c !== 'grey'))
        }
        // Advance current step
        const idx = WGCNA_STEPS.findIndex((s) => s.name === stepName)
        if (idx >= currentStep) setCurrentStep(idx + 1)
      } catch (err: unknown) {
        const msg = (err as Error)?.message ?? '步骤执行失败'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    [cutHeight, abline, power, mergeCutHeight, currentStep, runStep]
  )

  // Generate MM vs GS plot
  const handleGenerateMMGS = useCallback(async () => {
    if (!selectedModule || !selectedTrait) {
      setError('请选择模块和性状')
      return
    }
    setError(null)
    try {
      const res = await getWGCNAPlot({
        step: 'mm_gs',
        module: selectedModule,
        trait: selectedTrait,
      })
      // Store SVG for display
      setSteps((prev) =>
        prev.map((s) =>
          s.name === 'module_trait'
            ? { ...s, svg: res.svg }
            : s
        )
      )
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? '生成图表失败'
      setError(String(msg))
    }
  }, [selectedModule, selectedTrait])

  // Generate TOM heatmap
  const handleGenerateTOM = useCallback(async () => {
    setError(null)
    try {
      const res = await getWGCNAPlot({
        step: 'tom',
        nSelect,
      })
      // Show TOM plot in result area
      setSteps((prev) =>
        prev.map((s) =>
          s.name === 'module_trait'
            ? { ...s, svg: res.svg }
            : s
        )
      )
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? '生成 TOM 热图失败'
      setError(String(msg))
    }
  }, [nSelect])

  // Export SVG
  const handleExportSVG = useCallback(
    (stepName: string) => {
      const step = steps.find((s) => s.name === stepName)
      if (!step?.svg) return
      const blob = new Blob([step.svg], { type: 'image/svg+xml' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `wgcna_${stepName}.svg`
      a.click()
      URL.revokeObjectURL(url)
    },
    [steps]
  )

  // Export RData
  const handleExportRData = useCallback(async () => {
    try {
      const res = await runWGCNA({ step: 'export' })
      const result = await pollTask(res.taskId)
      if (result && typeof result.data === 'string') {
        const byteCharacters = atob(result.data)
        const byteNumbers = new Array(byteCharacters.length)
        for (let i = 0; i < byteCharacters.length; i++) {
          byteNumbers[i] = byteCharacters.charCodeAt(i)
        }
        const byteArray = new Uint8Array(byteNumbers)
        const blob = new Blob([byteArray], { type: 'application/octet-stream' })
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = (result.filename as string) || 'wgcna_results.rda'
        a.click()
        URL.revokeObjectURL(url)
      }
    } catch {
      setError('RData 导出失败')
    }
  }, [pollTask])

  // Handle params loaded from RDS
  const handleParamsLoaded = useCallback(
    (loadedParams: Record<string, unknown>) => {
      if (loadedParams.cutHeight) setCutHeight(loadedParams.cutHeight as number)
      if (loadedParams.abline) setAbline(loadedParams.abline as number)
      if (loadedParams.mergeCutHeight) setMergeCutHeight(loadedParams.mergeCutHeight as number)
      if (loadedParams.nSelect) setNSelect(loadedParams.nSelect as number)
    },
    []
  )

  // Handle SVG loaded from RDS
  const handleSvgLoaded = useCallback((svgContent: string) => {
    // Store SVG in the last completed step
    setSteps((prev) => {
      const lastDone = [...prev].reverse().find((s) => s.status === 'done')
      if (lastDone) {
        return prev.map((s) =>
          s.name === lastDone.name ? { ...s, svg: svgContent } : s
        )
      }
      return prev
    })
  }, [])

  // Check if can run
  const canRun = dataSource === 'workspace' ? hasNormData : exprFile !== null

  // Step status indicator
  const StepIndicator = ({ step }: { step: WGCNAStep }) => {
    const statusColors: Record<string, string> = {
      pending: 'bg-gray-200',
      running: 'bg-blue-400 animate-pulse',
      done: 'bg-green-500',
      error: 'bg-red-500',
    }
    return (
      <div
        data-testid={`step-${step.name}`}
        className="flex items-center gap-2 text-sm"
      >
        <div className={`w-3 h-3 rounded-full ${statusColors[step.status]}`} />
        <span className={step.status === 'done' ? 'text-green-700' : 'text-gray-600'}>
          {step.label}
        </span>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">WGCNA 共表达网络分析</h2>

      {/* Data Source Section */}
      <div data-testid="wgcna-data-source" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">数据来源</h3>

        <div className="flex gap-4 mb-4">
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              data-testid="source-norm"
              checked={dataSource === 'workspace'}
              onChange={() => setDataSource('workspace')}
              disabled={!hasNormData}
              className="w-4 h-4"
            />
            <span className="text-sm">使用工作区标准化矩阵</span>
            {!hasNormData && (
              <span className="text-xs text-gray-400">(需先上传数据)</span>
            )}
          </label>
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              data-testid="source-upload"
              checked={dataSource === 'upload'}
              onChange={() => setDataSource('upload')}
              className="w-4 h-4"
            />
            <span className="text-sm">上传表达矩阵</span>
          </label>
        </div>

        {dataSource === 'upload' && (
          <div className="space-y-3">
            <div>
              <label className="block text-sm text-gray-600 mb-1">
                标准化表达矩阵 (xlsx/csv/tsv)
              </label>
              <input
                type="file"
                accept=".xlsx,.csv,.tsv"
                onChange={(e) => setExprFile(e.target.files?.[0] ?? null)}
                className="w-full border border-gray-300 rounded px-3 py-2 text-sm"
              />
              {exprFile && (
                <p className="text-xs text-gray-500 mt-1">已选择: {exprFile.name}</p>
              )}
            </div>
          </div>
        )}

        {dataSource === 'workspace' && hasNormData && (
          <p className="text-sm text-green-600">
            已加载: {exprNorm!.rows} 基因 x {exprNorm!.cols} 样本
          </p>
        )}

        {dataSource === 'workspace' && !hasNormData && (
          <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded">
            工作区无数据，请先在 Home 页上传数据或切换到上传模式。
          </div>
        )}
      </div>

      {/* Trait Upload */}
      <div data-testid="trait-upload" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">Trait 数据 (可选)</h3>
        <p className="text-sm text-gray-500 mb-2">
          用于 Module-Trait 关联分析。若已上传 sampleInfo，可直接使用。
        </p>
        <input
          type="file"
          accept=".xlsx,.csv,.tsv"
          onChange={(e) => setTraitFile(e.target.files?.[0] ?? null)}
          className="w-full border border-gray-300 rounded px-3 py-2 text-sm"
        />
        {traitFile && (
          <p className="text-xs text-gray-500 mt-1">已选择: {traitFile.name}</p>
        )}
        {sampleInfo && !traitFile && (
          <p className="text-sm text-green-600 mt-2">将使用工作区 sampleInfo 作为 trait 数据</p>
        )}
      </div>

      {/* Analysis Parameters */}
      <div data-testid="wgcna-params-section" className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">分析参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {/* cutHeight */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">异常样本切除高度</label>
            <input
              data-testid="param-cut-height"
              type="number"
              min="1"
              max="100"
              value={cutHeight}
              onChange={(e) => setCutHeight(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* abline */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">R² 阈值 (abline)</label>
            <input
              data-testid="param-abline"
              type="number"
              step="0.05"
              min="0.5"
              max="1"
              value={abline}
              onChange={(e) => setAbline(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* mergeCutHeight */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">模块合并高度</label>
            <input
              data-testid="param-merge-cut-height"
              type="number"
              step="0.05"
              min="0"
              max="1"
              value={mergeCutHeight}
              onChange={(e) => setMergeCutHeight(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* nSelect for TOM */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">TOM 热图基因数</label>
            <input
              data-testid="param-nselect"
              type="number"
              min="50"
              max="2000"
              value={nSelect}
              onChange={(e) => setNSelect(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>
      </div>

      {/* Progress Stepper */}
      <div data-testid="wgcna-progress" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">分析进度</h3>
        <div className="flex flex-wrap gap-4">
          {steps.map((step) => (
            <div key={step.name} className="flex items-center gap-2">
              <StepIndicator step={step} />
              {step.status === 'pending' && currentStep === WGCNA_STEPS.findIndex((s) => s.name === step.name) && (
                <button
                  data-testid={`run-step-${step.name}`}
                  onClick={() => handleRunStep(step.name)}
                  disabled={loading || !canRun}
                  className="text-xs px-2 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200 disabled:opacity-50"
                >
                  单步运行
                </button>
              )}
            </div>
          ))}
        </div>
      </div>

      {/* Run All Button */}
      <div className="flex gap-3">
        <button
          data-testid="run-wgcna"
          onClick={handleRunAll}
          disabled={loading || !canRun}
          className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:bg-gray-300 disabled:cursor-not-allowed text-sm font-medium"
        >
          {loading ? '分析中...' : '运行 WGCNA 全流程'}
        </button>
        <button
          data-testid="export-svg"
          onClick={() => {
            const lastDone = [...steps].reverse().find((s) => s.status === 'done' && s.svg)
            if (lastDone) handleExportSVG(lastDone.name)
          }}
          disabled={!steps.some((s) => s.status === 'done' && s.svg)}
          className="px-4 py-2 bg-gray-100 text-gray-700 rounded-lg hover:bg-gray-200 disabled:opacity-50 text-sm"
        >
          导出 SVG
        </button>
        <button
          data-testid="export-rdata"
          onClick={handleExportRData}
          disabled={!steps.some((s) => s.status === 'done')}
          className="px-4 py-2 bg-gray-100 text-gray-700 rounded-lg hover:bg-gray-200 disabled:opacity-50 text-sm"
        >
          导出 RData
        </button>
      </div>

      {/* Plot Save/Load */}
      <PlotSaveLoad
        module="wgcna"
        params={{
          cutHeight,
          abline,
          mergeCutHeight,
          nSelect,
        }}
        plotData={steps.some((s) => s.status === 'done') ? { steps: steps.filter((s) => s.status === 'done') } : null}
        disabled={!steps.some((s) => s.status === 'done')}
        onParamsLoaded={handleParamsLoaded}
        onSvgLoaded={handleSvgLoaded}
      />

      {/* Error display */}
      {error && (
        <div className="bg-red-50 border border-red-200 rounded-lg p-3 text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Result Area */}
      <div data-testid="wgcna-result-area" className="space-y-4">
        {steps.map((step) => (
          step.svg && (
            <div
              key={step.name}
              className="border border-gray-200 rounded-lg p-4"
              data-testid={`result-${step.name}`}
            >
              <div className="flex justify-between items-center mb-3">
                <h4 className="font-medium">{step.label}</h4>
                <div className="flex gap-2">
                  <button
                    onClick={() => handleExportSVG(step.name)}
                    className="text-xs px-2 py-1 bg-gray-100 rounded hover:bg-gray-200"
                  >
                    导出 SVG
                  </button>
                  {step.name === 'network' && (
                    <button
                      onClick={handleGenerateTOM}
                      className="text-xs px-2 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200"
                    >
                      生成 TOM 热图
                    </button>
                  )}
                </div>
              </div>

              {/* Step-specific info */}
              {step.name === 'sampletree' && step.result && (
                <p className="text-sm text-gray-600 mb-2">
                  样本数: {(step.result as Record<string, unknown>).n_samples as number},{' '}
                  基因数: {(step.result as Record<string, unknown>).n_genes as number}
                </p>
              )}
              {step.name === 'power' && step.result && (
                <p className="text-sm text-gray-600 mb-2">
                  推荐 Power: <strong>{(step.result as Record<string, unknown>).recommended_power as number}</strong>
                </p>
              )}
              {step.name === 'network' && step.result && (
                <div className="text-sm text-gray-600 mb-2">
                  <p>
                    模块数: <strong>{(step.result as Record<string, unknown>).n_modules as number}</strong>
                  </p>
                  {/* Module-trait controls */}
                  {traitNames.length > 0 && moduleColors.length > 0 && (
                    <div className="flex gap-3 mt-2 flex-wrap">
                      <div>
                        <label className="block text-xs text-gray-500 mb-1">模块</label>
                        <select
                          value={selectedModule}
                          onChange={(e) => setSelectedModule(e.target.value)}
                          className="border border-gray-300 rounded px-2 py-1 text-xs"
                        >
                          <option value="">选择模块</option>
                          {moduleColors.map((c) => (
                            <option key={c} value={c}>
                              {c}
                            </option>
                          ))}
                        </select>
                      </div>
                      <div>
                        <label className="block text-xs text-gray-500 mb-1">性状</label>
                        <select
                          value={selectedTrait}
                          onChange={(e) => setSelectedTrait(e.target.value)}
                          className="border border-gray-300 rounded px-2 py-1 text-xs"
                        >
                          <option value="">选择性状</option>
                          {traitNames.map((t) => (
                            <option key={t} value={t}>
                              {t}
                            </option>
                          ))}
                        </select>
                      </div>
                      <button
                        onClick={handleGenerateMMGS}
                        className="text-xs px-2 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200 self-end"
                      >
                        生成 MM-GS 散点图
                      </button>
                    </div>
                  )}
                </div>
              )}

              {/* SVG Display */}
              <div
                className="border border-gray-100 rounded bg-white overflow-auto"
                dangerouslySetInnerHTML={{ __html: step.svg }}
              />
            </div>
          )
        ))}
      </div>
    </div>
  )
}
