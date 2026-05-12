import { useCallback, useRef } from 'react'
import { savePlotRds, loadPlotRds } from '../api/client'

interface PlotSaveLoadProps {
  module: string
  params: Record<string, unknown>
  plotData: Record<string, unknown> | null
  disabled: boolean
  onParamsLoaded?: (params: Record<string, unknown>) => void
  onSvgLoaded?: (svg: string) => void
}

export function PlotSaveLoad({
  module,
  params,
  plotData,
  disabled,
  onParamsLoaded,
  onSvgLoaded,
}: PlotSaveLoadProps) {
  const fileInputRef = useRef<HTMLInputElement>(null)

  const handleSave = useCallback(async () => {
    if (!plotData) return
    try {
      const result = await savePlotRds(module, params, plotData)
      if (result.status === 'ok') {
        // Decode base64 and download
        const byteChars = atob(result.data)
        const byteNumbers = new Array(byteChars.length)
        for (let i = 0; i < byteChars.length; i++) {
          byteNumbers[i] = byteChars.charCodeAt(i)
        }
        const byteArray = new Uint8Array(byteNumbers)
        const blob = new Blob([byteArray], { type: 'application/octet-stream' })
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = result.filename
        a.click()
        URL.revokeObjectURL(url)
      }
    } catch {
      // Error handled by parent
    }
  }, [module, params, plotData])

  const handleLoadClick = useCallback(() => {
    fileInputRef.current?.click()
  }, [])

  const handleFileChange = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return

      try {
        const result = await loadPlotRds(file)
        if (result.status === 'ok') {
          if (onParamsLoaded) {
            onParamsLoaded(result.params)
          }
          if (onSvgLoaded) {
            onSvgLoaded(result.svg)
          }
        }
      } catch {
        // Error handled by parent
      }

      // Reset input so same file can be re-selected
      if (fileInputRef.current) {
        fileInputRef.current.value = ''
      }
    },
    [onParamsLoaded, onSvgLoaded]
  )

  return (
    <div className="flex gap-3 flex-wrap items-center">
      <button
        data-testid="save-plot-rds"
        onClick={handleSave}
        disabled={disabled || !plotData}
        className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
      >
        保存绘图 (RDS)
      </button>
      <button
        data-testid="load-plot-rds"
        onClick={handleLoadClick}
        className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200"
      >
        加载绘图 (RDS)
      </button>
      <input
        ref={fileInputRef}
        data-testid="load-plot-file-input"
        type="file"
        accept=".rds"
        onChange={handleFileChange}
        className="hidden"
      />
    </div>
  )
}
