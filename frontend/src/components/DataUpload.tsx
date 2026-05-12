import { useState, useCallback } from 'react'
import { uploadFile, loadDemoData } from '../api/client'
import { useWorkspaceStore } from '../stores/workspace'

const DATA_TYPES = [
  { value: 'expr_raw', label: 'Raw Expression', storeKey: 'exprRaw' },
  { value: 'expr_norm', label: 'Norm Expression', storeKey: 'exprNorm' },
  { value: 'sampleInfo', label: 'Sample Info', storeKey: 'sampleInfo' },
  { value: 'deseq2_result', label: 'DESeq2 Result', storeKey: null },
  { value: 'genelist', label: 'Gene List', storeKey: null },
  { value: 'trait', label: 'Trait Data', storeKey: null },
] as const

type DataType = typeof DATA_TYPES[number]['value']

interface PreviewData {
  preview: string[][]
  rows: number
  cols: number
}

export default function DataUpload() {
  const [selectedType, setSelectedType] = useState<DataType>('expr_raw')
  const [preview, setPreview] = useState<PreviewData | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [loading, setLoading] = useState(false)

  const setExprRaw = useWorkspaceStore((s) => s.setExprRaw)
  const setExprNorm = useWorkspaceStore((s) => s.setExprNorm)
  const setSampleInfo = useWorkspaceStore((s) => s.setSampleInfo)

  const storeData = useCallback(
    (type: DataType, data: PreviewData) => {
      const typeInfo = DATA_TYPES.find((t) => t.value === type)
      const storeEntry = {
        name: type,
        rows: data.rows,
        cols: data.cols,
        preview: data.preview,
      }

      if (typeInfo?.storeKey === 'exprRaw') setExprRaw(storeEntry)
      else if (typeInfo?.storeKey === 'exprNorm') setExprNorm(storeEntry)
      else if (typeInfo?.storeKey === 'sampleInfo') setSampleInfo(storeEntry)
    },
    [setExprRaw, setExprNorm, setSampleInfo]
  )

  const handleFileUpload = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return

      setError(null)
      setPreview(null)
      setLoading(true)

      try {
        const res = await uploadFile(file, selectedType)
        const data = res.data as PreviewData
        setPreview(data)
        storeData(selectedType, data)
      } catch (err: unknown) {
        const msg =
          (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
          (err as Error)?.message ??
          'Upload failed'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    [selectedType, storeData]
  )

  const handleDemoLoad = useCallback(
    async (type: DataType) => {
      setError(null)
      setPreview(null)
      setLoading(true)

      try {
        const res = await loadDemoData(type)
        const data = res.data as PreviewData
        setPreview(data)
        storeData(type, data)
      } catch (err: unknown) {
        const msg =
          (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
          (err as Error)?.message ??
          'Failed to load demo data'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    [storeData]
  )

  return (
    <div className="space-y-4">
      {/* Data type selector */}
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          Data Type
        </label>
        <div className="flex flex-wrap gap-2">
          {DATA_TYPES.map((dt) => (
            <button
              key={dt.value}
              data-testid={`type-${dt.value}`}
              onClick={() => setSelectedType(dt.value)}
              className={`px-3 py-1.5 rounded-md text-sm ${
                selectedType === dt.value
                  ? 'bg-blue-100 text-blue-700 font-medium'
                  : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
              }`}
            >
              {dt.label}
            </button>
          ))}
        </div>
      </div>

      {/* File upload */}
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          Upload File (CSV / XLSX)
        </label>
        <input
          type="file"
          accept=".csv,.xlsx"
          data-testid="file-input"
          onChange={handleFileUpload}
          disabled={loading}
          className="block w-full text-sm text-gray-500 file:mr-4 file:py-2 file:px-4 file:rounded-md file:border-0 file:text-sm file:font-medium file:bg-blue-50 file:text-blue-700 hover:file:bg-blue-100"
        />
      </div>

      {/* Demo data buttons */}
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          Load Demo Data
        </label>
        <div className="flex flex-wrap gap-2">
          {DATA_TYPES.filter((dt) =>
            ['expr_raw', 'expr_norm', 'sampleInfo'].includes(dt.value)
          ).map((dt) => (
            <button
              key={dt.value}
              data-testid={`demo-${dt.value}`}
              onClick={() => handleDemoLoad(dt.value)}
              disabled={loading}
              className="px-3 py-1.5 rounded-md text-sm bg-green-50 text-green-700 hover:bg-green-100 disabled:opacity-50"
            >
              {dt.label}
            </button>
          ))}
        </div>
      </div>

      {/* Loading indicator */}
      {loading && (
        <div className="text-sm text-gray-500">Loading...</div>
      )}

      {/* Error message */}
      {error && (
        <div
          data-testid="error-message"
          className="p-3 bg-red-50 border border-red-200 rounded-md text-sm text-red-700"
        >
          {error}
        </div>
      )}

      {/* Data preview */}
      {preview && (
        <div data-testid="data-preview">
          <div className="text-sm text-gray-600 mb-2">
            {preview.rows} rows x {preview.cols} cols
          </div>
          <div className="overflow-auto max-h-96 border border-gray-200 rounded-md">
            <table className="min-w-full text-sm">
              <thead className="bg-gray-50 sticky top-0">
                <tr>
                  {preview.preview[0]?.map((col, i) => (
                    <th
                      key={i}
                      className="px-3 py-2 text-left font-medium text-gray-700 border-b"
                    >
                      {col}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {preview.preview.slice(1).map((row, ri) => (
                  <tr key={ri} className={ri % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                    {row.map((cell, ci) => (
                      <td key={ci} className="px-3 py-1.5 border-b border-gray-100">
                        {cell}
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}
    </div>
  )
}
