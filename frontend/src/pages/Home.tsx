import DataUpload from '../components/DataUpload'
import { useWorkspaceStore } from '../stores/workspace'

export default function Home() {
  const exprRaw = useWorkspaceStore((s) => s.exprRaw)
  const sampleInfo = useWorkspaceStore((s) => s.sampleInfo)

  return (
    <div className="max-w-4xl mx-auto space-y-6">
      <h2 className="text-2xl font-bold">RNAseqTool — Data Upload</h2>

      <DataUpload />

      {/* Data status summary */}
      {(exprRaw || sampleInfo) && (
        <div className="mt-6 p-4 bg-gray-50 rounded-lg">
          <h3 className="text-sm font-medium text-gray-700 mb-2">Loaded Data</h3>
          <div className="space-y-1 text-sm text-gray-600">
            {exprRaw && (
              <div>
                Expression: {exprRaw.rows} genes x {exprRaw.cols} samples
              </div>
            )}
            {sampleInfo && (
              <div>
                Sample Info: {sampleInfo.rows} samples x {sampleInfo.cols} columns
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  )
}
