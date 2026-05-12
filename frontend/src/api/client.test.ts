import { describe, it, expect, vi, beforeEach } from 'vitest'

// Use vi.hoisted to define mocks before module loading
const { mockPost } = vi.hoisted(() => {
  return { mockPost: vi.fn() }
})

// Mock axios before importing the module
vi.mock('axios', () => ({
  default: {
    create: () => ({
      post: mockPost,
    }),
  },
}))

import { uploadFile, loadDemoData } from './client'

describe('API Client', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  describe('uploadFile', () => {
    it('sends FormData with file and type', async () => {
      const mockResponse = {
        data: {
          status: 'ok',
          data: { preview: [], rows: 0, cols: 0 },
        },
      }
      mockPost.mockResolvedValue(mockResponse)

      const file = new File(['Gene,S1\nA,10'], 'test.csv', { type: 'text/csv' })
      const result = await uploadFile(file, 'expr_raw')

      expect(mockPost).toHaveBeenCalledWith(
        '/data/upload',
        expect.any(FormData)
      )
      expect(result.status).toBe('ok')
    })

    it('handles upload errors', async () => {
      mockPost.mockRejectedValue({
        response: { data: { error: 'Invalid file format' } },
      })

      const file = new File(['bad'], 'bad.txt', { type: 'text/plain' })
      await expect(uploadFile(file, 'sampleInfo')).rejects.toThrow()
    })
  })

  describe('loadDemoData', () => {
    it('sends POST with type parameter', async () => {
      const mockResponse = {
        data: {
          status: 'ok',
          data: {
            type: 'expr_raw',
            preview: [['Gene', 'S1']],
            rows: 1,
            cols: 2,
          },
        },
      }
      mockPost.mockResolvedValue(mockResponse)

      const result = await loadDemoData('expr_raw')

      expect(mockPost).toHaveBeenCalledWith('/data/demo', { type: 'expr_raw' })
      expect(result.data.type).toBe('expr_raw')
    })

    it('handles demo data errors', async () => {
      mockPost.mockRejectedValue({
        response: { data: { error: 'Failed to load demo data' } },
      })

      await expect(loadDemoData('invalid_type')).rejects.toThrow()
    })
  })
})
