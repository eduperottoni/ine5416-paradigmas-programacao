import numpy as np

def construct_matrix():
    matrix = np.zeros((729, 324), dtype=int)
    for r in range(9):
        for c in range(9):
            for v in range(9):
                matrix[r * 81 + c * 9 + v][r * 9 + c] = 1
                matrix[r * 81 + c * 9 + v][81 + r * 9 + v] = 1
                matrix[r * 81 + c * 9 + v][162 + c * 9 + v] = 1
                matrix[r * 81 + c * 9 + v][243 + (r // 3 * 3 + c // 3) * 9 + v] = 1
    return matrix

dlx_matrix = construct_matrix()
print(dlx_matrix.shape)  # Output: (729, 324)
np.savetxt('dlx_matrix.csv', dlx_matrix, fmt='%d', delimiter=',')