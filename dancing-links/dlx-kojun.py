import numpy as np

class KojunDLXBuilder:
    def __init__(self, grid_size, region_sizes, region_labels):
        self.grid_size = grid_size
        self.region_sizes = region_sizes
        self.region_labels = region_labels
        self.num_regions = len(region_sizes)
        self.num_cells = grid_size ** 2
        self.num_columns = self.num_cells * self.num_regions
        self.dlx_matrix = np.zeros((self.num_cells * self.num_regions * self.grid_size, self.num_columns), dtype=int)

    def build_matrix(self):
        self.fill_constraints()
        return self.dlx_matrix

    def fill_constraints(self):
        for row in range(self.num_cells):
            for region_idx in range(self.num_regions):
                self.fill_cell_constraints(row, region_idx)

    def fill_cell_constraints(self, row, region_idx):
        region_size = self.region_sizes[region_idx]
        row_start = (row * self.num_regions + region_idx) * self.grid_size
        region_label = self.region_labels[region_idx]

        # Constraint 1: Each number appears exactly once in each region
        for value in range(1, region_size + 1):
            col = row_start + region_idx
            self.dlx_matrix[row_start + value - 1][col] = 1

        # Constraint 2: Numbers in orthogonally adjacent cells must be different
        if row % self.grid_size != self.grid_size - 1:  # Check if not last column
            self.dlx_matrix[row_start:(row_start + region_size), row_start + 1] = -1
            self.dlx_matrix[row_start + 1:(row_start + region_size), row_start] = -1

        # Constraint 3: Number in upper cell must be greater than number in lower cell in vertical regions
        if region_size == 1:  # Skip if region size is 1
            return
        for i in range(region_size - 1):
            upper_row = row_start + i
            lower_row = row_start + i + 1
            self.dlx_matrix[upper_row][upper_row + region_idx] = -1
            self.dlx_matrix[lower_row][lower_row + region_idx] = 1

def main():
    # Define puzzle parameters
    grid_size = 4
    region_sizes = [2, 3]
    region_labels = ['A', 'B']

    # Build DLX matrix
    dlx_builder = KojunDLXBuilder(grid_size, region_sizes, region_labels)
    dlx_matrix = dlx_builder.build_matrix()

    # Output DLX matrix to file
    np.savetxt('kojun_dlx_matrix.csv', dlx_matrix, fmt='%d', delimiter=',')

if __name__ == "__main__":
    main()