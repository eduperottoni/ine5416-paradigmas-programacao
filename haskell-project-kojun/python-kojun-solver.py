"""
Kojun solver written using Python
"""

# class Position:
#     """
#     Class representing the position of the table 
#     """
#     row: int = None
#     column: int = None

original_table = [
    [0, 0, 0, 0, 0, 2],
    [2, 0, 0, 5, 0, 0],
    [0, 0, 3, 0, 0, 4],
    [0, 0, 0, 3, 0, 1],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 3, 0, 2, 5]
]

regions_table = [
    [0 , 1 , 2, 2, 3, 3],
    [0 , 1 , 4, 3, 3, 3],
    [0 , 0 , 4, 4, 4, 5],
    [6 , 6 , 7, 5, 5, 5],
    [6 , 6 , 7, 8, 9, 9],
    [10, 10, 8, 8, 8, 8]
]


def print_table(table: list[list[int]]):
    """Just prints the table"""
    for row in table:
        print(str(row)
              .replace(',', ' ')
              .replace('[', '')
              .replace(']', ''))


def define_regions(regions_table: list[list[int]]):
    """
    Define regions indexing structure. It's an array indexed by the number of the region.
    Each i position will contain a list of tuples indicating the squares that are
    on region i of the original table.
    """
    regions_indexing = [[] for i in range(max(max(row) for row in regions_table) + 1)]

    for index_i, row in enumerate(regions_table):
        for index_j, column in enumerate(row):
            print(f' {index_i},{index_j}')
            print(column)
            regions_indexing[column].append((index_i, index_j))
    return regions_indexing


def get_region_from_position(position: tuple[int], regions_table: list[list[int]]):
    return regions_table[position[0], position[1]]


def is_number_valid(number: int, position: tuple[int], table: list[list[int]], regions_indexing: list[list[tuple[int]]]):
    """
    Checks if the number is valid for the given position
    """
    # 1 - Number is unique within its region
    current_region = get_region_from_position(position)
    for position in regions_indexing[current_region]:
        if table[position[0], position[1]] == number:
            return False
        else if 
    # 2 - If two cells are vertically adjacent in the same region, 
    # the number in the upper cell must be greater than the number in the lower cell.
    # 3 - Numbers in orthogonally adjacent cells must be different
    







def solve(original_table: list[list[int]], regions_table: list[list[int]]):
    """
    Solves the puzzle
    """


def main():
    print(define_regions(regions_table))

main()
