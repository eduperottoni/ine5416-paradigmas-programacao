"""
Kojun solver written using Python
"""

# class Position:
#     """
#     Class representing the position of the table 
#     """
#     row: int = None
#     column: int = None
# original_table = [
#     [0, 0, 3, 0, 2, 0, 3, 0, 0, 0, 7, 0, 0, 5, 0, 3, 6],
#     [0, 0, 0, 0, 0, 2, 0, 6, 0, 2, 0, 0, 3, 0, 0, 2, 0],
#     [0, 2, 0, 0, 0, 0, 0, 0, 4, 0, 5, 1, 0, 2, 0, 0, 0],
#     [3, 0, 7, 0, 0, 5, 0, 0, 0, 0, 0, 7, 4, 0, 2, 0, 0],
#     [2, 1, 0, 2, 0, 0, 0, 3, 0, 5, 0, 0, 6, 0, 0, 0, 1],
#     [0, 3, 4, 0, 0, 0, 3, 0, 0, 0, 3, 0, 5, 0, 0, 0, 3],
#     [3, 1, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#     [0, 7, 0, 5, 2, 6, 0, 0, 0, 0, 1, 0, 1, 0, 2, 0, 6],
#     [0, 0, 5, 0, 0, 2, 0, 5, 0, 5, 0, 7, 0, 3, 0, 4, 0],
#     [0, 0, 0, 0, 7, 0, 3, 0, 2, 3, 0, 0, 4, 0, 0, 3, 1],
#     [5, 0, 3, 0, 5, 0, 6, 0, 0, 4, 0, 4, 0, 3, 0, 0, 0],
#     [0, 0, 0, 6, 0, 0, 0, 0, 4, 0, 5, 0, 0, 2, 0, 2, 0],
#     [0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 1, 0, 0, 4, 0, 5],
#     [0, 6, 3, 4, 0, 4, 5, 0, 6, 2, 0, 0, 1, 4, 0, 3, 0],
#     [0, 2, 0, 0, 0, 0, 3, 0, 4, 0, 3, 0, 6, 0, 0, 0, 0],
#     [0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0],
#     [6, 2, 0, 0, 1, 3, 1, 7, 1, 2, 0, 4, 0, 4, 1, 0, 6]
# ]

# regions_table = [
#     [0 , 1 , 1 , 1 , 1 , 2 , 3 , 4 , 4 , 5 , 6 , 6 , 7 , 7 , 8 , 9 , 9 ],
#     [10, 11, 11, 12, 3 , 3 , 3 , 13, 13, 13, 6 , 6 , 6 , 7 , 7 , 9 , 9 ],
#     [10, 10, 12, 12, 14, 14, 14, 13, 13, 13, 6 , 6 , 7 , 7 , 9 , 9 , 9 ],
#     [15, 15, 16, 16, 16, 14, 14, 17, 17, 18, 18, 18, 18, 19, 19, 20, 20],
#     [15, 15, 16, 16, 21, 22, 14, 17, 17, 18, 23, 24, 19, 19, 25, 26, 20],
#     [27, 28, 16, 16, 21, 22, 22, 22, 18, 18, 23, 19, 19, 25, 25, 26, 26],
#     [29, 28, 28, 29, 21, 30, 30, 22, 31, 23, 23, 32, 19, 25, 25, 33, 34],
#     [29, 29, 29, 29, 29, 35, 30, 30, 31, 23, 23, 32, 32, 25, 33, 33, 36],
#     [37, 38, 38, 38, 35, 35, 35, 30, 31, 39, 39, 39, 40, 40, 33, 36, 36],
#     [37, 37, 38, 41, 41, 35, 35, 30, 43, 39, 39, 39, 44, 40, 36, 36, 36],
#     [37, 37, 38, 41, 41, 41, 42, 43, 43, 43, 45, 39, 44, 44, 44, 44, 46],
#     [37, 37, 38, 38, 47, 41, 42, 42, 48, 48, 45, 45, 45, 44, 49, 46, 46],
#     [50, 51, 51, 52, 47, 41, 42, 42, 54, 48, 48, 45, 45, 49, 49, 49, 49],
#     [50, 50, 51, 52, 47, 47, 53, 42, 55, 48, 48, 56, 57, 57, 49, 58, 58],
#     [50, 50, 51, 52, 52, 53, 53, 59, 55, 55, 56, 56, 56, 57, 60, 58, 58],
#     [50, 50, 61, 61, 52, 53, 53, 59, 55, 55, 56, 62, 62, 57, 60, 60, 63],
#     [61, 61, 61, 61, 52, 52, 53, 53, 55, 55, 56, 56, 62, 60, 60, 60, 60]
# ]

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


def define_regions(regions_table: list[list[int]]) -> list[list[tuple[int]]]:
    """
    Define regions indexing structure. It's an array indexed by the number of the region.
    Each i position will contain a list of tuples indicating the squares that are
    on region i of the original table.
    """
    regions_indexing = [[] for i in range(max(max(row) for row in regions_table) + 1)]

    for index_i, row in enumerate(regions_table):
        for index_j, column in enumerate(row):
            regions_indexing[column].append((index_i, index_j))
    return regions_indexing


def get_region_from_position(position: tuple[int], regions_table: list[list[int]]):
    return regions_table[position[0]][position[1]]


def get_adjacent_cells(row: int, column: int, table: list[list[int]]) -> list[int]:
    left = table[row][column - 1] if column > 0 else None
    right = table[row][column + 1] if column < len(table) - 1 else None
    up = table[row - 1][column] if row > 0 else None
    down = table[row + 1][column] if row < len(table) - 1 else None
    # print(f'left={left},right={right},up={up},down={down}')
    return [i for i in [left, right, up, down] if i is not None]

def is_number_valid(number: int, position: tuple[int], table: list[list[int]], regions_indexing: list[list[tuple[int]]]):
    """
    Checks if the number is valid for the given position
    """
    # print(f'number={number}')
    current_region = get_region_from_position(position, regions_table)
    # print(f'current_region={current_region}')
    for r_position in regions_indexing[current_region]:
        r_number = table[r_position[0]][r_position[1]]
        # print(f'r_number={r_number}')
        # print(f'r_position={r_position}')
        # 1 - Number is unique within its region
        if r_number == number:
            print('#1')
            return False
        # 2 - If two cells are vertically adjacent in the same region, 
        # the number in the upper cell must be greater than the number in the lower cell.
        if (r_position[1] == position[1]):
            # down
            if (r_position[0] - position[0] == 1) and (r_number > number):
                print('#2.1')
                return False
            # up
            if (r_position[0] - position[0] == -1) and (r_number < number):
                print('#2.2')
                return False

    # 3 - Numbers in orthogonally adjacent cells must be different
    print(f'adjacents={get_adjacent_cells(position[0], position[1], table)}')

    if number in get_adjacent_cells(position[0], position[1], table):
        print('#3')
        return False

    return True


def find_empty(table: list[list[str]]) -> tuple[int, int] | None:
    for i, row in enumerate(table):
        for j, column in enumerate(row):
            if column == 0:
                return (i, j)

    return None


def solve(table: list[list[int]], regions_table: list[list[int]]):
    """
    Solves the puzzle
    """
    regions_index = define_regions(regions_table)

    empty: tuple[int] = find_empty(table)
    if not empty:
        return True, table
    
    print(f'empty={empty}')
    row, col = empty
    current_region = get_region_from_position((row, col), regions_table)
    max_of_region = len(regions_index[current_region]) + 1

    for i in range(1, max_of_region):
        print(f'Trying with {i}')
        if is_number_valid(i, empty, table, regions_index):
            table[row][col] = i
            print_table(table)
            if solve(table, regions_table):
                return True
            
            table[row][col] = 0
    
    return False


def main():
    print(solve(original_table, regions_table))


main()
