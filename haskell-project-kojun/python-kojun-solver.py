"""
Kojun solver written using Python
"""

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


def solve(original_table: list[list[int]], regions_table: list[list[int]])