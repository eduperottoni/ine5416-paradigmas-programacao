board = [
    [7, 8, 0, 4, 0, 0, 1, 2, 0],
    [6, 0, 0, 0, 7, 5, 0, 0, 9],
    [0, 0, 0, 6, 0, 1, 0, 7, 8],
    [0, 0, 7, 0, 4, 0, 2, 6, 0],
    [0, 0, 1, 0, 5, 0, 9, 3, 0],
    [9, 0, 4, 0, 6, 0, 0, 0, 5],
    [0, 7, 0, 3, 0, 0, 0, 1, 2],
    [1, 2, 0, 0, 0, 7, 4, 0, 0],
    [0, 4, 9, 2, 0, 6, 0, 0, 7]
]

# Approach to the backtracking
# 1 - Pick an empty square
# 2 - Try all numbers
# 3 - Find one that works
# 5 - Backtrack (when the solution is not right)
# 4 - Repeat (coming back to 1)


def print_board(board: list[list[str]]) -> None:
    for i, row in enumerate(board):
        if i % 3 == 0 and i != 0:
            print("-"*21)

        print(
            f'{row[:3]} | {row[3:6]} | {row[6:]}'
            .replace('[', '')
            .replace(']', '')
            .replace(', ', ' ')
        )


def find_empty(board: list[list[str]]) -> tuple[int, int] | None:
    for i, row in enumerate(board):
        for j, column in enumerate(row):
            if column == 0:
                return (i, j)
            
    return None


def is_position_valid(board: list[list[int]], 
                      num: int,  # 1 - 9
                      position: tuple[int, int]) -> bool:
    
    # Check in the line
    for i, number in enumerate(board[position[0]]):
        if i != position[1] and number == num:
            return False
    
    # Check in the column
    for i, row in enumerate(board):
        if row[position[1]] == num and i != position[0]:
            return False
        
    # Check in the square
    box_x = position[1] // 3
    box_y = position[0] // 3
    for i, line in enumerate(board[box_y*3:((box_y + 1)*3)]):
        for j, number in enumerate(line[box_x*3:((box_x + 1)*3)]):
            if number == num and position != (i, j):
                return False
            
    return True


def solve(board: list[list[int]]) -> bool:
    empty_found = find_empty(board)
    if not empty_found:
        return True
    
    row, col = empty_found
    for i in range(1, 10):
        if is_position_valid(board, i, empty_found):
            board[row][col] = i

            if solve(board):
                return True
            
            board[row][col] = 0
    
    return False
    

print_board(board)
solve(board)
print('-'*50)
print_board(board)