regions = [
    ["a", "a", "b", "b", "c", "d", "e", "e"],
    ["a", "a", "f", "b", "g", "d", "d", "e"],
    ["f", "f", "f", "h", "g", "i", "j", "j"],
    ["k", "k", "k", "h", "g", "i", "i", "j"],
    ["l", "h", "h", "h", "h", "i", "i", "j"],
    ["l", "m", "n", "n", "n", "o", "p", "j"],
    ["m", "m", "m", "m", "q", "o", "o", "o"],
    ["r", "q", "q", "q", "q", "o", "s", "s"]
]


puzzle = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 3, 0, 0],
    [0, 0, 3, 0, 0, 0, 0, 0],
    [0, 5, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 3, 0],
    [0, 0, 5, 3, 0, 0, 0, 0]
]


positions_per_region = {}

for i, row in enumerate(regions):
    for j, r in enumerate(row):
        if r not in positions_per_region:
            positions_per_region[r] = [(i, j)]
        else:
            positions_per_region[r].append((i, j))

num_of_rows_m = 0
for positions in positions_per_region.values():
    num_of_rows_m += (len(positions)**2)


num_of_columns_m = 0
# 1st rule
num_of_columns_m += len(puzzle)**2
# 2nd rule
max_value_possible = 0
for positions in positions_per_region.values():
    max_value_possible = max(max_value_possible, len(positions))
num_of_columns_m += max_value_possible
# 3rd rule
for positions in positions_per_region.values():
    num_of_columns_m += (len(positions)**2)

print(positions_per_region)
print(num_of_rows_m)
print(num_of_columns_m)
print(max_value_possible)


new_matrix = [[0 for _ in range(num_of_columns_m)] for _ in range(num_of_rows_m)]
with open("m.txt", 'w') as file:
    for row in new_matrix:
        file.write(str(row).replace(', ', ''))
        file.write('\n')