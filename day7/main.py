from typing import List, Tuple

def irradiate(current_frame: List[int], counter : int, env: str) -> Tuple[List[int], int]:
    result = [0 for _ in current_frame]
    is_valid = lambda pos : pos >= 0 and pos < len(current_frame)
    for (index, c) in enumerate(env):
        n_rays = current_frame[index]
        if n_rays: # has laser
            if c == '^':
                if is_valid(index - 1):
                    result[index - 1] += n_rays
                if is_valid(index + 1):
                    result[index + 1] += n_rays
                counter += 1
            if c == '.':
                result[index] += n_rays
    return (result, counter)

def find_initial_pos(matrix: List[str]) -> Tuple[int, int]:
    return (matrix[0].find("S"), 0)

def pretty_print(lines : List[str], rays : List[Tuple[List[int], int]]) -> None:
    for (l, t) in zip(lines, rays):
        (ray, timelines) = t
        merged_line = ['|' if r else pos for (pos, r) in zip(l, ray)]
        print(''.join(merged_line) + f'   {timelines} timelines: {sum(ray)} '.ljust(20) + f'{ray}')

if __name__ == "__main__":
    with open("day7/example.txt") as f:
        lines = f.readlines()

    lines = [i.strip() for i in lines]
    initial_pos = find_initial_pos(lines)

    initial_frame = [0] * len(lines[0])
    initial_frame[initial_pos[0]] = 1
    result = [(initial_frame, 0)]
    for line in lines[1:]:
        result.append(irradiate(*result[-1], line))

    pretty_print(lines, result)
