"""Home of code for day 14 of AOC 2022."""

import numpy as np
import os
import time


def load(fn):
    with open(fn) as f:
        contents = f.read()
    lines = [
        [tuple(int(n) for n in point.split(",")) for point in line.split(" -> ")]
        for line in contents.strip().split("\n")
    ] + [[(500, 0)]]
    minj = min(j for line in lines for (j, _) in line)
    maxj = max(j for line in lines for (j, _) in line)
    mini = min(i for line in lines for (_, i) in line)
    maxi = max(i for line in lines for (_, i) in line)
    return (
        [[(i - mini, j - minj) for j, i in ln] for ln in lines],
        mini,
        maxi,
        minj,
        maxj,
    )


def cave(lines, mini, maxi, minj, maxj):
    m, n = maxi - mini + 1, maxj - minj + 1
    mtx = np.full((m, n), ".")
    for ln in lns:
        ip, jp = None, None
        for (i, j) in ln:
            # if (i, j) == (0, 500 - 483):
            if (i, j) == (0, 500 - minj):
                mtx[i, j] = "+"
                continue
            elif (ip, jp) != (None, None):
                if ip != i:
                    step = -1 if i < ip else 1
                    for ii in range(ip, i, step):
                        mtx[ii, j] = "#"
                elif jp != j:
                    step = -1 if j < jp else 1
                    for jj in range(jp, j, step):
                        mtx[i, jj] = "#"
            mtx[i, j] = "#"
            ip, jp = i, j
    space = np.full((m, m), ".")
    mtx = np.append(mtx, space, axis=1)
    mtx = np.append(space, mtx, axis=1)
    # bottom = np.full((1,mtx.shape
    mtx = np.append(mtx, np.full((1, mtx.shape[1]), "."), axis=0)
    mtx = np.append(mtx, np.full((1, mtx.shape[1]), "#"), axis=0)
    return mtx


def show(mtx):
    print("\n".join(["".join(ln) for ln in mtx]))


def process_single_grain(mtx, start):
    cave = np.copy(mtx)
    i, j = start
    ibellow, jleft, jright = i + 1, j - 1, j + 1

    if ibellow >= cave.shape[0]:
        # into abyss down, exit early
        cave[i, j] = "."
        return cave
    elif cave[ibellow, j] == ".":
        cave[i, j] = "." if start is not None else "+"
        cave[ibellow, j] = "o"
        return process_single_grain(cave, (ibellow, j))
    elif jleft < 0:
        # into abyss left, exit early
        cave[i, j] = "."
        return cave
    elif cave[ibellow, jleft] == ".":
        cave[i, j] = "."
        cave[ibellow, jleft] = "o"
        return process_single_grain(cave, (ibellow, jleft))
    elif jright >= cave.shape[1]:
        # into abyss right, exit early
        cave[i, j] = "."
        return cave
    elif cave[ibellow, jright] == ".":
        cave[i, j] = "."
        cave[ibellow, jright] = "o"
        return process_single_grain(cave, (ibellow, jright))
    else:
        cave[i, j] = "o"
    # hit steady-state, exit
    return cave


def process_sand(cave):
    prev = cave
    start = np.where(cave == "+")
    i = 0
    while np.any((current := process_single_grain(prev, start)) != prev):
        prev = np.copy(current)
        if i % 100 == 0:
            os.system("clear")
            show(current)
            time.sleep(0.01)
        i += 1
    return current


if __name__ == "__main__":
    lns, mini, maxi, minj, maxj = load("input.txt")
    # print(mini, maxi, minj, maxj)
    mtx = cave(lns, mini, maxi, minj, maxj)
    fin = process_sand(mtx)
    print(f"N grains of sand: {np.sum(fin == 'o')}")
    # newmtx = process_single_grain(mtx)
