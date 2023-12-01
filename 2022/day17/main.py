"""Home of code for day 17 of AOC 2022."""

import numpy as np
import os
import time


def load(fn):
    with open(fn) as f:
        cmds = f.read()
    return cmds


def table():
    return np.full((5, 7), ".")


def place_new(tbl, nrock):
    h = height(tbl)
    shape = nrock % 5
    if h + 7 > tbl.shape[0]:
        tbl = np.append(np.full((4, 7), "."), tbl, axis=0)

    if shape == 1:  # -
        tbl[-1 - h - 3, 2:6] = "@"
    elif shape == 2:  # +
        tbl[-1 - h - 4, 2:5] = "@"
        tbl[-1 - h - 3, 3] = "@"
        tbl[-1 - h - 5, 3] = "@"
    elif shape == 3:  # L
        tbl[-1 - h - 3, 2:5] = "@"
        tbl[(-1 - h - 5) : (-1 - h - 3), 4] = "@"  # noqa
    elif shape == 4:  # |
        tbl[(-1 - h - 2 - 4) : (-1 - h - 2), 2] = "@"  # noqa
    elif shape == 0:  # ▫️
        tbl[-1 - h - 3, 2:4] = "@"
        tbl[-1 - h - 4, 2:4] = "@"

    return tbl


def move_horiz(tbl, cmd):
    (I, J) = np.where(tbl == "@")
    d = -1 if cmd == "<" else 1
    Jn = J + d
    if not np.all((Jn >= 0) & (Jn < tbl.shape[1])):
        return tbl
    if not np.all(tbl[I, Jn] != "#"):
        return tbl
    tbl[I, J] = "."
    tbl[I, Jn] = "@"
    return tbl


def move_down(tbl):
    (I, J) = np.where(tbl == "@")
    In = I + 1
    if not np.all((In >= 0) & (In < tbl.shape[0])):
        tbl[I, J] = "#"
        return tbl, True
    if not np.all(tbl[In, J] != "#"):
        tbl[I, J] = "#"
        return tbl, True
    tbl[I, J] = "."
    tbl[In, J] = "@"
    return tbl, False


def move(tbl, cmd):
    tbl = move_horiz(tbl, cmd)
    return move_down(tbl)


def height(tbl):
    return np.any(tbl != ".", axis=1).sum()


def show(tbl):
    # os.system("clear")
    print("\n------------------------")
    print("\n".join("".join(e for e in row) for row in tbl[:50, :]))
    # time.sleep(0.1)


def process_one_shape(cmds, tbl, n, icmd):
    tbl = place_new(tbl, n)
    # show(tbl)
    stopped = False
    ncmds = len(cmds)
    while not stopped:
        tbl, stopped = move(tbl, cmds[icmd % ncmds])
        # show(tbl)
        # print(f"icmd: {icmd}: step: {n}, height: {height(tbl)}")
        icmd += 1
    return tbl, icmd % ncmds


cache = {}


def process_shapes(tbl, cmds, start=1, n=1000):
    icmd = 0
    for nrock in range(start, n + 1):
        tbl, icmd = process_one_shape(cmds, tbl, nrock, icmd)
        hgh = height(tbl)
        key = icmd, nrock % 5, "".join(tbl[-hgh, :])
        if key in cache:
            prevh, prevnrock = cache[key]
            totalh = (1000000000000 - prevnrock) // (nrock - prevnrock) * (
                hgh - prevh
            ) + prevh
            rest = (1000000000000 - prevnrock) % (nrock - prevnrock)
            if rest == 0:
                print(f"total: {totalh}, nrock: {nrock}, rest: {rest}")
        cache[key] = hgh, nrock
        # print(icmd, nrock % 5)
        # print(tbl[-height(tbl), :])
        # show(tbl)
        # __import__("ipdb").set_trace()
        # print(tbl[-hgh, :])
        # if np.all(tbl[-hgh, 3] == "#") and icmd == 0 and nrock % 5 == 1:
        # if icmd == 0:
        # print("BOOM")
        # show(tbl)
        # print(icmd, nrock, nrock % 5, height(tbl))
        # show(tbl)
        # if np.any(np.all(tbl == "#", axis=1)):
        # cutoff = np.where(np.all(tbl == "#", axis=1))
        # hgh += height(tbl)
        # tbl = tbl[: cutoff[0][0], :]
        # hgh -= height(tbl)
    # print(height(tbl))
    # print(hgh)
    # print(hgh + height(tbl))
    return tbl


if __name__ == "__main__":
    cmds = load("input.txt").strip()
    print(set(cmds))
    tbl = table()
    tbl = process_shapes(tbl, cmds, 1, 10000)
    print(height(tbl))
