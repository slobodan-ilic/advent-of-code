"""Home of code for day 14, AOC 2022."""

import numpy as np


def load(fn):
    with open(fn) as f:
        lns = f.readlines()
    points = []
    for ln in lns:
        parts = ln.split("closest")
        xs = int(parts[0].split(" ")[2][2:-1])
        ys = int(parts[0].split(" ")[3][2:-1])
        xb = int(parts[1].split(" ")[4][2:-1])
        yb = int(parts[1].split(" ")[5][2:-1])
        md = abs(xs - xb) + abs(ys - yb)
        points.append(((xs, ys), (xb, yb), md))
    # minx = min(min(xs, xb) for (xs, _), (xb, _), _ in points)
    # maxx = max(max(xs, xb) for (xs, _), (xb, _), _ in points)
    # miny = min(min(ys, yb) for (_, ys), (_, yb), _ in points)
    # maxy = max(max(ys, yb) for (_, ys), (_, yb), _ in points)
    # print(minx, miny, maxx, maxy)

    # inds = []
    # for ((xs, ys), (xb, yb), md) in points:
    #     is_, js_ = ys - miny, xs - minx
    #     ib_, jb_ = yb - miny, xb - minx
    #     inds.append(((is_, js_), (ib_, jb_), md))

    return points
    # return inds, maxy - miny + 1, maxx - minx + 1


def create_tunnels(inds, m, n):
    tunnels = np.full((m, n), ".")
    for (is_, js_), (ib_, jb_), md in inds:
        tunnels[is_, js_] = "S"
        tunnels[ib_, jb_] = "B"
    return tunnels


def create_md_map(mtx, ip, jp):
    m, n = mtx.shape
    return np.array([[abs(i - ip) + abs(j - jp) for j in range(n)] for i in range(m)])


def process(it, lns, m, n):
    for (is_, js_), (ib_, jb_), md in lns:
        pass


def show(tunnels):
    print("\n")
    print("\n")
    print("\n".join("".join(el for el in ln) for ln in tunnels))


def is_inside_range(i, j, lns, m, n):
    for (is_, js_), (ib_, jb_), tmd in lns:
        md = abs(is_ - i) + abs(js_ - j)
        # print(((i, j), (is_, js_), (ib_, jb_), tmd, md, md <= tmd))
        if md <= tmd and (i, j) != (is_, js_) and (i, j) != (ib_, jb_):
            return True
    __import__("ipdb").set_trace()
    return False


def process_line(i, lns, m, n):
    res = 0
    for j in range(-2, n + 2):
        res += is_inside_range(i, j, lns, m, n)
    return res


if __name__ == "__main__":
    points = load("input.txt")
    # min_, max_ = 0, 20
    min_, max_ = 0, 4000000
    for (xs, ys), (xb, yb), maxmd in points:
        # if (xs, ys) != (20, 1):
        #     continue
        print("\n\n")
        print("*************")
        print(xs, ys, xb, yb)
        x0, y0 = xs - maxmd - 1, ys
        print(x0, y0)
        print("-----------------")
        for x, y in zip(range(x0, x0 + maxmd + 1), range(y0, y0 - maxmd - 1, -1)):
            if x < min_ or x > max_ or y < min_ or y > max_:
                continue
            # print(f"edge: {(x, y)}")
            for (xss, yss), (xbb, ybb), mdt in points:
                if (xss, yss) == (xs, ys):
                    continue
                md = abs(x - xss) + abs(y - yss)
                if md <= mdt:
                    break
            else:
                print("BOOM1")
                print(xss, yss)
                print(x, y)
                print(f"freq: {x*4000000+y}")

        x1, y1 = xs, ys - maxmd - 1
        for x, y in zip(range(x1, x1 + maxmd + 1), range(y1, y1 + maxmd + 1)):
            # print(x, y)
            if x < min_ or x > max_ or y < min_ or y > max_:
                continue
            # print(f"edge: {(x, y)}")
            for (xss, yss), (xbb, ybb), mdt in points:
                if (xss, yss) == (xs, ys):
                    continue
                md = abs(x - xss) + abs(y - yss)
                if md <= mdt:
                    break
            else:
                print("BOOM2")
                print(xss, yss)
                print(x, y)
                print(f"freq: {x*4000000+y}")

        x2, y2 = xs + maxmd + 1, ys
        for x, y in zip(range(x2, x2 - maxmd - 1, -1), range(y2, y2 + maxmd + 1)):
            if x < min_ or x > max_ or y < min_ or y > max_:
                continue
            # print(f"edge: {(x, y)}")
            for (xss, yss), (xbb, ybb), mdt in points:
                if (xss, yss) == (xs, ys):
                    continue
                md = abs(x - xss) + abs(y - yss)
                if md <= mdt:
                    break
            else:
                print("BOOM3")
                print(xss, yss)
                print(x, y)
                print(f"freq: {x*4000000+y}")

        x3, y3 = xs, ys + maxmd + 1
        for x, y in zip(range(x3, x3 - maxmd - 1, -1), range(y3, y3 - maxmd - 1, -1)):
            if x < min_ or x > max_ or y < min_ or y > max_:
                continue
            # print(f"edge: {(x, y)}")
            for (xss, yss), (xbb, ybb), mdt in points:
                if (xss, yss) == (xs, ys):
                    continue
                md = abs(x - xss) + abs(y - yss)
                if md <= mdt:
                    break
            else:
                print("BOOM4")
                print(xss, yss)
                print(x, y)
                print(f"freq: {x*4000000+y}")
