"""Home of code for day 16 of AOC 2022."""

# from copy import deepcopy
import itertools


def load(fn):
    with open(fn) as f:
        lns = f.readlines()
    g = {}
    closed_valves = set()
    for ln in lns:
        parts = ln.split(" ")
        valve = parts[1]
        flow = int(parts[4].split("=")[1][:-1])
        tunnels = [v[:2] for v in parts[9:]]
        g[valve] = {"id": valve, "flow": flow, "tunnels": tunnels}
        if flow:
            closed_valves.add(valve)
    return g, frozenset(closed_valves)


def path_release(g, path, nmins):
    for (closed, p1, p2), m in path:
        pass


visited = set()


def bfs(g, node, nmins):
    visited.add(node)
    queue = [[(node, 0)]]
    max_ = 0

    while queue:
        path = queue.pop(0)
        (closed, p1, p2), m = path[-1]
        if not closed or m == nmins:
            __import__("ipdb").set_trace()

        ts1, ts2 = g[p1]["tunnels"], g[p2]["tunnels"]
        neighbors = (
            [(closed - {p1} - {p2}, p1, p2)]
            + [(closed - {p1}, p1, t2) for t2 in ts2]
            + [(closed - {p2}, t1, p2) for t1 in ts1]
            + [(closed, t1, t2) for t1, t2 in itertools.product(ts1, ts2)]
        )

        for n in neighbors:
            if n in visited:
                continue

            visited.add(n)
            queue.append(path + [(n, m + 1)])
    print(len(visited))


if __name__ == "__main__":
    g, closed = load("input.txt")
    bfs(g, (closed, "AA", "AA"), 26)
