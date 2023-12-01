"""Home of code for day 16 of AOC 2022."""

from copy import deepcopy
import itertools


def load(fn):
    with open(fn) as f:
        lns = f.readlines()
    g = {}
    opens = set()
    for ln in lns:
        parts = ln.split(" ")
        valve = parts[1]
        flow = int(parts[4].split("=")[1][:-1])
        tunnels = [v[:2] for v in parts[9:]]
        g[valve] = {"id": valve, "flow": flow, "tunnels": tunnels}
        opens.add(valve)
    return g, opens


cache = {}


def max_flow_contrib(g, opens, minute, positions):
    # if opens is None:
    #     __import__("ipdb").set_trace()
    #     pass
    key = (frozenset(opens), minute, frozenset(positions))
    if key in cache:
        return cache[key]

    # if minute < 1:
    #     __import__("ipdb").set_trace()
    #     pass

    if minute <= 0:
        return 0

    flows = [g[pos]["flow"] for pos in positions]
    open_valves = [pos not in opens for pos in positions]
    params_combinations = [
        el
        for el in itertools.product(
            *[
                [(t, ov, flw) for t in g[pos]["tunnels"]]
                for (flw, ov, pos) in zip(flows, open_valves, positions)
            ]
        )
    ]

    contribs = []
    for ((t1, ov1, flw1), (t2, ov2, flw2)) in params_combinations:
        contribs.append(max_flow_contrib(g, opens, minute - 1, [t1, t2]))
        if not ov1 and flw1 != 0 and ov2:
            pos1 = positions[0]
            contrib1 = flw1 * (minute - 1)
            print(f"1 opening: {pos1}, flow: {flw1}")
            nopens = deepcopy(opens)
            nopens.discard(pos1)
            contribs.append(
                contrib1 + max_flow_contrib(g, nopens, minute - 1, [pos1, t2])
            )
        if not ov2 and flw2 != 0 and ov1:
            pos2 = positions[1]
            contrib2 = flw2 * (minute - 1)
            nopens = deepcopy(opens)
            nopens.discard(pos2)
            contribs.append(
                contrib2 + max_flow_contrib(g, nopens, minute - 1, [t1, pos2])
            )
        if not ov2 and flw2 and not ov1 and flw1:
            contrib = flw1 * (minute - 1) + flw2 * (minute - 1)
            nopens = deepcopy(opens)
            [pos1, pos2] = positions
            nopens.discard(pos1)
            nopens.discard(pos2)
            contribs.append(
                contrib + max_flow_contrib(g, nopens, minute - 1, [pos1, pos2])
            )

    max_contrib = max(contribs)
    cache[key] = max_contrib
    return cache[key]


if __name__ == "__main__":
    g, opens = load("input.txt")
    # mc = max_flow_contrib(g, opens, 30, "AA")
    __import__("ipdb").set_trace()
    mc = max_flow_contrib(g, opens, 26, ["AA", "AA"])
    print(mc)
