"""Home of code for day18 of AOC 2022."""


def load(fn):
    with open(fn) as f:
        lns = f.readlines()
    cubes = [tuple(int(coord) for coord in ln.split(",")) for ln in lns]
    return cubes


def surrounding(cube):
    x, y, z = cube
    return set(
        [
            (x + 1, y, z),
            (x - 1, y, z),
            (x, y + 1, z),
            (x, y - 1, z),
            (x, y, z + 1),
            (x, y, z - 1),
        ]
    )


if __name__ == "__main__":
    cbs = set(load("input.txt"))
    pockets = set()
    airs = set()
    free = len(cbs) * 6
    for cb in cbs:
        surr = surrounding(cb)
        covered = len(surr & cbs)
        airs.update(surr)
        free -= covered
    airs.difference_update(cbs)
    print(f"Initial free: {free}")

    # Fill entire chunk of space with air
    minx = min(x for x, _, _ in cbs)
    maxx = max(x for x, _, _ in cbs)
    miny = min(y for _, y, _ in cbs)
    maxy = max(y for _, y, _ in cbs)
    minz = min(z for _, _, z in cbs)
    maxz = max(z for _, _, z in cbs)
    for x in range(minx - 1, maxx + 2):
        for y in range(miny - 1, maxy + 2):
            for z in range(minz - 1, maxz + 2):
                if (x, y, z) not in cbs:
                    airs.add((x, y, z))

    # eliminate outers
    while outers := set(air for air in airs if surrounding(air) - airs - cbs):
        airs.difference_update(outers)

    for air in airs:
        contact = surrounding(air) & cbs
        free -= len(contact)

    # edgeair = [
    #     (x, y, z)
    #     for x, y, z in airs
    #     if (
    #         (x <= minx or x >= maxx)
    #         or (y <= miny or y >= maxy)
    #         or (z <= minz or z >= maxz)
    #     )
    # ]
    # airs.difference_update(edgeair)

    # comps = []
    # aircomp = set([airs.pop()])
    # while airs:
    #     conns = set(s for a in aircomp for s in surrounding(a)) & airs
    #     if not conns:
    #         comps.append(aircomp)
    #         aircomp = set([airs.pop()])
    #     else:
    #         aircomp.update(conns)
    #         airs.difference_update(conns)
    # if aircomp:
    #     comps.append(aircomp)

    # for pocket in comps:
    #     surr = set(s for a in pocket for s in surrounding(a)).difference(pocket)
    #     if not surr.difference(cbs):
    #         for sr in surr:
    #             contact = surrounding(sr) & pocket
    #             free -= len(contact)

    # print(free)

    # for air in airs:
    #     surr = surrounding(air)
    #     covered = len(surr & cbs)
    #     if covered == 6:
    #         free -= covered
    #     # print(f"air: {air}, covered: {covered}")
    # print(airs)
    print(free)
