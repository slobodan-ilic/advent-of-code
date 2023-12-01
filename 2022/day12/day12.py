"""Home of python code for day 12 for AOC 2022."""


def create_graph(map_, start, end):
    graph = {}
    m, n = len(map_), len(map_[0])

    for i, row in enumerate(map_):
        for j, el in enumerate(row):
            curr = "a" if (i, j) == start else "z" if (i, j) == end else map_[i][j]
            ngb_inds = [
                (ii, jj)
                for (ii, jj) in [(i, j + 1), (i, j - 1), (i - 1, j), (i + 1, j)]
                if 0 <= ii < m and 0 <= jj < n
            ]
            # __import__("ipdb").set_trace()
            neighbors = [
                # (ii, jj) for (ii, jj) in ngb_inds if ord(map_[ii][jj]) - ord(curr) <= 1
                (ii, jj)
                for (ii, jj) in ngb_inds
                if ord(map_[ii][jj]) - ord(curr) >= -1
            ]
            graph[(i, j)] = neighbors
    return graph


def load(fn):
    with open(fn) as f:
        contents = f.read().strip()
        return contents.split("\n")


def bfs(map_, graph, start, end):
    visited = [start]
    queue = [[start]]
    # paths = []

    while queue:
        path = queue.pop(0)
        m = path[-1]

        for neighbour in graph[m]:
            if neighbour in visited:
                continue
            newpath = path + [neighbour]
            # paths.append(newpath)

            i, j = neighbour
            if map_[i][j] == "a":
                return newpath

            # if neighbour == end:
            #     continue

            visited.append(neighbour)
            queue.append(newpath)
    return None


def start_end(map_):
    for i, row in enumerate(map_):
        for j, el in enumerate(row):
            if el == "S":
                start = i, j
            if el == "E":
                end = i, j
    return start, end


if __name__ == "__main__":
    map_ = load("input.txt")
    start, end = start_end(map_)
    graph = create_graph(map_, start, end)
    # print(graph)
    # print(graph)
    # paths = sorted(bfs(map_, graph, start, end))
    path = bfs(map_, graph, end, start)
    print(path)
    print(len(path) - 1)
    # paths = [p for p in sorted(bfs(map_, graph, start, end)) if p[-1] == end]
    # paths = [
    #     p
    #     for p in sorted(bfs(map_, graph, end, start))
    #     if map_[p[-1][0]][p[-1][1]] == "a"
    # ]
    # print("paths")
    # for pth in sorted(paths):
    #     print(pth)
    # print(f"shortest: {len(paths[0])}")
    # print("\n".join(map_))
    # print(path)
