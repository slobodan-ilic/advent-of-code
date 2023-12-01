"""Home of day 13 code."""
import functools


# def load_pairs(fn):
#     with open(fn, "r") as f:
#         contents = f.read()
#         pairs = contents.split("\n\n")
#         res = []
#         for pair in pairs:
#             els = pair.split("\n")
#             e1, e2 = eval(els[0]), eval(els[1])
#             res.append((e1, e2))
#     return res


def load_packets(fn):
    with open(fn, "r") as f:
        contents = f.read()
        pairs = contents.split("\n\n")
        res = []
        for pair in pairs:
            els = pair.split("\n")
            e1, e2 = eval(els[0]), eval(els[1])
            res.append(e1)
            res.append(e2)
    return res


def cmp(e1, e2):
    # print(f"e1: {e1}")
    # print(f"e2: {e2}")
    if isinstance(e1, int) and isinstance(e2, int):
        return e1 - e2
    elif isinstance(e1, int):
        return cmp([e1], e2)
    elif isinstance(e2, int):
        return cmp(e1, [e2])
    elif not e1 and not e2:
        return 0
    elif not e1:
        return -1
    elif not e2:
        return 1
    else:
        el1, el2 = e1[0], e2[0]
        curr = cmp(el1, el2)
        if curr == 0:
            return cmp(e1[1:], e2[1:])
        else:
            return curr
    raise ValueError((e1, e2))


if __name__ == "__main__":
    packets = load_packets("input.txt")
    srt = sorted(packets + [[[2]], [[6]]], key=functools.cmp_to_key(cmp))
    print(f"ind [[2]]: {srt.index([[2]])}")
    print(f"ind [[6]]: {srt.index([[6]])}")
    print(packets)
    # res = 0
    # for i, (e1, e2) in enumerate(pairs):
    #     ord_ = cmp(e1, e2)
    #     if ord_ < 0:
    #         res += i + 1
    #     # print(f"{e1} {'<' if cmp(e1, e2) < 0 else '>='} {e2}")
    # print(f"res: {res}")
