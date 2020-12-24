"""Advent of code, day 23, pt II"""


class Node:
    def __init__(self, val, next_=None):
        self.val = val
        self.next = next_

    # def __repr__(self):
    #     xs = []
    #     node = self
    #     # while node is not None:
    #     while node.next is not self:
    #         xs.append(node.val)
    #         node = node.next
    #     else:
    #         xs.append(node.val)
    #     return str(xs)


class Game:
    def __init__(self, node):
        self.head = node
        self.cmap = {}
        node = self.head
        self.max = node.val if node is not None else 0
        while node is not None:
            self.cmap[node.val] = node
            node = node.next
        self.curr = self.head
        self.tail = self.head

    def __repr__(self):
        xs = []
        node = self.head
        while node.next is not self.head:
            s = str(node.val)
            if node == self.curr:
                s = "(" + s + ")"
            xs.append(s)
            node = node.next
        else:
            s = str(node.val)
            if node == self.curr:
                s = "(" + s + ")"
            xs.append(s)
        return ", ".join(xs)

    def add(self, node):
        self.cmap[node.val] = node
        node.next = self.head
        self.tail = self.head
        self.head = node
        self.curr = self.head
        self.max = max(node.val, self.max)

    def insertAt(self, val, segment):
        node = self.cmap[val]
        fst = lst = segment
        while lst.next is not None:
            self.cmap[lst.val] = lst
            lst = lst.next
        lst.next = node.next
        node.next = fst

    def takeNext3(self, val):
        node = self.cmap[val]
        seg = node.next
        end = seg.next.next
        jmp = end.next
        node.next = jmp
        end.next = None
        return seg

    def loop(self):
        node = self.head
        while node.next is not None:
            node = node.next
        node.next = self.head

    @classmethod
    def populate(cls):
        g = Game(None)
        for n in reversed(range(10, 1000001)):
            # for n in reversed(range(10, 101)):
            g.add(Node(n))
        # nums = [3, 8, 9, 1, 2, 5, 4, 6, 7]
        nums = [9, 6, 2, 7, 1, 3, 8, 5, 4]
        for n in reversed(nums):
            g.add(Node(n))
        g.loop()
        return g

    def dest(self, k, seg):
        d = k - 1 or self.max
        picked = [seg.val, seg.next.val, seg.next.next.val]
        while True:
            if d > 0 and d not in picked and d in self.cmap:
                return d
            d -= 1
            if d <= 0:
                d = self.max

    def move(self):
        k = self.curr.val
        seg = self.takeNext3(k)
        d = self.dest(self.curr.val, seg)
        self.insertAt(d, seg)
        self.curr = self.curr.next


if __name__ == "__main__":
    g = Game.populate()
    for i in range(10000000):
        g.move()
    res = g.cmap[1].next.val * g.cmap[1].next.next.val
    print("res: %s" % res)
