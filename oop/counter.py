class Counter:
    def __init__(self, n: int) -> None:
        self.n = n

    def increase(self) -> int:
        self.n += 1
        return self.n


counter = Counter(7)

counter.increase()

result = counter.increase()

print("Done, result= ", result)
