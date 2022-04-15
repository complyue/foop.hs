class Animal:
    def greet(self, mate):
        print(f"{type(self).__name__} remains silent facing {mate}.")


class Spider(Animal):
    pass


class Cat(Animal):
    def greet(self, mate):
        print(f"Meow, meow!")


class Dog(Animal):
    def greet(self, mate):
        print(f"Woof, woof!")


class People(Animal):
    def greet(self, mate):
        print(f"Hello {mate}!")


def main():
    Spider().greet("you")
    Cat().greet("you")
    Dog().greet("you")
    People().greet("you")


main()
