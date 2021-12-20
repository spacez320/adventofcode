#
# Advent of Code 2021, Day 1
#
# Reads a list of numbers and returns how many times a number is followed by a larger number.
#
# Usage: python 1.py

def main():
    increases = 0

    # Read the file.
    input_d = []
    with open("1.txt") as f:
        input_d = f.readlines()

    # Loop each line and count increasing numbers.
    prev = None
    for line in input_d:
        if prev is not None and int(line) > int(prev):
            increases += 1
        prev = line

    # Print the result.
    print(f"Count: {len(input_d)}")
    print(f"Increases: {increases}")


if __name__ == "__main__":
    main()
