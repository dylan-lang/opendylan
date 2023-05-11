#!/usr/bin/python3

def main ():
    s = "x" * 100
    with open("/tmp/100mb.python.txt", "w") as fd:
        for i in range(1024 * 1024):
            fd.write(s)

if __name__ == "__main__":
    main()
