#!/usr/bin/env python3

# This file remove golden (done) tests from ./tests/success.txt


def read_successes():
    with open('./tests/success.txt', 'r') as f:
        return list(set(f.readlines()))


def main():
    new_suceesses = list()
    successed = read_successes()
    with open('tests/done.txt', 'r') as done:
        for d in done.readlines():
            for s in successed:
                new_suceesses.append(s)
    with open('./tests/success.txt', 'w') as f:
        f.write(''.join(new_suceesses))


if __name__ == '__main__':
    main()
