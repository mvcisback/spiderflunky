import os
import fnmatch
from operator import itemgetter
from itertools import imap

import click
from funcy import pluck, imapcat, compose, partial

from spiderflunky.typescript import parse


def join_paths(root, paths):
    return imap(partial(os.path.join, root), paths)


@click.command()
@click.option('--root')
def main(root):
    pluck_paths = compose(lambda x: join_paths(*x), itemgetter(0, 2))
    files = fnmatch.filter(imapcat(pluck_paths, os.walk(root)), '*.d.ts')

    for fpath in files:
        with open(fpath) as fp:
            parse(fp)

if __name__ == '__main__':
    main()
