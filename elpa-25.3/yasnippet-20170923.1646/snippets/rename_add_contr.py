#!/usr/bin/env python
import os
import re
from os.path import join
from shutil import move


def rename(root, f):
    if f.endswith('.yasnippet'):
        base, _ = f.split('.')
        print("move %s to %s" % (join(root, f), join(root, base)))
        move(join(root, f), join(root, base))


CONT = "# contributor: Andrea crotti\n# --"
END = "# --\n\n"

orig = "# --\n\n"
to  = "# --\n"

def insert(root, f, orig, to):
    fname = join(root, f)
    text = open(fname).read()
    nex_text = re.sub(orig, to, text)
    open(fname, 'w').write(nex_text)

if __name__ == '__main__':
    for root, dirs, files in os.walk('.'):
        if "mode" in root:
            # os.popen("git add *yasnippet")
            for f in files:
                rename(root, f)
                # insert(root, f, orig, to)


            
