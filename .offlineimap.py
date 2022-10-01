#! /usr/bin/env python3
from subprocess import check_output


def get_pass():
    return check_output("gpg -dq ~/.offlineimap.gpg", shell=True).strip(b"\n")
