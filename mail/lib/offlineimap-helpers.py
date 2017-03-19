import os
import re
import subprocess

def get_password(account, server):
    devnull = os.open('/dev/null', os.O_WRONLY)
    password = subprocess.check_output(["security", "find-internet-password", "-a", account, "-s", server, "-w"])
    return password
