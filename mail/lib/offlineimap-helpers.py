import os
import re
import subprocess

# offlineimap-helpers.py includes the `get_password' function that will pull
# IMAP/SMTP passwords from the OS X keychain; they no longer need to be
# specified in plaintext. To use this feature the "name" of the keychain item
# should be the SMTP server for the account; the "kind" should be "Internet
# password", the "account" should be the account, the "where" should be
# "smtp://SMTP_SERVER".
def get_password(account, server):
    devnull = os.open('/dev/null', os.O_WRONLY)
    password = subprocess.check_output(["security", "find-internet-password", "-a", account, "-s", server, "-w"])
    return password
