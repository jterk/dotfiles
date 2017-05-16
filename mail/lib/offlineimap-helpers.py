# offlineimap-helpers.py includes the `get_password' function that will pull
# IMAP/SMTP passwords from the OS X keychain; they no longer need to be
# specified in plaintext. To use this feature the "name" of the keychain item
# should be the SMTP server for the account; the "kind" should be "Internet
# password", the "account" should be the account, the "where" should be
# "smtp://SMTP_SERVER".
#
# An easy way to create the right keychain entry is with the following command
# (specifying `-w` last will cause `security` to prompt you for the new
# password:
#
#   security add-internet-password -a ACCOUNT -s SERVER -r smtp -w
#
import subprocess


def get_password(account, server):
    password = subprocess.check_output(
        ["security",
         "find-internet-password",
         "-a", account,
         "-s", server, "-w"]
    )
    return password
