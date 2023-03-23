import sys
import argparse
import logging
import multiprocessing
import os
import time
from datetime import datetime, timedelta
from decimal import ROUND_DOWN, Decimal
from multiprocessing import Process
from random import randint
from time import sleep

import pytz
import requests
import urllib3
from pytz import timezone

from bitcoinlib.services.services import Service
from bitcoinlib.wallets import wallet_create_or_open

assert len(sys.argv) == 5
assert sys.argv[4] == "password"

print("Running hello.py")

sender_addr = sys.argv[1]
amount = float(sys.argv[2])
receiver_addr = sys.argv[3]

# IMPORTANT PARAMETERS from ENV Variables
VAULT_BTC_WALLET_ADDRESS = os.getenv('VAULT_BTC_WALLET_ADDRESS')
VAULT_BTC_WALLET_ID = os.getenv('VAULT_BTC_WALLET_ID')
VAULT_BTC_WALLET_MNEMONIC = os.getenv('VAULT_BTC_WALLET_MNEMONIC')

DB_URI = os.getenv('DB_URI')

for VAR in [
        VAULT_BTC_WALLET_ADDRESS, VAULT_BTC_WALLET_ID,
        VAULT_BTC_WALLET_MNEMONIC
]:
    if VAR is None:
        raise Exception("Important ENV Variable was not found")
print("Succesfully loaded ENV vars.")

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
UTC_TZ = pytz.utc
EST_TZ = timezone('US/Eastern')

w = wallet_create_or_open(VAULT_BTC_WALLET_ID,
                            keys=VAULT_BTC_WALLET_MNEMONIC,
                            network='testnet')
w.scan()

MAX_TIMESTEPS = 2
REQUEST_RATE = 300  # sec

def redeem(btc_wallet_addr, amount):  # pragma: no cover
        """Redeems BTC to user BTC wallet address."""
        # Re connect wallet to API everytime. Might be unecessary.
        # Should only be on Error/Disconnect.
        #if not args.local:
        #    w = wallet_create_or_open(VAULT_BTC_WALLET_ID,
        #                              keys=VAULT_BTC_WALLET_MNEMONIC,
        #                              network='testnet',
        #                              db_uri=db_uri)
        #else:
        #    w = wallet_create_or_open(VAULT_BTC_WALLET_ID,
        #                              keys=VAULT_BTC_WALLET_MNEMONIC,
        #                              network='testnet')
        #w.scan()
        t = 0
        tx_id = None
        amount = int(
            int(amount) * 0.995) - 10000  # 0.0001 and 0.5% for bridge fee
        try:
            while t <= MAX_TIMESTEPS:
                tx_id = w.send_to(btc_wallet_addr,
                                  amount,
                                  fee=2000,
                                  offline=False)
                if tx_id:
                    print("Success")
                    return True
                time.sleep(REQUEST_RATE)
                t += 1
        except Exception as e:
            print(
                f"Failed to send from {VAULT_BTC_WALLET_ADDRESS} to {btc_wallet_addr} amount {amount}"
            )
            print("Error - {}".format(str(e)))
        print("Failed sending timed out")
        return False

assert sender_addr == VAULT_BTC_WALLET_ADDRESS
result = redeem(receiver_addr, amount)
print(sender_addr, amount, receiver_addr, "status =", result)