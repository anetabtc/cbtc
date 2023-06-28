import { Network, Tx } from "lucid-cardano"
import mempoolJS from "@mempool/mempool.js"
import { MempoolReturn } from "@mempool/mempool.js/lib/interfaces/index"
import { ADATransaction, ADATransactionMetaData } from "../endpoints/types"

// Environment Variables
const blockfrostKey = process.env.BLOCKFROST_KEY as string
const adaAPI = process.env.ADA_API
const mintPolicyID = process.env.MINT_POLICY_ID as string
const btcVaultAddress = process.env.VAULT_BTC_WALLET_ADDRESS

// Type: Get
// Description: Get Pending ADA Transactions to Policy and return Array
export const getPendingADATransactionsToPolicy = async (): Promise<
  string[]
> => {
  try {
    const res: Response = await fetch(
      adaAPI + "assets/" + mintPolicyID + "/history?count=30&page=1&order=desc",
      {
        headers: {
          PROJECT_ID: blockfrostKey,
          "Content-Type": "application/json",
        },
        method: "GET",
      }
    )

    let txs: string[] = await res.json()

    return txs
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// This is wrong /// TODO: Remove?
// Description: Get Pending BTC Transactions and return Array
export const getPendingADATransactions = async (): Promise<string[]> => {
  try {
    const res = await fetch(adaAPI + "assets/" + mintPolicyID + "/history", {
      headers: {
        PROJECT_ID: blockfrostKey,
        "Content-Type": "application/json",
      },
      method: "GET",
    })

    let txs: string[] = await res.json()

    return txs ?? []
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// Type: Get
// Description: Get ADA Transaction by tx id
export const getADATransaction = async (tx: string) => {
  try {
    const res = await fetch(adaAPI + "txs/" + tx, {
      headers: {
        PROJECT_ID: blockfrostKey,
        "Content-Type": "application/json",
      },
      method: "GET",
    })

    let data: ADATransaction = await res.json()

    return data
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// Type: Get
// Description: Get ADA Transaction UTXO by TX id
export const getADATransactionUTXOs = async (tx: string) => {
  try {
    const res = await fetch(adaAPI + "txs/" + tx + "/utxos", {
      headers: {
        PROJECT_ID: blockfrostKey,
        "Content-Type": "application/json",
      },
      method: "GET",
    })

    let data: { key: string | number | boolean }[] = await res.json()

    return data ?? {}
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// Type: Get
// Description: Get ADA Transaction Metada by TX id
export const getADATransactionMetadata = async (tx: string) => {
  try {
    const res = await fetch(adaAPI + "txs/" + tx + "/metadata", {
      headers: {
        PROJECT_ID: blockfrostKey,
        "Content-Type": "application/json",
      },
      method: "GET",
    })

    let data: ADATransactionMetaData[] = await res.json()

    return data
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// Type: Get with mempool
// Description: Get BTC Transaction by TX id
export const getBTCTransactionMP = async (txid) => {
  try {
    const {
      bitcoin: { transactions },
    }: Pick<MempoolReturn, "bitcoin"> = mempoolJS({
      hostname: "mempool.space",
      network: "testnet",
    })

    const tx = await transactions.getTx({ txid })

    return tx
  } catch (error) {
    console.log("ERROR:", error)
  }
}

// Type: Get with mempool
// Description: Get Pending BTC Transactions and stored them in Array
export const getPendingBTCTransactionsMP = async (): Promise<string[] | []> => {
  try {
    const {
      bitcoin: { addresses },
    }: Pick<MempoolReturn, "bitcoin"> = mempoolJS({
      hostname: "mempool.space",
      network: "testnet",
    })

    const address: string = btcVaultAddress
    const data = await addresses.getAddressTxsChain({ address })

    let txs: string[] = []
    for (let i in data) {
      txs.push(data[i].txid)
    }
    return txs ?? []
  } catch (error) {
    console.log("ERROR:", error)
  }
}
