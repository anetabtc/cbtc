import { Data, KeyHash, Script, TxHash, Unit, UTxO } from "lucid-cardano"

export type ValidDatumUTXO = {
  datum: { bridgeAmount: bigint; cardanoAddress: string; btcAddress: string }
  utxo: UTxO
}

export type AnyDatumUTXO = {
  isValid: boolean
  datum: { amountDeposit: bigint; address: string } | Data
  utxo: UTxO
}

export type ConfigMultiSig = {
  keys: KeyHash[]
  requiredCount: number
}

export type ConfigUpdateMultiSig = {
  multiSigValidator: Script
  unit: Unit
  oldKeys: KeyHash[]
  newConfig: ConfigMultiSig
}

export type ConfigFullFill = {
  unit: Unit
  scripts: DeployedScripts
  keys: KeyHash[]
}

export type DeployedScripts = {
  multiSigValidator: Script
  multiSigMintingPolicy: Script
  guardianValidator: Script
  cBTCMintingPolicy: Script
}

export type Deployments = {
  txHash: TxHash
  scripts: DeployedScripts
  units: {
    multiSigCert: Unit
    cBTC: Unit
  }
}

type TransactionStatus = {
  confirmed: boolean
  block_height: number
  block_hash: string
  block_time: number
}

export type BTCTransaction = {
  txid: string
  version: number
  locktime: number
  vin: any[]
  vout: any[]
  size: number
  weight: number
  fee: number
  status: TransactionStatus
}

type ADAOutputAmount = {
  unit: string
  quantity: string
}

export type ADATransaction = {
  hash: string
  block: string
  block_height: number
  block_time: number
  slot: number
  index: number
  output_amount: ADAOutputAmount
  fees: string
  deposit: string
  size: number
  invalid_before: string | null
  invalid_hereafter: string | null
  utxo_count: number
  withdrawal_count: number
  mir_cert_count: number
  delegation_count: number
  stake_cert_count: number
  pool_update_count: number
  pool_retire_count: number
  asset_mint_or_burn_count: number
  redeemer_count: number
  valid_contract: boolean
}

type JSON_METADATA = { [key: string]: string }

export type ADATransactionMetaData = {
  label: string
  json_metadata: JSON_METADATA
}
