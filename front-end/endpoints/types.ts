import { Data, UTxO } from "lucid-cardano";

export type ValidDatumUTXO = {
	datum: { amountDeposit: bigint; address: string };
	utxo: UTxO;
};

export type AnyDatumUTXO = {
	isValid: boolean;
	datum: { amountDeposit: bigint; address: string } | Data;
	utxo: UTxO;
};
