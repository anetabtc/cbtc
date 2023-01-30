import { Data, Unit, UTxO } from "lucid-cardano";

export type ValidDatumUTXO = {
	datum: { amountDeposit: bigint; address: string };
	utxo: UTxO;
};

export type AnyDatumUTXO = {
	isValid: boolean;
	datum: { amountDeposit: bigint; address: string } | Data;
	utxo: UTxO;
};


export type ConfigMultisig = {
	threshold : number
	cosignerKeys : string[]
}

export type ConfigUpdate = {
	unit: Unit;
	oldCosignerKeys: string[];
	newConfig : ConfigMultisig;
}
