import { Data, Script, Unit, UTxO } from "lucid-cardano";

export type ValidDatumUTXO = {
	datum: { amountDeposit: bigint; address: string };
	utxo: UTxO;
};

export type AnyDatumUTXO = {
	isValid: boolean;
	datum: { amountDeposit: bigint; address: string } | Data;
	utxo: UTxO;
};

export type ConfigInit = {
	threshold: number;
	cosignerKeys: string[];
};

export type ConfigUpdate = {
	unit: Unit;
	oldCosignerKeys: string[];
	newConfig: ConfigInit;
};

export type ConfigSign = {
	unit: Unit;
	guardianValApplied: Script,
	consignerKeys: string[];
};
