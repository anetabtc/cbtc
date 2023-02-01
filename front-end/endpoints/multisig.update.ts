import { guardianMultisig } from "@/utils/validators";
import { Constr, Data, Lucid } from "lucid-cardano";
import { ConfigUpdate } from "./types";

export const build = async (lucid: Lucid, config: ConfigUpdate) => {
	const scriptUtxo = await lucid.utxoByUnit(config.unit);
	console.log(scriptUtxo.assets);

	const guardianMultisigAddr = lucid.utils.validatorToAddress(guardianMultisig);

	const Datum = Data.to(
		new Constr(0, [
			BigInt(config.newConfig.threshold),
			config.newConfig.cosignerKeys,
		])
	);
	const RedeemerUpdate = Data.to(new Constr(0, [])); // Update

	const signers = config.oldCosignerKeys
		.map((cosignerKey) => {
			return lucid.newTx().addSignerKey(cosignerKey);
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const tx = await lucid
		.newTx()
		.collectFrom([scriptUtxo], RedeemerUpdate)
		.attachSpendingValidator(guardianMultisig)
		.payToContract(guardianMultisigAddr, { inline: Datum }, scriptUtxo.assets)
		.compose(signers)
		.complete();

	return tx;
};

export const signWitness = async (lucid: Lucid, txAsCbor: string) => {
	return await lucid.fromTx(txAsCbor).partialSign();
};

export const assemble = async (
	lucid: Lucid,
	txAsCbor: string,
	witnesses: string[]
) => {
	const signedTx = await lucid.fromTx(txAsCbor).assemble(witnesses).complete();
	const txHash = signedTx.submit();
	return txHash;
};
