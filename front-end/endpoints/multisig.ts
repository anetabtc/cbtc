import { guardianMultisig, oneShotPolicy } from "@/utils/validators";
import {
	applyParamsToScript,
	Constr,
	Data,
	fromText,
	Lucid,
	MintingPolicy,
	toUnit,
	Unit,
} from "lucid-cardano";
import { Script } from "vm";
import { ConfigMultisig, ConfigUpdate } from "./types";

export const initialize = async (lucid: Lucid, config: ConfigMultisig) => {
	const walletUtxos = await lucid.wallet.getUtxos();
	const walletTxHash = walletUtxos[0].txHash;
	const walletOutputIndex = walletUtxos[0].outputIndex;

	// TODO: we need a guardian multisig script
	// using Always succeeds for now
	const guardianMultisigAddr = lucid.utils.validatorToAddress(guardianMultisig);

	// TODO: we need a new oneshot policy
	// using a temp minting policy which force to spend the utxo of the parameter and use "yep" as tokenname
	const policy: MintingPolicy = {
		type: "PlutusV1",
		script: applyParamsToScript(oneShotPolicy.cborHex, [
			new Constr(0, [new Constr(0, [walletTxHash]), BigInt(walletOutputIndex)]),
		]),
	};

	const policyId = lucid.utils.mintingPolicyToId(policy);
	console.log(policyId);

	const unit = toUnit(policyId, fromText("yep"));
	const asset = { [unit]: BigInt(1) };
	console.log(policy);

	const Datum = Data.to(
		new Constr(0, [BigInt(config.threshold), config.cosignerKeys])
	);

	const RedeemerPolicy = Data.to(new Constr(0, []));

	const tx = await lucid
		.newTx()
		.collectFrom([walletUtxos[0]])
		.attachMintingPolicy(policy)
		.mintAssets(asset, RedeemerPolicy)
		.payToContract(guardianMultisigAddr, { inline: Datum }, asset)
		.complete();

	const signedTx = await tx.sign().complete();
	const txHash = await signedTx.submit();
	return { txHash, policy, unit };
};

export const update = async (lucid: Lucid, config: ConfigUpdate) => {
	const scriptUtxo = await lucid.utxoByUnit(config.unit);
	const datumAsCbor = scriptUtxo.datum || "";
	const datumAsData = Data.from(datumAsCbor);
	console.log("old threshold: ", datumAsData.fields[0]);
	console.log("old cosigners: ", datumAsData.fields[1]);
	console.log("new config", config.newConfig);
	console.log(scriptUtxo.assets)

	const guardianMultisigAddr = lucid.utils.validatorToAddress(guardianMultisig);

	const Datum = Data.to(
		new Constr(0, [
			BigInt(config.newConfig.threshold),
			config.newConfig.cosignerKeys,
		])
	);
	const RedeemerUpdate = Data.to(new Constr(0, []));

	const tx = await lucid
		.newTx()
		.collectFrom([scriptUtxo], RedeemerUpdate)
		.attachSpendingValidator(guardianMultisig)
		.payToContract(guardianMultisigAddr, Datum, scriptUtxo.assets)
		.complete();
	
	// TODO: convert to a partial sign so that the old cosigners can sign this tx

	const signedTx = await tx.sign().complete();
	const txHash = await signedTx.submit();
	return txHash

};
