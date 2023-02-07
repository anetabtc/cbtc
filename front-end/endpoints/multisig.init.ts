import { multisigMintingPolicy, multisigValidator } from "@/utils/validators";
import {
	applyParamsToScript,
	Constr,
	Data,
	fromText,
	Lucid,
	MintingPolicy,
	toUnit,
} from "lucid-cardano";
import { ConfigInit } from "./types";

export const init = async (lucid: Lucid, config: ConfigInit) => {
	const walletUtxos = await lucid.wallet.getUtxos();
	const walletTxHash = walletUtxos[0].txHash;
	const walletOutputIndex = walletUtxos[0].outputIndex;

	const multisigValidatorAddr =
		lucid.utils.validatorToAddress(multisigValidator);

	const multisigPolicy: MintingPolicy = {
		type: "PlutusV2",
		script: applyParamsToScript(multisigMintingPolicy.script, [
			config.cosignerKeys[0], // (PAsData PPubKeyHash)
			lucid.utils.validatorToScriptHash(multisigValidator), // (PAsData PScriptHash)
			new Constr(0, [new Constr(0, [walletTxHash]), BigInt(walletOutputIndex)]), // PTxOutRef
		]),
	};
	const multisigPolicyId = lucid.utils.mintingPolicyToId(multisigPolicy);

	const unit = toUnit(multisigPolicyId, fromText("MultiSigCert"));
	const asset = {[unit]: BigInt(1)};

	const Datum = Data.to(
		new Constr(0, [config.cosignerKeys, BigInt(config.threshold)])
	);

	const RedeemerPolicy = Data.to(new Constr(0, [])); // PMintGuardianCrt

	const tx = await lucid
		.newTx()
		.collectFrom([walletUtxos[0]])
		.attachMintingPolicy(multisigPolicy)
		.mintAssets(asset, RedeemerPolicy)
		.mintAssets(asset)
		.payToContract(multisigValidatorAddr, { inline: Datum }, {...asset,lovelace: BigInt(2000000)} )
		.complete();

	const signedTx = await tx.sign().complete();
	const txHash = await signedTx.submit();
	return { txHash, multisigPolicy, unit };
};
