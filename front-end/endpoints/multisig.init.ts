import { guardianMultisig } from "@/utils/validators";
import { Constr, Data, fromText, Lucid, toUnit } from "lucid-cardano";
import { ConfigInit } from "./types";

export const init = async (lucid: Lucid, config: ConfigInit) => {
	const walletUtxos = await lucid.wallet.getUtxos();
	const walletTxHash = walletUtxos[0].txHash;
	const walletOutputIndex = walletUtxos[0].outputIndex;

	console.log(walletUtxos);

	// TODO: we need a guardian multisig script
	// using Always succeeds for now
	const guardianMultisigAddr = lucid.utils.validatorToAddress(guardianMultisig);

	// TODO: we need a new oneshot policy, using a temp native minting policy 
	const { paymentCredential } = lucid.utils.getAddressDetails(
		await lucid.wallet.address()
	);
	const policy = lucid.utils.nativeScriptFromJson({
		type: "all",
		scripts: [
			{ type: "sig", keyHash: paymentCredential?.hash! },
			{
				type: "before",
				slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),
			},
		],
	});

	const policyId = lucid.utils.mintingPolicyToId(policy);

	const unit = toUnit(policyId, fromText("MultiSig"));
	const asset = { [unit]: BigInt(1) };
	console.log(policy);

	const Datum = Data.to(
		new Constr(0, [BigInt(config.threshold), config.cosignerKeys])
	);

	const RedeemerPolicy = Data.to(new Constr(0, []));

	//TODO: disable when using one shot policy
	const tx = await lucid
		.newTx()
		.attachMintingPolicy(policy)
		.validTo(Date.now() + 100000)
		.mintAssets(asset)
		.payToContract(guardianMultisigAddr, { inline: Datum }, asset)
		.complete();

	//TODO: enable when using one shot policy
	// const tx = await lucid
	// 	.newTx()
	// 	.collectFrom([walletUtxos[0]])
	// 	.attachMintingPolicy(policy)
	// 	.mintAssets(asset, RedeemerPolicy)
	// 	.mintAssets(asset)
	// 	.payToContract(guardianMultisigAddr, { inline: Datum }, asset)
	// 	.complete();

	const signedTx = await tx.sign().complete();
	const txHash = await signedTx.submit();
	return { txHash, policy, unit };
};
