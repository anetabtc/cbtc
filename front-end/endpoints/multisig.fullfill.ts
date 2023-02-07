import {
	cBTCMintingPolicy,
	multisigValidator,
	guardianValidator,
} from "@/utils/validators";
import { Constr, Lucid, Data, fromText, toUnit, SpendingValidator, applyParamsToScript, MintingPolicy } from "lucid-cardano";
import { ConfigSign, ValidDatumUTXO } from "./types";

export const build = async (
	lucid: Lucid,
	guardianDatumUtxoList: ValidDatumUTXO[],
	config: ConfigSign
) => {

	const cBTCMintingPolicyApplied: MintingPolicy = {
		type: "PlutusV2",
		script: applyParamsToScript(cBTCMintingPolicy.script,[
			lucid.utils.validatorToScriptHash(config.guardianValApplied)
		])
	}

	const info = {
		multisig: {
			validator: multisigValidator,
			utxo: await lucid.utxoByUnit(config.unit),
			address: lucid.utils.validatorToAddress(multisigValidator),
			redeemer: Data.to(new Constr(1, [])), // PSign
		},
		guardian: {
			validator: config.guardianValApplied,
			redeemer: Data.to(new Constr(0, [])), // PApproveWrap
			utxos: guardianDatumUtxoList.map((value) => {
				return value.utxo;
			}),
		},
		minter: {
			policy: cBTCMintingPolicyApplied,
			policyID: lucid.utils.mintingPolicyToId(cBTCMintingPolicyApplied),
			unit: toUnit(
				lucid.utils.mintingPolicyToId(cBTCMintingPolicyApplied),
				fromText("cBTC")
			),
			redeemer: Data.to(new Constr(0, [])), // PMintBTC
		},
	};
	console.log('info', info)
	console.log(lucid.utils.validatorToScriptHash(info.multisig.validator))
	console.log(lucid.utils.validatorToScriptHash(info.guardian.validator))
	console.log(lucid.utils.validatorToScriptHash(info.minter.policy))

	const totalAmount = guardianDatumUtxoList.reduce((acc, value) => {
		return acc + value.datum.amountDeposit;
	}, BigInt(0));

	const totalAssets = { [info.minter.unit]: totalAmount };

	const outputs = guardianDatumUtxoList
		.map((value) => {
			return lucid.newTx().payToAddress(value.datum.address, {
				[info.minter.unit]: value.datum.amountDeposit,
			});
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const signers = config.consignerKeys
		.map((cosignerKey) => {
			return lucid.newTx().addSignerKey(cosignerKey);
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	const txa = lucid.newTx()
		.attachSpendingValidator(info.guardian.validator)
		.collectFrom(info.guardian.utxos, info.guardian.redeemer)

	const txb = lucid.newTx()
		.attachSpendingValidator(info.multisig.validator)
		.collectFrom([info.multisig.utxo], info.multisig.redeemer)
		.payToContract(
			info.multisig.address,
			{ inline: info.multisig.utxo.datum || "" },
			info.multisig.utxo.assets
		)
	const tx = await lucid
			.newTx()
			.compose(txa)
			.compose(txb)
			.attachMintingPolicy(info.minter.policy)
		 	.mintAssets(totalAssets, info.minter.redeemer)
			.compose(outputs)
			.compose(signers)
			.complete()


	// const tx = await lucid
	// 	.newTx()
	// 	.attachMintingPolicy(info.minter.policy)
	// 	.mintAssets(totalAssets, info.minter.redeemer)
	// 	.collectFrom(info.guardian.utxos, info.guardian.redeemer)
	// 	.attachSpendingValidator(info.guardian.validator)
	// 	.collectFrom([info.multisig.utxo], info.multisig.redeemer)
	// 	.attachSpendingValidator(info.multisig.validator)
	// 	.payToContract(
	// 		info.multisig.address,
	// 		{ inline: info.multisig.utxo.datum || "" },
	// 		info.multisig.utxo.assets
	// 	)
	// 	.compose(outputs)
	// 	.compose(signers)
	// 	.complete();

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
