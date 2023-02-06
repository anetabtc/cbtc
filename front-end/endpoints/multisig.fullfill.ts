import {
	guardianMinter,
	guardianMultisig,
	guardianValidator,
} from "@/utils/validators";
import { Constr, Lucid, Data, fromText, toUnit } from "lucid-cardano";
import { ConfigSign, ValidDatumUTXO } from "./types";

export const build = async (
	lucid: Lucid,
	guardianDatumUtxoList: ValidDatumUTXO[],
	config: ConfigSign
) => {
	const info = {
		multisig: {
			validator: guardianMultisig, //TODO: New Script is needed here
			utxo: await lucid.utxoByUnit(config.unit),
			address: lucid.utils.validatorToAddress(guardianMultisig),
			redeemer: Data.to(new Constr(1, [])), // PSign
		},
		guardian: {
			validator: guardianValidator, //TODO: New Script is needed here
			redeemer: Data.to(new Constr(0, [])), // PApproveWrap
			utxos: guardianDatumUtxoList.map((value) => {
				return value.utxo;
			}),
		},
		minter: {
			policy: guardianMinter,
			policyID: lucid.utils.mintingPolicyToId(guardianMinter),
			unit: toUnit(
				lucid.utils.mintingPolicyToId(guardianMinter),
				fromText("cBTC")
			),
			redeemer: Data.to(new Constr(0, [])), // PMintBTC
		},
	};

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

	const tx = await lucid
		.newTx()
		.collectFrom(info.guardian.utxos, info.guardian.redeemer)
		.attachSpendingValidator(info.guardian.validator)
		.attachMintingPolicy(info.minter.policy)
		.mintAssets(totalAssets, info.minter.redeemer)
		.collectFrom([info.multisig.utxo], info.multisig.redeemer)
		.attachSpendingValidator(info.multisig.validator)
		.payToContract(
			info.multisig.address,
			{ inline: info.multisig.utxo.datum || "" },
			info.multisig.utxo.assets
		)
		.compose(outputs)
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
