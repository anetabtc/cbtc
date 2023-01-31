import {
	guardianMinter,
	guardianMultisig,
	guardianValidator,
} from "@/utils/validators";
import { Constr, Lucid, Data, Address, fromText, toUnit } from "lucid-cardano";
import { ConfigSign, ValidDatumUTXO } from "./types";

export const build = async (
	lucid: Lucid,
	datumUtxoList: ValidDatumUTXO[],
	config: ConfigSign
) => {
	//TODO:  organize guardianMultisig, guardianValidator, guardianMinter in objects so it's more readable
	const multisigScriptUtxo = await lucid.utxoByUnit(config.unit);
	const multisigDatumAsCbor = multisigScriptUtxo.datum || "";
	const multisigScriptAddr = lucid.utils.validatorToAddress(guardianMultisig);
	const RedeemerSign = Data.to(new Constr(1, []));

	// pkh should match the list of [PubKeyHash] of the script
	// [PubKeyHash] parameter at the Guardian Validator is hardcoded with my nami wallet PubKeyHash
	// const pkh: string =
	// 	lucid.utils.getAddressDetails(await lucid.wallet.address())
	// 		.paymentCredential?.hash || "";
	// console.log("My PubKeyHash: ", pkh);
	// const pkh = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"
	const RedeemerGuardian = Data.to(new Constr(0, []));
	const RedeemerPolicy = Data.to(new Constr(0, []));

	const policyID = lucid.utils.mintingPolicyToId(guardianMinter);
	const unit = toUnit(policyID, fromText("cBTC"));

	console.log("datumUtxoList: ", datumUtxoList);

	const totalAmount = datumUtxoList.reduce((acc, value) => {
		return acc + value.datum.amountDeposit;
	}, BigInt(0));

	console.log("totalAmount: ", totalAmount);

	const totalAssets = { [unit]: totalAmount };

	console.log("totalAssets: ", totalAssets);
	const utxoList = datumUtxoList.map((value) => {
		return value.utxo;
	});

	console.log("utxoList: ", utxoList);

	const outputs = datumUtxoList
		.map((value) => {
			console.log(
				`Adding payToAddress: ${value.datum.address}, ${value.datum.amountDeposit}`
			);
			return lucid.newTx().payToAddress(value.datum.address, {
				[unit]: value.datum.amountDeposit,
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

	//TODO: build a new guardianValidator with the new pubkeyhashes
	const tx = await lucid
		.newTx()
		.collectFrom(utxoList, RedeemerGuardian)
		.attachSpendingValidator(guardianValidator)
		.attachMintingPolicy(guardianMinter)
		.mintAssets(totalAssets, RedeemerPolicy)
		.collectFrom([multisigScriptUtxo], RedeemerSign)
		.attachSpendingValidator(guardianMultisig)
		.payToContract(
			multisigScriptAddr,
			{ inline: multisigDatumAsCbor },
			multisigScriptUtxo.assets
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
