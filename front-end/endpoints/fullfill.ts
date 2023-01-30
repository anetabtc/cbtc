import { guardianMinter, guardianValidator } from "@/utils/validators";
import { Constr, Lucid, Data, Address, fromText, toUnit } from "lucid-cardano";
import { ValidDatumUTXO } from "./types";

export const buildTx = async (
	lucid: Lucid,
	datumUtxoList: ValidDatumUTXO[],
	cosigners: Address[]
) => {
	// pkh should match the list of [PubKeyHash] of the script
	// [PubKeyHash] parameter at the Guardian Validator is hardcoded with my nami wallet PubKeyHash
	// const pkh: string =
	// 	lucid.utils.getAddressDetails(await lucid.wallet.address())
	// 		.paymentCredential?.hash || "";
	// console.log("My PubKeyHash: ", pkh);
	// const pkh = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"
	const Redeemer = Data.to(new Constr(0, []));
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

	const tx1 = datumUtxoList
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

	const tx2 = cosigners
		.map((cosigner) => {
			const cosignerHash = lucid.utils.paymentCredentialOf(cosigner).hash;
			return lucid.newTx().addSignerKey(cosignerHash);
		})
		.reduce((prevTx, tx) => {
			return prevTx.compose(tx);
		});

	//TODO: Create a compose tx with .addSignerKey when testing with list of cosigners
	//TODO: Find a way to submit the tx to all cosigners

	const tx = await lucid
		.newTx()
		.collectFrom(utxoList, Redeemer)
		.attachSpendingValidator(guardianValidator)
		.attachMintingPolicy(guardianMinter)
		.mintAssets(totalAssets, RedeemerPolicy)
		.compose(tx1)
		.compose(tx2)
		.complete();

	return tx.toString();
};

export const partialSignTx = async (lucid: Lucid, txAsCbor: string) => {
	return await lucid.fromTx(txAsCbor).partialSign();
};

export const assembleTx = async (
	lucid: Lucid,
	txAsCbor: string,
	witnesses: string[]
) => {
	const signedTx = await lucid.fromTx(txAsCbor).assemble(witnesses).complete();
	const txHash = signedTx.submit();
    return txHash
};
