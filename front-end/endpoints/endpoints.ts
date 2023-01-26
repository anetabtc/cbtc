import { guardianMinter, guardianValidator } from "@/utils/validators";
import {
	Constr,
	Lucid,
	Data,
	Address,
	utf8ToHex,
	AddressDetails,
	Credential,
} from "lucid-cardano";
import { AnyDatumUTXO, ValidDatumUTXO } from "./types";

export const submitRequest = async (
	lucid: Lucid,
	address: string,
	amount: bigint
) => {
	try {
		console.log("submiting request");
		const walletAddrDetails: AddressDetails =
			lucid.utils.getAddressDetails(address);
		const guardianValidatorAddr: Address =
			lucid.utils.validatorToAddress(guardianValidator);
		console.log("walletAddr", walletAddrDetails);
		const addressAsData = new Constr(0, [
			new Constr(0, [walletAddrDetails.paymentCredential?.hash || ""]),
			new Constr(0, [
				new Constr(0, [
					new Constr(0, [walletAddrDetails.stakeCredential?.hash || ""]),
				]),
			]),
		]);
		const amountDeposit = amount;
		const Datum = Data.to(new Constr(0, [amountDeposit, addressAsData]));
		const tx = await lucid
			.newTx()
			.payToContract(
				guardianValidatorAddr,
				{ inline: Datum },
				{ lovelace: BigInt(2000000) }
			)
			.complete();

		const signedTx = await tx.sign().complete();

		const txHash = signedTx.submit();
		console.log("Transaction submitted:", txHash);

		return null;
	} catch (error) {
		console.log(error);
		if (error instanceof Error) return error.message;
		return `unknown error in submitRequest: ${JSON.stringify(error)}`;
	}
};

export const fullfillRequests = async (
	lucid: Lucid,
	datumUtxoList: ValidDatumUTXO[]
) => {
	try {
		console.log("fullfilling requests");

		// pkh should match the list of [PubKeyHash] of the script
		// [PubKeyHash] parameter at the Guardian Validator is hardcoded with my nami wallet PubKeyHash
		const pkh: string =
			lucid.utils.getAddressDetails(await lucid.wallet.address())
				.paymentCredential?.hash || "";
		console.log("My PubKeyHash: ", pkh);
		// const pkh = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"
		const Redeemer = Data.to(new Constr(0, []));
		const RedeemerPolicy = Data.to(new Constr(0, []));

		const policyID = lucid.utils.mintingPolicyToId(guardianMinter);
		const unit = policyID + utf8ToHex("cBTC");

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

		const txs = datumUtxoList
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

		//TODO: Create a compose tx with .addSignerKey when testing with list of cosigners
		//TODO: Find a way to submit the tx to all cosigners

		const tx = await lucid
			.newTx()
			.collectFrom(utxoList, Redeemer)
			.attachSpendingValidator(guardianValidator)
			.attachMintingPolicy(guardianMinter)
			.mintAssets(totalAssets, RedeemerPolicy)
			.compose(txs)
			.addSignerKey(pkh)
			.complete();

		const signedTx = await tx.sign().complete();

		const txHash = await signedTx.submit();
		await lucid.awaitTx(txHash);
		console.log("Transaction submitted:", txHash);

		return null;
	} catch (error) {
		console.log(error);
		if (error instanceof Error) return error.message;
		return `unknown error in fullfillRequests: ${JSON.stringify(error)}`;
	}
};

export const getAllDatums = async (lucid: Lucid): Promise<AnyDatumUTXO[]> => {
	console.log("Getting All Datums");

	const guardianValidatorAddr: Address =
		lucid.utils.validatorToAddress(guardianValidator);

	const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
	if (!scriptUtxos.length) return [] as AnyDatumUTXO[];

	const datumUtxoList = scriptUtxos.map((utxo) => {
		const datumCbor = utxo.datum || "";
		const datumAsData = Data.from(datumCbor);
		// Try parsing Data -> Address
		// Address: must have StakingHash
		// Valid Address type:  (PubKeyCredential (<PubKeyHash>)) (Just (StakingHash (PubKeyCredential (<PubKeyHash>))))
		const paymentCredentialHash: string =
			datumAsData.fields[1]?.fields[0]?.fields[0];
		const stakeCredentialHash: string =
			datumAsData.fields[1]?.fields[1]?.fields[0]?.fields[0]?.fields[0];
		const amount = datumAsData.fields[0];

		if (!paymentCredentialHash || !stakeCredentialHash || !amount) {
			return {
				isValid: false,
				datum: datumAsData,
				utxo: utxo,
			};
		}

		const paymentCredential: Credential = lucid.utils.keyHashToCredential(
			paymentCredentialHash
		);

		const stakeCredential: Credential =
			lucid.utils.keyHashToCredential(stakeCredentialHash);

		const readableDatum = {
			amountDeposit: amount,
			address: lucid.utils.credentialToAddress(
				paymentCredential,
				stakeCredential
			), // Convert to Bech32 Address
		};

		return {
			isValid: true,
			datum: readableDatum,
			utxo: utxo,
		};
	});
	return datumUtxoList;
};

export const getValidDatums = async (
	lucid: Lucid
): Promise<ValidDatumUTXO[]> => {
	console.log("Getting Valid Datums");

	const guardianValidatorAddr: Address =
		lucid.utils.validatorToAddress(guardianValidator);

	const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
	if (!scriptUtxos.length) return [] as ValidDatumUTXO[];

	const datumUtxoList = scriptUtxos.reduce((acc: ValidDatumUTXO[], utxo) => {
		const datumCbor = utxo.datum || "";
		const datumAsData = Data.from(datumCbor);
		// Try parsing Data -> Address
		// Address: must have StakingHash
		// Valid Address type:  (PubKeyCredential (<PubKeyHash>)) (Just (StakingHash (PubKeyCredential (<PubKeyHash>))))
		const paymentCredentialHash: string =
			datumAsData.fields[1]?.fields[0]?.fields[0];
		const stakeCredentialHash: string =
			datumAsData.fields[1]?.fields[1]?.fields[0]?.fields[0]?.fields[0];
		const amount = datumAsData.fields[0];

		if (paymentCredentialHash && stakeCredentialHash && amount) {
			const paymentCredential: Credential = lucid.utils.keyHashToCredential(
				paymentCredentialHash
			);

			const stakeCredential: Credential =
				lucid.utils.keyHashToCredential(stakeCredentialHash);

			const readableDatum = {
				amountDeposit: amount,
				address: lucid.utils.credentialToAddress(
					paymentCredential,
					stakeCredential
				), // Convert to Bech32 Address
			};
			const newdata = {
				datum: readableDatum,
				utxo: utxo,
			};

			acc.push(newdata);
		}
		return acc;
	}, []);
	return datumUtxoList;
};
