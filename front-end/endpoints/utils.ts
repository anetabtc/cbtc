import { guardianValidator } from "@/utils/validators";
import {
	Lucid,
	Data,
	Address,
	Credential,
} from "lucid-cardano";
import { AnyDatumUTXO, ValidDatumUTXO } from "./types";

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