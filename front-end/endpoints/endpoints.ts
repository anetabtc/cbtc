import { guardianMinter, guardianValidator } from "@/utils/validators";
import {
	Constr,
	Lucid,
	Data,
	Address,
	utf8ToHex,
} from "lucid-cardano";

export const submitRequest = async (lucid: Lucid) => {
	if (lucid) {
		try {
			console.log("submiting request");
			const pkh: string = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential?.hash || "";
			console.log(pkh);

			const guardianValidatorAddr: Address = lucid.utils.validatorToAddress(guardianValidator);
			const Datum = Data.to(new Constr(0, []));
			const tx = await lucid
				.newTx()
				.payToContract(
					guardianValidatorAddr,
					{ inline: Datum },
					{ lovelace: BigInt(5000000) }
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
	}
};

export const fullfillRequest = async (lucid: Lucid) => {
	if (lucid) {
		try {
			console.log("fullfilling request");

			const pkh: string = lucid.utils.getAddressDetails(await lucid.wallet.address()).paymentCredential?.hash || "";
			console.log(pkh);

			const guardianValidatorAddr: Address = lucid.utils.validatorToAddress(guardianValidator);

			const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
            if (!scriptUtxos.length) throw new Error("No utxos at Script");

			const walletUtxos = await lucid.wallet.getUtxos();
            if (!walletUtxos.length) throw new Error("No utxos at Wallet");

			const Redeemer = Data.to(new Constr(0, []));
			const RedeemerPolicy = Data.to(new Constr(0, [])) 
			console.log(scriptUtxos);

			const policyID = lucid.utils.mintingPolicyToId(guardianMinter)
			const unit = policyID + utf8ToHex("cBTC")
			const asset = {[unit]: BigInt(1)}

			const tx = await lucid
				.newTx()
				.collectFrom(scriptUtxos, Redeemer)
				.attachSpendingValidator(guardianValidator)
				.attachMintingPolicy(guardianMinter)
				.mintAssets(asset, RedeemerPolicy)
				.addSignerKey(pkh)
				.complete();

			const signedTx = await tx.sign().complete();

			const txHash = signedTx.submit();
			console.log("Transaction submitted:", txHash);

			return null;
		} catch (error) {
			console.log(error);
			if (error instanceof Error) return error.message;
			return `unknown error in fullfillRequest: ${JSON.stringify(error)}`;
		}
	}
};
