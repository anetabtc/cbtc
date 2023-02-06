import { guardianValidator } from "@/utils/validators";
import { Constr, Lucid, Data, Address, AddressDetails } from "lucid-cardano";

export const submit = async (
	lucid: Lucid,
	bridgeAmount: number,
	cardanoAddr: string,
	otherChainAddr: string,
) => {
	try {
		console.log("Submiting request");
		const walletAddrDetails: AddressDetails =
			lucid.utils.getAddressDetails(cardanoAddr);
		const guardianValidatorAddr: Address =
			lucid.utils.validatorToAddress(guardianValidator);
		console.log("walletAddr", walletAddrDetails);
		// Only Address with Staking Credential is supported
		const addressAsData = new Constr(0, [
			new Constr(0, [walletAddrDetails.paymentCredential?.hash || ""]),
			new Constr(0, [
				new Constr(0, [
					new Constr(0, [walletAddrDetails.stakeCredential?.hash || ""]),
				]),
			]),
		]);
		const Datum = Data.to(new Constr(0, [BigInt(bridgeAmount), ,addressAsData, otherChainAddr]));
		const tx = await lucid
			.newTx()
			.payToContract(
				guardianValidatorAddr,
				{ inline: Datum },
				{ lovelace: BigInt(1000000) }
			)
			.complete();

		const signedTx = await tx.sign().complete();

		const txHash = signedTx.submit();
		return txHash;
	} catch (error) {
		if (error instanceof Error) return error;
		return Error(`unknown error : ${JSON.stringify(error)}`);
	}
};
