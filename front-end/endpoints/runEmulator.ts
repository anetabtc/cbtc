import { guardianMultisig } from "@/utils/validators";
import {
	Emulator,
	generatePrivateKey,
	getAddressDetails,
	Lucid,
} from "lucid-cardano";
import { initialize, update } from "./multisig";
import { ConfigMultisig, ConfigUpdate } from "./types";

export const runEmulator = async () => {
	const privateKey1 = generatePrivateKey();
	const privateKey2 = generatePrivateKey();

	const address1 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey1)
		.wallet.address();

	const address2 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey2)
		.wallet.address();

	const address1Details = getAddressDetails(address1);
	const address2Details = getAddressDetails(address2);

	const emulator = new Emulator([
		{ address: address1, assets: { lovelace: BigInt(3000000000) } },
		{ address: address2, assets: { lovelace: BigInt(3000000000) } },
	]);

	const lucid = await Lucid.new(emulator);

	lucid.selectWalletFromPrivateKey(privateKey1);

	const initConfig: ConfigMultisig = {
		threshold: 2,
		cosignerKeys: [address1Details.paymentCredential?.hash!],
	};

	const resultInitialize = await initialize(lucid, initConfig);

	emulator.awaitBlock(4);

	console.log("wallet utxos: ", await lucid.wallet.getUtxos());
	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);

	const updateConfig: ConfigUpdate = {
		unit: resultInitialize.unit,
		oldCosignerKeys: [address1Details.paymentCredential?.hash!],
		newConfig: {
			threshold: 3,
			cosignerKeys: [address2Details.paymentCredential?.hash!],
		},
	};

	await update(lucid, updateConfig);

	emulator.awaitBlock(4);

	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);
};
