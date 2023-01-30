import { guardianMultisig } from "@/utils/validators";
import { Assets, Emulator, generatePrivateKey, Lucid } from "lucid-cardano";
import {
	assembleUpdate,
	buildUpdate,
	initialize,
	signUpdate,
} from "./multisig";
import { ConfigMultisig, ConfigUpdate } from "./types";

export const runEmulator = async () => {

	const buildAddress = async (assets: Assets) => {
		const privKey = generatePrivateKey()
		const address = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privKey)
		.wallet.address();

		return {
			privateKey: privKey,
			address: address,
			assets : assets
		}

	}
	const privateKey1  = generatePrivateKey();
	const privateKey2  = generatePrivateKey();
	const privateKey3  = generatePrivateKey();
	const privateKey11 = generatePrivateKey();
	const privateKey12 = generatePrivateKey();
	const privateKey13 = generatePrivateKey();

	// const address1 = await (await Lucid.new(undefined, "Custom"))
		// .selectWalletFromPrivateKey(privateKey1)
		// .wallet.address();
	const address1 = await buildAddress({lovelace: BigInt(7000000000)})

	const address2 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey2)
		.wallet.address();

	const address3 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey3)
		.wallet.address();

	const address11 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey11)
		.wallet.address();

	const address12 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey12)
		.wallet.address();

	const address13 = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privateKey13)
		.wallet.address();

	// TODO: start using buildAddress function
	const emulator = new Emulator([
		address1,
		{ address: address2, assets: { lovelace: BigInt(3000000000) } },
		{ address: address3, assets: { lovelace: BigInt(3000000000) } },
		{ address: address11, assets: { lovelace: BigInt(3000000000) } },
		{ address: address12, assets: { lovelace: BigInt(3000000000) } },
		{ address: address13, assets: { lovelace: BigInt(3000000000) } },
	]);

	console.log('emulator', emulator.ledger)
	console.log('address1.address', address1.address)

	const lucid = await Lucid.new(emulator);

	lucid.selectWalletFromPrivateKey(address1.privateKey);

	const initConfig: ConfigMultisig = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(address1.address).hash,
			lucid.utils.paymentCredentialOf(address2).hash,
			lucid.utils.paymentCredentialOf(address3).hash,
		],
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
		oldCosignerKeys: [
			lucid.utils.paymentCredentialOf(address1.address).hash,
			lucid.utils.paymentCredentialOf(address2).hash,
			lucid.utils.paymentCredentialOf(address3).hash,
		],
		newConfig: {
			threshold: 3,
			cosignerKeys: [
				lucid.utils.paymentCredentialOf(address11).hash,
				lucid.utils.paymentCredentialOf(address12).hash,
				lucid.utils.paymentCredentialOf(address13).hash,
			],
		},
	};

	const resultBuildUpdate = await buildUpdate(lucid, updateConfig);

	lucid.selectWalletFromPrivateKey(privateKey1);
	const witnesses1 = await signUpdate(lucid, resultBuildUpdate.txCbor);

	lucid.selectWalletFromPrivateKey(privateKey2);
	const witnesses2 = await signUpdate(lucid, resultBuildUpdate.txCbor);

	lucid.selectWalletFromPrivateKey(privateKey3);
	const witnesses3 = await signUpdate(lucid, resultBuildUpdate.txCbor);

	lucid.selectWalletFromPrivateKey(privateKey1);

	await assembleUpdate(lucid, resultBuildUpdate.txCbor, [
		witnesses1,
		witnesses2,
		witnesses3,
	]);

	emulator.awaitBlock(4);

	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);
};
