import { guardianMultisig } from "@/utils/validators";
import {
	Assets,
	Emulator,
	fromText,
	generatePrivateKey,
	generateSeedPhrase,
	Lucid,
	toUnit,
} from "lucid-cardano";
import * as multisig_update from "./multisig.update";
import * as multisig_init from "./multisig.init";
import { ConfigInit, ConfigUpdate } from "./types";

const buildAddress = async (assets: Assets) => {
	const privKey = generatePrivateKey();
	const address = await (await Lucid.new(undefined, "Custom"))
		.selectWalletFromPrivateKey(privKey)
		.wallet.address();

	return {
		privateKey: privKey,
		address: address,
		assets: assets,
	};
};

async function generateAccount(assets: Assets) {
	const seedPhrase = generateSeedPhrase();
	return {
		seedPhrase,
		address: await (await Lucid.new(undefined, "Custom"))
			.selectWalletFromSeed(seedPhrase)
			.wallet.address(),
		assets,
	};
}

export const runEmulator = async () => {
	const account1 = await buildAddress({ lovelace: BigInt(7000000000) });
	const account2 = await buildAddress({ lovelace: BigInt(100000000) });
	const account3 = await buildAddress({ lovelace: BigInt(100000000) });
	const account11 = await buildAddress({ lovelace: BigInt(100000000) });
	const account12 = await buildAddress({ lovelace: BigInt(100000000) });
	const account13 = await buildAddress({ lovelace: BigInt(100000000) });

	// TODO: start using buildAddress function
	const emulator = new Emulator([
		account1,
		account2,
		account3,
		account11,
		account12,
		account13,
	]);

	console.log("emulator", emulator.ledger);
	console.log("address1.address", account1.address);

	const lucid = await Lucid.new(emulator);

	lucid.selectWalletFromPrivateKey(account1.privateKey);

	const initConfig: ConfigInit = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(account1.address).hash,
			lucid.utils.paymentCredentialOf(account2.address).hash,
			lucid.utils.paymentCredentialOf(account3.address).hash,
		],
	};

	const initResult = await multisig_init.init(lucid, initConfig);

	emulator.awaitBlock(4);

	console.log("wallet utxos: ", await lucid.wallet.getUtxos());
	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);

	const configUpdate: ConfigUpdate = {
		unit: initResult.unit,
		oldCosignerKeys: [
			lucid.utils.paymentCredentialOf(account1.address).hash,
			lucid.utils.paymentCredentialOf(account2.address).hash,
			lucid.utils.paymentCredentialOf(account3.address).hash,
		],
		newConfig: {
			threshold: 3,
			cosignerKeys: [
				lucid.utils.paymentCredentialOf(account11.address).hash,
				lucid.utils.paymentCredentialOf(account12.address).hash,
				lucid.utils.paymentCredentialOf(account13.address).hash,
			],
		},
	};

	const updateTx = await multisig_update.build(lucid, configUpdate);

	lucid.selectWalletFromPrivateKey(account1.privateKey);
	const witness1 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	lucid.selectWalletFromPrivateKey(account2.privateKey);
	const witness2 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	lucid.selectWalletFromPrivateKey(account3.privateKey);
	const witness3 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	await multisig_update.assemble(lucid, updateTx.toString(), [
		witness1,
		witness2,
		witness3,
	]);

	emulator.awaitBlock(4);

	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);
};

export const runEmulator1 = async () => {
	const ACCOUNT_0 = await generateAccount({ lovelace: BigInt(100000000) });
	const ACCOUNT_1 = await generateAccount({ lovelace: BigInt(100000000) });

	const emulator = new Emulator([ACCOUNT_0, ACCOUNT_1]);

	const lucid = await Lucid.new(emulator);

	const { paymentCredential } = lucid.utils.getAddressDetails(
		ACCOUNT_0.address
	);
	const { paymentCredential: paymentCredential2 } =
		lucid.utils.getAddressDetails(ACCOUNT_1.address);

	console.log("emulator.now()", emulator.now());
	const mintingPolicy = lucid.utils.nativeScriptFromJson({
		type: "all",
		scripts: [
			{
				type: "before",
				slot: lucid.utils.unixTimeToSlot(emulator.now() + 60000),
			},
			{ type: "sig", keyHash: paymentCredential?.hash! },
			{ type: "sig", keyHash: paymentCredential2?.hash! },
		],
	});

	const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);

	async function mint() {
		console.log("policyId", policyId);
		console.log("mintingPolicy", mintingPolicy);
		console.log("emulator", emulator.now());
		lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase);
		const tx = await lucid
			.newTx()
			.mintAssets({ [toUnit(policyId, fromText("Wow"))]: BigInt(123) })
			.validTo(emulator.now() + 30000)
			.attachMintingPolicy(mintingPolicy)
			.complete();

		// await tx.partialSign();
		// lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
		// await tx.partialSign();
		// lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase);
		// const signedTx = await tx.complete();
		const w1 = await lucid
			.selectWalletFromSeed(ACCOUNT_1.seedPhrase)
			.fromTx(tx.toString())
			.partialSign();
		const w2 = await lucid
			.selectWalletFromSeed(ACCOUNT_0.seedPhrase)
			.fromTx(tx.toString())
			.partialSign();
		const tx1 = await lucid.fromTx(tx.toString()).assemble([w1, w2]).complete();
		console.log(tx1.txSigned.to_json());
		return tx1.submit();
	}

	await mint();

	emulator.awaitBlock(4);

	console.log("lucid.wallet.getUtxos", await lucid.wallet.getUtxos());
};
