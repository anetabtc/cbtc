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
import * as multisig_update from "../multisig.update";
import * as multisig_init from "../multisig.init";
import { ConfigInit, ConfigUpdate } from "../types";

const generateAccountPrivateKey = async (assets: Assets) => {
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

const generateAccountSeedPhrase = async (assets: Assets) => {
	const seedPhrase = generateSeedPhrase();
	return {
		seedPhrase,
		address: await (await Lucid.new(undefined, "Custom"))
			.selectWalletFromSeed(seedPhrase)
			.wallet.address(),
		assets,
	};
};

export const update = async () => {
	const signers = {
		account1: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
		account2: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
		account3: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
		account11: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
		account12: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
		account13: await generateAccountPrivateKey({
			lovelace: BigInt(1000000000),
		}),
	};
	const user = {
		account1: await generateAccountSeedPhrase({ lovelace: BigInt(1000000000) }),
	};

	const emulator = new Emulator([
		signers.account1,
		signers.account2,
		signers.account3,
		signers.account11,
		signers.account12,
		signers.account13,
		user.account1,
	]);

	console.log("emulator", emulator.ledger);
	console.log("address1.address", signers.account1.address);

	const lucid = await Lucid.new(emulator);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	// Set multisig cosigners
	const initConfig: ConfigInit = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};
	// initialize Multisig NFT with Datum
	const initResult = await multisig_init.init(lucid, initConfig);

	emulator.awaitBlock(4);

	console.log("wallet utxos: ", await lucid.wallet.getUtxos());
	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);

	// Set old and new cosigners
	const configUpdate: ConfigUpdate = {
		unit: initResult.unit,
		oldCosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
		newConfig: {
			threshold: 3,
			cosignerKeys: [
				lucid.utils.paymentCredentialOf(signers.account11.address).hash,
				lucid.utils.paymentCredentialOf(signers.account12.address).hash,
				lucid.utils.paymentCredentialOf(signers.account13.address).hash,
			],
		},
	};

	// Build update transaction
	const updateTx = await multisig_update.build(lucid, configUpdate);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const witness1 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account2.privateKey);
	const witness2 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account3.privateKey);
	const witness3 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	//Assemble old cosigner signatures
	await multisig_update.assemble(lucid, updateTx.toString(), [
		witness1,
		witness2,
		witness3,
	]);

	emulator.awaitBlock(4);

	// Print new Datum
	console.log(
		"guardianMultisig utxos: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianMultisig))
	);
};

// the below is for testing only
export const runEmulator1 = async () => {
	const ACCOUNT_0 = await generateAccountSeedPhrase({
		lovelace: BigInt(100000000),
	});
	const ACCOUNT_1 = await generateAccountSeedPhrase({
		lovelace: BigInt(100000000),
	});

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
