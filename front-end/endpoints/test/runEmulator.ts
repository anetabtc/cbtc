import { guardianValidator, multisigValidator } from "@/utils/validators";
import {
	applyParamsToScript,
	Assets,
	Constr,
	Emulator,
	fromText,
	generatePrivateKey,
	generateSeedPhrase,
	Lucid,
	SpendingValidator,
	toUnit,
} from "lucid-cardano";
import * as multisig_update from "../multisig.update";
import * as multisig_init from "../multisig.init";
import * as multisig_fullfill from "../multisig.fullfill";
import * as user_request from "../user.request";
import * as utils from "../utils";


import { ConfigInit, ConfigSign, ConfigUpdate } from "../types";

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
	console.log("[State]: initializing Emulator")
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

	console.log("[INFO] Emulator Ledger:", emulator.ledger);


	const lucid = await Lucid.new(emulator);

	console.log("[State] Initializing Multisig")

	// Set initial signers, at the moment the MultiSigMintPolicy takes one signer to initialize the datum
	const initConfig: ConfigInit = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};
	console.log("[INFO] Initial Configuration", initConfig)
	
	// initialize and submit MultiSigCert NFT with Datum
	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const initResult = await multisig_init.init(lucid, initConfig);
	const multiSigCertPolicyId = lucid.utils.mintingPolicyToId(initResult.multisigPolicy)
	const multiSigCertUnit = initResult.unit
	emulator.awaitBlock(4);
	console.log("[INFO] Endpoint result (multisig_init.init): ", initResult)
	console.log(
		"[INFO] New UTXO at multisigValidator: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(multisigValidator))
	);

	console.log("[State] Updating Multisig")
	// Set MultiSigCert NFT , old and new signers
	const configUpdate: ConfigUpdate = {
		unit: multiSigCertUnit,
		oldCosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
		],
		newConfig: {
			threshold: 2,
			cosignerKeys: [
				lucid.utils.paymentCredentialOf(signers.account11.address).hash,
				lucid.utils.paymentCredentialOf(signers.account12.address).hash,
				lucid.utils.paymentCredentialOf(signers.account13.address).hash,
			],
		},
	};
	console.log("[INFO] Setting Configuration Update", configUpdate)

	// Build update transaction
	const updateTx = await multisig_update.build(lucid, configUpdate);

	// Select original signer to witness the tx
	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const witness1 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	//Assemble and submit old signer tx
	await multisig_update.assemble(lucid, updateTx.toString(), [
		witness1,
	]);

	emulator.awaitBlock(4);

	console.log(
		"[INFO] New UTXO at multisigValidator: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(multisigValidator))
	);

	console.log("[State] Requesting cBTC")

	const guardianValidatorApplied: SpendingValidator = {
		type: "PlutusV2",
		script: applyParamsToScript(guardianValidator.script, [
			lucid.utils.validatorToScriptHash(multisigValidator), multiSigCertPolicyId,
		]),
	};

	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	// This Address has Staking Credential
	const myAddress = await lucid.wallet.address();
	const hardcodedAmount = 10;
	console.log(`Requesting ${hardcodedAmount} cBTC to ${myAddress}`);
	const result = await user_request.submit(
		lucid,
		hardcodedAmount,
		myAddress,
		"",
		guardianValidatorApplied,
	);

	emulator.awaitBlock(4);

	console.log("[INFO] Endpoint result (user_request.submit): ", result)
	console.log(
		"[INFO] New UTXO at guardianValidator: ",
		await lucid.utxosAt(lucid.utils.validatorToAddress(guardianValidatorApplied))
	);

	console.log("[State] Getting Valid Datum and UTXO")

	const validDatumUtxoList = await utils.getValidDatums(lucid, guardianValidatorApplied);
	if (!validDatumUtxoList?.length) {
		console.log("[INFO] No valid datums at Guardian Script");
		return null;
	}
	console.log("[INFO] validDatumUtxoList: ", validDatumUtxoList);

	const configSign: ConfigSign = {
		unit: multiSigCertUnit,
		guardianValApplied : guardianValidatorApplied,
		consignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account11.address).hash,
			lucid.utils.paymentCredentialOf(signers.account12.address).hash,
			lucid.utils.paymentCredentialOf(signers.account13.address).hash,
		],
	};

	lucid.selectWalletFromPrivateKey(signers.account11.privateKey);

	const fulfillTx = await multisig_fullfill.build(
		lucid,
		validDatumUtxoList,
		configSign
	);

	lucid.selectWalletFromPrivateKey(signers.account11.privateKey);
	const witness11 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account12.privateKey);
	const witness12 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account13.privateKey);
	const witness13 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	const assembleTx = await multisig_fullfill.assemble(
		lucid,
		fulfillTx.toString(),
		[witness11, witness12, witness13]
	);
	
	emulator.awaitBlock(4);


	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	console.log(
		"[INFO] UTXOs at user wallet: ",
		await lucid.wallet.getUtxos())
	
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
