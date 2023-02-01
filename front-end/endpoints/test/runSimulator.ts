import { generatePrivateKey, Lucid } from "lucid-cardano";
import * as multisig_update from "../multisig.update";
import * as multisig_init from "../multisig.init";
import * as multisig_fullfill from "../multisig.fullfill";
import { ConfigInit, ConfigSign, ConfigUpdate } from "../types";
import * as utils from "../utils";
import * as client_request from "../client.request";

// Accounts generated with utils.generateAddressPrivateKey()
// These addresses don't have StakingCredential
const signers = {
	account1: {
		privateKey:
			"ed25519_sk1lma0ma2unt7k0q6h8v2hkamuvrr5y79vma9ee4cqdnw5w3c3m2ls78aj2s",
		address: "addr_test1vzdekzrwlc0qnrnqck58edn3wyevzd3tasl0c5sx6gzsvyqxt6pfs",
	}, // 100
	account2:  {
		privateKey:
			"ed25519_sk1ml60ac9yv0g9nhgwfvkd003wcszpty76ejd9f32v5tf4t34ed8xschs9xq",
		address: "addr_test1vzd0jhkjnzj9jju7m93n377v8zhpy23d8e5esxn9lvravwgauckcr",
	}, // 100
	account3 : {
		privateKey:
			"ed25519_sk1vjyuq42dggug9evhczhug0m89d5tqcs7laf402a7sq53g4p2n33qk8rhk6",
		address: "addr_test1vzy73swp6dq5jepsq3hn0j7xafdfqqj8lgga73qe6g34vcq4f7hq5",
	}, // 100
	account11 : {
		privateKey:
			"ed25519_sk15a2my6ra9utpl9etw6y5pcrgnk5hs2qsxzcez4wr2v2tzjkh878s9d39sh",
		address: "addr_test1vz0v7def4487mksx6cxgjdn5zvjllyxhtukd0jyh8m2tmncgyjy3q",
	}, // 100
	account12 : {
		privateKey:
			"ed25519_sk1r7saa5mh0tqavnv9cq0gmle6ecqxwjq9qrwkke26zkehn4eghcmqg3zsv8",
		address: "addr_test1vz0hg3kh70584yjl2kkvfgjav89jvprqvvuypzdjz7t08tqlcca4t",
	}, // 100
	account13 : {
		privateKey:
			"ed25519_sk12nzkcqxruxma4rmd6cde7l9vla7xq3cawl0t4c39rvzuw9kvjmpszkya4r",
		address: "addr_test1vr5vyy87vky2hky445e2pmyy8cvyapuqwr520vk3th30cqclpytwx",
	},
}

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const user = {
	account1 : {
		address:
			"addr_test1qpjf2g3534w46xxc54nf8zqpglx7skj0d9v7psxua8hggszunzz9vkcmfrzxg2ttqsdtt4zxfhh84853gm9p8vg6364qqsnjmt",
		seedPhrase:
			"couch energy usual pioneer item like gesture turn yard mystery skate glance mimic hip father enable lobster lunar helmet advice marriage market pear delay",
	},
}


// Only run this once to mint multisig nft and set datum with cosigners at multisig script
export const init = async (lucid: Lucid) => {
	const initConfig: ConfigInit = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	console.log(initConfig);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	const initResult = await multisig_init.init(lucid, initConfig);

	console.log(initResult);
};

export const update = async (lucid: Lucid) => {
	const initResultHarcoded = {
		txHash: "e5eec838f441d5602df99c68570d4d251364c81c5d90b2d58c9b04354f031182",
		policy: {
			script:
				"8201828200581c9b9b086efe1e098e60c5a87cb6717132c1362bec3efc5206d205061082051a0129af2d",
			type: "Native",
		},
		unit: "6ae33efc0218cb4cacd4185d7efa26e3615a924655beb7d875f41f354d756c7469536967",
	};

	const configUpdate: ConfigUpdate = {
		unit: initResultHarcoded.unit,
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
	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

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

	const assembleTx = await multisig_update.assemble(
		lucid,
		updateTx.toString(),
		[witness1, witness2, witness3]
	);

	console.log(assembleTx);
};

// Fullfill requests from users
export const fullfil = async (lucid: Lucid) => {
	const initResultHarcoded = {
		txHash: "e5eec838f441d5602df99c68570d4d251364c81c5d90b2d58c9b04354f031182",
		policy: {
			script:
				"8201828200581c9b9b086efe1e098e60c5a87cb6717132c1362bec3efc5206d205061082051a0129af2d",
			type: "Native",
		},
		unit: "6ae33efc0218cb4cacd4185d7efa26e3615a924655beb7d875f41f354d756c7469536967",
	};

	const configSign: ConfigSign = {
		unit: initResultHarcoded.unit,
		consignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	// Get Valid Datums from Guardian Script
	const validDatumUtxoList = await utils.getValidDatums(lucid);
	if (!validDatumUtxoList?.length) {
		console.log("No valid datums at Guardian Script");
		return null;
	}
	console.log("validDatumUtxoList: ", validDatumUtxoList);

	// Build transaction with Valid Datums and UTXOs
	// Guardian Minter, Guardian Script and Guardian Multisig are inlcuded
	const fulfillTx = await multisig_fullfill.build(
		lucid,
		validDatumUtxoList,
		configSign
	);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const witness1 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account2.privateKey);
	const witness2 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account3.privateKey);
	const witness3 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	const assembleTx = await multisig_fullfill.assemble(
		lucid,
		fulfillTx.toString(),
		[witness1, witness2, witness3]
	);

	console.log(assembleTx);
};

export const request = async (lucid: Lucid) => {
	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	// This Address has Staking Credential
	const myAddress = await lucid.wallet.address();
	const hardcodedAmount = BigInt(10);
	console.log(`Requesting ${hardcodedAmount} BTC to ${myAddress}`);
	const result = await client_request.submit(lucid, myAddress, hardcodedAmount);
	console.log(result);
};
