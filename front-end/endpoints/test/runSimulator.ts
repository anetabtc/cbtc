import { generatePrivateKey, Lucid } from "lucid-cardano";
import * as multisig_update from "../multisig.update";
import * as multisig_init from "../multisig.init";
import * as multisig_fullfill from "../multisig.fullfill";
import { ConfigInit, ConfigSign, ConfigUpdate } from "../types";
import * as utils from "../utils";
import * as user_request from "../user.request";

// Accounts generated with utils.generateAddressPrivateKey()
// These addresses don't have StakingCredential
const signers = {
	account1: {
		privateKey:
			"ed25519_sk1lma0ma2unt7k0q6h8v2hkamuvrr5y79vma9ee4cqdnw5w3c3m2ls78aj2s",
		address: "addr_test1vzdekzrwlc0qnrnqck58edn3wyevzd3tasl0c5sx6gzsvyqxt6pfs",
	}, // 100
	account2: {
		privateKey:
			"ed25519_sk1ml60ac9yv0g9nhgwfvkd003wcszpty76ejd9f32v5tf4t34ed8xschs9xq",
		address: "addr_test1vzd0jhkjnzj9jju7m93n377v8zhpy23d8e5esxn9lvravwgauckcr",
	}, // 100
	account3: {
		privateKey:
			"ed25519_sk1vjyuq42dggug9evhczhug0m89d5tqcs7laf402a7sq53g4p2n33qk8rhk6",
		address: "addr_test1vzy73swp6dq5jepsq3hn0j7xafdfqqj8lgga73qe6g34vcq4f7hq5",
	}, // 100
	account11: {
		privateKey:
			"ed25519_sk15a2my6ra9utpl9etw6y5pcrgnk5hs2qsxzcez4wr2v2tzjkh878s9d39sh",
		address: "addr_test1vz0v7def4487mksx6cxgjdn5zvjllyxhtukd0jyh8m2tmncgyjy3q",
	}, // 100
	account12: {
		privateKey:
			"ed25519_sk1r7saa5mh0tqavnv9cq0gmle6ecqxwjq9qrwkke26zkehn4eghcmqg3zsv8",
		address: "addr_test1vz0hg3kh70584yjl2kkvfgjav89jvprqvvuypzdjz7t08tqlcca4t",
	}, // 100
	account13: {
		privateKey:
			"ed25519_sk12nzkcqxruxma4rmd6cde7l9vla7xq3cawl0t4c39rvzuw9kvjmpszkya4r",
		address: "addr_test1vr5vyy87vky2hky445e2pmyy8cvyapuqwr520vk3th30cqclpytwx",
	},
};

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const user = {
	account1: {
		address:
			"addr_test1qpjf2g3534w46xxc54nf8zqpglx7skj0d9v7psxua8hggszunzz9vkcmfrzxg2ttqsdtt4zxfhh84853gm9p8vg6364qqsnjmt",
		seedPhrase:
			"couch energy usual pioneer item like gesture turn yard mystery skate glance mimic hip father enable lobster lunar helmet advice marriage market pear delay",
	},
};

// Only run this once to mint multisig nft and set datum with cosigners at multisig script
export const init = async (lucid: Lucid) => {
	const initConfig: ConfigInit = {
		threshold: 1,
		cosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	console.log(initConfig);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	const initResult = await multisig_init.init(lucid, initConfig);

	console.log(initResult);
};

export const update = async (lucid: Lucid) => {
	const initResultHarcoded = {
		txHash: "229ff425f2f3c64239aadb4d815daf8047792d578fa9740266948d0e1819f60e",
		policy: {
			script:
				"5903b55903b2010000333232323232323232323232323232323232323222223333222232323232533301c3370e900000109919191919191919299981219b87480100084c8c8c8c94ccc0a0cdc3a40040042a6603266e1c029200013370e01290010a9980c99b8700a4800854cc064cdc3804a40002a66032a6603266ebcc06cdd618128021ba94893839623962303836656665316530393865363063356138376362363731373133326331333632626563336566633532303664323035303631300013370e6eb4c094c098011200213322332302722533302e00114a02a6646605c0022944c00cc0a80044c008c0ac0048c8cdd79ba7302b003374e6056002605060540020046eb0c09403004cc0a4008c0a4004dd500798119919191919002a99981419b87480000084c8c8c8c8c8c8c926533302f001149858c0ac00cdd680098140009814001999181311299981680088108998111801981480098011815000919299981599b87371a002901c08010b1bae0013758002604a0022c605200460520026ea8004c08400458c094008c094004dd5180f180f800980f180f9809999180e912999812000880c0992999812180200089980d0009801981100109801981100118100009191919299981219b8748000008528099b8f375c6042002020604a004604a0026ea8c8c07cc084004c078c080004dd6180e80199980b119b8800148000dd7180e0021bab301c301d301d00233301523371090000009bae301b003375660366038603800260366036002603660320082c603a004603a0026ea8c058c05c004c05c004dd700200199191919003299980b99b87480000084c926533301800114985854ccc05ccdc3a400400426493299980c0008a4c2c2c603000460300026ea800800488ccc04400800400c528129998078008b0a99980718011806000898058008b2ba34bd702ba022233333300400f00e375200400246660104464a666020600e002266e000040084008dd69808801240006eac004520002222333300533006004002001232223002003300400112250012300422533300b001122500113330033007001222300200313002300800122253330093375e00460060022446004006244a00244600644a6660140022006266008600c0026004600e002464600446600400400246004466004004002ae855d1118021baa0015734aae7d55cf2ab9d4c11e581cb41ff49bd3edcf69ae669aae701ca71626d377c47de342af5b6f1ac6004c012bd8799fd8799f58202eb680cbe5ae64b54f5ae3d8c98e10890eaf3a4d8c8afad3cab5e6fd6bf4bf3aff01ff0001",
			type: "PlutusV2",
		},
		unit: "fa9b05717080fc22815caac4524c2eb91c1fd58ec8879f73fed1ef154d756c746953696743657274",
	};

	const configUpdate: ConfigUpdate = {
		unit: initResultHarcoded.unit,
		oldCosignerKeys: [
			lucid.utils.paymentCredentialOf(signers.account11.address).hash,

			//lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
		newConfig: {
			threshold: 1,
			cosignerKeys: [
				lucid.utils.paymentCredentialOf(signers.account12.address).hash,
				lucid.utils.paymentCredentialOf(signers.account13.address).hash,

				//lucid.utils.paymentCredentialOf(signers.account11.address).hash,
				// lucid.utils.paymentCredentialOf(signers.account12.address).hash,
				// lucid.utils.paymentCredentialOf(signers.account13.address).hash,
			],
		},
	};
	console.log(configUpdate)
	lucid.selectWalletFromPrivateKey(signers.account11.privateKey);

	const updateTx = await multisig_update.build(lucid, configUpdate);

	lucid.selectWalletFromPrivateKey(signers.account11.privateKey);
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
	lucid.selectWalletFromPrivateKey(signers.account11.privateKey);


	const assembleTx = await multisig_update.assemble(
		lucid,
		updateTx.toString(),
		// [witness1, witness2, witness3]
		[witness1]
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
	const hardcodedAmount = 10;
	console.log(`Requesting ${hardcodedAmount} BTC to ${myAddress}`);
	const result = await user_request.submit(
		lucid,
		hardcodedAmount,
		myAddress,
		""
	);
	console.log(result);
};
