import { initLucidWithoutWallet, Lucid } from "./utils/lucid";
import { getADATransactionMetadata, getADATransactionUTXOs, getPendingBTCTransactions } from "./utils/relay";
import { getPendingADATransactionsToPolicy } from "./utils/relay";
import { mintPolicyID } from "./utils/relay";
import { getBTCTransaction } from "./utils/relay";
import { btcVaultAddress } from "./utils/relay";
import { getADATransaction } from "./utils/relay";

import * as utils from "./endpoints/utils";
import * as user_request from "./endpoints/user.request";
import { deployments } from "./endpoints/config.deployment";

import {
	ConfigFullFill,
	ConfigMultiSig,
	ConfigUpdateMultiSig,
	DeployedScripts,
} from "./endpoints/types";

import fetch from "node-fetch";

import * as multisig_fullfill from "./endpoints/multisig.fullfill";

interface Props {
	lucid: Lucid;
}

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const signers = {
	account1: {
		seedPhrase:
			"accuse total squeeze adapt stand slam joy lamp poet party pear level sunny banana boil glue because either shine come drum stand shoulder damp",
		address:
			"addr_test1qzk8up9zntq4l08r53p7gmpsgc3jdg9fwk79wreqqpsm2r97fdk28qxaf96cj78p5dmupfqe8jrcfxjqrc4hectl93xqr2levc",
	}, // 100
	account2: {
		seedPhrase:
			"machine drip toe west mimic tissue fuel you audit almost segment ridge suffer wood diet priority reflect gadget crack weather course vast alpha minute",
		address:
			"addr_test1qqy4xaqqeqk8x08dghtyp32hdn40w3ph93k8erg6lt9qtquxdjv0eq7qec7p0vjux3cx3gclxnl4clpqwehzvxqgpjlsxd6uwq",
	}, // 100
	account3: {
		seedPhrase:
			"foster lunar steel trim echo blame emotion siege scissors problem audit slender soup daughter soap world symptom behind high lawsuit squeeze employ path rebuild",
		address:
			"addr_test1qp70hfk0dtqef3y77e7weduhes5qn9dyjflxsdtkanzttmkherm3zelk4cty8xnmpfx9k3hjmxsy3d3sn43469x79dsqd8azq2",
	}, // 100
	account11: {
		seedPhrase:
			"uniform monster match glimpse supply glide term load whip hard fee wrong behind curtain nephew lesson predict appear pink vendor doctor visit quality glory",
		address:
			"addr_test1qrs8llu8zcqst2dldrvh84f5jphnxjxxscmel4tes6jtm3jcfcs3snyd0tdklx8n9qnkdnvy7v2q20qxawsqsekny4gsuen575",
	}, // 100
	account12: {
		seedPhrase:
			"ugly thunder nut horn canvas common library this force solid winter guide spirit sure wagon vault bus lens mercy install wife club priority original",
		address:
			"addr_test1qz52xndt25hvu8evdshep4puv6jvvh9wmsfcm53x903rpw853z7tyfhplvl8gjq4hgpgcecmj0x2vfe2ppgkwwc7wejsqyv62y",
	}, // 100
	account13: {
		seedPhrase:
			"fire iron mirror accuse glass pact retire lava tongue vague clap combine solve prefer raven attract lens tape unable brave stock loyal okay try",
		address:
			"addr_test1qz6z9pah4v4legh2a6kren7pfl9fnclklvngg5jf8rrt0jjfpqpd6q2h56urmzl9uljzta5pwqfre5zuzckd9plyzf8qrjs37p",
	},
};

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const user = {
	account1: {
		seedPhrase:
			"click path wonder art cage duty infant shy split rookie extend first unaware boat group provide exercise leopard size mammal monitor bamboo dilemma grow",
		address:
			"addr_test1qze80dj5wtpnnrflfjag7hdyn235vwqskhm0fr98kqhey4wwatz92rau7w5ny8lzctkc2tnhfxwvp9gze5yh8j957t6s6ejca0",
	},
};

// Initiates request for a user.
export const request = async (lucid: Lucid, amount: number, ada_addr: string, btc_addr: string) => {
	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	// This Address has Staking Credential
	const myAddress = ada_addr;
	const bridgeAmount = amount;
	const btcAddress = btc_addr;
	console.log(`Requesting ${bridgeAmount} cSatoshis to ${myAddress}`);
	const result = await user_request.submit(
		lucid,
		bridgeAmount,
		myAddress,
		btcAddress,
		deployments.scripts.guardianValidator
	);
	console.log(result);
};

// Fullfill requests from users
export const fullfil = async (lucid: Lucid) => {
	const configSign: ConfigFullFill = {
		unit: deployments.units.multiSigCert,
		scripts: deployments.scripts,
		keys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	lucid.selectWalletFromSeed(signers.account1.seedPhrase);

	// Get Valid Datums from Guardian Script
	const validDatumUtxoList = await utils.getValidDatums(
		lucid,
		deployments.scripts.guardianValidator
	);
	if (!validDatumUtxoList?.length) {
		console.log("No valid datums at Guardian Script");
		return null;
	}
	console.log("validDatumUtxoList: ", validDatumUtxoList);

	if(validDatumUtxoList.length == 0){
		return null;
	}

	// Build transaction with Valid Datums and UTXOs
	// Guardian Minter, Guardian Script and Guardian Multisig are inlcuded
	const fulfillTx = await multisig_fullfill.build(
		lucid,
		[validDatumUtxoList[0]],
		configSign
	);

	lucid.selectWalletFromSeed(signers.account1.seedPhrase);
	const witness1 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromSeed(signers.account2.seedPhrase);
	const witness2 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromSeed(signers.account3.seedPhrase);
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
	return true;
};


// const CryptoAccount = require("send-crypto");

export const sendBitcoin = async (recieverAddress: string, amountToSend: number) => {
	// /* Load account from private key */
	// const privateKey = process.env.PRIVATE_KEY || CryptoAccount.newPrivateKey();
	// const account = new CryptoAccount(privateKey, {
	// 	network: "testnet",
	// });
	// console.log("secret:")
	// console.log(privateKey);

	// /* Print address */
	// console.log(await account.address("BTC"));
	// // > "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"

	// /* Print balance */
	// console.log(await account.getBalance("BTC"));
	// // > 0.01

	// /* Send 0.01 BTC */
	// const txHash = await account
	// 	.send(recieverAddress, amountToSend, "BTC")
	// 	.on("transactionHash", console.log)
	// 	// > "3387418aaddb4927209c5032f515aa442a6587d6e54677f08a03b8fa7789e688"
	// 	.on("confirmation", console.log);
	// // > 1
	// // > 2 ...

	console.log('sending BTC');
}
	 

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

// Internal queue to keep track of pending orders.
let mint_queue = new Array();
let redeem_queue = new Array();
let mint_queue_time = {};
let redeem_queue_time = {};

// Internal database of done transactions.
let mint_db = new Set();
let mint_finished = new Set();
let redeem_db = new Set();
let redeem_finished = new Set();

// Time delayed between each check.
let epoch_delay = 20000 // ms

// Start Time.
let start_time = Math.floor(Date.now() / 1000);


async function update_mint_queue(){
	// Step 1 Get all transactions using getPendingBTCTransactions
	let txs = await getPendingBTCTransactions().then((res) => {return res});
	// Step 2 Check time (after server start) and direction (incoming)	
	// Step 3 Add to mint_queue the ones not in db and add them to db
	for(let i in txs){
		let tx_id = txs[i as keyof typeof txs];
		if(!(mint_db.has(tx_id))){
			let tx = await getBTCTransaction(tx_id);
			// Check if tx is going to our vault
			let is_incoming = false;
			for(let o in tx.outputs){
				if(tx.outputs[o].address == btcVaultAddress){
					is_incoming = true;
				}
			}
			if(tx_id=="ef6991e820e65bbe36a8dff40cc7aad0dbef60678021dba24bd19b1a82e70380"){
				console.log(tx.time)
				console.log(start_time)
				console.log(tx)
				console.log("HERE")
			}
			// Check if tx is after server start time
			let is_after_start_time = false;
			if(tx.time > start_time){
				is_after_start_time = true;
			}
			if(is_incoming && tx_id=="ef6991e820e65bbe36a8dff40cc7aad0dbef60678021dba24bd19b1a82e70380"){
				// Check if tx is not in mint_db already
				mint_queue.push(tx_id);
			}
		}
		mint_db.add(tx_id);
	}

	console.log(mint_queue.length)
}

async function mint(lucid: Lucid){
	// Step 4 runSimulator.fulfill(lucid)
	// await runSimulator.fullfil(lucid);
	let result = await fullfil(lucid);
	console.log(result);
	return true;
}

async function execute_mint(lucid: Lucid){
	if(mint_queue.length > 0){
		// Step 1 Pop next transaction in mint_queue
		let tx_id = mint_queue.shift();
		let tx = await getBTCTransaction(tx_id);

		let btc_addr = null;
		try {
			btc_addr = tx.inputs[0].coin.address;
		} catch (error) {
			console.log(error);
		}
		let OP_RETURN = null;
		let amount = null;

		for(let o in tx.outputs){
			if(tx.outputs[o].address == null){
				OP_RETURN = Buffer.from(tx.outputs[o].script, 'hex').toString().substring(2);
			}
			if(tx.outputs[o].address == btcVaultAddress){
				amount = tx.outputs[o].value;
			}
		}

		// Step 2 Verify BTC transaction is good
		if(OP_RETURN != null && amount != null && btc_addr != null){
			// Step 3 runSimulator.request(lucid)
			// bytes.fromhex(OP_TURN).decode('utf-8')[2:]
			let ada_addr = OP_RETURN;
			if(ada_addr.startsWith("P")){
				ada_addr = ada_addr.slice(1);
				console.log("Slicing first char")
				console.log(ada_addr)
			}
			let paymentCreds = lucid.utils.paymentCredentialOf(ada_addr);

			console.log("Minting with this info")
			console.log(ada_addr)
			console.log(amount)
			console.log(btc_addr)
			console.log(lucid.utils.credentialToAddress(paymentCreds))

			await request(lucid, amount, lucid.utils.credentialToAddress(paymentCreds), btc_addr);
			if(await mint(lucid)){
				return true;
			}
		}
		// Step 5 if failed log and requeue	
		return false;
	}
	// Step 5 if failed log and requeue	
	return true;
}

async function update_redeem_queue(){
	// Step 1 Get all transactions using getPendingADATransactions
	let txs = await getPendingADATransactionsToPolicy().then((res) => {return res});
	// Step 2 Check time (after server start) and direction (incoming)
	// Step 3 Add to redeem_queue the ones not in db and add them to db
	for(let i in txs){
		let tx_id = txs[i as keyof typeof txs]["tx_hash"]; // "ffa3f3263803c64ea350c2eabd065072b012c3018951edafd9ee276cb5aa2b0c"
		let amount = txs[i as keyof typeof txs]["amount"];
		if(redeem_db.has(tx_id)){
			continue;
		}
		let tx = await getADATransactionUTXOs(tx_id);
		let tx_data = await getADATransaction(tx_id);
		// Check if tx is going to our smart contract
		let is_incoming = false;
		for(let o in tx.outputs){
			if(tx.outputs[o].address == mintPolicyID){
				is_incoming = true;
				for(let itx in tx.inuts){
					if(tx.inputs[itx].address == mintPolicyID){
						is_incoming = false;
					}
				}
			}
		}
		if(txs[i as keyof typeof txs]["action"] == "burned"){
			is_incoming = true;
		}

		// Check if tx is after server start time
		let is_after_start_time = false;
		if(tx_data.block_time > start_time){
			is_after_start_time = true;
		}
		if(is_incoming && is_after_start_time){
			let metadata = await getADATransactionMetadata(tx_id);
			if(metadata.length != 0){
				// Check if tx is not in redeem_db already
				if(!(redeem_db.has(tx_id))){
					redeem_queue.push([tx_id, amount]);
					redeem_db.add(tx_id);
				}
			}
		}
	}

	console.log(redeem_queue.length);
}

function redeem(btc_addr: string, amount: number){
	// Step 3 Send BTC back to user
	try {
		sendBitcoin(btc_addr, amount);
	} catch (error) {
		console.error(error);
		return false;
	}
	return true;
}


async function RedeemAPI(sender_addr: string, amount: string, receiver_addr: string): Promise<{ [key: string]: any }> {
	const params = {"sender_addr": sender_addr,
    				"amount": amount,
    				"receiver_addr": receiver_addr}
	console.log("Sending Redeem with params:")
	console.log(params)
	const response: { [key: string]: any } = await fetch('/api/shell', {
		method: 'POST',
		headers: {
		  'Content-Type': 'application/json',
		},
		body: JSON.stringify(params),
	  })
	const data: { [key: string]: any } = await response.json();
	console.log(data)
	return data;
}
  

async function execute_redeem(){
	if(redeem_queue.length > 0){
		// Step 1 Pop next transaction in redeem_queue
		let tx_amount_pair = redeem_queue.shift();
		let tx = tx_amount_pair[0];
		let amount_num = Number(tx_amount_pair[1].slice(1));
		let amount = amount_num.toString();
		// Step 2 Verify Burn cBTC transaction is good
		console.log("redeem");
		console.log(tx);
		let metadata = await getADATransactionMetadata(tx);
		let receiver_addr = metadata[0].json_metadata.btcAddress;
		let tx_data = await getADATransactionUTXOs(tx);
		console.log(receiver_addr);
		console.log(tx_data);
		console.log(amount);
		
		const sender_addr = btcVaultAddress
		// const amount = "20000"
		// const receiver_addr = "2Mvv9VrwFYWFGz18tQ8E6EZ6SKf2Dhm6htK"

		const result_str: { [key: string]: any } = await RedeemAPI(sender_addr, amount, receiver_addr);
		console.log("Result:")
		console.log(result_str);
		if(result_str["response"].includes("True")){
			return true;
		}

		// Step 4 if failed log and requeue	
		return false;
	}
	return true;
}
  
function Run({ lucid }: Props){
	(async () => {
		let epoch = 0;
		while(true){
			// TODO - Print to user
			// console.log("Austin:")
			// let ada_addr = "addr_test1qpz5749x4uzgm9vejkpd2djd5gr3y7h8g8pht2a2xk5vruzv9xa76fawl56mzmtunu7p574yv9ghe5cnr6qjf06zhj7s0z8xal" //"addr_test1qqxm6xdfgy9700tal3je94s7eqeusu08cku0rkvmsqkldytj60xr5crz6tmy995kskqtgukfhjearmcejld8z0wzsegqkp23xu"
			// let paymentCreds = lucid.utils.paymentCredentialOf(ada_addr)
			// console.log(paymentCreds.hash)
			// console.log(lucid.utils.credentialToAddress(paymentCreds))

			if(epoch >= 5){
				// Read Minting Requests and Add to Queue
				try {
					await update_mint_queue();
				}
				catch(err) {
					console.log(err);
				}

				epoch = 0;
			}

			// Pop and Try to Complete Next Minting Request
			try {
				execute_mint(lucid);
			}
			catch(err) {
				console.log(err);
			}

			/// Mint Outstanding orders
			try {
				await mint(lucid);
			}
			catch(err) {
				console.log(err);
			}
			///


			// Read Redeem Requests (using getPendingADATransactions()) and Add to Queue
			try {
				await update_redeem_queue();
			}
			catch(err) {
				console.log(err);
			}
			// Pop and Try to Complete Next Redeem Request
			for (let i = 0; i < 2; i++) {
				try {
					let result = await execute_redeem();
				}
				catch(err) {
					console.log(err);
				}
			}
			
			await sleep(epoch_delay);
			epoch += 1;
		}
	})();
}
  
const MainServer = async () => {
	const lucid = await initLucidWithoutWallet().then((lucid) => {return lucid});

    Run({lucid});
};

//export default MainServer;
MainServer();