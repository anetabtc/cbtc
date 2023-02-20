import initLucid from "@/utils/lucid";
import { getPendingBTCTransactions } from "@/utils/relay";
import { getPendingADATransactions } from "@/utils/relay";
import { getBTCTransaction } from "@/utils/relay";
import { getADATransaction } from "@/utils/relay";
import { useStoreState } from "@/utils/store";
import { Lucid } from "lucid-cardano";
import { useEffect, useState } from "react";
import { Utils } from "./Utils";
import { User } from "./User";
import { Simulator } from "./Simulator";
import { Emulator } from "./Emulator";
import * as runSimulator from "@/endpoints/tests/runSimulator";

import * as utils from "@/endpoints/utils";
import * as user_request from "@/endpoints/user.request";
import * as user_burn from "@/endpoints/user.burn";
import { deployments } from "@/endpoints/config.deployment";

import {
	ConfigFullFill,
	ConfigMultiSig,
	ConfigUpdateMultiSig,
	DeployedScripts,
} from "@/endpoints/types";

import * as multisig_fullfill from "@/endpoints/multisig.fullfill";

import React from 'react';

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


// Initiates request for a user.
export const request = async (lucid: Lucid, amount: number, ada_addr: string, btc_addr: string) => {
	// This Address has Staking Credential
	const myAddress = ada_addr;
	const bridgeAmount = amount;
	const btcAddress = btc_addr;
	console.log(`Requesting ${bridgeAmount} BTC to ${myAddress}`);
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
};

interface Props {
	lucid: Lucid;
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

const ServerInfo = () => {
	return (
	  <div>
		<h2>Running Server</h2>
	  </div>
	);
}

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
let epoch_delay = 20000 * 100 // ms


async function update_mint_queue(){
	// Step 1 Get all transactions using getPendingBTCTransactions
	let txs = await getPendingBTCTransactions().then((res) => {return res});
	// Step 2 Check time (after server start) and direction (incoming)	
	// Step 3 Add to mint_queue the ones not in db and add them to db
	for(let i in txs){
		let tx_id = txs[i as keyof typeof txs];
		let tx = await getBTCTransaction(tx_id)
		// Check if tx is going to our vault
		// Check if tx is not in mint_db already
		// Check if tx is after server start time
		// console.log(tx['inputs']['0']['address'])
		if(true){
			if(!(mint_db.has(tx_id))){
				mint_queue.push(tx_id);
				mint_db.add(tx_id);
			}
		}
	}

	console.log(mint_queue.length)
}

async function mint(lucid: Lucid){
	// Step 4 runSimulator.fulfill(lucid)
	// await runSimulator.fullfil(lucid);
	fullfil(lucid);
	return true;
}

async function execute_mint(lucid: Lucid){
	// Step 1 Pop next transaction in mint_queue
	let tx = mint_queue.shift()
	// Step 2 Verify BTC transaction is good
	if(true){
		// Step 3 runSimulator.request(lucid)
		let amount = 10.0;
		let ada_addr = "addr_test1qpj5w4sru6qg4rgp4623wm5t7nz7m2rmhvk3d4gxfnak0vnfvdytm4537jmpy0hex7y2qnzya994t60nqet8qxefx5msfjpwlm";
		let btc_addr = "9i5no316Em2jfnc4T3absEBbSj1A2h61pkVHTCaKYvwK8KaQAST";
		await request(lucid, amount, ada_addr, btc_addr);
		if(await mint(lucid)){
			return true;
		}
	}
	// Step 5 if failed log and requeue	
	return false
}

async function update_redeem_queue(){
	// Step 1 Get all transactions using getPendingADATransactions
	let txs = await getPendingADATransactions().then((res) => {return res});
	// Step 2 Check time (after server start) and direction (incoming)
	// console.log(txs[0])
	// Check if tx is going to our smart contract
	// Check if tx is not in redeem_db already
	// Check if tx is after server start time
	if(true){
		// Step 3 Add to redeem_queue the ones not in db and add them to db
		for(let i in txs){
			let tx = txs[i];
			if(!(redeem_db.has(tx))){
				redeem_queue.push(tx)
				redeem_db.add(tx)
			}
		}
	}

	console.log(redeem_queue.length)
}

function redeem(){
	// Step 3 Send BTC back to user
	return true
}

function execute_redeem(){
	// Step 1 Pop next transaction in redeem_queue
	let tx = redeem_queue.shift()
	// Step 2 Verify Burn cBTC transaction is good
	if(true){
		if(redeem()){
			return true;
		}
	}
	// Step 4 if failed log and requeue	
	return false
}


function Run({ lucid }: Props){
	(async () => {
		while(true){
			console.log(`Time ${Math.floor(Date.now() / 1000)}`);
			// Read Minting Requests and Add to Queue
			update_mint_queue()
			// Pop and Try to Complete Next Minting Request
			execute_mint(lucid)
			// Read Redeem Requests (using getPendingADATransactions()) and Add to Queue
			update_redeem_queue()
			// Pop and Try to Complete Next Redeem Request
			execute_redeem()
			
			await sleep(epoch_delay);
		}
	})();

	return (
		<div>
		  <h2>Started async server loop.</h2>
		</div>
	);
}
  
const MainServer = () => {
	const walletStore = useStoreState((state: any) => state.wallet);
	const [lucid, setLucid] = useState<Lucid>();
	const [whiteListed, setWhiteListed] = useState(false);

	const isWhiteListed = async () => {
		const result = await window.cardano[
			walletStore.name.toLowerCase()
		]?.isEnabled();
		result ? setWhiteListed(true) : setWhiteListed(false);
	};

	useEffect(() => {
		isWhiteListed();
		console.log("whiteListed: ", whiteListed);
		if (whiteListed && walletStore.connected && !lucid) {
			console.log("initialize lucid");
			initLucid(walletStore.name).then((Lucid: Lucid) => {
				setLucid(Lucid);
			});
		}
	}, [lucid, walletStore.connected, whiteListed]);

	if (!lucid) return null;

	return (
		<div className="flex-column">
			<ServerInfo/>
			<div className="divider"></div>
			<Run lucid={lucid} />
			<div className="divider"></div>
		</div>
	);
};

export default MainServer;
