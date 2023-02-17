import initLucid from "@/utils/lucid";
import { getPendingBTCTransactions } from "@/utils/relay";
import { getPendingADATransactions } from "@/utils/relay";
import { useStoreState } from "@/utils/store";
import { Lucid } from "lucid-cardano";
import { useEffect, useState } from "react";
import { Utils } from "./Utils";
import { User } from "./User";
import { Simulator } from "./Simulator";
import { Emulator } from "./Emulator";
import * as runSimulator from "@/endpoints/tests/runSimulator";

import React from 'react';

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
let epoch_delay = 20000 // ms


async function update_mint_queue(){
	// Step 1 Get all transactions using getPendingBTCTransactions
	let txs = await getPendingBTCTransactions().then((res) => {return res});
	// Step 2 Check time (after server start) and direction (incoming)
	if(true){
		// Step 3 Add to mint_queue the ones not in db and add them to db
		for(let i in txs){
			let tx = txs[i];
			if(!(mint_db.has(tx))){
				mint_queue.push(tx);
				mint_db.add(tx);
			}
		}
	}

	console.log(mint_queue.length)
}

function mint(){
	// Step 4 runSimulator.fulfill(lucid)
	// await runSimulator.fullfil(lucid);
	return true;
}

function execute_mint(){
	// Step 1 Pop next transaction in mint_queue
	let tx = mint_queue.shift()
	// Step 2 Verify BTC transaction is good
	if(true){
		// Step 3 runSimulator.request(lucid)
		if(mint()){
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


function Run(){
	(async () => {
		while(true){
			console.log(`Time ${Math.floor(Date.now() / 1000)}`);
			// Read Minting Requests and Add to Queue
			update_mint_queue()
			// Pop and Try to Complete Next Minting Request
			execute_mint()
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
	return (
		<div className="flex-column">
			<ServerInfo/>
			<div className="divider"></div>
			<Run/>
			<div className="divider"></div>
		</div>
	);
};

export default MainServer;
