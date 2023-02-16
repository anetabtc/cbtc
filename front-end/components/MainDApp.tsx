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

/*
	TODO Turn this Dockerized Front-End into our cBTC-Cardano-Node
	Outline:

	# Internal queue to keep track of pending orders.
	let mint_queue = []
	let redeem_queue = []
	let mint_queue_time = {}
	let redeem_queue_time = {}

	# Internal database of done transactions.
	let mint_db = set()
	let mint_finished = set()
	let redeem_db = set()
	let redeem_finished = set()

	while (true) {
		
		console.log(Time)

		# # Read Minting Requests and Add to Queue
		# Step 1 Get all transactions using getPendingBTCTransactions
		# Step 2 Check time (after server start) and direction (incoming)
		# Step 3 Add to mint_queue the ones not in db and add them to db

		update_mint_queue()

		# # Pop and Try to Complete Next Minting Request
		# Step 1 Pop next transaction in mint_queue
		# Step 2 Verify BTC transaction is good
		# Step 3 runSimulator.request(lucid)
		# Step 4 runSimulator.fulfill(lucid)
		# Step 5 if failed log and requeue

		execute_mint()

		# Read Redeem Requests (using getPendingADATransactions()) and Add to Queue
		# Step 1 Get all transactions using getPendingBTCTransactions
		# Step 2 Check time (after server start) and direction (incoming)
		# Step 3 Add to redeem_queue the ones not in db and add them to db

		update_redeem_queue()

		# Pop and Try to Complete Next Redeem Request
		# Step 1 Pop next transaction in redeem_queue
		# Step 2 Verify Burn cBTC transaction is good
		# Step 3 Send BTC back to user
		# Step 4 if failed log and requeue

		execute_redeem()

		time.sleep(epoch_delay)
	}
*/


const MainDApp = () => {
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
		//
	
		getPendingBTCTransactions().then((res) => console.log(res));
		getPendingADATransactions().then((res) => console.log(res));

		//
	}, [lucid, walletStore.connected, whiteListed]);

	if (!lucid) return null;

	return (
		<div className="flex-column">
			<User lucid={lucid} />
			<div className="divider"></div>
			<Utils lucid={lucid} />
			<div className="divider"></div>
			<Simulator lucid={lucid} />
			<div className="divider"></div>
			<Emulator lucid={lucid} />
		</div>
	);
};

export default MainDApp;
