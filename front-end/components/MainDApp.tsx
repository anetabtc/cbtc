import initLucid from "@/utils/lucid";
import { useStoreState } from "@/utils/store";
import { Lucid } from "lucid-cardano";
import { useEffect, useState } from "react";

const MainDApp = () => {
	const walletStore = useStoreState((state: any) => state.wallet);
	const [lucid, setLucid] = useState<Lucid>();
	const [rafflePolicy, setRafflePolicy] = useState();
	const [whiteListed, setWhiteListed] = useState(false);

	const isWhiteListed = async () => {
		const result = await window.cardano[
			walletStore.name.toLowerCase()
		]?.isEnabled();
		result ? setWhiteListed(true) : setWhiteListed(false);
	};

	useEffect(() => {
		isWhiteListed();
		console.log("Raffle.tsx -> whiteListed", whiteListed);
		console.log("Raffle.tsx -> lucid", lucid);
		console.log("Raffle.tsx -> walletStore.connected", walletStore.connected);
		if (whiteListed && walletStore.connected && !lucid) {
			console.log("Raffle.tsx -> initLucid");
			initLucid(walletStore.name).then((Lucid: Lucid) => {
				setLucid(Lucid);
			});
		}
	}, [lucid, walletStore.connected, whiteListed]);

	return (
        <div className="flex items-center">

        </div>
    )
};

export default MainDApp;
