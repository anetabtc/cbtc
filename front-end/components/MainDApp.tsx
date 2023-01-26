import initLucid from "@/utils/lucid";
import { useStoreState } from "@/utils/store";
import { Lucid } from "lucid-cardano";
import { useEffect, useState } from "react";
import { FullfillRequests } from "./FullfillRequests";
import { Request } from "./Request";

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
	}, [lucid, walletStore.connected, whiteListed]);

	if (!lucid) return null;

	return (
		<div className="flex-column justify-center items-center">
			<Request lucid={lucid} />
			<FullfillRequests lucid={lucid} />
		</div>
	);
};

export default MainDApp;
