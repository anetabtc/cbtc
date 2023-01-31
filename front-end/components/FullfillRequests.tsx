import { getAllDatums, getValidDatums } from "@/endpoints/utils";
import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import { assembleTx, buildTx, partialSignTx } from "@/endpoints/fullfill";
import * as runSimulator from "@/endpoints/runSimulator"

interface Props {
	lucid: Lucid;
}

export const FullfillRequests = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick = async () => {
		const validDatums = await getValidDatums(lucid);
		console.log("validDatums: ", validDatums);
		if (!validDatums?.length) {
			console.log("No valid datums at Guardian Script");
			return null;
		}
		// const result = await fullfillRequests(lucid, validDatums);
		// result ? setError(result) : setError("");
		const cosigner = await lucid.wallet.address() 
		const txAsCbor = await buildTx(lucid,validDatums,[cosigner])
		const witness1 = await partialSignTx(lucid, txAsCbor)
		const txHash = await assembleTx(lucid,txAsCbor,[witness1])
		console.log(txHash)
	};

	const handleClick1 = async () => {
		const result = await getAllDatums(lucid);
		console.log(result);
	};

	const handleClick2 = async () => {
		const result = await getValidDatums(lucid);
		console.log(result);
	};

	const handleClick3 = async () => {
		await runSimulator.sign(lucid)
	};


	return (
		<div>
			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick()}
			>
				Fullfill Request
			</button>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick1()}
			>
				Get All Datums
			</button>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick2()}
			>
				Get Valid Datums
			</button>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick3()}
			>
				Emulator
			</button>

			{error && (
				<div className="alert alert-error shadow-lg">
					<div>
						<svg
							xmlns="http://www.w3.org/2000/svg"
							className="stroke-current flex-shrink-0 h-6 w-6"
							fill="none"
							viewBox="0 0 24 24"
						>
							<path
								strokeLinecap="round"
								strokeLinejoin="round"
								strokeWidth="2"
								d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
							/>
						</svg>
						<span>{error}</span>
					</div>
				</div>
			)}
		</div>
	);
};
