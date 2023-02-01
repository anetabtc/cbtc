import * as client_request from "@/endpoints/client.request";
import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import { isNativeError } from "util/types";

interface Props {
	lucid: Lucid;
}

export const Request = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick = async () => {
		const myAddress = await lucid.wallet.address();
		const hardcodedAmount = BigInt(10);
		const result = await client_request.submit(lucid, myAddress, hardcodedAmount);
		if (result instanceof Error) {
			setError(result.message);
		} else {
			console.log(result);
		}
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Client</h1>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick()}
			>
				Submit Request
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
