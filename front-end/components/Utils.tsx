import * as utils from "@/endpoints/utils";
import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";

interface Props {
	lucid: Lucid;
}

export const Utils = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick1 = async () => {
		const result = await utils.getAllDatums(lucid);
		console.log(result);
	};

	const handleClick2 = async () => {
		const result = await utils.getValidDatums(lucid);
		console.log(result);
	};

	const handleClick3 = async () => {
		const result = await utils.generateAddressSeedPhrase(lucid);
		console.log(result);
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Utils</h1>

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
				Generate Address
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
