import { Lucid } from "lucid-cardano";
import React, { useEffect, useState } from "react";
import * as runEmulator from "@/endpoints/test/runEmulator";

interface Props {
	lucid: Lucid;
}

export const Emulator = ({ lucid }: Props) => {
	const [error, setError] = useState("");

	useEffect(() => {
		if (!error) return;

		const timeout = setTimeout(() => setError(""), 5000);

		return () => {
			clearTimeout(timeout);
		};
	}, [error]);

	const handleClick1 = async () => {
		//TODO: Code pending
		// await runEmulator.request(lucid);
	};

	const handleClick2 = async () => {
		//TODO: Code pending
		// await runEmulator.fullfil(lucid);
	};

	const handleClick3 = async () => {
		await runEmulator.update();
	};

	const handleClick4 = async () => {
		// Code pending
		// await runEmulator.init(lucid);
	};

	return (
		<div>
			<h1 className="text-5xl font-bold text-center">Emulator</h1>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick1()}
			>
				Request(TODO)
			</button>
			
			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick2()}
			>
				FullFill(TODO)
			</button>

			<button
				className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick3()}
			>
				Update
			</button>

			<button
				className="btn btn-outline btn-accent btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
				onClick={() => handleClick4()}
			>
				Init(TODO)
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
