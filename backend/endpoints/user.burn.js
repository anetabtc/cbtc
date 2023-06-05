import { Constr, Data, toUnit, fromText } from "lucid-cardano";
export const submit = async (lucid, burnAmount, btcAddress, cBTCMintingPolicy) => {
    try {
        const unit = toUnit(lucid.utils.mintingPolicyToId(cBTCMintingPolicy), fromText("cBTC"));
        const walletUtxos = await lucid.utxosAtWithUnit(await lucid.wallet.address(), unit);
        const redeemer = Data.to(new Constr(1, []));
        const totalAssets = { [unit]: BigInt(burnAmount) };
        const tx = await lucid
            .newTx()
            .collectFrom(walletUtxos)
            .attachMintingPolicy(cBTCMintingPolicy)
            .mintAssets(totalAssets, redeemer)
            .attachMetadata(0, { btcAddress: btcAddress })
            .complete();
        const signedTx = await tx.sign().complete();
        const txHash = signedTx.submit();
        return txHash;
    }
    catch (error) {
        if (error instanceof Error)
            return error;
        return Error(`error : ${JSON.stringify(error)}`);
    }
};
