import { Constr, Data, fromText, } from "lucid-cardano";
export const submit = async (lucid, bridgeAmount, cardanoAddress, btcAddress, guardianValidator) => {
    try {
        const walletAddrDetails = lucid.utils.getAddressDetails(cardanoAddress);
        const guardianValidatorAddr = lucid.utils.validatorToAddress(guardianValidator);
        const paymentCred = new Constr(0, [
            walletAddrDetails.paymentCredential?.hash,
        ]);
        const stakingCred = walletAddrDetails.stakeCredential?.hash
            ? new Constr(0, [
                new Constr(0, [
                    new Constr(0, [walletAddrDetails.stakeCredential?.hash]),
                ]),
            ])
            : new Constr(1, []);
        // Supports Address {addressCredential :: Credential,addressStakingCredential :: Maybe StakingCredential}
        const addressAsData = new Constr(0, [paymentCred, stakingCred]);
        const Datum = Data.to(new Constr(0, [BigInt(bridgeAmount), fromText(btcAddress), addressAsData]));
        const tx = await lucid
            .newTx()
            .payToContract(guardianValidatorAddr, { inline: Datum }, { lovelace: BigInt(1000000) })
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
