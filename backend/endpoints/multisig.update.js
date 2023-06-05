import { Constr, Data } from "../utils/lucid";
export const build = async (lucid, config) => {
    const scriptUtxo = await lucid.utxoByUnit(config.unit);
    const multisigValidatorAddr = lucid.utils.validatorToAddress(config.multiSigValidator);
    // const Datum = Data.to(
    // 	new Constr(0, [
    // 		config.newConfig.cosignerKeys,
    // 		BigInt(config.newConfig.threshold),
    // 	])
    // )
    const MyDatum = Data.Object({
        keys: Data.Array(Data.String),
        requiredCount: Data.BigInt,
    });
    const datum = {
        keys: config.newConfig.keys,
        requiredCount: BigInt(config.newConfig.requiredCount),
    };
    const Datum = Data.to(datum, MyDatum);
    const RedeemerUpdate = Data.to(new Constr(0, [])); // Update
    const signers = config.oldKeys
        .map((key) => {
        return lucid.newTx().addSignerKey(key);
    })
        .reduce((prevTx, tx) => {
        return prevTx.compose(tx);
    });
    const tx = await lucid
        .newTx()
        .collectFrom([scriptUtxo], RedeemerUpdate)
        .attachSpendingValidator(config.multiSigValidator)
        .payToContract(multisigValidatorAddr, { inline: Datum }, scriptUtxo.assets)
        .compose(signers)
        .complete();
    return tx;
};
export const signWitness = async (lucid, txAsCbor) => {
    return await lucid.fromTx(txAsCbor).partialSign();
};
export const assemble = async (lucid, txAsCbor, witnesses) => {
    const signedTx = await lucid.fromTx(txAsCbor).assemble(witnesses).complete();
    const txHash = signedTx.submit();
    return txHash;
};
