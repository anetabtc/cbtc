import * as validators from "../utils/validators";
import { Data, generatePrivateKey, generateSeedPhrase, applyParamsToScript, Constr, toText, } from "lucid-cardano";
export const getAllDatums = async (lucid, guardianValApplied) => {
    console.log("Getting All Datums");
    const guardianValidatorAddr = lucid.utils.validatorToAddress(guardianValApplied);
    const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
    if (!scriptUtxos.length)
        return [];
    const datumUtxoList = scriptUtxos.map((utxo) => {
        const datumCbor = utxo.datum || "";
        const datumAsData = Data.from(datumCbor);
        // Try parsing Data -> Address
        // Address: must have StakingHash
        // Valid Address type:  (PubKeyCredential (<PubKeyHash>)) (Just (StakingHash (PubKeyCredential (<PubKeyHash>))))
        const bridgeAmount = datumAsData.fields[0];
        const btcAddress = toText(datumAsData.fields[1]);
        const paymentCredentialHash = datumAsData.fields[2]?.fields[0]?.fields[0];
        const stakeCredentialHash = datumAsData.fields[2]?.fields[1]?.fields[0]?.fields[0]?.fields[0];
        if (!paymentCredentialHash ||
            !stakeCredentialHash ||
            !bridgeAmount ||
            !btcAddress) {
            return {
                isValid: false,
                datum: datumAsData,
                utxo: utxo,
            };
        }
        const paymentCredential = lucid.utils.keyHashToCredential(paymentCredentialHash);
        const stakeCredential = lucid.utils.keyHashToCredential(stakeCredentialHash);
        const readableDatum = {
            bridgeAmount: bridgeAmount,
            btcAddress: btcAddress,
            cardanoAddress: lucid.utils.credentialToAddress(paymentCredential, stakeCredential), // Convert to Bech32 Address
        };
        return {
            isValid: true,
            datum: readableDatum,
            utxo: utxo,
        };
    });
    return datumUtxoList;
};
// Only Address with Staking Credential is supported
//TODO: Maybe consider using TypeBox or Zod for safety data validation
export const getValidDatums = async (lucid, guardianValApplied) => {
    const guardianValidatorAddr = lucid.utils.validatorToAddress(guardianValApplied);
    const scriptUtxos = await lucid.utxosAt(guardianValidatorAddr);
    if (!scriptUtxos.length)
        return [];
    const datumUtxoList = scriptUtxos.reduce((acc, utxo) => {
        const datumCbor = utxo.datum || "";
        const datumAsData = Data.from(datumCbor);
        const bridgeAmount = datumAsData.fields[0];
        const btcAddress = toText(datumAsData.fields[1]);
        const paymentCredentialHash = datumAsData.fields[2]?.fields[0]?.fields[0];
        // return staking credential hash if exits, otherwise returns undefined
        const stakeCredentialHash = datumAsData.fields[2]?.fields[1]?.fields[0]?.fields[0]?.fields[0];
        //Only allow stakeCredentialHash with hash or undefined
        if (paymentCredentialHash && bridgeAmount && btcAddress && (stakeCredentialHash !== '')) {
            const paymentCredential = lucid.utils.keyHashToCredential(paymentCredentialHash);
            const stakeCredential = lucid.utils.keyHashToCredential(stakeCredentialHash);
            const cardanoAddress = stakeCredentialHash
                ? lucid.utils.credentialToAddress(paymentCredential, stakeCredential)
                : lucid.utils.credentialToAddress(paymentCredential);
            const readableDatum = {
                bridgeAmount: bridgeAmount,
                btcAddress: btcAddress,
                cardanoAddress: cardanoAddress,
            };
            const newdata = {
                datum: readableDatum,
                utxo: utxo,
            };
            acc.push(newdata);
        }
        return acc;
    }, []);
    return datumUtxoList;
};
// Only use this if you want to create new hardcoded accounts in prepod, then these accounts must be funded from your wallet
export const generateAddressPrivateKey = async (lucid) => {
    const privKey = generatePrivateKey();
    const address = await lucid
        .selectWalletFromPrivateKey(privKey)
        .wallet.address();
    return {
        privateKey: privKey,
        address: address,
    };
};
// Only use this if you want to create new hardcoded accounts in prepod, then these accounts must be funded from your wallet
export const generateAddressSeedPhrase = async (lucid) => {
    const seedPhrase = generateSeedPhrase();
    const address = await lucid.selectWalletFromSeed(seedPhrase).wallet.address();
    return {
        seedPhrase: seedPhrase,
        address: address,
    };
};
export const buildScripts = (lucid, key, txHash, outputIndex) => {
    const multiSigMintingPolicy = {
        type: "PlutusV2",
        script: applyParamsToScript(validators.multiSigMintingPolicy.script, [
            key,
            lucid.utils.validatorToScriptHash(validators.multiSigValidator),
            new Constr(0, [new Constr(0, [txHash]), BigInt(outputIndex)]), // PTxOutRef
        ]),
    };
    const guardianValidator = {
        type: "PlutusV2",
        script: applyParamsToScript(validators.guardianValidator.script, [
            lucid.utils.validatorToScriptHash(validators.multiSigValidator),
            lucid.utils.mintingPolicyToId(multiSigMintingPolicy),
        ]),
    };
    const cBTCMintingPolicy = {
        type: "PlutusV2",
        script: applyParamsToScript(validators.cBTCMintingPolicy.script, [
            lucid.utils.validatorToScriptHash(guardianValidator),
        ]),
    };
    return {
        multiSigValidator: validators.multiSigValidator,
        multiSigMintingPolicy: multiSigMintingPolicy,
        guardianValidator: guardianValidator,
        cBTCMintingPolicy: cBTCMintingPolicy,
    };
};
