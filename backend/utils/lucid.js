import { Constr, Data, Blockfrost, Lucid } from 'lucid-cardano';
const blockfrostKey = "preprodSU7lq1Sscp83v5KTuIwVixlLiT6zsrxW"; //process.env.BLOCKFROST_KEY as string
const apiURL = "https://cardano-preprod.blockfrost.io/api/v0"; //process.env.API_URL as string
const network = "Preprod"; //process.env.NETWORK as Network
// const initLucid = async (wallet: string) => {
//     const api = await window.cardano[wallet.toLowerCase()].enable()
//     const lucid = await Lucid.new(new Blockfrost(apiURL, blockfrostKey), network)
//     lucid.selectWallet(api)
//     return lucid;
// }
const initLucidWithoutWallet = async () => {
    console.log(apiURL);
    const lucid = await Lucid.new(new Blockfrost(apiURL, blockfrostKey), network);
    return lucid;
};
export { initLucidWithoutWallet, Lucid, Data, Constr };
