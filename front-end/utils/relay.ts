import { Blockfrost, Lucid, Network } from 'lucid-cardano';

const blockfrostKey = process.env.BLOCKFROST_KEY as string
const apiURL = process.env.API_URL as string
const network = process.env.NETWORK as Network

const adaAPI = "https://cardano-preprod.blockfrost.io/api/v0/"
export const mintPolicyID = "2c04fa26b36a376440b0615a7cdf1a0c2df061df89c8c055e265050563425443" //process.env.MINT_POLICY_ID as string
// export const mintPolicyAddress = "addr_test1vr93h9esl962tww08u0q4nv7hd6w9cr6vg2q5aqvkw05qvs5nqxxn" //"addr_test1wr2x24tlcpr37sjrscaqsh6z4tue3k7zx8qt8n0kscen2jct0wkz7" // "5910d65910d301000032223232323232325333573466e1d2000002132323232323253335734646464a666ae68cdc3a40000042646464646464a666ae68cdc3a400800426464646464a666ae68cdc3a40040042a66ae71241455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37373a31352d36340016153357386466e592410b505075624b657948617368003372c9210120003372c9210230780033232300223300200200123002233002002001223322325333573466e1c005200010031323233004002001333718900119b8100248008014cdc7002240006e3400d24100223372c646466e58c94ccd5cd19b8700148000524101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e1490101660016002325333573466e1c00520001490101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e14901016600160013370a006901019b8400248080c010004004dd71aba100115332233357340040022940c94ccd5cd0008800899ab9c49111434254434d696e74506f6c696379206631000013370e0280082a6644666ae68008004528192999ab9a00110011335738920111434254434d696e74506f6c6963792066320000133232323002233002002001230022330020020012253335573e002294054cc8cd5cd0008a51300335742002260046ae88004cc888c8c8c8c94ccd5cd19b87480080085280a9991199ab9a00200114a066e3cdd71aba100100613371201666644466664644446646460044660040040024600446600400400244a666aae7c004401454ccd5cd19baf35573a6ae840040184c010d55cf1aba100113002357440020026ea4004009200023333232222332323002233002002001230022330020020012253335573e002200a2a666ae68cdd79aab9d3574200200c260086aae78d5d0800898011aba2001001375200200490003ad37560020066eacd5d09aba200400748904634254430035573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e37540020026eb8d5d080a9bae357420026eb0d5d080c0992999ab9a00110011335738920111434254434d696e74506f6c696379206633000013370e02690001aab9e00235573a0026ea8c8d5d0991aab9e37540020026ae84d5d11aba2002375a6ae84004c8d55cf1baa00132323232320055333573466e1d20000021323232323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba2003375a0026ae84004d5d10019bad001357420022a66ae712413f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10013574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10011533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001357440066eb8004d5d08009aba2003375a0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37333a31352d3434001635573c0046aae74004dd51aba1357440026ae88004c8d55cf1baa001357426ae88c8d55cf1baa0010013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba20023574200246644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd70099aba135744646aae78dd50008009bac3574201c2a6644666ae68008004528192999ab9a00110011335738920111434254434d696e74506f6c696379206634000013370e01290000992999ab9a00110011335738920111434254434d696e74506f6c696379206635000013371001090001aab9e00235573a0026ea8038526165333573466e1d20000021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50051999111999999111199991919180111980100100091801119801001000912999aab9f00112250011333003357420024446004006260046ae88004cc8894ccd5cd19baf002300300112230020031225001004002001232223002003300400112250015573aaae78dd48010009199911919180111980100100091801119801001000912999aab9f00110031330043574200260046ae8800488c94ccd5cd1803800899b800010021002375a6aae780092000375600229000119b8800148000dd71aba100237566ae84d5d11aba200533322233333322223333232323002233002002001230022330020020012253335573e002244a00226660066ae84004888c00800c4c008d5d1000999112999ab9a3375e00460060022446004006244a0020080040024644460040066008002244a002aae7555cf1ba900200123332232323002233002002001230022330020020012253335573e00220062660086ae84004c008d5d100091192999ab9a300700113370000200420046eb4d55cf001240006eac0045200023371090000009bae357420026eacd5d09aba2357440082a66ae712401445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a35343a332d3137001635573c0046aae74004dd51aba1357440066ae88d5d1000991aab9e37540026ae84004c8d55cf1baa0010014c011e581c83c23aa91f61efbf848fded7f222525ab6a32b45883f626cdacd76890001"
		
const btcAPI = "https://tn-btc-api.anetabtc.io:443/" //https://api.blockcypher.com/v1/"
export const btcVaultAddress = "n4YDfMoo1i3rzF8XEq9zyfo8TFfnroLjy6" //process.env.VAULT_BTC_WALLET_ADDRESS as string

export async function getPendingADATransactionsToPolicy() {
    try {
		const res = await fetch(adaAPI + "assets/" + mintPolicyID + "/history",
            {
                headers: {
                    'PROJECT_ID': blockfrostKey,
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		let txs = await res.json();
        // console.log("getPendingADATransactions")
        return txs;
	} catch (err) {
		console.log(err);
        return {};
	}
}

// This is wrong
export async function getPendingADATransactions() {
    try {
		const res = await fetch(adaAPI + "assets/" + mintPolicyID + "/history",
            {
                headers: {
                    'PROJECT_ID': blockfrostKey,
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		let txs = await res.json();
        // console.log("getPendingADATransactions")
        return txs;
	} catch (err) {
		console.log(err);
        return {};
	}
}
//

export async function getPendingBTCTransactions() {

    try {
		const res = await fetch(btcAPI + "tx/address/" + btcVaultAddress,
            {
                headers: {
                    'Authorization': 'Basic ' + btoa('x:8Al881pe8jSX'),
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		const data = await res.json();
        console.log("getPendingBTCTransactions")
        let txs = []
        for(let i in data) {
            txs.push(data[i].hash)
        }
        console.log("getPendingBTCTransactions")
        return txs;
	} catch (err) {
		console.log(err);
        return {};
	}
}

export async function getADATransaction(tx: string) {
    try {
		const res = await fetch(adaAPI + "txs/" + tx,
            {
                headers: {
                    'PROJECT_ID': blockfrostKey,
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		let data = await res.json();
        return data;
	} catch (err) {
		console.log(err);
        return {};
	}
}

export async function getADATransactionUTXOs(tx: string) {
    try {
		const res = await fetch(adaAPI + "txs/" + tx + "/utxos",
            {
                headers: {
                    'PROJECT_ID': blockfrostKey,
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		let data = await res.json();
        return data;
	} catch (err) {
		console.log(err);
        return {};
	}
}

export async function getADATransactionMetadata(tx: string) {
    try {
		const res = await fetch(adaAPI + "txs/" + tx + "/metadata",
            {
                headers: {
                    'PROJECT_ID': blockfrostKey,
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		let data = await res.json();
        return data;
	} catch (err) {
		console.log(err);
        return {};
	}
}

export async function getBTCTransaction(tx: string) {
    try {
		const res = await fetch(btcAPI + "tx/" + tx,
            {
                headers: {
                    'Authorization': 'Basic ' + btoa('x:8Al881pe8jSX'),
                    'Content-Type': 'application/json',
                },
                method: 'GET'
            }
        );
		const data = await res.json();
        return data;
	} catch (err) {
		console.log(err);
        return {};
	}
}
