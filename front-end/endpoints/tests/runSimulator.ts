import { Lucid } from "lucid-cardano";
import * as multisig_update from "../multisig.update";
import * as multisig_deploy from "../multisig.deploy";
import * as multisig_fullfill from "../multisig.fullfill";
import { ConfigFullFill, ConfigMultiSig, ConfigUpdateMultiSig, DeployedScripts } from "../types";
import * as utils from "../utils";
import * as user_request from "../user.request";

// Accounts generated with utils.generateAddressPrivateKey()
// These addresses don't have StakingCredential
const signers = {
	account1: {
		privateKey:
			"ed25519_sk1lma0ma2unt7k0q6h8v2hkamuvrr5y79vma9ee4cqdnw5w3c3m2ls78aj2s",
		address: "addr_test1vzdekzrwlc0qnrnqck58edn3wyevzd3tasl0c5sx6gzsvyqxt6pfs",
	}, // 100
	account2: {
		privateKey:
			"ed25519_sk1ml60ac9yv0g9nhgwfvkd003wcszpty76ejd9f32v5tf4t34ed8xschs9xq",
		address: "addr_test1vzd0jhkjnzj9jju7m93n377v8zhpy23d8e5esxn9lvravwgauckcr",
	}, // 100
	account3: {
		privateKey:
			"ed25519_sk1vjyuq42dggug9evhczhug0m89d5tqcs7laf402a7sq53g4p2n33qk8rhk6",
		address: "addr_test1vzy73swp6dq5jepsq3hn0j7xafdfqqj8lgga73qe6g34vcq4f7hq5",
	}, // 100
	account11: {
		privateKey:
			"ed25519_sk15a2my6ra9utpl9etw6y5pcrgnk5hs2qsxzcez4wr2v2tzjkh878s9d39sh",
		address: "addr_test1vz0v7def4487mksx6cxgjdn5zvjllyxhtukd0jyh8m2tmncgyjy3q",
	}, // 100
	account12: {
		privateKey:
			"ed25519_sk1r7saa5mh0tqavnv9cq0gmle6ecqxwjq9qrwkke26zkehn4eghcmqg3zsv8",
		address: "addr_test1vz0hg3kh70584yjl2kkvfgjav89jvprqvvuypzdjz7t08tqlcca4t",
	}, // 100
	account13: {
		privateKey:
			"ed25519_sk12nzkcqxruxma4rmd6cde7l9vla7xq3cawl0t4c39rvzuw9kvjmpszkya4r",
		address: "addr_test1vr5vyy87vky2hky445e2pmyy8cvyapuqwr520vk3th30cqclpytwx",
	},
};

// Accounts generated with utils.generateAddressSeedPhrase()
// These account have StakingCredential
const user = {
	account1: {
		address:
			"addr_test1qpjf2g3534w46xxc54nf8zqpglx7skj0d9v7psxua8hggszunzz9vkcmfrzxg2ttqsdtt4zxfhh84853gm9p8vg6364qqsnjmt",
		seedPhrase:
			"couch energy usual pioneer item like gesture turn yard mystery skate glance mimic hip father enable lobster lunar helmet advice marriage market pear delay",
	},
};

// Only run this once to mint multisig nft and set datum with cosigners at multisig script
export const deploy = async (lucid: Lucid) => {
	const multiSigConfig: ConfigMultiSig = {
		requiredCount: 1,
		keys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			// lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	console.log(multiSigConfig);

	// lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	const deployments = await multisig_deploy.submit(lucid, multiSigConfig);

	console.log(deployments);
};

export const deployments = {
    txHash: "7f1958cf29f2a7e82d06a1f6b215867d39a38c0f35b20390e087e538a7c26ea6",
    scripts : {
        multiSigValidator: {
            type: "PlutusV2",
            script: "590abf590abc010000222323232325333573466e1d2002002132323232323232325333573466e1d2002002132323232325333573466e1d200400213232323232323232323253335734a6644666ae68008004528192999ab9a00110011335738921144d756c746953696756616c696461746f72206631000013332223371200464a666aae7c004520001332332322253335573e002200426600666e0000920023574400246600400400246444a666aae7c00440084cc00ccdc0001240046ae880048cc008008004004cdc02400090011aba200133232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba2002357420024664646460044660040040024600446600400400244a666aae7c0045280a999ab9a3375e6ae8400400c528898011aba200100100400137586ae84014dd69aba13574400a6eb0d5d0991aba2357446ae88d5d11aba20013574402a2a6644666ae68008004528192999ab9a00110011335738921144d756c746953696756616c696461746f7220663200001332233232323002233002002001230022330020020012253335573e002294454cc88ccd5cd0010008a50300335742002260046ae880048cc8c8c8c0088cc0080080048c0088cc008008004894ccd55cf8008a5115332233357340040022940c00cd5d0800898011aba20012337126eb4d55cf0009991199991911119919180111980100100091801119801001000912999aab9f001100515333573466ebcd55ce9aba10010061300435573c6ae840044c008d5d10008009ba9001002480008cccc8c8888cc8c8c0088cc0080080048c0088cc008008004894ccd55cf80088028a999ab9a3375e6aae74d5d0800803098021aab9e35742002260046ae88004004dd480080124000eb4dd58008031bae35573a0046eb8d55ce8009bab35573c0020026eacd5d08071bab357426ae8804c4c8c8c94ccd5cd19b87480000084c8c94cc88ccd5cd0010008a5032533357340022002266ae71241144d756c746953696756616c696461746f72206633000013371090000008a9991199ab9a00200114a064a666ae6800440044cd5ce249144d756c746953696756616c696461746f72206634000013371090001bad357426ae8800854cc88ccd5cd0010008a5032533357340022002266ae71241144d756c746953696756616c696461746f7220663500001337126eb4d5d09aba2002001132533357340022002266ae712401144d756c746953696756616c696461746f72206636000013233323230022330020020012300223300200200122253335573e004294454ccd5cd1991919180111980100100091801119801001000912999aab9f00114a02a666ae68cdd79aba100100314a2260046ae88004d5d08010008a501330033574400466ae80d5d0801000800a5eb80dd61aba10023253335573e00229000099919919112999aab9f00110021330033370000490011aba20012330020020012322253335573e002200426600666e0000920023574400246600400400200266e01200048008d5d10009bac35742002646aae78dd50008068992999ab9a00110011335738921144d756c746953696756616c696461746f722066370000133223375e6e9cc8d55cf1baa001002374e646aae78dd50008008108069aab9e00235573a0026ea8074526165333573466e1d20000021324994ccd55cf8008a4c2a66ae71241317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd500c991aab9e3754002032a666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd500a9919191919002a999ab9a3370e900000109919191919191924ca666aae7c004526153357389201317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a38323a332d3235001635573c0046aae74004dd51aba1357440026ae88c8d55cf1baa0013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba2002357420024646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd61aba1007375c6ae8400454cd5ce249475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a37373a332d3434001635573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e375400200266446646460044660040040024600446600400400244a666aae7c004584c94ccd5cd199119baf374e646aae78dd50008011ba73235573c6ea8004004010d5d080089aba135744002260066ae88008c8d55cf1baa001357420020046eb0d5d08010019aba235744002646aae78dd50009aba1005357420022a66ae712401475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a37303a332d3139001635573c0046aae74004dd51aba135744002646aae78dd50008009"
        },
        multiSigMintingPolicy: {
            type: "PlutusV2",
            script: "59082f59082c01000033322222323232325333573466e1d2000002132323232323232325333573466e1d20040021323232323253335734646464a666ae68cdc3a40040042a6644666ae68008004528192999ab9a001100113357389201154d756c74695369674d696e74506f6c696379206635000013370e01c90000992999ab9a001100113357389201154d756c74695369674d696e74506f6c696379206636000013370e01a90010a9991199ab9a00200114a064a666ae6800440044cd5ce249154d756c74695369674d696e74506f6c696379206631000013370e01c90010a9991199ab9a00200114a064a666ae6800440044cd5ce249154d756c74695369674d696e74506f6c696379206632000013370e01a90000a9991199ab9a00200114a064a666ae6800440044cd5ce249154d756c74695369674d696e74506f6c696379206633000015332233357340040022940cdd7992999aab9f001161533357346ae8cd5d100089aba100115335738921244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001637586ae840200644cdc39bad357426ae880212002132533357340022002266ae712401154d756c74695369674d696e74506f6c69637920663400001332233232323002233002002001230022330020020012253335573e002294054cc8cd5cd0008a51300335742002260046ae880048c8cdd79ba73235573c6ea800400cdd3991aab9e37540020026ae84c8d55cf1baa00100100237586ae8404005cd55cf0011aab9d00137540262930b2999ab9a3370e900000109924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd5007991aab9e3754002646464646400aa666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c74695369674d696e74506f6c6963792e68733a35353a352d3331001635573c0046aae74004dd51aba1357440026ae88c8d55cf1baa0013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba200235742002644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd70061bac35742006666444666666444466664646460044660040040024600446600400400244a666aae7c00448940044ccc00cd5d08009111801001898011aba2001332225333573466ebc008c00c004488c00800c48940040100080048c888c00800cc010004489400555ceaab9e3752004002466644646460044660040040024600446600400400244a666aae7c004400c4cc010d5d080098011aba2001223253335734600e002266e000040084008dd69aab9e00248000dd58008a4000466e200052000375c6ae84010dd59aba1357446ae88008ccc888cccccc8888cccc8c8c8c0088cc0080080048c0088cc008008004894ccd55cf800891280089998019aba10012223002003130023574400266444a666ae68cdd780118018008911801001891280080200100091911180100198020008912800aab9d5573c6ea40080048ccc88c8c8c0088cc0080080048c0088cc008008004894ccd55cf80088018998021aba10013002357440024464a666ae68c01c0044cdc000080108011bad35573c00490001bab0011480008cdc4240000026eb8d5d08019bab357426ae88d5d10009aba235744002646aae78dd50009aba100415335738921485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c74695369674d696e74506f6c6963792e68733a34373a352d3436001635573c0046aae74004dd51aba135744002646aae78dd5000800a611e581c9b9b086efe1e098e60c5a87cb6717132c1362bec3efc5206d2050610004c011e581cb8cf6213f0f6e79854ce2fd2a375ff6ef499c0d6a1b7b036a2aefd71004c012bd8799fd8799f5820f88468792ce3caf559a2a5f4445f6e28dff35034bca6e29d0f7ec42f9b50640cff00ff0001"
        },
        guardianValidator: {
            type: "PlutusV2",
            script: "59046b5904680100003322222323232325333573466e1d20020021323232323232323232325333573466e1d20020021323253335734a6644666ae68008004528192999ab9a00110011335738920114477561726469616e56616c696461746f722066310000135746664646460044660040040024600446600400400244a666aae7c00452f5c0264a666ae68c0100044cd5d000098019aba200213003357440046ae84004c88c8c8c94ccd5cd19b8748000008528099b8f375c6ae84004014d55cf0011aab9d0013754646ae84c8d55cf1baa00100135742646aae78dd50008008011bac35742014264a666ae6800440044cd5ce24914477561726469616e56616c696461746f7220663200001337109000199911199991199991919180111980100100091801119801001000912999aab9f00112250011333003357420024446004006260046ae8800494ccd5cd19baf00335573a0022446004006244a0020024644460040066aae780044894004dd48010009199911919180111980100100091801119801001000912999aab9f00110031330043574200260046ae8800488c94ccd5cd1803800899b800010021002375a6aae780092000375600229000119b8848000004dd700a1bab357426ae88c8d55cf1baa00132357426ae88c8d55cf1baa0010013253335573e0022c2a666ae68d5d19aba20011357420022a66ae712401244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba20023574200246644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004008d5d09aba23235573c6ea8004004dd61aba100b149858dd700a1bae357420022a66ae712401485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f477561726469616e56616c696461746f722e68733a3132383a352d3436001635573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e375400200266446646460044660040040024600446600400400244a666aae7c004584c94ccd5cd1919baf374e646aae78dd50008029ba73235573c6ea8004004d5d080089aba135744002260066ae88008c8d55cf1baa001357420020046eb0d5d0802002991aba2357446ae88004d5d10009aba2357440026ae88d5d1000991aab9e37540026ae84014d5d08008a99ab9c4901485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f477561726469616e56616c696461746f722e68733a3132313a352d3231001635573c0046aae74004dd51aba135744002646aae78dd5000800a611e581cb8cf6213f0f6e79854ce2fd2a375ff6ef499c0d6a1b7b036a2aefd71004c011e581cad1e2bc25aadc89922bd8c9e3c54ba790e1cb24470ac1ac93c917ddc0001"
        },
        cBTCMintingPolicy: {
            type: "PlutusV2",
            script: "5910d55910d201000032223232323232325333573466e1d2000002132323232323253335734646464a666ae68cdc3a40040042a6644666ae68008004528192999ab9a0011001133573892111434254434d696e74506f6c696379206631000013370e01290000992999ab9a00110011335738920111434254434d696e74506f6c6963792066310000133710900000409919191919192999ab9a3370e9002001099191919192999ab9a3370e90010010a99ab9c491455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37373a31352d36340016153357386466e592410b505075624b657948617368003372c9210120003372c9210230780033232300223300200200123002233002002001223322325333573466e1c005200010031323233004002001333718900119b8100248008014cdc7002240006e3400d24100223372c646466e58c94ccd5cd19b8700148000524101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e1490101660016002325333573466e1c00520001490101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e14901016600160013370a006901019b8400248080c010004004dd71aba100115332233357340040022940c94ccd5cd0008800899ab9c49111434254434d696e74506f6c696379206631000013370e0280082a6644666ae68008004528192999ab9a00110011335738920111434254434d696e74506f6c6963792066320000133232323002233002002001230022330020020012253335573e002294054cc8cd5cd0008a51300335742002260046ae88004cc888c8c8c8c94ccd5cd19b87480080085280a9991199ab9a00200114a066e3cdd71aba100100613371201666644466664644446646460044660040040024600446600400400244a666aae7c004401454ccd5cd19baf35573a6ae840040184c010d55cf1aba100113002357440020026ea4004009200023333232222332323002233002002001230022330020020012253335573e002200a2a666ae68cdd79aab9d3574200200c260086aae78d5d0800898011aba2001001375200200490003ad37560020066eacd5d09aba200400748904634254430035573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e37540020026eb8d5d080a9bae357420026eb0d5d080c0992999ab9a00110011335738920111434254434d696e74506f6c696379206633000013370e02690001aab9e00235573a0026ea8c8d5d0991aab9e37540020026ae84d5d11aba2002375a6ae84004c8d55cf1baa00132323232320055333573466e1d20000021323232323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba2003375a0026ae84004d5d10019bad001357420022a66ae712413f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10013574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10011533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001357440066eb8004d5d08009aba2003375a0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37333a31352d3434001635573c0046aae74004dd51aba1357440026ae88004c8d55cf1baa001357426ae88c8d55cf1baa0010013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba20023574200246644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd70099aba135744646aae78dd50008009bac3574201c6aae78008d55ce8009baa00e1498594ccd5cd19b87480000084c92653335573e0022930a99ab9c4901317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50051999111999999111199991919180111980100100091801119801001000912999aab9f00112250011333003357420024446004006260046ae88004cc8894ccd5cd19baf002300300112230020031225001004002001232223002003300400112250015573aaae78dd48010009199911919180111980100100091801119801001000912999aab9f00110031330043574200260046ae8800488c94ccd5cd1803800899b800010021002375a6aae780092000375600229000119b8800148000dd71aba100237566ae84d5d11aba200533322233333322223333232323002233002002001230022330020020012253335573e002244a00226660066ae84004888c00800c4c008d5d1000999112999ab9a3375e00460060022446004006244a0020080040024644460040066008002244a002aae7555cf1ba900200123332232323002233002002001230022330020020012253335573e00220062660086ae84004c008d5d100091192999ab9a300700113370000200420046eb4d55cf001240006eac0045200023371090000009bae357420026eacd5d09aba2357440082a66ae712401445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a35333a332d3137001635573c0046aae74004dd51aba1357440066ae88d5d1000991aab9e37540026ae84004c8d55cf1baa0010014c011e581c61124f91e925a0a84cfb4040b2fb16c458663f6c2a3a4575b06e67fb0001"
        }
    } as DeployedScripts,
    unit: "ad1e2bc25aadc89922bd8c9e3c54ba790e1cb24470ac1ac93c917ddc4d756c746953696743657274"
}


export const update = async (lucid: Lucid) => {

	const configUpdate: ConfigUpdateMultiSig = {
		unit: deployments.unit,
		multiSigValidator: deployments.scripts.multiSigValidator,
		oldKeys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash, // Original signer
		],
		newConfig: {
			requiredCount: 2,
			keys: [
				lucid.utils.paymentCredentialOf(signers.account1.address).hash,
				lucid.utils.paymentCredentialOf(signers.account2.address).hash,
				lucid.utils.paymentCredentialOf(signers.account3.address).hash,
			],
		},
	};
	console.log(configUpdate)
	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	const updateTx = await multisig_update.build(lucid, configUpdate);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const witness1 = await multisig_update.signWitness(
		lucid,
		updateTx.toString()
	);

	// lucid.selectWalletFromPrivateKey(signers.account2.privateKey);
	// const witness2 = await multisig_update.signWitness(
	// 	lucid,
	// 	updateTx.toString()
	// );

	// lucid.selectWalletFromPrivateKey(signers.account3.privateKey);
	// const witness3 = await multisig_update.signWitness(
	// 	lucid,
	// 	updateTx.toString()
	// );

	const assembleTx = await multisig_update.assemble(
		lucid,
		updateTx.toString(),
		// [witness1,witness2, witness3]
		[witness1,]
	);

	console.log(assembleTx);
};


// Fullfill requests from users
export const fullfil = async (lucid: Lucid) => {

	const configSign: ConfigFullFill = {
		unit: deployments.unit,
		scripts: deployments.scripts,
		keys: [
			lucid.utils.paymentCredentialOf(signers.account1.address).hash,
			lucid.utils.paymentCredentialOf(signers.account2.address).hash,
			lucid.utils.paymentCredentialOf(signers.account3.address).hash,
		],
	};

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);

	// Get Valid Datums from Guardian Script
	const validDatumUtxoList = await utils.getValidDatums(lucid, deployments.scripts.guardianValidator);
	if (!validDatumUtxoList?.length) {
		console.log("No valid datums at Guardian Script");
		return null;
	}
	console.log("validDatumUtxoList: ", validDatumUtxoList);

	// Build transaction with Valid Datums and UTXOs
	// Guardian Minter, Guardian Script and Guardian Multisig are inlcuded
	const fulfillTx = await multisig_fullfill.build(
		lucid,
		validDatumUtxoList,
		configSign
	);

	lucid.selectWalletFromPrivateKey(signers.account1.privateKey);
	const witness1 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account2.privateKey);
	const witness2 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	lucid.selectWalletFromPrivateKey(signers.account3.privateKey);
	const witness3 = await multisig_fullfill.signWitness(
		lucid,
		fulfillTx.toString()
	);

	const assembleTx = await multisig_fullfill.assemble(
		lucid,
		fulfillTx.toString(),
		[witness1, witness2, witness3]
	);

	console.log(assembleTx);
};

export const request = async (lucid: Lucid) => {
	lucid.selectWalletFromSeed(user.account1.seedPhrase);

	// This Address has Staking Credential
	const myAddress = await lucid.wallet.address();
	const hardcodedAmount = 10;
	console.log(`Requesting ${hardcodedAmount} BTC to ${myAddress}`);
	const result = await user_request.submit(
		lucid,
		hardcodedAmount,
		myAddress,
		"",
		deployments.scripts.guardianValidator,
	);
	console.log(result);
};