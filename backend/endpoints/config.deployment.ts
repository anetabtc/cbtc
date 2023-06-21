import { Deployments } from "./types"

// Preprod Deployment
export const deployments: Deployments = {
  txHash: "6dacfa11d4211e29b4d2c2b43142bae316b1efc57986b41222dabe64ffb065c8",
  scripts: {
    multiSigValidator: {
      type: "PlutusV2",
      script:
        "590abf590abc010000222323232325333573466e1d2002002132323232323232325333573466e1d2002002132323232325333573466e1d200400213232323232323232323253335734a6644666ae68008004528192999ab9a00110011335738921144d756c746953696756616c696461746f72206631000013332223371200464a666aae7c004520001332332322253335573e002200426600666e0000920023574400246600400400246444a666aae7c00440084cc00ccdc0001240046ae880048cc008008004004cdc02400090011aba200133232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba2002357420024664646460044660040040024600446600400400244a666aae7c0045280a999ab9a3375e6ae8400400c528898011aba200100100400137586ae84014dd69aba13574400a6eb0d5d0991aba2357446ae88d5d11aba20013574402a2a6644666ae68008004528192999ab9a00110011335738921144d756c746953696756616c696461746f7220663200001332233232323002233002002001230022330020020012253335573e002294454cc88ccd5cd0010008a50300335742002260046ae880048cc8c8c8c0088cc0080080048c0088cc008008004894ccd55cf8008a5115332233357340040022940c00cd5d0800898011aba20012337126eb4d55cf0009991199991911119919180111980100100091801119801001000912999aab9f001100515333573466ebcd55ce9aba10010061300435573c6ae840044c008d5d10008009ba9001002480008cccc8c8888cc8c8c0088cc0080080048c0088cc008008004894ccd55cf80088028a999ab9a3375e6aae74d5d0800803098021aab9e35742002260046ae88004004dd480080124000eb4dd58008031bae35573a0046eb8d55ce8009bab35573c0020026eacd5d08071bab357426ae8804c4c8c8c94ccd5cd19b87480000084c8c94cc88ccd5cd0010008a5032533357340022002266ae71241144d756c746953696756616c696461746f72206633000013371090000008a9991199ab9a00200114a064a666ae6800440044cd5ce249144d756c746953696756616c696461746f72206634000013371090001bad357426ae8800854cc88ccd5cd0010008a5032533357340022002266ae71241144d756c746953696756616c696461746f7220663500001337126eb4d5d09aba2002001132533357340022002266ae712401144d756c746953696756616c696461746f72206636000013233323230022330020020012300223300200200122253335573e004294454ccd5cd1991919180111980100100091801119801001000912999aab9f00114a02a666ae68cdd79aba100100314a2260046ae88004d5d08010008a501330033574400466ae80d5d0801000800a5eb80dd61aba10023253335573e00229000099919919112999aab9f00110021330033370000490011aba20012330020020012322253335573e002200426600666e0000920023574400246600400400200266e01200048008d5d10009bac35742002646aae78dd50008068992999ab9a00110011335738921144d756c746953696756616c696461746f722066370000133223375e6e9cc8d55cf1baa001002374e646aae78dd50008008108069aab9e00235573a0026ea8074526165333573466e1d20000021324994ccd55cf8008a4c2a66ae71241317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd500c991aab9e3754002032a666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd500a9919191919002a999ab9a3370e900000109919191919191924ca666aae7c004526153357389201317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a38323a332d3235001635573c0046aae74004dd51aba1357440026ae88c8d55cf1baa0013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba2002357420024646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd61aba1007375c6ae8400454cd5ce249475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a37373a332d3434001635573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e375400200266446646460044660040040024600446600400400244a666aae7c004584c94ccd5cd199119baf374e646aae78dd50008011ba73235573c6ea8004004010d5d080089aba135744002260066ae88008c8d55cf1baa001357420020046eb0d5d08010019aba235744002646aae78dd50009aba1005357420022a66ae712401475061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c746953696756616c696461746f722e68733a37303a332d3139001635573c0046aae74004dd51aba135744002646aae78dd50008009",
    },
    multiSigMintingPolicy: {
      type: "PlutusV2",
      script:
        "5906bc5906b901000033322222323232325333573466e1d2000002132323232323232325333573466e1d200400213253335734a6644666ae68008004528192999ab9a00110011335738921154d756c74695369674d696e74506f6c696379206631000013370e00e90010a9991199ab9a00200114a064a666ae6800440044cd5ce249154d756c74695369674d696e74506f6c696379206632000013370e00c90000a9991199ab9a00200114a064a666ae6800440044cd5ce249154d756c74695369674d696e74506f6c696379206633000015332233357340040022940cdd7992999aab9f001161533357346ae8cd5d100089aba100115335738921244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001637586ae840040484cdc39bad357426ae880052002132533357340022002266ae712401154d756c74695369674d696e74506f6c69637920663400001332233232323002233002002001230022330020020012253335573e002294054cc8cd5cd0008a51300335742002260046ae880048c8cdd79ba73235573c6ea800400cdd3991aab9e37540020026ae84c8d55cf1baa00100100237586ae84024040526163235573c6ea8004c8c8c8c8c80154ccd5cd19b87480000084c8c8c8c8c8c8c92653335573e0022930a99ab9c4901317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba200333232323002233002002001230022330020020012253335573e002297ae0133574060066ae84004c008d5d10009192999ab9a3370e6e3400520381002153357389212c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026eb0004d5d08008a99ab9c4913f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c74695369674d696e74506f6c6963792e68733a35353a352d3331001635573c0046aae74004dd51aba1357440026ae88c8d55cf1baa0013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba200235742002644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd70061bac35742006666444666666444466664646460044660040040024600446600400400244a666aae7c00448940044ccc00cd5d08009111801001898011aba2001332225333573466ebc008c00c004488c00800c48940040100080048c888c00800cc010004489400555ceaab9e3752004002466644646460044660040040024600446600400400244a666aae7c004400c4cc010d5d080098011aba2001223253335734600e002266e000040084008dd69aab9e00248000dd58008a4000466e200052000375c6ae84010dd59aba1357446ae88008ccc888cccccc8888cccc8c8c8c0088cc0080080048c0088cc008008004894ccd55cf800891280089998019aba10012223002003130023574400266444a666ae68cdd780118018008911801001891280080200100091911180100198020008912800aab9d5573c6ea40080048ccc88c8c8c0088cc0080080048c0088cc008008004894ccd55cf80088018998021aba10013002357440024464a666ae68c01c0044cdc000080108011bad35573c00490001bab0011480008cdc4240000026eb8d5d08019bab357426ae88d5d10009aba235744002646aae78dd50009aba100415335738921485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f4d756c74695369674d696e74506f6c6963792e68733a34373a352d3436001635573c0046aae74004dd51aba135744002646aae78dd5000800a611e581cac7e04a29ac15fbce3a443e46c30462326a0a975bc570f200061b50c004c011e581cb8cf6213f0f6e79854ce2fd2a375ff6ef499c0d6a1b7b036a2aefd71004c012bd8799fd8799f5820f692e1e97e198d881774b294019f186375d428055e06a62d63a7d8d76538a724ff01ff0001",
    },
    guardianValidator: {
      type: "PlutusV2",
      script:
        "59046b5904680100003322222323232325333573466e1d20020021323232323232323232325333573466e1d20020021323253335734a6644666ae68008004528192999ab9a00110011335738920114477561726469616e56616c696461746f722066310000135746664646460044660040040024600446600400400244a666aae7c00452f5c0264a666ae68c0100044cd5d000098019aba200213003357440046ae84004c88c8c8c94ccd5cd19b8748000008528099b8f375c6ae84004014d55cf0011aab9d0013754646ae84c8d55cf1baa00100135742646aae78dd50008008011bac35742014264a666ae6800440044cd5ce24914477561726469616e56616c696461746f7220663200001337109000199911199991199991919180111980100100091801119801001000912999aab9f00112250011333003357420024446004006260046ae8800494ccd5cd19baf00335573a0022446004006244a0020024644460040066aae780044894004dd48010009199911919180111980100100091801119801001000912999aab9f00110031330043574200260046ae8800488c94ccd5cd1803800899b800010021002375a6aae780092000375600229000119b8848000004dd700a1bab357426ae88c8d55cf1baa00132357426ae88c8d55cf1baa0010013253335573e0022c2a666ae68d5d19aba20011357420022a66ae712401244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba20023574200246644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004008d5d09aba23235573c6ea8004004dd61aba100b149858dd700a1bae357420022a66ae712401485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f477561726469616e56616c696461746f722e68733a3132383a352d3436001635573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e375400200266446646460044660040040024600446600400400244a666aae7c004584c94ccd5cd1919baf374e646aae78dd50008029ba73235573c6ea8004004d5d080089aba135744002260066ae88008c8d55cf1baa001357420020046eb0d5d0802002991aba2357446ae88004d5d10009aba2357440026ae88d5d1000991aab9e37540026ae84014d5d08008a99ab9c4901485061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f477561726469616e56616c696461746f722e68733a3132313a352d3231001635573c0046aae74004dd51aba135744002646aae78dd5000800a611e581cb8cf6213f0f6e79854ce2fd2a375ff6ef499c0d6a1b7b036a2aefd71004c011e581c5af422ac99e3e6294f1de4e55dc0884dc0948659f77e1e09f5a5c25e0001",
    },
    cBTCMintingPolicy: {
      type: "PlutusV2",
      script:
        "5910d65910d301000032223232323232325333573466e1d2000002132323232323253335734646464a666ae68cdc3a40000042646464646464a666ae68cdc3a400800426464646464a666ae68cdc3a40040042a66ae71241455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37373a31352d36340016153357386466e592410b505075624b657948617368003372c9210120003372c9210230780033232300223300200200123002233002002001223322325333573466e1c005200010031323233004002001333718900119b8100248008014cdc7002240006e3400d24100223372c646466e58c94ccd5cd19b8700148000524101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e1490101660016002325333573466e1c00520001490101300015333573466e1c00520021490101310015333573466e1c00520041490101320015333573466e1c00520061490101330015333573466e1c00520081490101340015333573466e1c005200a1490101350015333573466e1c005200c1490101360015333573466e1c005200e1490101370015333573466e1c00520101490101380015333573466e1c00520121490101390015333573466e1c00520141490101610015333573466e1c00520161490101620015333573466e1c00520181490101630015333573466e1c005201a1490101640015333573466e1c005201c1490101650015333573466e1c005201e14901016600160013370a006901019b8400248080c010004004dd71aba100115332233357340040022940c94ccd5cd0008800899ab9c49111434254434d696e74506f6c696379206631000013370e0280082a6644666ae68008004528192999ab9a00110011335738920111434254434d696e74506f6c6963792066320000133232323002233002002001230022330020020012253335573e002294054cc8cd5cd0008a51300335742002260046ae88004cc888c8c8c8c94ccd5cd19b87480080085280a9991199ab9a00200114a066e3cdd71aba100100613371201666644466664644446646460044660040040024600446600400400244a666aae7c004401454ccd5cd19baf35573a6ae840040184c010d55cf1aba100113002357440020026ea4004009200023333232222332323002233002002001230022330020020012253335573e002200a2a666ae68cdd79aab9d3574200200c260086aae78d5d0800898011aba2001001375200200490003ad37560020066eacd5d09aba200400748904634254430035573c0046aae74004dd5191aba13235573c6ea8004004d5d0800991aab9e37540020026eb8d5d080a9bae357420026eb0d5d080c0992999ab9a00110011335738920111434254434d696e74506f6c696379206633000013370e02690001aab9e00235573a0026ea8c8d5d0991aab9e37540020026ae84d5d11aba2002375a6ae84004c8d55cf1baa00132323232320055333573466e1d20000021323232323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a4000004264646464646464932999aab9f001149854cd5ce2481317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00163574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021323232323232323232324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440066eb4004d5d08009aba2003375a0026ae84004d5d10019bad001357420022a66ae712413f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba100115333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10013574400ca666ae68cdc3a400000426464a666ae68cdc39b8d001480e04c8c92653335573e0022930a99ab9c491317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454ccd5cd19b87480080084c8c94ccd5cd19b87371a002901c0991924ca666aae7c00452615335738921317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0016357440062a66ae7124012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e670016375c0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba10011533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001357440066eb8004d5d08009aba2003375a0026ae8400454cd5ce24813f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50009aba1001153357389201455061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a37333a31352d3434001635573c0046aae74004dd51aba1357440026ae88004c8d55cf1baa001357426ae88c8d55cf1baa0010013253335573e0022c2a666ae68d5d19aba20011357420022a66ae71241244c69737420636f6e7461696e73206d6f7265207468616e206f6e6520656c656d656e742e001633232323002233002002001230022330020020012253335573e002297ae0132533357346008002266ae80004c00cd5d1001098019aba20023574200246644646464a666ae68cdc3a400000429404cdc79bae3574200200a6aae78008d55ce8009baa3235742646aae78dd50008009aba13235573c6ea8004004dd70099aba135744646aae78dd50008009bac3574201c2a6644666ae68008004528192999ab9a00110011335738920111434254434d696e74506f6c696379206634000013370e01290000992999ab9a00110011335738920111434254434d696e74506f6c696379206635000013371001090001aab9e00235573a0026ea8038526165333573466e1d20000021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f001615333573466e1d20020021324994ccd55cf8008a4c2a66ae712401317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f00161533573892013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72001635573c0046aae74004dd50051999111999999111199991919180111980100100091801119801001000912999aab9f00112250011333003357420024446004006260046ae88004cc8894ccd5cd19baf002300300112230020031225001004002001232223002003300400112250015573aaae78dd48010009199911919180111980100100091801119801001000912999aab9f00110031330043574200260046ae8800488c94ccd5cd1803800899b800010021002375a6aae780092000375600229000119b8800148000dd71aba100237566ae84d5d11aba200533322233333322223333232323002233002002001230022330020020012253335573e002244a00226660066ae84004888c00800c4c008d5d1000999112999ab9a3375e00460060022446004006244a0020080040024644460040066008002244a002aae7555cf1ba900200123332232323002233002002001230022330020020012253335573e00220062660086ae84004c008d5d100091192999ab9a300700113370000200420046eb4d55cf001240006eac0045200023371090000009bae357420026eacd5d09aba2357440082a66ae712401445061747465726e206d61746368206661696c75726520696e2027646f2720626c6f636b206174207372632f434254434d696e74506f6c6963792e68733a35343a332d3137001635573c0046aae74004dd51aba1357440066ae88d5d1000991aab9e37540026ae84004c8d55cf1baa0010014c011e581c6212928d65668d6d5bb008cdbca6080dd82cd8a8de822cd9d46c5a560001",
    },
  },
  units: {
    multiSigCert:
      "5af422ac99e3e6294f1de4e55dc0884dc0948659f77e1e09f5a5c25e4d756c746953696743657274",
    cBTC: "2c04fa26b36a376440b0615a7cdf1a0c2df061df89c8c055e265050563425443",
  },
}
