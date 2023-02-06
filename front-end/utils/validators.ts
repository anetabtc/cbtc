import { MintingPolicy, SpendingValidator } from "lucid-cardano";

export const guardianValidator: SpendingValidator = {
    type: "PlutusV2",
    script: "59023f59023c010000323232323232323232323232322222323232323253330113370e9001001099191919191919191919299980d99b8748000008584c8c94ccc0754cc88ccc07c008004528180b1980b980d0011bac301f00a133710900019991119999119999181091299981200089128008999801981380091118010018980118140009299981199baf003302400112230020031225001001232223002003302800112250013752004002466644604244a6660480022006266008604e002600460500024464a666048600e002266e000040084008dd69814001240006eac0045200023371090000009bae301f30200113756603e604060426460406042604400264a66603a0022c2a66603c602e6042002260400022c6602e4660360046040604260440026eb0c07c02c52616375c603c0206eb8c074004c07c008c068004dd519180d980e800980d000980d80099911980b11299980c8008b099299980d9919baf374e604000a6e9cc080004c0740044c074c0780044c00cc078008c078c070004008dd6180c00200299180c980c980c800980c000980b980b800980b180b000980b180a00298098008b180a80118080009baa301030110013011002301000457464600a44a6660100022008264a666014600800226600c0026006601a00426006601a004601600297ae0574044646464a66601066e1d200000214a0266e3cdd71805000802980600118038009baa323008300a001300730090012323002233002002001230022330020020015573eae6955ceaba15744460046ea800555cf01",
};

export const cBTCMintingPolicy : MintingPolicy = {
    type: "PlutusV2",
    script: "590529590526010000323232323232323232323232323232323222233322232323232323253330183370e900000109919191919299980e99b87480000084c8c8c8c8c8c94ccc08ccdc3a400800426464646464a66605066e1d20000021533302853301f3370e0200082a6603e664604e44a666054002294054cc8cc0b0004528980198170008980118168009991119191919299981799b87480080085854cc098cdc79bae3032001006133712016666444666604e00490001199981400124000eb4dd58008019bab3032303100400748810463425443003033002302e00137546460606062002605e002605e0026eb8c0ac044dd718158009bac302b01413370e01e90000a4c2c2c6058004604e0026ea8c8c0a4c0a8004c0a0c09cc09c008dd6981380098139919191919002a99981399b87480000084c8c8c8c8c8c8c8c8c8c8c8c8c9265333032001149858c0d00194ccc0c4cdc3a400000426464646464646464646464646493299981e0008a4c2c607c00ca66607666e1d20000021323232323232324994ccc100004526163042006533303f3370e900000109919191919191924ca6660880022930b1823003299982199b87480000084c8c94ccc114cdc39b8d001480e04c8c9265333045001149858c11c00c58dd700098230008a99982199b87480080084c8c94ccc114cdc39b8d001480e04c8c9265333045001149858c11c00c58dd700098230008b182380118210009baa00130420011533303f3370e900100109919191919191919191924ca66608e0022930b18248019bad00130480013046003375a002608a00260860066eb4004c10800458c10c008c0f8004dd5000981f0008a99981d99b87480080084c926533303a00114985858c0fc008c0e8004dd5000981d000981c003299981a99b87480000084c8c94ccc0dccdc39b8d001480e04c8c9265333037001149858c0e400c58dd7000981c0008a99981a99b87480080084c8c94ccc0dccdc39b8d001480e04c8c9265333037001149858c0e400c58dd7000981c0008b181c801181a0009baa0013034001163035002303000137540026060002605c0066eb8004c0b4004c0ac00cdd680098150008b181580118130009baa001302600116302700230220013754604660440026042002604460426040604400264a66603a0022c2a66603c6ae8cc0800044c08400458cc8c070894ccc07c00452f5c0264a6660426008002266ae80004c00cc08c0084c00cc08c008c08c0048c8c8c8c94ccc088cdc3a400000429404cdc79bae30250010133026002302100137546460466048002604460460026042604060440026eb0c08002854ccc0754cc050cdc3802a4000266e212000004149858c084008c070004dd5005199809119b8800148000dd7180e0011bab301c301b301b00533301123371090000009bae301b00137566036603460340082c6038004602e0026ea8c060c05c00cc058c058004c05cc058004c058004dd7001801000918011ba900122223300c22533300f0011005153330103375e6022602600200c26008602a6026002260046024002002446660160040020062940888cccccc01002c03cdd48010009199804111929998071803800899b800010021002375a602400490001bab0011480008888cccc014cc0180100080048c888c00800cc01000448940048c010894ccc01c00448940044ccc00cc02c004888c00800c4c008c0280048894ccc01ccdd78011801800891180100189128009118019129998030008801899802180500098011804800919180111980100100091801119801001000aab9f5734aae755d12ba1230023754002aae781"
}

export const multisigMintingPolicy : MintingPolicy = {
   type: "PlutusV2",
   script : "5903635903600100003232323232323232323232323232323232323222223333222232323232533301c3370e900000109919191919191919299981219b87480100084c8c8c8c94ccc0a0cdc3a40040042a6603266e1c029200013370e01290010a9980c99b8700a4800854cc064cdc3804a40002a66032a6603266ebcc06cdd618128021ba94893839623962303836656665316530393865363063356138376362363731373133326331333632626563336566633532303664323035303631300013370e6eb4c094c098011200213322332302722533302e00114a02a6646605c0022944c00cc0a80044c008c0ac0048c8cdd79ba7302b003374e6056002605060540020046eb0c09403004cc0a4008c0a4004dd500798119919191919002a99981419b87480000084c8c8c8c8c8c8c926533302f001149858c0ac00cdd680098140009814001999181311299981680088108998111801981480098011815000919299981599b87371a002901c08010b1bae0013758002604a0022c605200460520026ea8004c08400458c094008c094004dd5180f180f800980f180f9809999180e912999812000880c0992999812180200089980d0009801981100109801981100118100009191919299981219b8748000008528099b8f375c6042002020604a004604a0026ea8c8c07cc084004c078c080004dd6180e80199980b119b8800148000dd7180e0021bab301c301d301d00233301523371090000009bae301b003375660366038603800260366036002603660320082c603a004603a0026ea8c058c05c004c05c004dd700200199191919003299980b99b87480000084c926533301800114985854ccc05ccdc3a400400426493299980c0008a4c2c2c603000460300026ea800800488ccc04400800400c528129998078008b0a99980718011806000898058008b2ba34bd702ba022233333300400f00e375200400246660104464a666020600e002266e000040084008dd69808801240006eac004520002222333300533006004002001232223002003300400112250012300422533300b001122500113330033007001222300200313002300800122253330093375e00460060022446004006244a00244600644a6660140022006266008600c0026004600e002464600446600400400246004466004004002ae855d1118021baa0015734aae7d55cf2ab9d1"
}

export const multisigValidator : SpendingValidator = {
    type: "PlutusV2",
    script: "5902fb5902f8010000323232323232323232323232323232322223232323253330113370e900100109919191919191919299980c99b8748000008584c8c8c8c8c94ccc078cdc3a400800426464a666040a6602e66644466e24008c074cc0788cc074004010004dd618118009bad302330240013758604664604a604a604a604a604a002604801a2a6602e66ebcc08cc09002cc08c0184c8c8c94ccc08ccdc3a400400426604803200a26464a6603866e212000001153301c3371090001bad30283029002153301c337126eb4c0a0c0a40080044c8ccc0908894ccc0a00085288a9998149981118160010008a50133003302d00233026302c00200100102237586050004603c6eb0c09c004c0a0014c0a0008c08c004dd500a8a4c2c604802a646464646400aa66604466e1d20000021323232323232324994ccc09c00452616302b003375a00260500026050006664604444a66604a00220442660466006605200260046054002464a66604a66e1cdc6800a407020042c6eb8004dd600098128008b181380118110009baa0013021001163023002301e0013754603c603e002603c603e64a6660340022c2a6660366ae8cc07c0044c07800458cc0548c8c8c94ccc078cdc3a400000429404cdc79bae30210010053023002301e001375464603e6042002603c60400026eb0c07401cdd7180e000980f001180c8009baa32301a301c0013019001301a0013322330142253330170011613253330193301a004301c0011301c301d00113003301d002301d301b0010023758602e004006602e602e002602e602a00a60280022c602c00460220026ea8c044c048004c04800488ccc02c00800400c528118031129998048008a501533300a3375e601a00200629444c008c038004cc0108894ccc02000440084cc00ccdc000124004601a002900011802112999803800880209929998049802000899803000980198068010980198068011805800a5eb815d0119180111980100100091801119801001000aab9f57344466ebcdd398030011ba730060015573aae855d1118011baa0015573c1"
}