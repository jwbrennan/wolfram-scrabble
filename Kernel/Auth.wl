BeginPackage["Scrabble`"]

GetGoogleAccessToken::usage = "Get a short-lived OAuth2 access token from a Google service account.";
ToFirestoreValue::usage = "Convert a value to Firestore REST field format.";
ToFirestoreFields::usage = "Convert plain Association to Firestore REST fields format.";
WriteToFirestore::usage = "Write data to a Firestore database using a privileged access token obtained from a Google service account.";
AppendLeftoverToFirestore::usage = "Appends leftover tiles information to an existing Firestore document.";
AppendFoundByToFirestore::usage = "Appends username that found the game to an existing Firestore document.";

Begin["`Private`"]

(* Get short-lived OAuth2 access token from service account *)
GetGoogleAccessToken[sa_Association] := 
Module[
	{now, header, payload, signingInput, privateKey, jwt, body, resp},
	now = UnixTime[];
	header = ExportString[{"alg" -> "RS256", "typ" -> "JWT"}, "JSON", "Compact" -> True];
	payload = 
	ExportString[
		<|
			"iss" -> sa["client_email"],
			"scope" -> "https://www.googleapis.com/auth/datastore",
			"aud" -> "https://oauth2.googleapis.com/token",
			"exp" -> now + 3600,
			"iat" -> now
		|>,
		"JSON", 
		"Compact" -> True
	];
	signingInput = 
	StringJoin[
		BaseEncode[
			ByteArray[ToCharacterCode[header, "UTF8"]]
		], ".", 
		BaseEncode[
			ByteArray[ToCharacterCode[payload, "UTF8"]]
		]
	];
	privateKey =
	First[
		ImportString[
			sa["private_key"], 
			"PEM"
		]
	];
	signature = 
	BaseEncode[
		GenerateDigitalSignature[
    		StringToByteArray[signingInput], 
    		privateKey
		][[1, "Signature"]]
	];
	jwt = StringJoin[signingInput, ".", signature];
	body = 
	URLQueryEncode[
		<|
		"grant_type" -> "urn:ietf:params:oauth:grant-type:jwt-bearer",
		"assertion" -> jwt
		|>
	];
	resp = 
	URLExecute[
		HTTPRequest[
			"https://oauth2.googleapis.com/token",
			<|
				"Method" -> "POST",
				"Headers" -> <|"Content-Type" -> "application/x-www-form-urlencoded"|>,
				"Body" -> body
			|>
  		],
  		"RawJSON"
	];
	resp["access_token"]
];

(* Convert plain Association to Firestore REST fields format *)
ToFirestoreValue[v_] := 
Which[
	StringQ[v], <|"stringValue" -> v|>,
	IntegerQ[v], <|"integerValue" -> ToString[v]|>,
	NumberQ[v], <|"doubleValue" -> ToString[v]|>,
	BooleanQ[v], <|"booleanValue" -> v|>,
	v === Null, <|"nullValue" -> Null|>,
	ListQ[v], <|"arrayValue" -> <|"values" -> Map[ToFirestoreValue, v]|>|>,
	AssociationQ[v], <|"mapValue" -> <|"fields" -> Association[KeyValueMap[#1 -> ToFirestoreValue[#2] &, v]]|>|>,
	DateObjectQ[v], <|"timestampValue" -> DateString[v, {"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "Second", "Z"}]|>,
	True, <|"stringValue" -> ToString[v]|>
];
ToFirestoreFields[data_Association] := 
<|
	"fields" -> Association[KeyValueMap[#1 -> ToFirestoreValue[#2] &, data]]
|>;

WriteToFirestore[___] := $Failed;
WriteToFirestore[gameData_Association /; PerfectScrabbleGameQ[gameData], foundBy_String] := 
Module[
	{accessToken, firestoreBody, url, response, resultAssoc},   
	(* Get privileged access token *)
	accessToken = GetGoogleAccessToken[CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]];
	leftover = FindLeftoverTiles[gameData];
	(* Build Firestore document body *)
	firestoreBody = 
	ExportString[
		ToFirestoreFields[
			Append[
				gameData, 
				{
					"timestamp" -> Now,
					"foundBy" -> foundBy,
					"leftover" -> leftover
				}
			]
		],
		"RawJSON"
	];
	(* Call Firestore REST API to ADD a new document *)
	url = 
	URLBuild[
		{
			"https://firestore.googleapis.com/v1/projects/",
			CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]["project_id"],
			"databases",
			"(default)",
			"documents",
			"perfect-scrabble-games"
		}
	];
	response = 
	URLExecute[
		HTTPRequest[
			url,
			<|
				"Method" -> "POST",
				"Headers" -> 
				<|
					"Authorization" -> "Bearer " <> accessToken,
					"Content-Type" -> "application/json"
				|>,
				"Body" -> firestoreBody
			|>
		],
		"RawJSON"
	];
	resultAssoc =
	If[
		KeyExistsQ[response, "error"],
		<|"success" -> False, "error" -> response["error"]|>,
		<|"success" -> True, "docId" -> Last[StringSplit[response["name"], "/"]]|>
	];
	HTTPResponse[
		ExportString[resultAssoc, "JSON"],
		<|"ContentType" -> "application/json"|>
	]
];

AppendLeftoverToFirestore[docId_String, leftover_Association] := 
Module[
	{accessToken, firestoreBody, url, response, resultAssoc},   
	(* Get privileged access token *)
	accessToken = GetGoogleAccessToken[CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]];
	
	(* Build Firestore body containing ONLY the 'leftover' field *)
	firestoreBody = 
	ExportString[
		ToFirestoreFields[leftover],
		"RawJSON"
	];
	(* Call Firestore REST API to PATCH / update the existing document *)
	url = 
	URLBuild[
		{
			"https://firestore.googleapis.com/v1/projects/",
			CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]["project_id"],
			"databases",
			"(default)",
			"documents",
			"perfect-scrabble-games",
			docId
		},
		{"updateMask.fieldPaths" -> "leftover"}
	];
	
	response = 
	URLExecute[
		HTTPRequest[
			url,
			<|
				"Method" -> "PATCH",
				"Headers" -> 
				<|
					"Authorization" -> "Bearer " <> accessToken,
					"Content-Type" -> "application/json"
				|>,
				"Body" -> firestoreBody
			|>
		],
		"RawJSON"
	];
	
	resultAssoc =
	If[
		KeyExistsQ[response, "error"],
		<|"success" -> False, "error" -> response["error"]|>,
		<|"success" -> True, "docId" -> docId|>
	];
	
	HTTPResponse[
		ExportString[resultAssoc, "JSON"],
		<|"ContentType" -> "application/json"|>
	]
];

AppendFoundByToFirestore[docId_String, foundBy_String] := 
Module[
	{accessToken, firestoreBody, url, response, resultAssoc},   
	(* Get privileged access token *)
	accessToken = GetGoogleAccessToken[CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]];
	
	(* Build Firestore body containing ONLY the 'foundBy' field *)
	firestoreBody = 
	ExportString[
		ToFirestoreFields[<|"foundBy" -> foundBy|>],
		"RawJSON"
	];
	(* Call Firestore REST API to PATCH / update the existing document *)
	url = 
	URLBuild[
		{
			"https://firestore.googleapis.com/v1/projects/",
			CloudSymbol["FIREBASE_SERVICE_ACCOUNT"]["project_id"],
			"databases",
			"(default)",
			"documents",
			"perfect-scrabble-games",
			docId
		},
		{"updateMask.fieldPaths" -> "foundBy"}
	];
	
	response = 
	URLExecute[
		HTTPRequest[
			url,
			<|
				"Method" -> "PATCH",
				"Headers" -> 
				<|
					"Authorization" -> "Bearer " <> accessToken,
					"Content-Type" -> "application/json"
				|>,
				"Body" -> firestoreBody
			|>
		],
		"RawJSON"
	];
	
	resultAssoc =
	If[
		KeyExistsQ[response, "error"],
		<|"success" -> False, "error" -> response["error"]|>,
		<|"success" -> True, "docId" -> docId|>
	];
	
	HTTPResponse[
		ExportString[resultAssoc, "JSON"],
		<|"ContentType" -> "application/json"|>
	]
];



End[]

EndPackage[]