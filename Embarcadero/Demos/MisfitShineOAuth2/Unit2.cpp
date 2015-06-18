//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop
#include <string>
#include <System.IOUtils.hpp>


#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
	UserPanel->Visible			= false;
	WebBrowser1->EnableCaching 	= false;


	IniFilePath = System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetDocumentsPath(), "misfit.ini");

	if (System::Ioutils::TFile::Exists(IniFilePath)) {
		Ini 			= new TIniFile(IniFilePath);
		AccessToken		= Ini->ReadString( "Misfit", "accessToken", "" );

		if ( AccessToken == "" ) {
			Memo1->Lines->Add( "No Access Token Found." );
		}
		else {
			Memo1->Lines->Add( "AccessToken: " + AccessToken );

			//- do a test API call to see if access token in INI is valid
			ProfileRESTRequest->Params->Items[0]->Value = AccessToken;
			ProfileRESTRequest->Execute();

			ProfileRESTResponse->RootElement	= "userId";
			TJSONObject * json = static_cast<TJSONObject *> ( ProfileRESTResponse->JSONValue );

			if ( !json ) {
				//- we have a bad accesstoken
				Memo1->Lines->Add( "Bad AccessToken found in ini" );

				//- remove bad accesstoken from ini
				Ini->DeleteKey( "Misfit", "accessToken" );
				Ini->UpdateFile();

			} else {
				//- we have a good token, hide the browser
				//		dont need to authorize for a new access token
				Memo1->Lines->Add( "We have a good accesstoken!" );
				WebBrowser1->Visible 	= false;
				WebBrowser1->URL 		= "";

	            GetUserDetails();
			}
		}
	}
	else {
		Memo1->Lines->Add( "No INI File Found!" );
    }
}

void __fastcall TForm2::GetUserDetails() {
	WebBrowser1->Visible	= false;
	UserPanel->Visible 		= true;
	UserPanel->Enabled		= true;

	//- get user info
	ProfileRESTRequest->Params->Items[0]->Value = AccessToken;
	ProfileRESTRequest->Execute();

	ProfileRESTResponse->RootElement	= "userId";
	TJSONObject * json 		= static_cast<TJSONObject *> ( ProfileRESTResponse->JSONValue );
	UnicodeString userID	= json->Value();
	ActiveUserIDLabel->Text	= userID;

	ProfileRESTResponse->RootElement	= "email";
	json 					= static_cast<TJSONObject *> ( ProfileRESTResponse->JSONValue );
	UnicodeString email		= json->Value();
	ActiveEmailLabel->Text	= email;

	//- get steps
	ActivityRESTRequest->Params->Items[0]->Value = AccessToken;
	ActivityRESTRequest->Execute();
	ActivityRESTResponse->RootElement	= "steps";

	json 					= static_cast<TJSONObject *> ( ActivityRESTResponse->JSONValue );
	UnicodeString steps		= json->Value();
	ActiveStepsLabel->Text	= steps;


}

//---------------------------------------------------------------------------
void __fastcall TForm2::WebBrowser1DidFinishLoad(TObject *ASender)
{
	//- Dont need the browser once the user panel is shown
	if ( UserPanel->Visible ) return;

	//- check for MISFIT supplied auth code which will be exchanged for
	//		access token.
	UnicodeString url 	= WebBrowser1->URL;
	int codePos			= url.Pos( "?code=" );

	if ( codePos >= 0 ) {
		//- address the URL vriable code
		//	   16 characters wide.
		UnicodeString code	= url.SubString(codePos+6,16);

		//- exchange code for access token
		TokenRESTRequest->Params->Items[1]->Value = code;
		TokenRESTRequest->Execute();

		//- extract returned access token
		TJSONObject * json 			= static_cast<TJSONObject *> ( TokenRESTResponse->JSONValue );
		TokenRESTResponse->RootElement	= "error";
		json 						= static_cast<TJSONObject *> ( TokenRESTResponse->JSONValue );

		if (json) {
			//- MISFIT rejected request for access_token
			Memo1->Lines->Add( "error: " +  json->Value() );
		} else {
			//- MISFIT has provided the access_token in exchange for the user auth code
			TokenRESTResponse->RootElement	= "access_token";
			json 						= static_cast<TJSONObject *> ( TokenRESTResponse->JSONValue );
			AccessToken 				= json->Value();

			//- add the access token to the ini
			Ini->WriteString( "Misfit", "accessToken", AccessToken );
			Ini->UpdateFile();

            GetUserDetails();
		}
	}
}
//---------------------------------------------------------------------------



