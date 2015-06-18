//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Types.hpp>
#include <FMX.WebBrowser.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <REST.Response.Adapter.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TMemo *Memo1;
	TRESTClient *TokenRESTClient;
	TRESTRequest *TokenRESTRequest;
	TRESTResponse *TokenRESTResponse;
	TRESTClient *ProfileRESTClient;
	TRESTRequest *ProfileRESTRequest;
	TRESTResponse *ProfileRESTResponse;
	TWebBrowser *WebBrowser1;
	TPanel *Container;
	TPanel *UserPanel;
	TPanel *UserIDPanel;
	TLabel *UserIDLabel;
	TLabel *ActiveUserIDLabel;
	TPanel *StepsPanel;
	TLabel *ActiveStepsLabel;
	TLabel *StepsLabel;
	TPanel *EmailPanel;
	TLabel *EmailLabel;
	TRESTClient *ActivityRESTClient;
	TRESTRequest *ActivityRESTRequest;
	TRESTResponse *ActivityRESTResponse;
	TLabel *ActiveEmailLabel;
	TToolBar *ToolBar1;
	TLabel *Label1;
	void __fastcall WebBrowser1DidFinishLoad(TObject *ASender);
private:	// User declarations
	String IniFilePath;
	String AccessToken;
	TIniFile * Ini;
	__fastcall void GetUserDetails();

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
