//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Media.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TButton *Button1;
	TButton *Button3;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TListBox *ListBox1;
	TButton *Button2;
	TEdit *Edit1;
	TProgressBar *ProgressBar1;
	TTabItem *TabItem2;
	TMediaPlayerControl *MediaPlayerControl1;
	TListBox *ListBox2;
	TMediaPlayer *MediaPlayer1;
	TIdHTTP *IdHTTP1;
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	void ScanText(String AText);
	String GetTagValue(String& AText);
	System::Boolean IsItemCorrect(String AItem);
	void AddItem(String AItem);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
