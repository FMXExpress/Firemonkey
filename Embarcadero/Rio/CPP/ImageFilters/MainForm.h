//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TImage *Image2;
	TButton *OpenButton;
	TOpenDialog *OpenDialog1;
	TSaveDialog *SaveDialog1;
	TTrackBar *TrackBar1;
	TLabel *Label1;
	TButton *SaveButton;
	void __fastcall OpenButtonClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall SaveButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    TFilterSepia * MySepiaFilter;
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
