//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ConnectionFrameUH
#define ConnectionFrameUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.EMSProvider.hpp>
#include <REST.Backend.EMSApi.hpp>
//---------------------------------------------------------------------------
class TEMSServerConnectionFrame : public TFrame
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TGroupBox *GroupBox1;
	TButton *ButtonTestConnection;
	TEdit *EditURLHost;
	TLabel *LabelURLHost;
	TEdit *EditURLPort;
	TLabel *LabelURLPort;
	void __fastcall ButtonTestConnectionClick(TObject *Sender);
	void __fastcall EditURLHostChange(TObject *Sender);
	void __fastcall EditURLPortChange(TObject *Sender);
private:	// User declarations
	TEMSProvider * FEMSProvider;
        void __fastcall SetEMSProvider(TEMSProvider * EMSProvider);
        TEMSProvider * __fastcall GetEMSProvider(void);
	void __fastcall EMSProviderChanged(void);
protected:
	virtual void __fastcall Notification(TComponent * AComponent, TOperation opeOperationration);
public:		// User declarations
	__fastcall TEMSServerConnectionFrame(TComponent* Owner);
        __property TEMSProvider * EMSProvider = {read=GetEMSProvider, write=SetEMSProvider};
};

//    procedure ButtonTestConnectionClick(Sender: TObject);
//    procedure EditURLHostChange(Sender: TObject);
//    procedure EditURLPortChange(Sender: TObject);
//  private
//    FEMSProvider: TEMSProvider;
//    function GetEMSProvider: TEMSProvider;
//    procedure SetEMSProvider(const Value: TEMSProvider);
//    procedure EMSProviderChanged;
//    { Private declarations }
//  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//  public
//    { Public declarations }
//    property EMSProvider: TEMSProvider read GetEMSProvider write SetEMSProvider;
//  end;

//---------------------------------------------------------------------------
extern PACKAGE TEMSServerConnectionFrame *EMSServerConnectionFrame;
//---------------------------------------------------------------------------
#endif
