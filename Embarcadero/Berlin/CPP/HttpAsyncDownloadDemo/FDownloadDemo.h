//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------

#ifndef FDownloadDemoH
#define FDownloadDemoH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.IOUtils.hpp>
#include <System.Net.HttpClient.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.ImageList.hpp>
#include <System.Net.URLClient.hpp>
//---------------------------------------------------------------------------
struct TMySyncProcedure : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)();
	TMySyncProcedure(TMyFunction AFunction): FFunction(AFunction) {}
	void __fastcall Invoke(void){FFunction();}
	TMyFunction	FFunction;
};
inline _di_TThreadProcedure	SyncLambda(TMySyncProcedure::TMyFunction AClosure)
{
	return new TMySyncProcedure(AClosure);
}
//---------------------------------------------------------------------------
template<typename P0>
struct TMySyncProcedure1 : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)(P0);
	TMySyncProcedure1(TMyFunction AFunction, P0 AP0): FFunction(AFunction), FParam0(AP0) {}
	void __fastcall Invoke(void){FFunction(FParam0);}
	P0			FParam0;
	TMyFunction	FFunction;
};
template<typename P0>
_di_TThreadProcedure	SyncLambda(typename TMySyncProcedure1<P0>::TMyFunction AClosure, P0 AP0)
{
	return new TMySyncProcedure1<P0>(AClosure, AP0);
}

//---------------------------------------------------------------------------
template<typename P0, typename P1, typename P2>
struct TMySyncProcedure3 : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)(P0, P1, P2);
	TMySyncProcedure3(TMyFunction AFunction, P0 AP0, P1 AP1, P2 AP2): FFunction(AFunction), FParam0(AP0), FParam1(AP1), FParam2(AP2) {}
	void __fastcall Invoke(void){FFunction(FParam0, FParam1, FParam2);}
	P0			FParam0;
	P1			FParam1;
	P2			FParam2;
	TMyFunction	FFunction;
};
template<typename P0, typename P1, typename P2>
_di_TThreadProcedure	SyncLambda(typename TMySyncProcedure3<P0, P1, P2>::TMyFunction AClosure, P0 AP0, P1 AP1, P2 AP2)
{
	return new TMySyncProcedure3<P0, P1, P2>(AClosure, AP0, AP1, AP2);
}

//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelTop;
	TLabel *LabelFile;
	TEdit *EditFileName;
	TLabel *LabelURL;
	TEdit *EditURL;
	TLabel *LabelGlobalSpeed;
	TButton *BStartDownload;
	TProgressBar *ProgressBarDownload;
	TButton *BStopDownload;
	TPanel *PanelCenter;
	TMemo *Memo1;
	TImageList *ImageList1;
	void __fastcall BStartDownloadClick(TObject *Sender);
	void __fastcall BStopDownloadClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);

private:	// User declarations
	THTTPClient *FClient;
	//Int64 FReadCount;
	UInt32 FGlobalStart;
	_di_IHTTPResponse FAsyncResponse;
	TStream *FDownloadStream;

//typedef void __fastcall (__closure *TAsyncProcedureEvent)(const System::Types::_di_IAsyncResult ASyncResult);
	void __fastcall DoEndDownload(const System::Types::_di_IAsyncResult AsyncResult);
	void __fastcall SampleDownload(void);

	void LogText(UnicodeString AText);
	void UpdateProgress(int AValue, int ASpeed, bool &Abort);
	void __fastcall ReceiveDataEvent(TObject * const Sender, __int64 AContentLength, __int64 AReadCount, bool &Abort);
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
