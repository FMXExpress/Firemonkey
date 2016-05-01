//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef FDownloadDemoH
#define FDownloadDemoH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Edit.hpp>
#include <FMX.ImgList.hpp>
#include <System.Classes.hpp>
#include <System.ImageList.hpp>
#include <System.IOUtils.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.URLClient.hpp>
//---------------------------------------------------------------------------
class TForm2;
//---------------------------------------------------------------------------
class TMyAnonymousProcedure : public TCppInterfacedObject<TProc>
{
public:
	typedef    void	(__closure *TMyFunction)();
	TMyAnonymousProcedure(TMyFunction AFunction){FFunction = AFunction;}
	void __fastcall Invoke(void){FFunction();}
private:
	TMyFunction	FFunction;
};
TMyAnonymousProcedure*	AnonymousLambda(TMyAnonymousProcedure::TMyFunction i)
{
	return new TMyAnonymousProcedure(i);
}
//---------------------------------------------------------------------------
struct TMySyncProcedure : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)();
	TMySyncProcedure(TMyFunction AFunction){FFunction = AFunction;}
	void __fastcall Invoke(void){FFunction();}
	TMyFunction	FFunction;
};
inline _di_TThreadProcedure	SyncLambda(TMySyncProcedure::TMyFunction AClosure)
{
	return new TMySyncProcedure(AClosure);
}
//---------------------------------------------------------------------------
template<typename P>
struct TMySyncProcedure1 : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)(P);
	TMySyncProcedure1(TMyFunction AFunction, P P0){FFunction = AFunction; FParam = P0;}
	void __fastcall Invoke(void){FFunction(FParam);}
	P			FParam;
	TMyFunction	FFunction;
};
template<typename P>
_di_TThreadProcedure	SyncLambda(typename TMySyncProcedure1<P>::TMyFunction AClosure, P P0)
{
	return new TMySyncProcedure1<P>(AClosure, P0);
}
//---------------------------------------------------------------------------
template<typename P0, typename P1>
struct TMySyncProcedure2 : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)(P0, P1);
	TMySyncProcedure2(TMyFunction AFunction, P0 AP0, P1 AP1){FFunction = AFunction; FParam0 = AP0; FParam1 = AP1;}
	void __fastcall Invoke(void){FFunction(FParam0, FParam1);}
	P0			FParam0;
	P1			FParam1;
	TMyFunction	FFunction;
};
template<typename P0, typename P1>
_di_TThreadProcedure	SyncLambda(typename TMySyncProcedure2<P0, P1>::TMyFunction AClosure, P0 AP0, P1 AP1)
{
	return new TMySyncProcedure2<P0, P1>(AClosure, AP0, AP1);
}
//---------------------------------------------------------------------------
template<typename P0, typename P1, typename P2, typename P3>
struct TMySyncProcedure4 : public TCppInterfacedObject<TThreadProcedure>
{
	typedef    void	(__closure *TMyFunction)(P0, P1, P2, P3);
	TMySyncProcedure4(TMyFunction AFunction, P0 AP0, P1 AP1, P2 AP2, P3 AP3){FFunction = AFunction; FParam0 = AP0; FParam1 = AP1; FParam2 = AP2; FParam3 = AP3;}
	void __fastcall Invoke(void){FFunction(FParam0, FParam1, FParam2, FParam3);}
	P0			FParam0;
	P1			FParam1;
	P2			FParam2;
	P3			FParam3;
	TMyFunction	FFunction;
};
template<typename P0, typename P1, typename P2, typename P3>
_di_TThreadProcedure	SyncLambda(typename TMySyncProcedure4<P0, P1, P2, P3>::TMyFunction AClosure, P0 AP0, P1 AP1, P2 AP2, P3 AP3)
{
	return new TMySyncProcedure4<P0, P1, P2, P3>(AClosure, AP0, AP1, AP2, AP3);
}

//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TMemo *Memo1;
	TPanel *Panel2;
	TLabel *LabelGlobalSpeed;
	TButton *BStartDownload;
	TLabel *LabelURL;
	TLabel *LabelFile;
	TEdit *EditFileName;
	TEdit *EditURL;
	TImageList *ImageList1;
	TPanel *Panel3;
	TButton *Button1;
	TLabel *Label1;
	TProgressBar *ProgressBarPart1;
	TPanel *Panel4;
	TButton *Button4;
	TLabel *Label4;
	TProgressBar *ProgressBarPart4;
	TPanel *Panel5;
	TButton *Button3;
	TLabel *Label3;
	TProgressBar *ProgressBarPart3;
	TPanel *Panel6;
	TButton *Button2;
	TLabel *Label2;
	TProgressBar *ProgressBarPart2;
	TPanel *Panel7;
	void __fastcall BStartDownloadClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ButtonStopClick(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
	TButton *ButtonCancelArray[4];
	TProgressBar *ProgressBarArray[4];
	TLabel *LabelProgressArray[4];

	volatile bool FAllowClossing;
	volatile bool FClosingForm;

	void __fastcall ReceiveThreadDataEvent(const TObject *Sender, int ThreadNo, int ASpeed, __int64 AContentLength, __int64 AReadCount, System::Boolean &Abort);
	void EnableStartButton();
	void ResetProgress(int Index, int MaxValue);
	void ShowGlobalString(UnicodeString);
	void AddLineToMemo(UnicodeString);
	void UpdateProgress(int AThreadIndex, int AValue, int ASpeed, bool* Abort);

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);

	void SampleDownload(void);
	friend class TMyThreadProcedure;
};
//---------------------------------------------------------------------------

typedef void __fastcall (__closure *TThreadDataEvent)(const System::TObject* Sender, int ThreadNo, int ASpeed, __int64 AContentLength, __int64 AReadCount, System::Boolean &Abort);

//typedef void __fastcall (__closure *TReceiveDataEvent)(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &Abort);

class TDownloadThread: public TThread
{
	protected:
	int FThreadNo;
	unsigned int FTimeStart;
	__int64 FStartPos;
	__int64 FEndPos;
	UnicodeString FURL, FFileName;
	TThreadDataEvent FOnThreadData;
	public:
	__fastcall TDownloadThread(UnicodeString &AURL, UnicodeString &AFileName, int AThreadNo, __int64 StartPos, __int64 EndPos)/* overload */;

	virtual void __fastcall Execute(void);

	void __fastcall ReceiveDataEvent(System::TObject * constSender, __int64 AContentLength, __int64 AReadCount, bool &Abort);
	__property TThreadDataEvent OnThreadData = {read=FOnThreadData, write=FOnThreadData};
};

//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
