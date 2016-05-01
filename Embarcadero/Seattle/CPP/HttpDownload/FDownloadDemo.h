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
class TMyThreadProcedure : public TCppInterfacedObject<TThreadProcedure>
{
public:
	TMyThreadProcedure(TForm2 *AForm, int ThreadNo, int ASpeed, __int64 AContentLength, __int64 AReadCount, bool Abort);
	void __fastcall Invoke(void);

	bool FAbort;
private:
	TForm2 *FForm;
	int FThreadNo;
	int FSpeed;
	__int64 FContentLength;
	__int64 FReadCount;
};
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TMemo *Memo1;
	TProgressBar *ProgressBarPart1;
	TProgressBar *ProgressBarPart2;
	TProgressBar *ProgressBarPart3;
	TProgressBar *ProgressBarPart4;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TButton *Button2;
	TButton *Button3;
	TButton *Button4;
	TButton *Button1;
	TPanel *Panel2;
	TLabel *LabelGlobalSpeed;
	TButton *BStartDownload;
	TLabel *LabelURL;
	TLabel *LabelFile;
	TEdit *EditFileName;
	TEdit *EditURL;
	TImageList *ImageList1;
	void __fastcall BStartDownloadClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ButtonStopClick(TObject *Sender);
private:	// User declarations
	TButton *ButtonCancelArray[4];
	TProgressBar *ProgressBarArray[4];
	TLabel *LabelProgressArray[4];

	void SampleDownload(void);

	void __fastcall ReceiveThreadDataEvent(const TObject *Sender, int ThreadNo, int ASpeed, __int64 AContentLength, __int64 AReadCount, System::Boolean &Abort);

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);

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
