//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef CustomAdapterUH
#define CustomAdapterUH

#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.StdCtrls.hpp>
#include <System.Generics.Collections.hpp>
#include <System.RegularExpressions.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.SyncObjs.hpp>
#include <map>
#include <queue>
#include <vector>
#include <memory>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>

// ---------------------------------------------------------------------------
class TCustomAdapter : public TCppInterfacedObject<IListViewAdapter, IListViewTextProvider, IListViewTextButtonProvider>
{
private:
	/* AnonymousThread */
	class TThreadProcedure : public TCppInterfacedObject<TProc>
	{
	private:
		TCustomAdapter * m_Adapter;
		TMemoryStream * m_stream;
		//TBitmap * m_bitmap;
		int m_index;

	public:
		__fastcall TThreadProcedure(TCustomAdapter * p_Adapter);
		virtual void __fastcall Invoke(void);
		void __fastcall InsertImage(void);
		void __fastcall LoadImages(void);
	};

	friend class TThreadProcedure;

public:
	static const int ThreadPoolSize = 1;

private:
	Fmx::Listview::TListViewBase * FParent;
	TStringList * FStrings;

	std::map<int, TBitmap*>FBitmaps;
	System::TObject* FRegexMonitor;

	TRegEx *FUriRegex;
	TRegEx *FIdRegex;
	TRegEx *FNameRegex;

	std::vector<TThread*>FThreads;
	std::queue<int>FRequests;

	int FIndex;
	TCriticalSection *FCS;
	TSemaphore *FSem;
	bool FExitRequested;

	Fmx::Objects::TImage* FBackdropImage;
	System::Classes::TNotifyEvent FOnButtonClicked;
	System::Classes::TNotifyEvent FOnItemsInvalidate;
	System::Classes::TNotifyEvent FOnItemsResize;
	System::Classes::TNotifyEvent FOnItemsMayChange;
	System::Classes::TNotifyEvent FOnItemsCouldHaveChanged;
	System::Classes::TNotifyEvent FOnResetView;
	void __fastcall MatchView(Fmx::Listview::Types::TListItem* const Item);
	void __fastcall CreateThreads(void);
	void __fastcall DestroyThreads(void);

	void __fastcall AddIndex(int Index);
	int  __fastcall NextIndex(); // wait for available, -1 if end requested

	System::UnicodeString __fastcall GetName(const int Index);
	System::UnicodeString __fastcall GetId(const int Index);
	// IListViewTextProvider
	System::UnicodeString __fastcall GetText(const int Index);
	System::UnicodeString __fastcall GetIndexTitle(const int Index);
	void __fastcall ButtonClicked(System::TObject* Sender);
	void __fastcall SetOnButtonClicked(const System::Classes::TNotifyEvent Value);
	void __fastcall ItemsResize(void);
	void __fastcall SetOnItemsResize(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnItemsMayChange(const System::Classes::TNotifyEvent Value);
	// IListViewTextButtonProvider
	Fmx::Listview::Types::TListItemTextButton * __fastcall GetTextButtonDrawable(const int Index);

protected:
	virtual void __fastcall DoCreateNewViews(void);
	virtual void __fastcall DoResetViews(const Fmx::Listview::Types::TListItemPurposes APurposes);
	virtual void __fastcall DoResetView(Fmx::Listview::Types::TListItem* const Item);
	void __fastcall StringListChanging(System::TObject* Sender);
	void __fastcall StringListChange(System::TObject* Sender);
	int __fastcall GetCount(void);
	TListItem* __fastcall GetItem(const int Index);

	int __fastcall IndexOf(Fmx::Listview::Types::TListItem* const AItem);
	System::Generics::Collections::TEnumerator__1<Fmx::Listview::Types::TListItem*> * __fastcall GetEnumerator(void);
	int __fastcall GetDefaultViewHeight(void);

public:
	__fastcall TCustomAdapter(Fmx::Listview::TListViewBase* const Parent,
	  const System::Classes::TStringList * AStrings);
	__fastcall virtual ~TCustomAdapter(void);
	System::UnicodeString __fastcall GetUri(const int Index);
	void __fastcall ImagesLoaded(void);
	void __fastcall ItemsInvalidate();
	void __fastcall ItemsMayChange(void);
	void __fastcall ItemsCouldHaveChanged(void);
	__property Fmx::Objects::TImage* BackdropImage =
	{read = FBackdropImage, write = FBackdropImage};
	__property System::Classes::TNotifyEvent OnButtonClicked =
	{read = FOnButtonClicked, write = SetOnButtonClicked};
	__property System::Classes::TNotifyEvent OnItemsResize =
	{write = SetOnItemsResize};
	__property System::Classes::TNotifyEvent OnItemsInvalidate =
	{write = SetOnItemsInvalidate};
	__property System::Classes::TNotifyEvent OnItemsMayChange =
	{write = SetOnItemsMayChange};
	__property System::Classes::TNotifyEvent OnItemsCouldHaveChanged =
	{write = SetOnItemsCouldHaveChanged};
	__property System::Classes::TNotifyEvent OnResetView =
	{write = SetOnResetView};

public: // ILIstViewAdapter virtual pure method

	virtual void __fastcall SetOnChanged(const System::Classes::TNotifyEvent Value)
	{
	}
	virtual void __fastcall SetOnItemsCouldHaveChanged(const System::Classes::TNotifyEvent Value);

	virtual void __fastcall SetOnItemsInvalidate(const System::Classes::TNotifyEvent Value)
	{
	}

	virtual void __fastcall SetOnResetView(const System::Classes::TNotifyEvent Value)
	{
		FOnResetView = Value;
	}

	virtual void __fastcall Sort
	  (System::DelphiInterface<System::Generics::Defaults::IComparer__1<TListItem*> >AComparer)
	{
	}

	virtual void __fastcall CreateNewViews(void)
	{
	}

	virtual void __fastcall ResetViews(const TListItemPurposes Purposes)
	{
	}
	virtual void __fastcall ResetView(TListItem* const Item);
};

static TObject * synObj = new TObject();

#endif
