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
#pragma hdrstop

#include "CustomAdapterU.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

__fastcall TCustomAdapter::TCustomAdapter(Fmx::Listview::TListViewBase* const Parent,
	const System::Classes::TStringList *AStrings)
{
	FStrings = const_cast<TStringList*>(AStrings);
	FParent = Parent;
	FUriRegex = new TRegEx("\\[(http://.*)\\]", TRegExOptions() << roNotEmpty);
	FNameRegex = new TRegEx("\\[([A-Za-z]+)\\]", TRegExOptions() << roNotEmpty);
	FIdRegex = new TRegEx("\\[([0-9]+)\\]", TRegExOptions() << roNotEmpty);
	CreateThreads();
	FStrings->OnChanging = StringListChanging;
	FStrings->OnChange = StringListChange;

	FCS = new TCriticalSection;
	FSem = new TSemaphore(NULL, 0, 1000, "", false);
	FExitRequested = false;
    FIndex = 0;
}

__fastcall TCustomAdapter::~TCustomAdapter(void)
{
	DestroyThreads();
	delete FRegexMonitor;
}

void __fastcall TCustomAdapter::ImagesLoaded(void)
{
	std::map<int,TBitmap*>::iterator it;
	for(it = FBitmaps.begin(); it != FBitmaps.end(); it++){
		if(FStrings->Count > 0) {
			TListItem * item = ((TListItem*)(FStrings->Objects[it->first]));
			TListItemImage * BitmapDrawable = static_cast<TListItemImage*>(item->View->FindDrawable("bitmap"));
			if((BitmapDrawable != NULL) && (BitmapDrawable->Bitmap == NULL)) {
				BitmapDrawable->Bitmap = it->second;
			}

			TListItemText* TextDrawable = static_cast<TListItemText*>(item->View->FindDrawable("blurb"));
			if(TextDrawable != NULL) {
				TextDrawable->Text = String().sprintf(L"%s is %dx%d and has ordinal number %d", GetName(it->first).c_str(),
					it->second->Width, it->second->Height, it->first);
			}
			MatchView(item);
		}
	}
	FParent->StopPullRefresh();
	ItemsResize();
	ItemsInvalidate();
}

void __fastcall TCustomAdapter::MatchView(Fmx::Listview::Types::TListItem* const Item)
{
	TListItemImage * BitmapDrawable = static_cast<TListItemImage*>(Item->View->FindDrawable("bitmap"));
	if((BitmapDrawable != NULL) && (BitmapDrawable->Bitmap != NULL)) {
		double Width = FParent->Width - FParent->ItemSpaces->Left - FParent->ItemSpaces->Right;
		BitmapDrawable->Width = Width;
		double Aspect = Width / BitmapDrawable->Bitmap->Width;
		Item->Height = BitmapDrawable->Bitmap->Height * Aspect + 0.5;

		TListItemText * TextDrawable = static_cast<TListItemText*>(Item->View->FindDrawable("blurb"));
		TextDrawable->Width = Width - 100;

		TListItemImage * BackdropDrawable = static_cast<TListItemImage*>(Item->View->FindDrawable("backdrop"));
		BackdropDrawable->Width = Width;
		BackdropDrawable->PlaceOffset->Y = -10;
	}
}

void __fastcall TCustomAdapter::ResetView(TListItem* const Item)
{
	if (FOnResetView != NULL){
		FOnResetView(Item);
	}
	DoResetView(Item);
}

__fastcall TCustomAdapter::TThreadProcedure::TThreadProcedure(TCustomAdapter * p_Adapter)
{
	m_Adapter = p_Adapter;
}

void __fastcall TCustomAdapter::TThreadProcedure::InsertImage(void){
	try {
		TBitmap * bitmap = new TBitmap();
		bitmap->LoadFromStream(m_stream);
		m_stream->Clear();
		if((bitmap != NULL) && (bitmap->Width > 0) && (bitmap->Height > 0)) {
			m_Adapter->FBitmaps.insert(std::pair<int, TBitmap*>(m_index, bitmap));
		}
		if((bitmap != NULL) && (bitmap->Width > 0) && (bitmap->Height > 0)) {
			m_Adapter->ImagesLoaded();
		}
	}
	catch(...){
	}
}

void __fastcall TCustomAdapter::TThreadProcedure::Invoke(void)
{
	TIdHTTP *Http = new TIdHTTP();
	UnicodeString URI;
	m_stream = new TMemoryStream;

	m_index = m_Adapter->NextIndex();
	while(m_index != -1)
	{
		URI = m_Adapter->GetUri(m_index);
		try
		{
			Http->Get(URI, m_stream);
			TThread::Synchronize(NULL, InsertImage);
		}
		catch(...)
		{
			m_Adapter->AddIndex(m_index); // push back so it's picked up later
			TThread::CurrentThread->Sleep(Random(100) + 100);
		}
		m_index = m_Adapter->NextIndex();
	}
	delete Http;
	delete m_stream;
}


void __fastcall TCustomAdapter::AddIndex(int Index)
{
	try
	{
		FCS->Acquire();
		FRequests.push(Index);
		FSem->Release(); //Increase available
	}
	__finally
	{
		FCS->Release();
	}
}

int __fastcall TCustomAdapter::NextIndex()	//wait for available, -1 if end requested
{
	int Result;
	FSem->Acquire();
	if(FExitRequested)
		return -1;
	try
	{
		FCS->Acquire();
		Result = FIndex;
		FIndex++;
	}
	__finally
	{
		FCS->Release();
	}
	return Result;
}

void __fastcall TCustomAdapter::CreateThreads(void)
{
	//FRequests = new TThreadedQueue_Integer(90, INFINITE, INFINITE);
	for(int i = 0; i < ThreadPoolSize; i++) {
		TThread * myThread = TThread::CreateAnonymousThread(new TThreadProcedure(this));
		myThread->FreeOnTerminate = false;
		myThread->Start();
		FThreads.push_back(myThread);
	}
}

void __fastcall TCustomAdapter::DestroyThreads(void)
{
	FExitRequested = True;
  	FSem->Release(ThreadPoolSize);

	for(size_t i = 0; i < FThreads.size(); i++) {
		TThread *th = FThreads[i];
		th->WaitFor();
		FThreads[i]->WaitFor();
	}
	FThreads.clear();
}
System::UnicodeString __fastcall TCustomAdapter::GetName(const int Index)
{
	TMatch match = FNameRegex->Match((*FStrings)[Index]);
	if(match.Success) {
		return match.Groups[1].Value;
	}
	return EmptyStr;
}

System::UnicodeString __fastcall TCustomAdapter::GetId(const int Index)
{
	TMatch match = FIdRegex->Match((*FStrings)[Index]);
	if(match.Success) {
		return match.Groups[1].Value;
	}
	return EmptyStr;
}

System::UnicodeString __fastcall TCustomAdapter::GetUri(const int Index)
{
	UnicodeString _return = EmptyStr;
	UnicodeString uri = (*FStrings)[Index];
	if(FStrings->Count > 0) {
		TMatch match = FUriRegex->Match((*FStrings)[Index]);
		if(match.Success)
			_return =  match.Groups[1].Value;
	}
	return _return;
}

System::UnicodeString __fastcall TCustomAdapter::GetText(const int Index)
{
  return GetName(Index);
}

System::UnicodeString __fastcall TCustomAdapter::GetIndexTitle(const int Index)
{
	return "";
}

void __fastcall TCustomAdapter::ButtonClicked(System::TObject* Sender)
{
	if(FOnButtonClicked != NULL) {
		OnButtonClicked(Sender);
    }
}

void __fastcall TCustomAdapter::SetOnButtonClicked(const System::Classes::TNotifyEvent Value)
{
	FOnButtonClicked = Value;
}

TListItemTextButton* __fastcall TCustomAdapter::GetTextButtonDrawable(const int Index)
{
	return static_cast<TListItemTextButton*>(static_cast<TListItem*>(FStrings->Objects[Index])->View->FindDrawable("button"));
}

void __fastcall TCustomAdapter::DoCreateNewViews(void)
{
	// nothing!
}

void __fastcall TCustomAdapter::ItemsMayChange(void) {
	if(FOnItemsMayChange != NULL) {
		FOnItemsMayChange(this);
    }
}

void __fastcall TCustomAdapter::SetOnItemsMayChange(const System::Classes::TNotifyEvent Value)
{
    FOnItemsMayChange = Value;
}

void __fastcall TCustomAdapter::SetOnItemsResize(const System::Classes::TNotifyEvent Value)
{
    FOnItemsResize = Value;
}

void __fastcall TCustomAdapter::ItemsResize(void) {
	if(FOnItemsResize != NULL) {
		FOnItemsResize(this);
	}
}

void __fastcall TCustomAdapter::ItemsInvalidate(void) {
	if(FOnItemsInvalidate != NULL) {
		FOnItemsInvalidate(this);
	}
}

void __fastcall TCustomAdapter::DoResetViews(const Fmx::Listview::Types::TListItemPurposes APurposes)
{
	for(int i = 0; i < FStrings->Count; i++) {
		this->ResetView(((TListItem*)FStrings->Objects[i]));
	}
	ItemsResize();
}

void __fastcall TCustomAdapter::DoResetView(Fmx::Listview::Types::TListItem* const Item)
{
	if(Item->View->Count == 0) {

		TListItemImage * BitmapDrawable = new TListItemImage(Item);
		BitmapDrawable->Name = "bitmap";
		BitmapDrawable->OwnsBitmap = false;
		BitmapDrawable->Bitmap = NULL;
		BitmapDrawable->Align = TListItemAlign::Trailing;
		BitmapDrawable->ScalingMode = TImageScalingMode::StretchWithAspect;

		TListItemImage * BackdropDrawable = new TListItemImage(Item);
		BackdropDrawable->Name = "backdrop";
		BackdropDrawable->OwnsBitmap = false;
		BackdropDrawable->Bitmap = FBackdropImage->Bitmap;
		BackdropDrawable->VertAlign = TListItemAlign::Trailing;
		BackdropDrawable->Align = TListItemAlign::Trailing;
		BackdropDrawable->ScalingMode = TImageScalingMode::Stretch;
		BackdropDrawable->Opacity = 0.25;
		BackdropDrawable->Height = 65;

		TListItemText * TextDrawable = new TListItemText(Item);
		TextDrawable->Name = "title";
		TextDrawable->Text = GetName(Item->Index).UpperCase();
		TextDrawable->Height = 80;
		TextDrawable->Font->Size = 48;
		TextDrawable->TextColor = TAlphaColorRec::Bisque;
		TextDrawable->SelectedTextColor = TAlphaColorRec::White;
		TextDrawable->PlaceOffset->X = 10;
		TextDrawable->PlaceOffset->Y = 10;

		TextDrawable = new TListItemText(Item);
		TextDrawable->Name = "blurb";
		TextDrawable->Text = GetId(Item->Index);
		TextDrawable->Font->Size = 16;
		TextDrawable->TextColor = TAlphaColorRec::White;
		TextDrawable->SelectedTextColor = TAlphaColorRec::White;
		TextDrawable->Align = TListItemAlign::Leading;
		TextDrawable->VertAlign = TListItemAlign::Trailing;
		TextDrawable->WordWrap = true;
		TextDrawable->Height = 60;
		TextDrawable->PlaceOffset->X = 10;

		TListItemTextButton *  TextButton = new TListItemTextButton(Item);
		TextButton->Name = "button";
		TextButton->Text = "o o o";
		TextButton->Align = TListItemAlign::Trailing;
		TextButton->VertAlign = TListItemAlign::Trailing;
		TextButton->Width = 48;
		TextButton->Height = 16;
		TextButton->PlaceOffset->Y = -TextDrawable->Height + TextDrawable->Height/2 - TextButton->Height/2;
		TextButton->PlaceOffset->X = -10;
		TextButton->OnSelect = ButtonClicked;
		TextButton->TagObject = Item; 	//Owner has been removed, using TagObject to store my parent.

		AddIndex(Item->Index);
	}
	else {
		MatchView(Item);
	}
}

void __fastcall TAbstractListViewAdapter::ItemsMayChange(void)
{
	if (FOnItemsMayChange != NULL){
		FOnItemsMayChange(this);
	}
}

void __fastcall TCustomAdapter::StringListChanging(System::TObject* Sender)
{
	ItemsMayChange();
}

void __fastcall TCustomAdapter::SetOnItemsCouldHaveChanged(const System::Classes::TNotifyEvent Value)
{
	FOnItemsCouldHaveChanged = Value;
}

void __fastcall TCustomAdapter::ItemsCouldHaveChanged(void)
{
	if(FOnItemsCouldHaveChanged != NULL){
		FOnItemsCouldHaveChanged(this);
	}
}

void __fastcall TCustomAdapter::StringListChange(System::TObject* Sender)
{
	ItemsCouldHaveChanged();
	ItemsResize();
	ItemsInvalidate();
}

int __fastcall TCustomAdapter::GetCount(void)
{
	return FStrings->Count;
}

TListItem * __fastcall TCustomAdapter::GetItem(const int Index)
{
	TListItem * _return = static_cast<TListItem*>(FStrings->Objects[Index]);
	if(_return == NULL) {
		_return = new TListItem(FParent->Adapter, *FParent);
		_return->Height = GetDefaultViewHeight();
		_return->Index = Index;
		FStrings->Objects[Index] = _return;
		this->ResetView(_return);
	}
	return _return;
}

int __fastcall TCustomAdapter::IndexOf(Fmx::Listview::Types::TListItem* const AItem)
{
	return -1;
}

System::Generics::Collections::TEnumerator__1<Fmx::Listview::Types::TListItem*>* __fastcall TCustomAdapter::GetEnumerator(void)
{
	return NULL;
}

int __fastcall TCustomAdapter::GetDefaultViewHeight(void)
{
	return 33;
}

