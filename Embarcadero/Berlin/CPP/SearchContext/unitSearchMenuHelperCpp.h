//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef unitSearchMenuHelperCppH
#define unitSearchMenuHelperCppH

#include <System.Classes.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <System.UITypes.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <Math.hpp>
#include <System.Contnrs.hpp>


  enum TSearchState {ssNone, ssNoMatch, ssPartial, ssFull};

  class TSearchItem : public TLayout
  {
  private :
	TSearchState FSearchState;
	TRectangle *BackGroundRec;
	TImage *Image;
	TGlowEffect *GlowPartial;
	TLabel *FooterLabel;
	TCircle *BackGroundGlow;
	TStringList *FSearchText;
	TNotifyEvent FOnDblClick;
	// events
	__fastcall void InternalOnDblClick(TObject *Sender);
	__fastcall void SetSearchText(const String Value);
	__fastcall String GetSearchText();
	__fastcall String GetText();
	__fastcall void SetSearchState(TSearchState Value);
  public :
	void TextSearch(String Value);
	TSearchItem (TComponent *AOwner, TAlphaColor BackGroundColor, TAlphaColor GlowColor, TImage *SourceImage, String aText);
	__fastcall ~TSearchItem ();
  __published :
	__property String __fastcall SearchText = {read=GetSearchText, write=SetSearchText};
	__property String Text = {read=GetText};
	__property TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
	__property TSearchState State = {read=FSearchState, write=SetSearchState};
  };


  class TSearchBand : public TLayout
  {
  private :
	TGridLayout *FItemGrid;

	//FSearchItems : TObjectList<TSearchItem>;
	TObjectList *FSearchItems;

	TLabel *FBandTitle;
	__fastcall float GetItemHeight();
	__fastcall float GetItemWidth();
	__fastcall void SetItemHeight(const float Value);
	__fastcall void SetItemWidth(const float Value);
	__fastcall int GetCount();
	__fastcall void InternalResize(TObject *Sender);
	__fastcall String GetText();
  public :
	TSearchBand (TComponent *AOwner, bool aOwnsObjects, TAlphaColor BackGroundColor, String aText, float aItemHeight, float aItemWidth);
	__fastcall ~TSearchBand ();

	void TextSearch(const String Value);
	void Add(TSearchItem *Item);
	__property int Count = {read=GetCount};
  __published :
	__property float ItemHeight = {read=GetItemHeight, write=SetItemHeight};
	__property float ItemWidth = {read=GetItemWidth, write=SetItemWidth};
	__property String Text = {read=GetText};
	__property Top;
  };

  class TSearchBandManager : public TLayout
  {
  private :// strict private?
	//FSearchBands : TObjectList<TSearchBand>;
	TObjectList *FSearchBands;

	TSearchBand *GetSBItem(int index);
	__fastcall int GetCount();
  public :
	TSearchBandManager (TComponent *AOwner, bool aOwnsObjects);
	__fastcall ~TSearchBandManager();
	void Add(TSearchBand *SearchBand);
	void TextSearch(String Value);
	void Clear();
	TSearchBand *BandByName(String name);
	__property TSearchBand *Items[int index] = {read=GetSBItem};
	__property int Count = {read=GetCount};
  };


//class DECLSPEC_DRTTI Foo : public TComponent
//class Foo
//{
//private:	// User declarations
//public:		// User declarations
//	int EchoString(int value);
//	//System::UnicodeString  ReverseString(System::UnicodeString value);
//};

// ---------------------------------------------------------------------------
#endif
