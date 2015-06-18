//
//  ZBar Interface for Delphi XE4 ver 0.1
//  -------------------------------------------------------------------------
//    Source : http://blog.naver.com/simonsayz
//    by Simon Choi
//

unit libzbar;

interface

uses
 SysUtils, Types, Classes, Math,

 System.TypInfo,        // PTypeInfo
 MacApi.ObjectiveC,     // TOCGenericImport
 iOSApi.CocoaTypes,     // NSObject, NSObjectClass
 iOSApi.Foundation,     // NSString
 iOSApi.CoreGraphics,   // CGRect
 iOSApi.UIKit,          // UIView

 iOSApi.QuartzCore,     // {$linkframework QuartzCore}
 iOSApi.CoreMedia,      // {$linkframework CoreMedia}
 iOSApi.CoreVideo,      // {$linkframework CoreVideo}
 iOSApi.AVFoundation;   // {$linkframework AVFoundation}

 {$Link libiconv.dylib} // Link Apple Dynamic Library. User Dynamic Lib can't use.
 {$Link libzbar.a}      // Link Object File (Static Library)

type
 TOnBarCode = Procedure (Sender : TObject; BarCode : String) of Object;

// ----------------------------------------------------------------------------
//
//  ZBar type Translation for Pascal
//  zbar.h
//  zbarReaderView.h
//
// ----------------------------------------------------------------------------

Type
 id                = Pointer;
 zbar_symbol_s     = Pointer;
 zbar_symbol_set_s = Pointer;
 zbar_symbol_t     = zbar_symbol_s;
 zbar_symbol_set_t = zbar_symbol_set_s;

 ZBarReaderView = interface(UIView)
   procedure setReaderDelegate(newValue : id ); cdecl;
   function  readerDelegate: id ; cdecl;
   procedure start; cdecl;
   procedure stop ; cdecl;
 end;

 ZBarReaderViewClass = interface(NSObjectClass) end;
 TZBarReaderViewClass = class (TOCGenericImport<ZBarReaderViewClass,ZBarReaderView>) end;

 ZBarSymbolSet = interface(NSObject)
   function count          : Integer;           cdecl;
   function zbarSymbolSet  : zbar_symbol_set_t; cdecl;
  end;

 ZBarSymbolSetClass = interface(NSObjectClass) end;
 TZBarSymbolSetClass = class (TOCGenericImport<ZBarSymbolSetClass,ZBarSymbolSet>) end;

 ZBarSymbol      = interface(NSObject)
   function symbol : zbar_symbol_t; cdecl;
   function data : NSString; cdecl;
   function initWithSymbol ( symbol : zbar_symbol_t ) : ZBarSymbol; cdecl;
  end;

 ZBarSymbolClass = interface(NSObjectClass) end;
 TZBarSymbolClass = class (TOCGenericImport<ZBarSymbolClass,ZBarSymbol>) end;

 ZBarReaderViewDelegate = interface(IObjectiveC)
   procedure readerView(readerView     : ZBarReaderView;
                        didReadSymbols : ZBarSymbolSet;
                        fromImage      : UIImage); cdecl;
 end;

 TZBarReaderViewDelegate = class(TOCLocal,ZBarReaderViewDelegate)
 Private
  FMe        : TObject;
  FOnBarCode : TOnBarCode;
 Public
   procedure readerView(readerView     : ZBarReaderView;
                        didReadSymbols : ZBarSymbolSet;
                        fromImage      : UIImage); cdecl;
 end;

 TZBarCode = Class(TObject)
  Private
   ZBarView    : ZBarReaderView;
   ZBarEvent   : TZBarReaderViewDelegate;
   FActive     : Boolean;
   Function    GetActive    : Boolean;
   Procedure   SetActive    ( value : Boolean);
   Function    GetOnBarCode : TOnBarCode;
   Procedure   SetOnBarCode ( value : TOnBarCode );
  Public
   Constructor Create;  virtual;
   Destructor  Destroy; override;
   Procedure   Free;
   Procedure SetFrame ( View : UIView; Frame : CGRect );
   Property  View       : ZBarReaderView Read ZBarView;
   Property  Active     : Boolean        Read GetActive    Write SetActive;
   Property  OnBarCode  : TOnBarCode     Read GetOnBarCode Write SetOnBarCode;
  end;

Implementation

{$O-}
function XE4_FakeLoader : ZBarReaderView; cdecl; external 'libzbar.a' name 'OBJC_CLASS_$_ZBarReaderView';
{$O+}

function zbar_symbol_set_first_symbol
                 (symbols : zbar_symbol_set_t) : zbar_symbol_t;
                 cdecl; external 'libzbar.a' name 'zbar_symbol_set_first_symbol';

Procedure TZBarReaderViewDelegate.readerView(readerView     : ZBarReaderView;
                                             didReadSymbols : ZBarSymbolSet;
                                             fromImage      : UIImage); cdecl;
 Var
  tZBarSymbol : zbar_symbol_t;
  oZBarSymbol : ZBarSymbol;
 begin
  If didReadSymbols.count < 1 then Exit;

  tZBarSymbol  := zbar_symbol_set_first_symbol (didReadSymbols.zBarSymbolSet);
  oZBarSymbol  := TZBarSymbolClass.Wrap(TZBarSymbolClass.OCClass.alloc).initWithSymbol(tZBarSymbol);

  If Assigned(FOnBarCode) then
   FOnBarCode(FMe, String(oZBarSymbol.data.UTF8String ));
  oZBarSymbol.release;
  oZBarSymbol := nil;
 end;

//-----------------------------------------------------------------------------
//
// TZBarCode class
//
//-----------------------------------------------------------------------------

Constructor TZBarCode.Create;
 begin
  // Create ZBar Delegate
  ZBarEvent            := TZBarReaderViewDelegate.Create;
  ZBarEvent.FMe        := Self;
  ZBarEvent.FOnBarCode := nil;

  // Create ZBarView
  ZBarView  := TZBarReaderViewClass.Create;
  ZBarView.setReaderDelegate( ZBarEvent.GetObjectID );

  FActive := False;
 end;

Destructor  TZBarCode.Destroy;
 begin
  ZBarView.Release;
  ZBarView  := nil;
  ZBarEvent.Free;
  ZBarEvent := nil;
  inherited;
 end;

Procedure   TZBarCode.Free;
 begin
 	If Self <> nil then
 	 Destroy;
 end;

Procedure   TZBarCode.SetFrame (View : UIView; Frame : CGRect );
 begin
  ZBarView.SetFrame (Frame    );
  View.addSubView   (ZBarView );
 end;

Function    TZBarCode.GetActive : Boolean;
 begin
  Result := FActive;
 end;

Procedure   TZBarCode.SetActive ( value : Boolean);
 begin
  FActive := value;
  Case FActive of
   True  : ZBarView.Start;
   False : ZBarView.Stop;
  End;
 end;

Function    TZBarCode.GetOnBarCode : TOnBarCode;
 begin
  Result := ZBarEvent.FOnBarCode;
 end;

Procedure   TZBarCode.SetOnBarCode ( value : TOnBarCode );
 begin
  ZBarEvent.FOnBarCode := value;
 end;

end.
