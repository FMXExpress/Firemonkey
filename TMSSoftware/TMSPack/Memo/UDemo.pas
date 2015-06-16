unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSBaseControl, FMX.TMSMemo, StrUtils, FMX.ListBox, FMX.Objects, FMX.Menus,
  FMX.TMSMemoStyles, FMX.StdCtrls, FMX.TMSXUtil;

const
  crlf: string = #13 + #10;

type
  TForm832 = class(TForm)
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    TMSFMXMemo2: TTMSFMXMemo;
    TMSFMXMemoPascalStyler1: TTMSFMXMemoPascalStyler;
    Label1: TLabel;
    procedure TMSFMXMemo1GetAutoCompletionList(Sender: TObject; AToken: string;
      AList: TStringList);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    SLFormProperties, SLButtonProperties, SLEditProperties: TStringList;
    SLFormMethods, SLButtonMethods, SLEditMethods: TStringList;
    SLFormEvents, SLButtonEvents, SLEditEvents: TStringList;
    SLFormHintEvents, SLButtonHintEvents, SLEditHintEvents: TStringList;

  public
    { Public declarations }
  end;

var
  Form832: TForm832;

implementation

{$R *.fmx}

procedure TForm832.Button1Click(Sender: TObject);
var
  I,CNT: Integer;
begin
  CNT := TMSFMXMemo2.Lines.Count - 10;
  for I := 0 to 10 do
    TMSFMXMemo2.BookmarkIndex[Random(CNT) + 10] := Random(CNT);

  for I := 0 to 10 do
    TMSFMXMemo2.BreakPoint[Random(CNT) + 10] := True;
end;

procedure TForm832.FormCreate(Sender: TObject);
begin
  TMSFMXMemo2.Lines.LoadFromFile(XGetRootDirectory + 'ueditor.pas');
  ReportMemoryLeaksOnShutdown := True;

  SLFormProperties := TStringList.Create;
  SLFormProperties.Add('MDIChildren: [I : Integer]: TForm;');
  SLFormProperties.Add('Action: TActionManager;');
  SLFormProperties.Add('ActiveControl: TWinControl;');
  SLFormProperties.Add('Align: TAlign;');
  SLFormProperties.Add('AlphaBlend: Boolean;');
  SLFormProperties.Add('AlphaBlendValue: Integer;');
  SLFormProperties.Add('Anchors: TAnchorKind;');
  SLFormProperties.Add('AutoScroll: Boolean;');
  SLFormProperties.Add('AutoSize: Boolean;');
  SLFormProperties.Add('BiDiMode: TBiDiMode;');
  SLFormProperties.Add('BorderIcons: TBorderIcon;');
  SLFormProperties.Add('BorderStyle: TFormBorderStyle;');
  SLFormProperties.Add('BorderWidth: Integer;');
  SLFormProperties.Add('Caption: String;');
  SLFormProperties.Add('ClientHeight: Integer;');
  SLFormProperties.Add('ClientWidth: Integer;');
  SLFormProperties.Add('Color: TColor;');
  SLFormProperties.Add('TransparentColor: Boolean;');
  SLFormProperties.Add('TransparentColorValue: TColor;');
  SLFormProperties.Add('Constraints: TSizeConstraints;');
  SLFormProperties.Add('Ctl3D: Boolean;');
  SLFormProperties.Add('UseDockManager: Boolean;');
  SLFormProperties.Add('DefaultMonitor: TDefaultMonitor;');
  SLFormProperties.Add('DockSite: Boolean;');
  SLFormProperties.Add('DoubleBuffered: Boolean;');
  SLFormProperties.Add('DragKind: TDragKind;');
  SLFormProperties.Add('DragMode: TDragMode;');
  SLFormProperties.Add('Enabled: Boolean;');
  SLFormProperties.Add('ParentFont: Boolean;');
  SLFormProperties.Add('Font: TFont;');
  SLFormProperties.Add('FormStyle: TFormStyle;');
  SLFormProperties.Add('GlassFrame: TGlassFrame;');
  SLFormProperties.Add('Height: Integer;');
  SLFormProperties.Add('HelpFile: String;');
  SLFormProperties.Add('HorzScrollBar: TScrollBar;');
  SLFormProperties.Add('Icon: TPicture;');
  SLFormProperties.Add('KeyPreview: Boolean;');
  SLFormProperties.Add('Padding: TPadding;');
  SLFormProperties.Add('Menu: TMainMenu;');
  SLFormProperties.Add('ObjectMenuItem: TObjectMenuItem;');
  SLFormProperties.Add('ParentBiDiMode: Boolean;');
  SLFormProperties.Add('PixelsPerInch: Integer;');
  SLFormProperties.Add('PopupMenu: TPopupMenu;');
  SLFormProperties.Add('PopupMode: TPopupMode;');
  SLFormProperties.Add('PopupParent: TPopupParent;');
  SLFormProperties.Add('Position: TPosition;');
  SLFormProperties.Add('PrintScale: TPrintScale;');
  SLFormProperties.Add('Scaled: Boolean;');
  SLFormProperties.Add('ScreenSnap: Boolean;');
  SLFormProperties.Add('ShowHint: Boolean;');
  SLFormProperties.Add('SnapBuffer: Integer;');
  SLFormProperties.Add('Touch: TTouchManager;');
  SLFormProperties.Add('VertScrollBar: TControlScrollBar;');
  SLFormProperties.Add('Visible: Boolean;');
  SLFormProperties.Add('Width: Integer;');
  SLFormProperties.Add('WindowState: TWindowState;');
  SLFormProperties.Add('WindowMenu: TMenuItem;');

  SLButtonProperties := TStringList.Create;

  SLEditProperties := TStringList.Create;

  SLFormMethods := TStringList.Create;
  SLFormMethods.Add('constructor Create(AOwner: TComponent);');
  SLFormMethods.Add('constructor CreateNew(AOwner: TComponent; Dummy:Integer = 0);');
  SLFormMethods.Add('destructor Destroy;');
  SLFormMethods.Add('procedure Close;');
  SLFormMethods.Add('function CloseQuery: Boolean;');
  SLFormMethods.Add('procedure DefaultHandler(var Message);');
  SLFormMethods.Add('procedure DefocusControl(Control: TWinControl; Removing: Boolean);');
  SLFormMethods.Add('procedure Dock(NewDockSite: TWinControl; ARect: TRect);');
  SLFormMethods.Add('procedure FocusControl(Control: TWinControl);');
  SLFormMethods.Add('procedure GetChildren(Proc: TGetChildProc; Root: TComponent);');
  SLFormMethods.Add('function GetFormImage: TBitmap;');
  SLFormMethods.Add('procedure Hide;');
  SLFormMethods.Add('function IsShortCut(var Message: TWMKey): Boolean; dynamic;');
  SLFormMethods.Add('procedure MakeFullyVisible(AMonitor: TMonitor = nil);');
  SLFormMethods.Add('procedure MouseWheelHandler(var Message: TMessage);');
  SLFormMethods.Add('procedure Print;');
  SLFormMethods.Add('procedure RecreateAsPopup(AWindowHandle: HWND);');
  SLFormMethods.Add('procedure Release;');
  SLFormMethods.Add('procedure SendCancelMode(Sender: TControl);');
  SLFormMethods.Add('procedure SetFocus; override;');
  SLFormMethods.Add('function SetFocusedControl(Control: TWinControl): Boolean;');
  SLFormMethods.Add('procedure Show;');
  SLFormMethods.Add('function ShowModal: Integer;');
  SLFormMethods.Add('procedure WantChildKey(Child: TControl; var Message: TMessage): Boolean;');
  SLFormMethods.Add('procedure set_PopupParent(Value: TCustomForm);');
  SLFormMethods.Add('procedure AfterConstruction;');
  SLFormMethods.Add('procedure BeforeDestruction;');
  SLButtonMethods := TStringList.Create;

  SLEditMethods := TStringList.Create;

  SLFormEvents := TStringList.Create;
  SLFormEvents.Add('OnActivate: TNotifyEvent;');
  SLFormEvents.Add('OnAlignInsertBefore: TAlignInsertBeforeEvent;');
  SLFormEvents.Add('OnAlignPosition: TAlignPositionEvent;');
  SLFormEvents.Add('OnCanResize: TCanResizeEvent;');
  SLFormEvents.Add('OnClick: TNotifyEvent;');
  SLFormEvents.Add('OnClose: TCloseEvent;');
  SLFormEvents.Add('OnCloseQuery: TCloseQueryEvent;');
  SLFormEvents.Add('OnConstrainedResize: TConstrainedResizeEvent;');
  SLFormEvents.Add('OnContextPopup: TContextPopupEvent;');
  SLFormEvents.Add('OnCreate: TNotifyEvent;');
  SLFormEvents.Add('OnDblClick: TNotifyEvent;');
  SLFormEvents.Add('OnDeactivate: TNotifyEvent;');
  SLFormEvents.Add('OnDestroy: TNotifyEvent;');
  SLFormEvents.Add('OnDockDrop: TDockDropEvent;');
  SLFormEvents.Add('OnDockOver: TDockOverEvent;');
  SLFormEvents.Add('OnDragDrop: TDragDropEvent;');
  SLFormEvents.Add('OnDragOver: TDragOverEvent;');
  SLFormEvents.Add('OnEndDock: TEndDragEvent;');
  SLFormEvents.Add('OnGesture: TGestureEvent;');
  SLFormEvents.Add('OnGetSiteInfo: TGetSiteInfoEvent;');
  SLFormEvents.Add('OnHelp: THelpEvent;');
  SLFormEvents.Add('OnHide: TNotifyEvent;');
  SLFormEvents.Add('OnKeyDown: TKeyEvent;');
  SLFormEvents.Add('OnKeyPress: TKeyPressEvent;');
  SLFormEvents.Add('OnKeyUp: TKEyEvent;');
  SLFormEvents.Add('OnMouseActivate: TMouseActivateEvent;');
  SLFormEvents.Add('OnMouseDown: TMouseEvent;');
  SLFormEvents.Add('OnMouseEnter: TNotifyEvent:');
  SLFormEvents.Add('OnMouseLeave: TNotifyEvent;');
  SLFormEvents.Add('OnMouseMove: TMouseMoveEvent;');
  SLFormEvents.Add('OnMouseUp: TMouseEvent;');
  SLFormEvents.Add('OnMouseWheel: TMouseWheelEvent;');
  SLFormEvents.Add('OnMouseWheelDown: TMouseWheelUpDownEvent;');
  SLFormEvents.Add('OnMouseWheelUp: TMouseWheelUpDownEvent;');
  SLFormEvents.Add('OnPaint: TNotifyEvent;');
  SLFormEvents.Add('OnResize: TNotifyEvent;');
  SLFormEvents.Add('OnShortCut: TShortCutEvent;');
  SLFormEvents.Add('OnShow: TNotifyEvent;');
  SLFormEvents.Add('OnStartDock: TStartDockEvent;');
  SLFormEvents.Add('OnUnDock: TUnDockEvent:');

  SLFormHintEvents := TStringList.Create;
  SLFormHintEvents.Add('OnActivate(Sender: TObject);');
  SLFormHintEvents.Add('OnAlignInsertBefore(Sender: TWinControl; C1, C2: TControl): Boolean;');
  SLFormHintEvents.Add('OnAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);');
  SLFormHintEvents.Add('OnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);');
  SLFormHintEvents.Add('OnClick(Sender: TObject);');
  SLFormHintEvents.Add('OnClose(Sender: TObject; var Action: TCloseAction);');
  SLFormHintEvents.Add('OnCloseQuery(Sender: TObject; var CanClose: Boolean);');
  SLFormHintEvents.Add('OnConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);');
  SLFormHintEvents.Add('OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);');
  SLFormHintEvents.Add('OnCreate(Sender: TObject);');
  SLFormHintEvents.Add('OnDblClick(Sender: TObject);');
  SLFormHintEvents.Add('OnDeactivate(Sender: TObject);');
  SLFormHintEvents.Add('OnDestroy(Sender: TObject);');
  SLFormHintEvents.Add('OnDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);');
  SLFormHintEvents.Add('OnDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);');
  SLFormHintEvents.Add('OnDragDrop(Sender, Source: TObject; X, Y: Integer);');
  SLFormHintEvents.Add('OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);');
  SLFormHintEvents.Add('OnEndDock(Sender, Target: TObject; X, Y: Integer);');
  SLFormHintEvents.Add('OnGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);');
  SLFormHintEvents.Add('OnGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);');
  SLFormHintEvents.Add('OnHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;');
  SLFormHintEvents.Add('OnHide(Sender: TObject);');
  SLFormHintEvents.Add('OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLFormHintEvents.Add('OnKeyPress(Sender: TObject; var Key: Char);');
  SLFormHintEvents.Add('OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLFormHintEvents.Add('OnMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);');
  SLFormHintEvents.Add('OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLFormHintEvents.Add('OnMouseEnter(Sender: TObject);');
  SLFormHintEvents.Add('OnMouseLeave(Sender: TObject);');
  SLFormHintEvents.Add('OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);');
  SLFormHintEvents.Add('OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLFormHintEvents.Add('OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);');
  SLFormHintEvents.Add('OnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);');
  SLFormHintEvents.Add('OnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);');
  SLFormHintEvents.Add('OnPaint(Sender: TObject);');
  SLFormHintEvents.Add('OnResize(Sender: TObject);');
  SLFormHintEvents.Add('OnShortCut(var Msg: TWMKey; var Handled: Boolean);');
  SLFormHintEvents.Add('OnShow(Sender: TObject);');
  SLFormHintEvents.Add('OnStartDock(Sender: TObject; var DragObject: TDragDockObject);');
  SLFormHintEvents.Add('OnUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);');

  SLButtonEvents := TStringList.Create;
  SLButtonHintEvents := TStringList.Create;
  SLButtonEvents.Add('OnClick(Sender: TObject);');
  SLButtonEvents.Add('OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);');
  SLButtonEvents.Add('OnDragDrop(Sender, Source: TObject; X, Y: Integer);');
  SLButtonEvents.Add('OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);');
  SLButtonEvents.Add('OnDropDownClick(Sender: TObject);');
  SLButtonEvents.Add('OnEndDock(Sender, Target: TObject; X, Y: Integer);');
  SLButtonEvents.Add('OnEndDrag(Sender, Target: TObject; X, Y: Integer);');
  SLButtonEvents.Add('OnEnter(Sender: TObject);');
  SLButtonEvents.Add('OnExit(Sender: TObject);');
  SLButtonEvents.Add('OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLButtonEvents.Add('OnKeyPress(Sender: TObject; var Key: Char);');
  SLButtonEvents.Add('OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLButtonEvents.Add('OnMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);');
  SLButtonEvents.Add('OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLButtonEvents.Add('OnMouseEnter(Sender: TObject);');
  SLButtonEvents.Add('OnMouseLeave(Sender: TObject);');
  SLButtonEvents.Add('OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);');
  SLButtonEvents.Add('OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLButtonEvents.Add('OnStartDock(Sender: TObject; var DragObject: TDragDockObject);');
  SLButtonEvents.Add('OnStartDrag(Sender: TObject; var DragObject: TDragObject);');

  SLEditEvents := TStringList.Create;
  SLEditHintEvents := TStringList.Create;
  SLEditEvents.Add('OnChange(Sender: TObject);');
  SLEditEvents.Add('OnClick(Sender: TObject);');
  SLEditEvents.Add('OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);');
  SLEditEvents.Add('OnDblClick(Sender: TObject);');
  SLEditEvents.Add('OnDragDrop(Sender, Source: TObject; X, Y: Integer);');
  SLEditEvents.Add('OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);');
  SLEditEvents.Add('OnEndDock(Sender, Target: TObject; X, Y: Integer);');
  SLEditEvents.Add('OnEndDrag(Sender, Target: TObject; X, Y: Integer);');
  SLEditEvents.Add('OnEnter(Sender: TObject);');
  SLEditEvents.Add('OnExit(Sender: TObject);');
  SLEditEvents.Add('OnGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);');
  SLEditEvents.Add('OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLEditEvents.Add('OnKeyPress(Sender: TObject; var Key: Char);');
  SLEditEvents.Add('OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);');
  SLEditEvents.Add('OnMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);');
  SLEditEvents.Add('OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLEditEvents.Add('OnMouseEnter(Sender: TObject);');
  SLEditEvents.Add('OnMouseLeave(Sender: TObject);');
  SLEditEvents.Add('OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);');
  SLEditEvents.Add('OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);');
  SLEditEvents.Add('OnStartDock(Sender: TObject; var DragObject: TDragDockObject);');
  SLEditEvents.Add('OnStartDrag(Sender: TObject; var DragObject: TDragObject);');

end;

procedure TForm832.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SLFormProperties);
  FreeAndNil(SLButtonProperties);
  FreeAndNil(SLEditProperties);

  FreeAndNil(SLFormMethods);
  FreeAndNil(SLButtonMethods);
  FreeAndNil(SLEditMethods);

  FreeAndNil(SLFormEvents);
  FreeAndNil(SLButtonEvents);
  FreeAndNil(SLEditEvents);

  FreeAndNil(SLFormHintEvents);
  FreeAndNil(SLButtonHintEvents);
  FreeAndNil(SLEditHintEvents);
end;

procedure TForm832.FormShow(Sender: TObject);
begin
  TMSFMXMemo2.SetFocus;
end;

procedure TForm832.TMSFMXMemo1GetAutoCompletionList(Sender: TObject;
  AToken: string; AList: TStringList);
var
  ridx, spos: integer; subs: string;
begin
  if pos('FORM',UpperCase(AToken)) > 0 then
  begin
    for ridx := 0 to SLFormMethods.Count - 1 do
      AList.AddObject(SLFormMethods.Strings[ridx], TObject(ttMethod));

    for ridx := 0 to SLFormProperties.Count - 1 do
    begin
      spos := pos(': ', SLFormProperties.Strings[ridx]) + 1 ;
      subs := RightStr(SLFormProperties.Strings[ridx], length(SLFormProperties.Strings[ridx]) - spos);
      AList.AddObject('Property ' + LeftStr(SLFormProperties.Strings[ridx], spos-2) + ': ' + subs, TObject(ttProp));
    end;

    for ridx := 0 to SLFormEvents.Count-1 do
    begin
      spos := pos(': ', SLFormEvents.Strings[ridx]) + 1 ;
      subs := RightStr(SLFormEvents.Strings[ridx], length(SLFormEvents.Strings[ridx]) - spos);
      AList.AddObject('Event ' + LeftSTr(SLFormEvents.Strings[ridx], spos) + ' ' + Subs, TObject(ttEvent));
    end;

    for ridx := 0 to SLFormMethods.Count-1 do
    begin
      spos := pos('(', SLFormMethods.Strings[ridx]);
      if spos > 0 then
      begin
        spos := pos(' ', SLFormMethods.Strings[ridx]);
        subs := RightStr(SLFormMethods.Strings[ridx],length(SLFormMethods.Strings[ridx]) - spos);
        TMSFMXMemoPascalStyler1.HintParameter.Parameters.Add(subs);
      end;
    end;
  end;

  if pos('EDIT',UpperCase(AToken)) > 0 then
  begin
    ALIst.AddObject('procedure Show;', TObject(ttMethod));
    ALIst.AddObject('procedure SetFocus;', TObject(ttMethod));
    ALIst.AddObject('property Text string', TObject(ttProp));
    ALIst.AddObject('property Name string', TObject(ttProp));
    ALIst.AddObject('property Top integer', TObject(ttProp));
    ALIst.AddObject('property Left integer', TObject(ttProp));
    ALIst.AddObject('property Enabled boolean', TObject(ttProp));
    ALIst.AddObject('event OnCreate TNotifyEvent', TObject(ttEvent));
  end;

  if pos('BUTTON',UpperCase(AToken)) > 0 then
  begin
    ALIst.AddObject('procedure Show;', TObject(ttMethod));
    ALIst.AddObject('procedure SetFocus;', TObject(ttMethod));
    ALIst.AddObject('property Caption string', TObject(ttProp));
    ALIst.AddObject('property Name string', TObject(ttProp));
    ALIst.AddObject('property Top integer', TObject(ttProp));
    ALIst.AddObject('property Left integer', TObject(ttProp));
    ALIst.AddObject('property Enabled boolean', TObject(ttProp));
    ALIst.AddObject('event OnCreate TNotifyEvent', TObject(ttEvent));
  end;
end;

end.
