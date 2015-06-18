unit HTMLHelpFMXViewer;

interface
uses  System.Types, FMX.Controls, FMX.Forms;

function SetHTMLHelpFile(PathAndFilename : string): boolean;
procedure ShowHTMLHelpContents;
procedure ShowHTMLHelp;
procedure ShowFocusedFormHTMLHelp(AForm : TCommonCustomForm);
procedure ShowControlHTMLHelp(aControl : TStyledControl);

procedure ShowPopupHTMLHelp;
procedure ShowFocusedFormPopupHTMLHelp(AForm : TCommonCustomForm);
procedure ShowControlPopupHTMLHelp(aControl : TStyledControl);

procedure RegisterFormForHelp(AForm : TCommonCustomForm);

implementation

uses Winapi.Windows, Winapi.Messages, System.Classes,FMX.Types, FMX.Platform.Win,
     System.SysUtils;
type
  TFMXHtmlHelpViewer = class(TInterfacedObject)
  private
     FInitializedCookie : DWORD;
     FInitialized       : boolean;
     FHelpFile          : String;
     FHelpHandle        : THandle;
     procedure SetHelpFile(const Value: String);
     function GetHelpFile(const Name: String): String;
     function GetHelpHandle: THandle;
     procedure SetHelpHandle(AHandle: THandle);
     procedure DisplayTextPopupData(Position: TPoint; ResInstance : HINST;
                                      AHelpString: string; HelpStringResId: UINT);

  protected

    procedure DisplayTopic(const Topic: String);

    procedure ShowHelp(const HelpString: String);
    procedure ShowTableOfContents;
    procedure ShutDown;
    procedure SoftShutDown;
    procedure ValidateHelpViewer;
    procedure DisplayTextPopup(Data: PHH_Popup);
    procedure DisplayHTMLTextPopup(Data: PHH_Popup);
    procedure DisplayTextPopupStr(AHelpString : string);
    procedure DisplayTextPopupRes(HelpStringResId : UINT);
    procedure DisplayTextPopupStrPos(Position : TPoint; AHelpString : string);
    procedure DisplayTextPopupResPos(Position : TPoint; HelpStringResId : UINT);

    procedure LookupALink(LinkPtr: PHH_AKLINK);
    procedure LookupKeyWord(LinkPtr: PHH_AKLINK);
    procedure SynchTopic;
    property HelpHandle: THandle read GetHelpHandle write SetHelpHandle;
   public
    function HelpContext(Context: THelpContext): Boolean;
    function HelpJump(const JumpID: string): Boolean;
    function HelpKeyword(const Keyword: string): Boolean;
    function HelpShowTableOfContents: Boolean;
    procedure DoHelp;
    procedure DoHelpOnForm(AForm : TCommonCustomForm);
    procedure DoHelpOnControl(aControl : TStyledControl);
    procedure DoPopupHelp;
    procedure DoPopupHelpOnForm(AForm : TCommonCustomForm);
    procedure DoPopupHelpOnControl(aControl : TStyledControl);

    property HelpFile : String read FHelpFile write SetHelpFile;
  end;
  TWndProc = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;


var
   FMXHtmlHelpViewer : TFMXHtmlHelpViewer = nil;
   OldFormWndProc : TWndProc = nil;

function HelpWndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin

  //unfortunately FMX does not pass the WM_HELP from the chils control
  //to parent when WS_EX_CONTEXTHELP is used. (the point is moot though
  //as FMX ignores biHelp)
  if (uMsg = WM_HELP) then
    if GetKeyState(VK_CONTROL) < 0 then
          ShowPopupHTMLHelp
      else
          ShowHTMLHelp;


  if @OldFormWndProc <> nil then
    Result := OldFormWndProc(hwnd,uMsg,wParam,lParam);
end;


procedure RegisterFormForHelp(AForm : TCommonCustomForm);
var
    FormWndProc : NativeInt;
begin
 FormWndProc := GetWindowLong(FmxHandleToHWND(AForm.Handle), GWL_WNDPROC);
 if @OldFormWndProc = nil then
      @OldFormWndProc := Pointer(FormWndProc);

 if FormWndProc = NativeInt(@OldFormWndProc) then
     SetWindowLong(FmxHandleToHWND(AForm.Handle), GWL_WNDPROC,NativeInt(@HelpWndProc));
end;


function SetHTMLHelpFile(PathAndFilename : string): boolean;
begin
 Result := false;
 if FMXHtmlHelpViewer <> nil then
       Result := FileExists(PathAndFilename);
 if Result then
        FMXHtmlHelpViewer.HelpFile := PathAndFilename;
end;

procedure ShowHTMLHelp;
begin
 if FMXHtmlHelpViewer <> nil then
    FMXHtmlHelpViewer.DoHelp;
end;

procedure ShowFocusedFormHTMLHelp(AForm : TCommonCustomForm);
begin
  if FMXHtmlHelpViewer <> nil then
    FMXHtmlHelpViewer.DoHelpOnForm(AForm);

end;

procedure ShowControlHTMLHelp(aControl : TStyledControl);
begin
    if FMXHtmlHelpViewer <> nil then
    FMXHtmlHelpViewer.DoHelpOnControl(aControl);
end;

procedure ShowHTMLHelpContents;
begin
  if FMXHtmlHelpViewer <> nil then
       FMXHtmlHelpViewer.ShowTableOfContents;
end;

procedure ShowPopupHTMLHelp;
begin
  if FMXHtmlHelpViewer <> nil then
              FMXHtmlHelpViewer.DoPopupHelp

end;

procedure ShowFocusedFormPopupHTMLHelp(AForm : TCommonCustomForm);
begin
  if FMXHtmlHelpViewer <> nil then
             FMXHtmlHelpViewer.DoPopupHelpOnForm(AForm);
end;

procedure ShowControlPopupHTMLHelp(aControl : TStyledControl);
begin
  if FMXHtmlHelpViewer <> nil then
          FMXHtmlHelpViewer.DoPopupHelpOnControl(aControl);
end;




{ TFMXHtmlHelpViewer }

function TFMXHtmlHelpViewer.GetHelpFile(const Name: String): String;
begin
  Result := '';
  if (Name = '')  then Result := FHelpFile;
end;

procedure TFMXHtmlHelpViewer.ValidateHelpViewer;
begin
  if not FInitialized then
  begin
    HtmlHelp(0, nil, HH_INITIALIZE, &FInitializedCookie);
    FInitialized := true;
  end;
end;

procedure TFMXHtmlHelpViewer.ShowTableOfContents;
begin
  ValidateHelpViewer;
  SynchTopic;
  HtmlHelp(HelpHandle, PChar(FHelpFile),  HH_DISPLAY_TOC, 0);
end;

procedure TFMXHtmlHelpViewer.SetHelpFile(const Value: String);
begin
  FHelpFile := Value;
end;

procedure TFMXHtmlHelpViewer.SetHelpHandle(AHandle: THandle);
begin
  if AHandle <> FHelpHandle  then
    FHelpHandle := AHandle;
end;

function TFMXHtmlHelpViewer.GetHelpHandle: THandle;
var
  FocusedForm : TCommonCustomForm;
begin
     if IsWindow(FHelpHandle)  then
       Result := FHelpHandle
     else
     begin
        FocusedForm := Screen.ActiveForm;
        if FocusedForm <> nil then
          Result := FmxHandleToHWND(FocusedForm.Handle)
        else
          Result := WinApi.Windows.GetDesktopWindow;
     end;
end;


function TFMXHtmlHelpViewer.HelpContext(Context: THelpContext): Boolean;
begin
  ValidateHelpViewer;
  SynchTopic;
  HtmlHelp(HelpHandle, PChar(FHelpFile), HH_HELP_CONTEXT, Context);
end;


function TFMXHtmlHelpViewer.HelpJump(const JumpID: string): Boolean;
begin
 ShowHelp(JumpID);
end;

function TFMXHtmlHelpViewer.HelpKeyword(const Keyword: string): Boolean;
begin
 ShowHelp(Keyword);
end;

function TFMXHtmlHelpViewer.HelpShowTableOfContents: Boolean;
begin
 ShowTableOfContents;
 Result := true;
end;


procedure TFMXHtmlHelpViewer.ShowHelp(const HelpString: String);
var
  Link: THH_AKLINK;
begin
  ValidateHelpViewer;
  SynchTopic;
  Link.cbStruct := sizeof (THH_AKLINK);
  Link.fReserved := false;
  Link.pszKeywords := PChar(HelpString);
  Link.pszUrl := nil;
  Link.pszMsgText := nil;
  Link.pszMsgTitle := nil;
  Link.pszWindow := nil;
  Link.fIndexOnFail := true;
  LookupKeyWord(@Link);
end;


{ SoftShutDown is called by the help manager to ask the viewer to
  terminate any externally spawned subsystem without shutting itself down. }
procedure TFMXHtmlHelpViewer.SoftShutDown;
begin
  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
end;


procedure TFMXHtmlHelpViewer.ShutDown;
begin
  SoftShutDown;
  if FInitialized then
  begin
    HtmlHelp(0, nil, HH_UNINITIALIZE, &FInitializedCookie);
    FInitialized := false;
    FInitializedCookie := 0;
  end;
end;


procedure TFMXHtmlHelpViewer.DisplayTopic(const Topic: String);
const
  InvokeSep = '::/';
  InvokeSuf = '.htm';
var
  HelpFile: String;
  InvocationString: String;
begin
  ValidateHelpViewer;
  InvocationString := HelpFile + InvokeSep + Topic + InvokeSuf;
  HtmlHelp(HelpHandle, PChar(InvocationString), HH_DISPLAY_TOPIC, 0);
end;



procedure TFMXHtmlHelpViewer.DoHelpOnControl(aControl: TStyledControl);
begin
    if aControl <> nil then
    begin
      if aControl.Root is TCommonCustomForm then
              HelpHandle := FmxHandleToHWND(TCommonCustomForm(aControl.Root).Handle);
      if aControl.HelpType = THelpType.htKeyword then
         HelpKeyword(aControl.HelpKeyword)
      else
         HelpContext(aControl.HelpContext);
    end;
end;

procedure TFMXHtmlHelpViewer.DoHelpOnForm(AForm: TCommonCustomForm);
begin
   if AForm <> nil then
    if AForm.Focused <> nil then
       if AForm.Focused.GetObject is TStyledControl then
          DoHelpOnControl(TStyledControl(AForm.Focused.GetObject));
end;


procedure TFMXHtmlHelpViewer.DoPopupHelpOnControl(aControl: TStyledControl);
var
   BottomLeft : TPointF;
   CallingForm : TCommonCustomForm;
begin
     if aControl <> nil then
    begin
      if aControl.Root is TCommonCustomForm then
          CallingForm := TCommonCustomForm(aControl.Root);
      if CallingForm <> nil then
      begin
          HelpHandle := FmxHandleToHWND(CallingForm.Handle);
          BottomLeft := CallingForm.ClientToScreen( PointF(aControl.ParentedRect.Left,
                                                             aControl.ParentedRect.Bottom));
          if aControl.HelpType = THelpType.htKeyword then
             DisplayTextPopupStrPos(BottomLeft.Round,aControl.HelpKeyword)
          else
             DisplayTextPopupResPos(BottomLeft.Round,aControl.HelpContext);
      end;
    end;
end;

procedure TFMXHtmlHelpViewer.DoPopupHelpOnForm(AForm: TCommonCustomForm);
begin
   if AForm <> nil then
    if AForm.Hovered <> nil then
       if AForm.Hovered.GetObject is TStyledControl then
        DoPopupHelpOnControl(TStyledControl(AForm.Hovered.GetObject));
end;

procedure TFMXHtmlHelpViewer.DoPopupHelp;
begin
 DoPopupHelpOnForm(Screen.ActiveForm);
end;

procedure TFMXHtmlHelpViewer.DoHelp;
begin
  DoHelpOnForm(Screen.ActiveForm);
end;

procedure TFMXHtmlHelpViewer.SynchTopic;
begin
  HtmlHelp(HelpHandle, PChar(FHelpFile), HH_DISPLAY_TOC, 0);
end;

procedure TFMXHtmlHelpViewer.LookupALink(LinkPtr: PHH_AKLINK);
begin
  HtmlHelp(HelpHandle, PChar(FHelpFile), HH_ALINK_LOOKUP, DWORD_PTR(LinkPtr));
end;

procedure TFMXHtmlHelpViewer.LookupKeyWord(LinkPtr: PHH_AKLINK);
begin
  HtmlHelp(HelpHandle, PChar(FHelpFile),HH_KEYWORD_LOOKUP, DWORD_PTR(LinkPtr));
end;

procedure TFMXHtmlHelpViewer.DisplayHTMLTextPopup(Data: PHH_Popup);
begin
  HtmlHelp(HelpHandle, PChar(FHelpFile), HH_DISPLAY_TEXT_POPUP, DWORD_PTR(Data));
end;

procedure TFMXHtmlHelpViewer.DisplayTextPopup(Data: PHH_Popup);
begin
  HtmlHelp(HelpHandle, nil, HH_DISPLAY_TEXT_POPUP, DWORD_PTR(Data));
end;

procedure TFMXHtmlHelpViewer.DisplayTextPopupData(Position : TPoint;
                                                   ResInstance : HINST;
                                                   AHelpString : string;
                                                    HelpStringResId : UINT);
var
   PopupData : HH_Popup;
begin
    PopupData.cbStruct := sizeof(HH_Popup);
    PopupData.hInst    := ResInstance;
    PopupData.idString := HelpStringResId;
    PopupData.pszText  := PChar(AHelpString);
    PopupData.pt       := Position;
    PopupData.clrForeground  := TColorRef(-1);
    PopupData.clrBackground  := TColorRef(-1);
    PopupData.rcMargins.Left   := -1;
    PopupData.rcMargins.Right  := -1;
    PopupData.rcMargins.Top    := -1;
    PopupData.rcMargins.Bottom := -1;
    PopupData.pszFont        :=  nil;
 DisplayTextPopup(@PopupData);
end;


procedure TFMXHtmlHelpViewer.DisplayTextPopupRes(HelpStringResId: UINT);
begin

end;

procedure TFMXHtmlHelpViewer.DisplayTextPopupResPos(Position: TPoint;
  HelpStringResId: UINT);
begin
 //code can be improved to specify a resource module for the strings
 DisplayTextPopupData(Position,GetModuleHandle(nil),'',HelpStringResId);
end;

procedure TFMXHtmlHelpViewer.DisplayTextPopupStr(AHelpString: string);
begin

end;

procedure TFMXHtmlHelpViewer.DisplayTextPopupStrPos(Position: TPoint;
  AHelpString: string);
begin
   DisplayTextPopupData(Position,0,AHelpString,0);
end;

initialization
 FMXHtmlHelpViewer := TFMXHtmlHelpViewer.Create;
 finalization
 if FMXHtmlHelpViewer <> nil then
 begin
    FMXHtmlHelpViewer.ShutDown;
    FMXHtmlHelpViewer := nil;
 end;



end.
