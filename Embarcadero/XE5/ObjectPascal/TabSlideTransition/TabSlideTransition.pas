unit TabSlideTransition;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Gestures, FMX.StdCtrls,
  FMX.Edit, FMX.ExtCtrls, FMX.TabControl, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.VirtualKeyboard,FMX.Menus,FMX.Platform,
  System.Actions, FMX.ActnList, System.Math, FMX.DateTimeCtrls, FMX.MobilePreview;


type
  TTabSlideTransitionFrmBase = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    infoAddress: TEdit;
    infoTelephone: TEdit;
    infoEmail: TEdit;
    GestureManager1: TGestureManager;
    Memo1: TMemo;
    efirstName: TEdit;
    edInstitution: TEdit;
    edAdmissionDate: TComboBox;
    edCity: TEdit;
    edGraduationDate: TComboBox;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    weEmpName: TEdit;
    weCity: TEdit;
    weOccupiedJob: TEdit;
    weFrom: TComboBox;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    weTo: TComboBox;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    Button1: TButton;
    ActionList1: TActionList;
    ChangeTabAction1: TChangeTabAction;
    Button2: TButton;
    Button3: TButton;
    ChangeTabAction2: TChangeTabAction;
    ChangeTabAction3: TChangeTabAction;
    Button4: TButton;
    Button5: TButton;
    ChangeTabAction4: TChangeTabAction;
    ChangeTabAction5: TChangeTabAction;
    Button6: TButton;
    Button7: TButton;
    ChangeTabAction6: TChangeTabAction;
    ChangeTabAction7: TChangeTabAction;
    Button8: TButton;
    ChangeTabAction8: TChangeTabAction;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ToolBar1: TToolBar;
    Label2: TLabel;
    ToolBar2: TToolBar;
    Label4: TLabel;
    ToolBar3: TToolBar;
    Label5: TLabel;
    ToolBar4: TToolBar;
    Label6: TLabel;
    ToolBar5: TToolBar;
    Label8: TLabel;
    NameList: TListBox;
    firstName: TListBoxItem;
    LastName: TListBoxItem;
    PersonalInfoList: TListBox;
    Address: TListBoxItem;
    Phone: TListBoxItem;
    Email: TListBoxItem;
    BirthDate: TListBoxItem;
    PersonalInformation: TListBoxGroupHeader;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    EducationList: TListBox;
    Institution: TListBoxItem;
    City: TListBoxItem;
    AdmissionDate: TListBoxItem;
    GraduationDate: TListBoxItem;
    Education: TListBoxGroupHeader;
    WorkList: TListBox;
    WorkExperience: TListBoxGroupHeader;
    Employer: TListBoxItem;
    EmployerCity: TListBoxItem;
    FromDate: TListBoxItem;
    ToDate: TListBoxItem;
    CurrentJob: TListBoxItem;
    PersonalInfoSummary: TLabel;
    eLastName: TEdit;
    infoDate: TCalendarEdit;
    VertScrollBox1: TVertScrollBox;
    MainLayout1: TLayout;
    procedure TabControl1Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure UpdateMemo;
    procedure FormCreate(Sender: TObject);
    procedure eUserChangeTracking(Sender: TObject);
    procedure infoChangeTracking(Sender: TObject);
    procedure edChangeTracking(Sender: TObject);
    procedure weChangeTracking(Sender: TObject);
    procedure FormFocusChanged(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);

  private
    FService: IFMXVirtualKeyboardToolbarService;
    FKBBounds: TRectF;
    FNeedOffset: Boolean;
    procedure CalcContentBoundsProc(Sender: TObject;
                                var ContentBounds: TRectF);
    procedure UpdateKBBounds;
    procedure RestorePosition;

  public
    { Public declarations }
  end;

var
  TabSlideTransitionFrmBase: TTabSlideTransitionFrmBase;

implementation

{$R *.fmx}

procedure TTabSlideTransitionFrmBase.FormCreate(Sender: TObject);
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(FService)) then
  begin
    FService.SetToolbarEnabled(True);
    FService.SetHideKeyboardButtonVisibility(True);
  end;
  VertScrollBox1.OnCalcContentBounds := CalcContentBoundsProc;
end;

procedure TTabSlideTransitionFrmBase.CalcContentBoundsProc(Sender: TObject;
                                       var ContentBounds: TRectF);
begin
  if FNeedOffset and (FKBBounds.Top > 0) then
  begin
    ContentBounds.Bottom := Max(ContentBounds.Bottom,
                                2 * ClientHeight - FKBBounds.Top);
  end;
end;

procedure TTabSlideTransitionFrmBase.FormFocusChanged(Sender: TObject);
begin
  UpdateKBBounds;
end;

{ On Android, capture the back button; if there are tabs to the left of current,
  navigate back, otherwise "fall off." }

procedure TTabSlideTransitionFrmBase.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if TabControl1.TabIndex > 0 then
    begin
      TabControl1.TabIndex := TabControl1.TabIndex - 1 mod TabControl1.TabCount;
      Key := 0;
    end;
  end;
end;

procedure TTabSlideTransitionFrmBase.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds.Create(0, 0, 0, 0);
  FNeedOffset := False;
  RestorePosition;
end;

procedure TTabSlideTransitionFrmBase.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds := TRectF.Create(Bounds);
  FKBBounds.TopLeft := ScreenToClient(FKBBounds.TopLeft);
  FKBBounds.BottomRight := ScreenToClient(FKBBounds.BottomRight);
  UpdateKBBounds;
end;


procedure TTabSlideTransitionFrmBase.edChangeTracking(Sender: TObject);
begin
  Button5.Enabled := (edInstitution.Text <> '') and (edAdmissionDate.ItemIndex <> -1)
                    and (edCity.Text <> '') and (edGraduationDate.ItemIndex <> -1);
end;

procedure TTabSlideTransitionFrmBase.eUserChangeTracking(Sender: TObject);
begin
  Button1.Enabled := (eFirstName.Text <> '') and (eLastName.Text <> '');
end;

procedure TTabSlideTransitionFrmBase.infoChangeTracking(Sender: TObject);
begin
  Button3.Enabled := (infoAddress.Text <> '') and (infoTelephone.Text <> '')
                      and (infoDate.Text <> '') and (infoEmail.Text <> '');
end;

procedure TTabSlideTransitionFrmBase.TabControl1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if (TabControl1.TabIndex = 0) and (Button1.Enabled = True) then
        ChangeTabAction1.ExecuteTarget(self)
      else if (TabControl1.TabIndex = 1) and (Button3.Enabled = True)then
        ChangeTabAction3.ExecuteTarget(self)
      else if (TabControl1.TabIndex = 2) and (Button5.Enabled = True)then
        ChangeTabAction5.ExecuteTarget(self)
      else if (TabControl1.TabIndex = 3) and (Button7.Enabled = True) then
      begin
        UpdateMemo;
        ChangeTabAction7.ExecuteTarget(self);
      end;
    end;

    sgiRight:
      if TabControl1.TabIndex = 1 then
        ChangeTabAction2.ExecuteTarget(self)
      else if TabControl1.TabIndex = 2 then
        ChangeTabAction4.ExecuteTarget(self)
      else if TabControl1.TabIndex = 3 then
        ChangeTabAction6.ExecuteTarget(self)
      else if TabControl1.TabIndex = 4 then
        ChangeTabAction8.ExecuteTarget(self);
  end;
  Handled := True;
end;

procedure TTabSlideTransitionFrmBase.UpdateMemo;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Personal information');
  Memo1.Lines.Add('Name: ' + eFirstName.Text + ' ' + eLastName.Text);
  Memo1.Lines.Add('Address: ' + infoAddress.Text);
  Memo1.Lines.Add('Telephone: ' + infoTelephone.Text);
  Memo1.Lines.Add('E-mail: ' + infoEmail.Text);
  Memo1.Lines.Add('Date of birth:' + infoDate.Text);
  Memo1.Lines.Add('');

  Memo1.Lines.Add('Education');
  Memo1.Lines.Add(edInstitution.Text + ', ' + edCity.Text + ', ' +
                  edAdmissionDate.Selected.Text + '-' + edGraduationDate.Selected.Text);
  Memo1.Lines.Add('');

  Memo1.Lines.Add('Work experience');
  Memo1.Lines.Add(weOccupiedJob.Text + ' at ' +  weEmpName.Text + ', ' + weCity.Text + ', ' +
                weFrom.Selected.Text + '-' + weTo.Selected.Text );
end;

procedure TTabSlideTransitionFrmBase.weChangeTracking(Sender: TObject);
begin
  Button7.Enabled := (weEmpName.Text <> '') and (weCity.Text <> '') and (weOccupiedJob.Text <> '')
                      and (weFrom.ItemIndex <> -1) and (weTo.ItemIndex <> -1);
  if Button7.Enabled = True then
    UpdateMemo;
end;

procedure TTabSlideTransitionFrmBase.UpdateKBBounds;
var
  LFocused : TControl;
  LFocusRect: TRectF;
begin
  FNeedOffset := False;
  if Assigned(Focused) then
  begin
    LFocused := TControl(Focused.GetObject);
    LFocusRect := LFocused.AbsoluteRect;
    LFocusRect.Offset(VertScrollBox1.ViewportPosition);
    if (LFocusRect.IntersectsWith(TRectF.Create(FKBBounds))) and
       (LFocusRect.Bottom > FKBBounds.Top) then
    begin
      FNeedOffset := True;
      MainLayout1.Align := TAlignLayout.alHorizontal;
      VertScrollBox1.RealignContent;
      Application.ProcessMessages;
      VertScrollBox1.ViewportPosition :=
        PointF(VertScrollBox1.ViewportPosition.X,
               LFocusRect.Bottom - FKBBounds.Top);
    end;
  end;
  if not FNeedOffset then
    RestorePosition;
end;

procedure TTabSlideTransitionFrmBase.RestorePosition;
begin
  VertScrollBox1.ViewportPosition := PointF(VertScrollBox1.ViewportPosition.X, 0);
  MainLayout1.Align := TAlignLayout.alClient;
  VertScrollBox1.RealignContent;
end;

end.
