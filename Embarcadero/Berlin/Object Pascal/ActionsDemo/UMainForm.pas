//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.StdActns, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Edit, UActiveForm, FMX.Platform,
  FMX.Layouts, FMX.Memo, FMX.SpinBox, FMX.ComboEdit, FMX.ComboTrackBar, FMX.Controls.Presentation, FMX.EditBox,
  FMX.NumberBox;

type
  TMainForm = class(TForm)
    FMXActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    FileMainMenuItem: TControlAction;
    MacOSMainMenuItem: TControlAction;
    PanelValueRange: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    SmallScrollBar1: TSmallScrollBar;
    ScrollBar1: TScrollBar;
    SmallScrollBar2: TSmallScrollBar;
    ScrollBar3: TScrollBar;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    ArcDial1: TArcDial;
    ArcDial2: TArcDial;
    Switch1: TSwitch;
    CheckBox1: TCheckBox;
    ComboTrackBar1: TComboTrackBar;
    ComboTrackBar2: TComboTrackBar;
    SpinBox1: TSpinBox;
    NumberBox1: TNumberBox;
    ValueRangeAction1: TValueRangeAction;
    ViewAction1: TViewAction;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    ViewAction2: TViewAction;
    MenuItem7: TMenuItem;
    PopupMenu1: TPopupMenu;
    Switch2: TSwitch;
    CheckBox2: TCheckBox;
    MenuItem8: TMenuItem;
    EditVirtualKeyboard1: TVirtualKeyboard;
    MenuItem9: TMenuItem;
    WindowClose1: TWindowClose;
    FileExitWin: TFileExit;
    FileExitOSX: TFileExit;
    MenuItem10: TMenuItem;
    FileHideApp1: TFileHideApp;
    MenuItem11: TMenuItem;
    FileHideAppOthers1: TFileHideAppOthers;
    MenuItem12: TMenuItem;
    procedure ViewAction2CreateComponent(Sender: TObject;
      var NewComponent: TComponent);
    procedure Switch2Switch(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WindowClose1CanActionExec(Sender: TCustomAction;
      var CanExec: Boolean);
    procedure FileExitWinCanActionExec(Sender: TCustomAction;
      var CanExec: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{$IFNDEF ANDROID}
uses FMX.DialogService.Sync;
{$ENDIF}

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  Caption := 'Action1Execute';
end;

procedure TMainForm.FileExitWinCanActionExec(Sender: TCustomAction;
  var CanExec: Boolean);
begin
  if (Sender is TFileExit) and (TFileExit(Sender).ShortcutPressed) then
  begin
    {$IFDEF ANDROID}
    CanExec := True;
    {$ELSE}
    CanExec := TDialogServiceSync.MessageDialog('Are you sure you want to terminate the application?', TMsgDlgType.mtConfirmation,
      mbYesNo, TMsgDlgBtn.mbNo, 0) = mrYes;
    {$ENDIF}
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ComboTrackBar1.Action := ValueRangeAction1;
  ComboTrackBar2.Action := ValueRangeAction1;
  SpinBox1.Action := ValueRangeAction1;
  NumberBox1.Action := ValueRangeAction1;
end;

procedure TMainForm.Switch2Switch(Sender: TObject);
begin
  self.CheckBox2.IsChecked := Switch2.IsChecked;
end;

procedure TMainForm.ViewAction2CreateComponent(Sender: TObject;
  var NewComponent: TComponent);
begin
  NewComponent := TActiveForm.Create(self);
end;

procedure TMainForm.WindowClose1CanActionExec(Sender: TCustomAction;
  var CanExec: Boolean);
begin
  {$IFDEF ANDROID}
  CanExec := True;
  {$ELSE}
  CanExec := TDialogServiceSync.MessageDialog('Oh, no! Do not close me.', TMsgDlgType.mtWarning, mbYesNo,
    TMsgDlgBtn.mbNo, 0) = mrYes;
  {$ENDIF}
end;

end.
