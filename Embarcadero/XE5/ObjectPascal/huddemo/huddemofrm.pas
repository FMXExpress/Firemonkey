
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit huddemofrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.ExtCtrls, FMX.ListBox, FMX.StdCtrls, FMX.Styles;

type
  TfrmHudDemo = class(TForm)
    Window: TPanel;
    StyleBook1: TStyleBook;
    Button: TButton;
    TrackBar1: TTrackBar;
    ClientLayout: TLayout;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Switch1: TSwitch;
    procedure WindowApplyStyleLookup(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    FIsActive: Boolean;
    procedure DoCloseClick(Sender: TObject);
  public
    { Public declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  published 
    property IsActive: Boolean read FIsActive;
  end;

var
  frmHudDemo: TfrmHudDemo;

implementation

{$R *.fmx}

procedure TfrmHudDemo.FormActivate(Sender: TObject);
begin
  FIsActive := True;
  ApplyTriggerEffect(Self, 'IsActive');
  StartTriggerAnimation(Self, 'IsActive');
end;

procedure TfrmHudDemo.FormDeactivate(Sender: TObject);
begin
  FIsActive := False;
  ApplyTriggerEffect(Self, 'IsActive');
  StartTriggerAnimation(Self, 'IsActive');
end;

procedure TfrmHudDemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited ;
  if (IControl(ObjectAtPoint(ClientToScreen(PointF(X, Y)))) = nil) then
    StartWindowDrag;
end;

procedure TfrmHudDemo.WindowApplyStyleLookup(Sender: TObject);
begin
  Window.StylesData['close'] := TValue.From<TNotifyEvent>(DoCloseClick); 
  Window.StylesData['text'] := Caption;
end;

procedure TfrmHudDemo.DoCloseClick(Sender: TObject);
begin
  Close;
end;

end.
