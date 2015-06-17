
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uFormFmxPhysics;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.ListBox, FMX.Controls.Presentation, FMX.TabControl,
  uFmxPhysicsDemo, FMX.ListView.Types, FMX.ListView, System.Generics.Collections;

type
  TFormFmxPhysics = class(TForm)
    Button4: TButton;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    Label19: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Text2: TLabel;
    ScrollBar1: TScrollBar;
    StringComboBox1: TComboBox;
    TrackBar6: TTrackBar;
    ScrollBar2: TScrollBar;
    RadioButton1: TRadioButton;
    TextBox1: TEdit;
    CheckBox1: TCheckBox;
    ButtonPhysics: TButton;
    ToolBar1: TToolBar;
    ResetBtn: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure ButtonPhysicsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
  private
    FFmxPhysicsDemo: TFmxPhysicsDemo;
  public
    { Public declarations }
    procedure ConstructWorld;
    function GetControls: TList<TControl>;
  end;

var
  FormFmxPhysics: TFormFmxPhysics;

implementation

{$R *.fmx}

uses System.Math, uFmxControlHelper;

procedure TFormFmxPhysics.ButtonPhysicsClick(Sender: TObject);
begin
  if not FFmxPhysicsDemo.IsRunning then
    FFmxPhysicsDemo.Start
  else
    FFmxPhysicsDemo.Stop;
end;

procedure TFormFmxPhysics.FormDestroy(Sender: TObject);
begin
  FFmxPhysicsDemo.Free;
end;

procedure TFormFmxPhysics.FormResize(Sender: TObject);
begin
  ConstructWorld;
end;

procedure TFormFmxPhysics.FormShow(Sender: TObject);
begin
  ConstructWorld;
end;

function TFormFmxPhysics.GetControls: TList<TControl>;
begin
  Result := TList<TControl>.Create;
  Result.Add(CheckBox1);
  Result.Add(TextBox1);
  Result.Add(RadioButton1);
  Result.Add(StringComboBox1);
  Result.Add(TrackBar6);
  Result.Add(ScrollBar1);
  Result.Add(ScrollBar2);
  Result.Add(Button4);
  Result.Add(ProgressBar1);
  Result.Add(Panel1);
  Result.Add(Text2);
end;

procedure TFormFmxPhysics.ConstructWorld;
var
  CtlList: TList<TControl>;
  Ctl: TControl;
begin
  FFmxPhysicsDemo.Free;
  FFmxPhysicsDemo := TFmxPhysicsDemo.Create;

  FFmxPhysicsDemo.AddScreenSize(ClientWidth, ClientHeight);
  CtlList := GetControls;
  try
    for Ctl in CtlList do
      FFmxPhysicsDemo.AddControl(Ctl);
  finally
    CtlList.Free;
  end;
end;

procedure TFormFmxPhysics.ResetBtnClick(Sender: TObject);

  procedure Shuffle(list : TList<TControl>);
  var
    R, I: Integer;
  begin
    Randomize;
    for I := 0 to list.Count-1 do
    begin
      R := Random(-I + list.Count);
      list.Exchange(I, I + R);
    end;
  end;

const
  XPad = 10;
  YPad = 10;
var
  CtlList: TList<TControl>;
  Ctl: TControl;
  X, Y, Highest: Single;
  W, H: Single;
begin
  FFmxPhysicsDemo.Stop;
  CtlList := GetControls;
  try
    Shuffle(CtlList);
    Highest := 0;
    X := XPad;
    Y := ToolBar1.Height + YPad;
    W := Self.Width;
    H := Self.Height;

    for Ctl in CtlList do
    begin
      Highest := Max(Highest, Ctl.Height);
      if (X + Ctl.Width) > W then
      begin
        X := XPad;
        Y := Y + Highest + YPad;
        Highest := Ctl.Height;
      end;
      Ctl.Position.X := X;
      Ctl.Position.Y := Y;
      Ctl.SetRotation(0);
      X := X + Ctl.Width + XPad;
    end;
  finally
    CtlList.Free;
  end;
  ConstructWorld;
end;

end.
