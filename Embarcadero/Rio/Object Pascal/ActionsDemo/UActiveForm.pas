//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UActiveForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TActiveForm = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Layout1: TLayout;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TActiveForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TActiveForm.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TActiveForm.Timer1Timer(Sender: TObject);
begin
  if Screen.ActiveForm <> nil then
  begin
    Edit1.Text := Screen.ActiveForm.Name;
    Edit2.Text := Screen.ActiveForm.Caption;
  end
  else
  begin
    Edit1.Text := '<nil>';
    Edit2.Text := '<nil>';
  end;
  if Screen.FocusControl <> nil then
  begin
    Edit3.Text := Screen.FocusControl.GetObject.Name;
    Edit4.Text := Screen.FocusControl.GetObject.ClassName;
  end
  else
  begin
    Edit3.Text := '<nil>';
    Edit4.Text := '<nil>';
  end;
end;

end.
