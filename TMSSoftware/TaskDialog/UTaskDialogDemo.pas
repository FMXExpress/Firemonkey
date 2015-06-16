unit UTaskDialogDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSBaseGroup, FMX.TMSCheckGroup, FMX.TMSTaskDialog,
  FMX.Layouts, FMX.ListBox;

type
  TForm4 = class(TForm)
    TMSFMXTaskDialog1: TTMSFMXTaskDialog;
    btShow: TButton;
    TMSFMXCheckGroup1: TTMSFMXCheckGroup;
    Label1: TLabel;
    lbEvents: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXCheckGroup1CheckBoxChange(Sender: TObject; Index: Integer);
    procedure btShowClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Init;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.btShowClick(Sender: TObject);
var
  s: string;
begin
  TMSFMXTaskDialog1.Show(
  procedure(ButtonID: Integer)
  begin
    case ButtonID of
      mrOk: s := 'OK';
      mrCancel: s := 'Cancel';
      mrYes: s := 'Yes';
      mrNo: s := 'No';
    else
      s := 'Other';
    end;
    lbEvents.Items.Add(s + ' button clicked');

    if TMSFMXTaskDialog1.InputSettings.InputType <> None then
      lbEvents.Items.Add('Input text: ' + TMSFMXTaskDialog1.InputSettings.Text);
  end
  );
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  it: TTMSFMXGroupItem;
begin
  TMSFMXCheckGroup1.BeginUpdate;
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show verification checkbox';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show expand/collaps button';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show title';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show footer';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show input';
  it := TMSFMXCheckGroup1.Items.Add;
  it.Text := 'Show OK/Cancel buttons';
  TMSFMXCheckGroup1.CheckAll;
  TMSFMXCheckGroup1.EndUpdate;

  TMSFMXCheckGroup1.IsChecked[4] := false;
  TMSFMXCheckGroup1.IsChecked[5] := false;

  TMSFMXTaskDialog1.InstructionText := 'Do you like the TMS TaskDialog for Firemonkey?';
  TMSFMXTaskDialog1.Content := 'The <b>TaskDialog for Firemonkey</b> provides an enhanced way for interacting with the user.';
  Init;
end;

procedure TForm4.Init;
begin
  if TMSFMXCheckGroup1.IsChecked[0] then
     TMSFMXTaskDialog1.VerificationText := 'Verification text'
  else
     TMSFMXTaskDialog1.VerificationText := '';

  if TMSFMXCheckGroup1.IsChecked[1] then
     TMSFMXTaskDialog1.ExpandedText := '<b><font color="#FF0000">Expanded text</font></b>'
  else
     TMSFMXTaskDialog1.ExpandedText := '';

  if TMSFMXCheckGroup1.IsChecked[2] then
    TMSFMXTaskDialog1.Title := 'TaskDialog with expandable text, footer and input'
  else
    TMSFMXTaskDialog1.Title := '';

  if TMSFMXCheckGroup1.IsChecked[3] then
     TMSFMXTaskDialog1.FooterText := 'Footer text'
  else
     TMSFMXTaskDialog1.FooterText := '';

  if TMSFMXCheckGroup1.IsChecked[4] then
     TMSFMXTaskDialog1.InputSettings.InputType := TFMXInputType.Edit
  else
     TMSFMXTaskDialog1.InputSettings.InputType := TFMXInputType.None;

  if TMSFMXCheckGroup1.IsChecked[5] then
    TMSFMXTaskDialog1.CommonButtons := [OK, Cancel]
  else
    TMSFMXTaskDialog1.CommonButtons := [Yes, No];
end;

procedure TForm4.TMSFMXCheckGroup1CheckBoxChange(Sender: TObject;
  Index: Integer);
begin
  Init;
end;

end.
