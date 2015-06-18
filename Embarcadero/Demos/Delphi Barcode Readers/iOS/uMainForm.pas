unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls,
  FMX.Objects, FMX.Platform.iOS, FMX.Layouts, FMX.Memo, FMX.ListBox,
  libzbar;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    swtONOFF: TSwitch;
    butClear: TButton;
    Label1: TLabel;
    edtResult: TEdit;
    btnCopy: TButton;
    memImage: TMemo;
    lstHistory: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure swtONOFFSwitch(Sender: TObject);
    procedure butClearClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private declarations }
    ZBarCode: TZBarCode;
    procedure OnFindBarCode(Sender: TObject; BarCode: String);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses iOSapi.CoreGraphics;

procedure TMainForm.butClearClick(Sender: TObject);
begin
  edtResult.Text := '';
  lstHistory.Items.Clear;
end;

procedure TMainForm.btnCopyClick(Sender: TObject);
begin
  edtResult.SelectAll;
  edtResult.CopyToClipboard;
end;

procedure TMainForm.OnFindBarCode(Sender: TObject; BarCode: String);
begin
  edtResult.Text := BarCode;
  lstHistory.Items.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) + ' - ' + BarCode);
end;

procedure TMainForm.swtONOFFSwitch(Sender: TObject);
begin
  if not Assigned(ZBarCode) then
  begin
    ZBarCode := TZBarCode.Create;
    ZBarCode.OnBarCode := OnFindBarCode;
    ZBarCode.setFrame(WindowHandleToPlatform(Self.Handle).View,
      CGRectMake(memImage.Position.X, memImage.Position.Y, memImage.Width,
      memImage.Height));
  end;

  ZBarCode.Active := swtONOFF.IsChecked;
end;

end.
