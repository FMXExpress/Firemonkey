unit TextFileFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  uSkinFireMonkeyLabel,
  uSkinFireMonkeyButton,
  uSkinFireMonkeyControl,
  uSkinFireMonkeyImage,
  FMX.Layouts,
  FMX.Memo,
  //  uSkinFireMonkeyMemo,
  uFileManage,
  uUIFunction, uSkinFireMonkeyPanel;

type
  TFrameTextFile = class(TFrame)
    memTextFile: TMemo;
    pnlToolBar: TSkinFMXPanel;
    btnEdit: TSkinFMXButton;
    btnReturn: TSkinFMXButton;
    lblFileName: TSkinFMXLabel;
    btnSave: TSkinFMXButton;
    btnCancel: TSkinFMXButton;
    procedure btnEditClick(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FIsEdit:Boolean;
    FFileItem:TFileItem;
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
  public
    function SetEdit(AIsEdit:Boolean):Boolean;
    function LoadFile(AFileItem:TFileItem):Boolean;
    { Public declarations }
  end;


var
  GlobalTextFileFrame:TFrameTextFile;

implementation



{$R *.fmx}

procedure TFrameTextFile.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
  FreeAndNil(GlobalTextFileFrame);
end;

procedure TFrameTextFile.btnCancelClick(Sender: TObject);
begin
  memTextFile.Lines.LoadFromFile(FFileItem.FilePath);
  SetEdit(False);
end;

procedure TFrameTextFile.btnEditClick(Sender: TObject);
begin
  SetEdit(True);
end;

procedure TFrameTextFile.btnSaveClick(Sender: TObject);
begin
  memTextFile.Lines.SaveToFile(FFileItem.FilePath);
  SetEdit(False);
end;

function TFrameTextFile.SetEdit(AIsEdit: Boolean): Boolean;
begin
  FIsEdit:=AIsEdit;
  if FIsEdit then
  begin
    memTextFile.ReadOnly:=False;
    btnEdit.Visible:=False;
    btnSave.Visible:=True;
    btnCancel.Visible:=True;
    btnReturn.Visible:=False;
  end
  else
  begin
    memTextFile.ReadOnly:=True;
    btnEdit.Visible:=True;
    btnSave.Visible:=False;
    btnCancel.Visible:=False;
    btnReturn.Visible:=True;
  end;
end;

function TFrameTextFile.LoadFile(AFileItem: TFileItem): Boolean;
begin
  Result:=False;

  FFileItem:=AFileItem;

  lblFileName.Text:=FFileItem.FileName;
  memTextFile.Lines.LoadFromFile(FFileItem.FilePath);

  SetEdit(False);

  Result:=True;
end;

procedure TFrameTextFile.OnReturnFrame(FromFrame: TFrame);
begin
end;

end.
