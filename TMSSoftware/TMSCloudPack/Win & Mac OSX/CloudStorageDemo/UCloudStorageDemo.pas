{***************************************************************************}
{ TMS DropBox Cloud access demo                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit UCloudStorageDemo;

interface

uses
  FMX.Forms, SysUtils, FMX.TMSCloudDropBox,FMX.TMSCloudGDrive, UITypes, FMX.Dialogs, FMX.TMSCloudBoxNet,
  FMX.Header, FMX.TMSCloudWinLive, FMX.TMSCloudBase, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, FMX.Controls, System.Classes, FMX.Types,
  FMX.TMSCloudBitCasa, FMX.TMSCloudCustomBitCasa, FMX.TMSCloudCustomWinLive,
  FMX.TMSCloudCustomGDrive, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomDropBox,
  FMX.TMSCloudCustomBoxNet, FMX.TMSCloudCustomAmazonCloudDrive,
  FMX.TMSCloudAmazonCloudDrive, FMX.TMSCloudCustomOpenStack,
  FMX.TMSCloudCustomHubic, FMX.TMSCloudHubic;

type
  TForm4 = class(TForm)
    TMSFMXCloudBoxNetDrive1: TTMSFMXCloudBoxNetDrive;
    TMSFMXCloudDropBox1: TTMSFMXCloudDropBox;
    TMSFMXCloudGDrive1: TTMSFMXCloudGDrive;
    TMSFMXCloudSkyDrive1: TTMSFMXCloudSkyDrive;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    RadioGroup1: TPanel;
    ConnectBtn: TButton;
    Panel2: TPanel;
    TreeView1: TTreeView;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FileName: TLabel;
    Created: TLabel;
    Size: TLabel;
    DownloadBtn: TButton;
    UploadBtn: TButton;
    ProgressBar1: TProgressBar;
    DisconnectBtn: TButton;
    CreateFolderBtn: TButton;
    DeleteBtn: TButton;
    clrAccess: TCheckBox;
    ProgressLabel: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TMSFMXCloudBitCasa1: TTMSFMXCloudBitCasa;
    RadioButton5: TRadioButton;
    TMSFMXCloudAmazonCloudDrive1: TTMSFMXCloudAmazonCloudDrive;
    RadioButton6: TRadioButton;
    TMSFMXCloudHubic1: TTMSFMXCloudHubic;
    RadioButton7: TRadioButton;
    procedure ConnectBtnClick(Sender: TObject);
    procedure TMSFMXCloudDropBox1ReceivedAccessToken(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
    procedure UploadBtnClick(Sender: TObject);
    procedure TMSFMXCloudDropBox1UploadProgress(Sender: TObject; FileName: string;
      Position, Total: Int64);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure CreateFolderBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure TMSFMXCloudDropBox1AuthFormClose(Sender: TObject);
    procedure RadioButton4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    rdg: Integer;
    Authenticated: boolean;
    IsDownloading: boolean;
    IsUploading: boolean;
    function Storage: TTMSFMXCloudStorageFMX;
    procedure ShowItem;
    procedure DoConnect;
    procedure DoDisconnect;
  end;

var
  Form4: TForm4;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  DropBoxAppkey = 'xxxxxxxxx';
//  DropBoxAppSecret = 'yyyyyyyy';
//
//  WinLiveClientID = 'xxxxxxxxxxx';
//  WinLiveClientSecret = 'yyyyyyyyyy';
//
//  GoogleAppKey = 'xxxxxxxxxxx';
//  GoogleAppSecret = 'yyyyyyyyyyy';
//
//  BoxNetAppKey = 'xxxxxxxxxxx';
//  BoxNetAppSecret = 'yyyyyyyyyyy';
//
//  BitCasaAppKey = 'xxxxxxxxxxx';
//  BitCasaAppSecret = 'yyyyyyyyyyy';
//
//  AmazonCloudDriveAppKey = 'xxxxxxxxxxx';
//  AmazonCloudDriveAppSecret = 'yyyyyyyyyyy';
//  AmazonCloudDriveCallBack = 'zzzzzzzzzzz';
//
//  HubicAppKey := 'xxxxxxxxxxx';
//  HubicAppSecret := 'yyyyyyyyyyy';
//  HubicCallBack = 'zzzzzzzzzzz';

{$I APPIDS.INC}

procedure TForm4.TMSFMXCloudDropBox1AuthFormClose(Sender: TObject);
begin
  if not Authenticated then
    DoDisconnect;
end;

procedure TForm4.TMSFMXCloudDropBox1ReceivedAccessToken(Sender: TObject);
var
  cs: TTMSFMXCloudStorageFMX;
begin
  Authenticated := true;

  if (Sender is TTMSFMXCloudStorageFMX) then
  begin
    cs := Sender as TTMSFMXCloudStorageFMX;
    cs.GetDriveInfo;
    cs.FillTreeView(TreeView1);
    cs.SaveTokens;
  end;

  DoConnect;
end;

procedure TForm4.TMSFMXCloudDropBox1UploadProgress(Sender: TObject; FileName: string;
  Position, Total: Int64);
begin
  progressbar1.Value := Position;
  ProgressBar1.Max := Total;
  if IsUploading then
    Progresslabel.Text  := InttoStr(Position) +' of ' + InttoStr(Total) +' uploaded';
end;


procedure TForm4.CreateFolderBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
  FolderName: string;

begin
  FolderName := '';

  if InputQuery('Name','Name for new folder',FolderName) and (FolderName <> '') then
  begin
    ci := nil;

    if Assigned(TreeView1.Selected) then
    begin
      ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
      if ci.ItemType <> ciFolder then
        ci := nil;
    end;

    Storage.CreateFolder(ci,FolderName);
    Storage.FillTreeView(Treeview1);
  end;
end;

procedure TForm4.ConnectBtnClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXClouddropbox1.App.Key := DropBoxAppKey;
  TMSFMXClouddropbox1.App.Secret := DropBoxAppSecret;
  TMSFMXCloudDropBox1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'dropbox.ini';

  TMSFMXCloudskydrive1.App.Key := WinLiveClientID;
  TMSFMXCloudskydrive1.App.Secret := WinLiveClientSecret;
  TMSFMXCloudskydrive1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'skydrive.ini';

  TMSFMXCloudgdrive1.App.Key := GoogleAppKey;
  TMSFMXCloudgdrive1.App.Secret := GoogleAppSecret;
  TMSFMXCloudgdrive1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'gdrive.ini';

  TMSFMXCloudBoxNetDrive1.App.Key := BoxNetAppKey;
  TMSFMXCloudBoxNetDrive1.App.Secret := BoxNetAppSecret;
  TMSFMXCloudBoxNetDrive1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'boxnetdrive.ini';

  TMSFMXCloudBitCasa1.App.Key := BitCasaAppKey;
  TMSFMXCloudBitCasa1.App.Secret := BitCasaAppSecret;
  TMSFMXCloudBitCasa1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'bitcasadrive.ini';

  TMSFMXCloudAmazonCloudDrive1.App.Key := AmazonCloudDriveAppKey;
  TMSFMXCloudAmazonCloudDrive1.App.Secret := AmazonCloudDriveAppSecret;
  TMSFMXCloudAmazonCloudDrive1.App.CallBackURL := AmazonCloudDriveCallBack;
  TMSFMXCloudAmazonCloudDrive1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'amazondrive.ini';

  TMSFMXCloudHubic1.App.Key := HubicAppKey;
  TMSFMXCloudHubic1.App.Secret := HubicAppSecret;
  TMSFMXCloudHubic1.App.CallBackURL := HubicCallBack;
  TMSFMXCloudHubic1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'hubic.ini';

  radiogroup1.Enabled := false;

  if Storage.App.Key <> '' then
  begin
    Storage.LoadTokens;

    acc := Storage.TestTokens;

    if not acc then
      acc := Storage.RefreshAccess;

    if not acc then
      Storage.DoAuth
    else
    begin
      Storage.GetDriveInfo;
      Storage.FillTreeView(TreeView1);
      DoConnect;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the Storage component');
end;


procedure TForm4.DeleteBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
begin
  if Assigned(treeview1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);

    if MessageDlg('Are you sure to delete item:'+ ci.FileName, TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
    begin
      Storage.Delete(ci);
      ci.Free;
      TreeView1.RemoveObject(TreeView1.Selected);
    end;
  end;
end;

procedure TForm4.DisconnectBtnClick(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TForm4.DoConnect;
begin
  ConnectBtn.Enabled := false;
  DisconnectBtn.Enabled := true;
  UploadBtn.Enabled := true;
  RadioGroup1.Enabled := false;
  CreateFolderBtn.Enabled := true;
  DeleteBtn.Enabled := true;
end;

procedure TForm4.DoDisconnect;
begin
  if clrAccess.IsChecked  then
    Storage.ClearTokens;

  Authenticated := false;
  TreeView1.Clear;
  ConnectBtn.Enabled := true;
  DisconnectBtn.Enabled := false;
  UploadBtn.Enabled := false;
  DownloadBtn.Enabled := false;
  CreateFolderBtn.Enabled := false;
  RadioGroup1.Enabled := true;
  DeleteBtn.Enabled := false;
  Size.Text  := '';
  Created.Text  := '';
  FileName.Text  := '';
end;

procedure TForm4.DownloadBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
  sv: TSaveDialog;
begin
  if Assigned(treeview1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    if ci.ItemType = ciFile then
    begin
      sv := TSaveDialog.Create(Self);
      sv.FileName := ci.FileName;
      if sv.Execute then
      begin
        ProgressBar1.Value := 0;
        ProgressBar1.Visible := true;
        IsDownloading := true;
        Storage.Download(ci,sv.FileName);
        ProgressBar1.Visible := false;
        IsDownloading := false;
        ProgressLabel.Text  := '';
        ShowMessage('File ' +  ci.FileName + ' downloaded');
      end;

      sv.Free;
    end;
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  rdg := 0;
end;

procedure TForm4.RadioButton4Change(Sender: TObject);
begin
  rdg := (Sender as TRadioButton).Tag;
end;

procedure TForm4.UploadBtnClick(Sender: TObject);
var
  fn: string;
  ci, nci: TTMSFMXCloudItem;
begin
  if OpenDialog1.Execute then
  begin
    fn := opendialog1.FileName;

    ProgressBar1.Value := 0;
    ProgressBar1.Visible := true;

    if Assigned(TreeView1.Selected) then
    begin
      IsUploading := true;
      ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
      nci := Storage.Upload(ci, fn);
      if Assigned(nci) then
        Storage.FillTreeView(TreeView1);
      IsUploading := false;
    end;

    ProgressBar1.Visible := false;
    ProgressLabel.Text  := '';
  end;
end;

procedure TForm4.ShowItem;
var
  ci: TTMSFMXCloudItem;
begin
  if Assigned(TreeView1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    if (ci is TBoxNetItem) then
      (ci as TBoxNetItem).LoadFileInfo;

    FileName.AutoSize := False;
    FileName.Text  := ci.FileName;
    FileName.Width := 1000;
    FileName.AutoSize := True;

    Created.AutoSize := False;
    if ci.CreationDate = 0 then
      Created.Text  := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat,ci.ModifiedDate)
    else
      Created.Text  := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat,ci.CreationDate);
    Created.Width := 1000;
    Created.AutoSize := True;

    Size.AutoSize := False;
    Size.Text  := IntToStr(ci.Size);
    Size.Width := 1000;
    Size.AutoSize := True;

    DownloadBtn.Enabled := ci.ItemType = ciFile;
  end;
end;

function TForm4.Storage: TTMSFMXCloudStorageFMX;
begin
  Result := nil;
  case rdg of
  0: Result := TMSFMXCloudDropBox1;
  1: Result := TMSFMXCloudGDrive1;
  2: Result := TMSFMXCloudSkyDrive1;
  3: Result := TMSFMXCloudBoxNetDrive1;
  4: Result := TMSFMXCloudBitCasa1;
  5: Result := TMSFMXCloudAmazonCloudDrive1;
  6: Result := TMSFMXCloudHubic1;
  end;
end;

procedure TForm4.TreeView1Change(Sender: TObject);
begin
  ShowItem;
end;

end.

