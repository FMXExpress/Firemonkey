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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.TreeView, FMX.TMSCloudWinLive, FMX.TMSCloudBoxNet, FMX.TMSCloudGDrive,
  FMX.TMSCloudBase, FMX.TMSCloudDropBox, FMX.Objects,
  FMX.Edit, FMX.Memo, FMX.TMSCloudImage, FMX.TMSCloudWebBrowser,
  FMX.TMSCloudBitCasa, FMX.TMSCloudCustomBitCasa, FMX.TMSCloudCustomWinLive,
  FMX.TMSCloudCustomBoxNet, FMX.TMSCloudCustomGDrive, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomDropBox, iOUtils, FMX.TMSCloudCustomAmazonCloudDrive,
  FMX.TMSCloudAmazonCloudDrive, FMX.TMSCloudCustomOpenStack,
  FMX.TMSCloudCustomHubic, FMX.TMSCloudHubic;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    ConnectBtn: TButton;
    DisconnectBtn: TButton;
    DownloadBtn: TButton;
    CreateFolderBtn: TButton;
    UploadBtn: TButton;
    DeleteBtn: TButton;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Panel2: TPanel;
    TMSFMXCloudDropBox1: TTMSFMXCloudDropBox;
    TMSFMXCloudGDrive1: TTMSFMXCloudGDrive;
    TMSFMXCloudBoxNetDrive1: TTMSFMXCloudBoxNetDrive;
    TMSFMXCloudSkyDrive1: TTMSFMXCloudSkyDrive;
    radiogroup1: TRectangle;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TMSFMXCloudBitCasa1: TTMSFMXCloudBitCasa;
    RadioButton5: TRadioButton;
    Line1: TLine;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Size: TLabel;
    Created: TLabel;
    FileName: TLabel;
    Edit1: TEdit;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Memo1: TMemo;
    Label4: TLabel;
    TMSFMXCloudAmazonCloudDrive1: TTMSFMXCloudAmazonCloudDrive;
    RadioButton6: TRadioButton;
    TMSFMXCloudHubic1: TTMSFMXCloudHubic;
    RadioButton7: TRadioButton;
    procedure ConnectBtnClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure UploadBtnClick(Sender: TObject);
    procedure CreateFolderBtnClick(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure TMSFMXCloudDropBox1AuthFormClose(Sender: TObject);
    procedure TMSFMXCloudDropBox1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Authenticated: boolean;
    function Storage: TTMSFMXCloudStorageFMX;
    procedure ShowItem;
    procedure DoConnect;
    procedure DoDisconnect;
  end;

var
  Form82: TForm82;

implementation

{$R *.fmx}

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
//  AmazonCloudDriveAppKey = '';
//  AmazonCloudDriveAppSecret = '';
//  AmazonCloudDriveCallBack = '';
//
//  HubicAppKey = '';
//  HubicAppSecret = '';
//  HubicCallBack = '';

{$I APPIDS.INC}

procedure TForm82.ConnectBtnClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXClouddropbox1.App.Key := DropBoxAppKey;
  TMSFMXClouddropbox1.App.Secret := DropBoxAppSecret;

  TMSFMXCloudskydrive1.App.Key := WinLiveClientID;
  TMSFMXCloudskydrive1.App.Secret := WinLiveClientSecret;

  TMSFMXCloudgdrive1.App.Key := GoogleAppKey;
  TMSFMXCloudgdrive1.App.Secret := GoogleAppSecret;

  TMSFMXCloudBoxNetDrive1.App.Key := BoxNetAppKey;
  TMSFMXCloudBoxNetDrive1.App.Secret := BoxNetAppSecret;

  TMSFMXCloudBitCasa1.App.Key := BitCasaAppKey;
  TMSFMXCloudBitCasa1.App.Secret := BitCasaAppSecret;

  TMSFMXCloudAmazonCloudDrive1.App.Key := AmazonCloudDriveAppKey;
  TMSFMXCloudAmazonCloudDrive1.App.Secret := AmazonCloudDriveAppSecret;
  TMSFMXCloudAmazonCloudDrive1.App.CallBackURL := AmazonCloudDriveCallBack;

  TMSFMXCloudHubic1.App.Key := HubicAppKey;
  TMSFMXCloudHubic1.App.Secret := HubicAppSecret;
  TMSFMXCloudHubic1.App.CallBackURL := HubicCallBack;

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

procedure TForm82.CreateFolderBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
  FolderName: string;

begin
  {$IFDEF IOS}
  if MessageDlg('Do you want to create a new folder ?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
  {$ENDIF}
  begin
    FolderName := Edit1.Text;

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

procedure TForm82.DeleteBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
begin
  if Assigned(treeview1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);

    {$IFDEF IOS}
    if MessageDlg('Are you sure you want to delete the file: '+ ci.FileName, TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
    {$ENDIF}
    begin
      Storage.Delete(ci);
      ci.Free;
      TreeView1.RemoveObject(TreeView1.Selected);
    end;
  end;
end;

procedure TForm82.DisconnectBtnClick(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TForm82.DoConnect;
begin
  ConnectBtn.Enabled := false;
  DisconnectBtn.Enabled := true;
  UploadBtn.Enabled := true;
  RadioGroup1.Enabled := false;
  CreateFolderBtn.Enabled := true;
  DeleteBtn.Enabled := true;
end;

procedure TForm82.DoDisconnect;
begin
  Authenticated := false;
  TreeView1.Clear;
  ConnectBtn.Enabled := true;
  DisconnectBtn.Enabled := false;
  UploadBtn.Enabled := false;
  DownloadBtn.Enabled := false;
  CreateFolderBtn.Enabled := false;
  RadioGroup1.Enabled := true;
  DeleteBtn.Enabled := false;
  Size.Text := '';
  Created.Text := '';
  FileName.Text := '';
end;

procedure TForm82.DownloadBtnClick(Sender: TObject);
var
  ci: TTMSFMXCloudItem;
  fn: String;
begin
  if Assigned(treeview1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    if ci.ItemType = ciFile then
    begin
      {$IFDEF IOS}
      if MessageDlg('Do you want to download and open the file: '+ ci.FileName, TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
      {$ENDIF}
      begin
        fn := TPath.GetDocumentsPath + '/' + ci.FileName;
        Storage.Download(ci, fn);
        ShowMessage('File ' +  ci.FileName + ' downloaded');
      end;
    end;
  end;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(ParamStr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
  TMSFMXCloudDropBox1.PersistTokens.Key := TPath.GetDocumentsPath+'/DTOKENS.INI';
  TMSFMXCloudGDrive1.PersistTokens.Key := TPath.GetDocumentsPath+'/GTOKENS.INI';
  TMSFMXCloudBoxNetDrive1.PersistTokens.Key := TPath.GetDocumentsPath+'/BTOKENS.INI';
  TMSFMXCloudSkyDrive1.PersistTokens.Key := TPath.GetDocumentsPath+'/STOKENS.INI';
  TMSFMXCloudBitCasa1.PersistTokens.Key := TPath.GetDocumentsPath+'/BCTOKENS.INI';
  TMSFMXCloudAmazonCloudDrive1.PersistTokens.Key := TPath.GetDocumentsPath+'/ATOKENS.INI';
  TMSFMXCloudHubic1.PersistTokens.Key := TPath.GetDocumentsPath+'/HTOKENS.INI';
end;

procedure TForm82.ShowItem;
var
  ci: TTMSFMXCloudItem;
  fn: string;
begin
  if Assigned(TreeView1.Selected) then
  begin
    ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    if (ci is TBoxNetItem) then
      (ci as TBoxNetItem).LoadFileInfo;

    FileName.AutoSize := False;
    FileName.Text := ci.FileName;
    FileName.Width := 1000;
    FileName.AutoSize := True;

    Memo1.Visible := False;
    TMSFMXCloudImage1.Visible := False;
    TMSFMXCloudImage1.URL := '';
    if (ExtractFileExt(UpperCase(FileName.Text)) = '.PNG') or (ExtractFileExt(UpperCase(FileName.Text)) = '.JPG')
      or (ExtractFileExt(UpperCase(FileName.Text)) = '.JPEG') then
    begin
      if ci is TBoxNetItem then
        fn := (ci as TBoxNetItem).Share
      else if ci is TDropBoxItem then
        fn := (ci as TDropBoxItem).Link
      else if (ci is TSkyDriveItem) then
        fn := (ci as TSkyDriveItem).Link
      else if (ci is TGDriveItem) then
        fn := (ci as TGDriveItem).DownloadURL;

      TMSFMXCloudImage1.Visible := True;
      TMSFMXCloudImage1.URL := fn;
    end
    else
    begin
      if ci is TBoxNetItem then
        fn := (ci as TBoxNetItem).Description
      else if ci is TDropBoxItem then
        fn := (ci as TDropBoxItem).FileName
      else if (ci is TSkyDriveItem) then
        fn := (ci as TSkyDriveItem).Description
      else if (ci is TGDriveItem) then
        fn := (ci as TGDriveItem).Description
      else if (ci is TBitCasaItem) then
        fn := (ci as TBitCasaItem).Description;

      Memo1.Visible := True;
      Memo1.Text := fn;
    end;


    Created.Width := 1000;
    Created.AutoSize := True;
    Created.Text := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat,ci.CreationDate);
    Size.Width := 1000;
    Size.AutoSize := True;
    Size.Text := IntToStr(ci.Size);
    Size.AutoSize := True;
    DownloadBtn.Enabled := ci.ItemType = ciFile;
  end;
end;

function TForm82.Storage: TTMSFMXCloudStorageFMX;
begin
  Result := nil;
  if RadioButton1.IsChecked then
    Result := TMSFMXCloudDropBox1
  else if RadioButton2.IsChecked then
    Result := TMSFMXCloudGDrive1
  else if RadioButton3.IsChecked then
    Result := TMSFMXCloudSkyDrive1
  else if RadioButton4.IsChecked then
    Result := TMSFMXCloudBoxNetDrive1
  else if RadioButton5.IsChecked then
    Result := TMSFMXCloudBitCasa1
  else if RadioButton6.IsChecked then
    Result := TMSFMXCloudAmazonCloudDrive1
  else if RadioButton7.IsChecked then
    Result := TMSFMXCloudHubic1;
end;

procedure TForm82.TMSFMXCloudDropBox1AuthFormClose(Sender: TObject);
begin
  if not Authenticated then
    DoDisconnect;
end;

procedure TForm82.TMSFMXCloudDropBox1ReceivedAccessToken(Sender: TObject);
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

procedure TForm82.TreeView1Change(Sender: TObject);
begin
  ShowItem;
end;

procedure TForm82.UploadBtnClick(Sender: TObject);
var
  fn: string;
  ci, nci: TTMSFMXCloudItem;
begin
  {$IFDEF IOS}
  if MessageDlg('Do you want to upload a sample file?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
  {$ENDIF}
  begin
    fn := TPath.GetDocumentsPath + '/sample.jpg';
    if Assigned(TreeView1.Selected) then
    begin
      ci := TTMSFMXCloudItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
      nci := Storage.Upload(ci, fn);
      if Assigned(nci) then
        Storage.FillTreeView(TreeView1);
    end;
  end;
end;

end.
