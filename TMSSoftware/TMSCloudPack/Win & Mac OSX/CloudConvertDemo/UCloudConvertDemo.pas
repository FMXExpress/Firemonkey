unit UCloudConvertDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomConvert,
  FMX.TMSCloudConvert, FMX.Objects, FMX.ListBox, FMX.Layouts, FMX.Memo;

type
  TForm11 = class(TForm)
    TMSFMXCloudConvert1: TTMSFMXCloudConvert;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    lbInputFile: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    lbProgress: TLabel;
    ProgressBar1: TProgressBar;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbStartTime: TLabel;
    lbEndTime: TLabel;
    lbInputFileSize: TLabel;
    lbOutputFileSize: TLabel;
    lbOutputFileName: TLabel;
    lbOutputDownload: TLabel;
    Label4: TLabel;
    cbOutput: TComboBox;
    btConvert: TButton;
    Image1: TImage;
    Memo1: TMemo;
    procedure btConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXCloudConvert1DownloadProgress(Sender: TObject;
      FileName: string; Position, Total: Int64);
    procedure TMSFMXCloudConvert1UploadProgress(Sender: TObject;
      FileName: string; Position, Total: Int64);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitLabels;
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  CloudConvertAppkey = 'xxxxxxxxx';

{$I APPIDS.INC}

procedure TForm11.btConvertClick(Sender: TObject);
var
  sv: TSaveDialog;
  InputFile, OutputFile, OutputFormat: string;
  txtFile: TStringList;
begin
  ProgressBar1.Value := 0;
  InitLabels;
  btConvert.Enabled := false;

  InputFile := lbInputFile.Text;
  OutputFormat := cbOutput.Items[cbOutput.ItemIndex];
  OutputFile := 'OrderTMScomponents.' + OutputFormat;

  sv := TSaveDialog.Create(Self);
  sv.FileName := OutputFile;
  if sv.Execute then
  begin
    if TMSFMXCloudConvert1.ConvertAndDownload(InputFile, sv.FileName) then
    begin
      lbStartTime.Text := DateTimeToStr(TMSFMXCloudConvert1.ConvertResult.StartTime);
      lbEndTime.Text := DateTimeToStr(TMSFMXCloudConvert1.ConvertResult.EndTime);
      lbOutputDownload.Text := DateTimeToStr(TMSFMXCloudConvert1.ConvertResult.ExpiryTime);
      lbInputFileSize.Text := IntToStr( TMSFMXCloudConvert1.ConvertResult.InputFile.FileSize);
      lbOutputFileSize.Text := IntToStr( TMSFMXCloudConvert1.ConvertResult.OutputFile.FileSize);
      lbOutputFileName.Text := TMSFMXCloudConvert1.ConvertResult.OutputFile.FileName;

      if FileExists(sv.FileName) then
      begin
        if OutputFormat <> 'TXT' then
        begin
          Image1.Visible := true;
          Memo1.Visible := false;
          Image1.Bitmap.LoadFromFile(sv.FileName);
        end
        else
        begin
          Image1.Visible := false;
          Memo1.Visible := true;
          txtFile := TStringList.Create;
          txtFile.LoadFromFile(sv.FileName);
          Memo1.Text := txtFile.Text;
          txtFile.Free;
        end;
      end;
    end
    else
    begin
      lbStartTime.Text := 'Conversion failed, please try again.';
    end;
  end;

  btConvert.Enabled := true;
end;

procedure TForm11.FormCreate(Sender: TObject);
begin
  InitLabels;
  cbOutput.ItemIndex := 0;

  TMSFMXCloudConvert1.App.Key := CloudConvertAppKey;
end;

procedure TForm11.InitLabels;
begin
  lbStartTime.Text := '';
  lbEndTime.Text := '';
  lbOutputDownload.Text := '';
  lbInputFileSize.Text := '';
  lbOutputFileSize.Text := '';
  lbOutputFileName.Text := '';
end;

procedure TForm11.TMSFMXCloudConvert1DownloadProgress(Sender: TObject;
  FileName: string; Position, Total: Int64);
begin
  ProgressBar1.Value := Position;
  ProgressBar1.Max := Total;
  lbProgress.Text := InttoStr(Position) +' of ' + InttoStr(Total) +' downloaded';
end;

procedure TForm11.TMSFMXCloudConvert1UploadProgress(Sender: TObject;
  FileName: string; Position, Total: Int64);
begin
  ProgressBar1.Value := Position;
  ProgressBar1.Max := Total;
  lbProgress.Text := InttoStr(Position) +' of ' + InttoStr(Total) +' uploaded';
end;

end.
