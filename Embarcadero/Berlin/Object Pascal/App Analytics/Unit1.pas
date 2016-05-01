//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Analytics, FMX.Analytics.AppAnalytics, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    AppAnalytics1: TAppAnalytics;
    ToolBar1: TToolBar;
    Label1: TLabel;
    GroupBoxConnection: TGroupBox;
    EnableButton: TButton;
    ClearSaveStateButton: TButton;
    Label2: TLabel;
    EdStatus: TEdit;
    EdUserID: TEdit;
    Label3: TLabel;
    GroupBoxActions: TGroupBox;
    MainFormEdit1: TEdit;
    LaunchForm2Button: TButton;
    RaiseExceptionButton: TButton;
    RefreshButton: TButton;
    procedure EnableButtonClick(Sender: TObject);
    procedure LaunchForm2ButtonClick(Sender: TObject);
    procedure RaiseExceptionButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure ClearSaveStateButtonClick(Sender: TObject);
    procedure UpdateUI();
    procedure RefreshButtonClick(Sender: TObject);
  private
    { Private declarations }
    FClearState: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.IOUtils, Unit2;

resourcestring
  sWarning = 'You''re using the default value for the ApplicationID property.' + #13#10 +
             'Please click F1 on ApplicationID property in Object Inspector to get more information.';

procedure TForm1.UpdateUI;
begin
  if AppAnalytics1.Enabled then
    EdStatus.Text := 'connected'
  else
    EdStatus.Text := 'disconnected';

  if AppAnalytics1.UserID.Length = 0 then
  begin
    EdUserID.TextSettings.HorzAlign := TTextAlign.Center;
    EdUserID.Text := '-';
  end
  else
  begin
    EdUserID.TextSettings.HorzAlign := TTextAlign.Leading;
    EdUserID.Text := AppAnalytics1.UserID;
  end;
end;

procedure TForm1.ClearSaveStateButtonClick(Sender: TObject);
begin
  SaveState.Stream.Clear;
  FClearState := True;
end;

procedure TForm1.EnableButtonClick(Sender: TObject);
begin
  if AppAnalytics1.ApplicationID.Length = 0 then
  begin
    ShowMessage( sWarning );
    exit;
  end;

  AppAnalytics1.Enabled := True;
  UpdateUI;
end;

procedure TForm1.LaunchForm2ButtonClick(Sender: TObject);
begin
  Form2.Show;
end;

procedure TForm1.RaiseExceptionButtonClick(Sender: TObject);
begin
  raise Exception.Create('Test Exception');
end;

procedure TForm1.RefreshButtonClick(Sender: TObject);
begin
  UpdateUI;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Reader: TBinaryReader;
begin
{$IFDEF IOS}
  SaveState.StoragePath := TPath.GetHomePath + '/Library/Caches';
{$ELSE}
  SaveState.StoragePath := TPath.GetHomePath;
{$ENDIF}
  SaveState.Name := 'FMXAnalytics.data';

  // Recover persistent analytics data, if found
  if SaveState.Stream.Size > 0 then
  begin
    Reader := TBinaryReader.Create(SaveState.Stream);
    try
      AppAnalytics1.UserID := Reader.ReadString;
      AppAnalytics1.AllowTracking := Reader.ReadBoolean;
    finally
      Reader.Free;
    end;
  end;

  FClearState := False;
  UpdateUI;
end;

procedure TForm1.FormSaveState(Sender: TObject);
var
  Writer: TBinaryWriter;
begin
  SaveState.Stream.Clear;
  if not FClearState and AppAnalytics1.AllowTracking then
  begin
    Writer := TBinaryWriter.Create(SaveState.Stream);
    try
      Writer.Write(AppAnalytics1.UserID);
      Writer.Write(AppAnalytics1.AllowTracking);
    finally
      Writer.Free;
    end;
  end;
end;

end.
