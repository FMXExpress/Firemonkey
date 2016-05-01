unit fmWorkBench;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Edit;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    mInput: TMemo;
    mOutput: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Button4: TButton;
    Button6: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btBeautify: TButton;
    btMinify: TButton;
    gbButtons: TGroupBox;
    btToWriter: TButton;
    btToDelphi: TButton;
    lbUseBuilders: TCheckBox;
    procedure btBeautifyClick(Sender: TObject);
    procedure btMinifyClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btToDelphiClick(Sender: TObject);
    procedure btToWriterClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.JSON.Types, System.IOUtils, System.DateUtils, Converters, Writers;

{$R *.fmx}

{ TForm1 }

procedure TForm1.btBeautifyClick(Sender: TObject);
begin
    try
    mOutput.Text := TConverters.JsonReformat(mInput.Text, True);
  except
    on E: EJsonException do
      mOutput.Text := E.Message;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  try
    Memo3.Text := TConverters.Json2BsonString(Memo4.Text);
  except
    on E: EJsonException do
      Memo3.Text := E.Message;
  end;
end;

procedure TForm1.btMinifyClick(Sender: TObject);
begin
  try
    mOutput.Text := TConverters.JsonReformat(mInput.Text, False);
  except
    on E: EJsonException do
      mOutput.Text := E.Message;
  end;
end;

procedure TForm1.btToDelphiClick(Sender: TObject);
begin
  try
    mOutput.Text := TConverters.Json2DelphiCode(mInput.Text)
  except
    on E: EJsonException do
      mOutput.Text := E.Message;
  end;
end;

procedure TForm1.btToWriterClick(Sender: TObject);
var
  LCode: string;
begin
  try
    if lbUseBuilders.IsChecked then
    begin
//      LCode := '
      LCode := TConverters.Json2JsonBuilderCode(mInput.Text, 'Builder');
    end
    else
    begin
      LCode := TConverters.Json2JsonWriterCode(mInput.Text, 'Writer');
    end;
    mOutput.Text := LCode;
  except
    on E: EJsonException do
      mOutput.Text := E.Message;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  try
    Memo4.Text := TConverters.BsonString2Json(Memo3.Text);
  except
    on E: EJsonException do
      Memo4.Text := E.Message;
  end;
end;

end.
