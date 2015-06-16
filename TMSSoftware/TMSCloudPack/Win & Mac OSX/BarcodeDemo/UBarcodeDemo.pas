unit UBarcodeDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.TMSCloudImage, FMX.TMSCloudBase, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomBarcode, FMX.TMSCloudBarcode, FMX.ListBox, FMX.Edit, TypInfo;

type
  TForm9 = class(TForm)
    TMSFMXCloudBarcode1: TTMSFMXCloudBarcode;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    cbCodeType: TComboBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbType: TComboBox;
    edWidth: TEdit;
    edHeight: TEdit;
    cbShowText: TCheckBox;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    cbSize: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbCodeTypeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitOptions;
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

procedure TForm9.Button1Click(Sender: TObject);
var
  bctype: TBarcodeType;
begin
  if Edit1.Text = '' then
  begin
    ShowMessage('Please enter a value first');
    exit;
  end;

  if cbCodeType.ItemIndex = 0 then
  begin
    case cbType.ItemIndex of
      1: bctype := btC128a;
      2: bctype := btC128b;
      3: bctype := btC128c;
      4: bctype := bti2of5;
    else
      bctype := btC39;
    end;

    TMSFMXCloudBarcode1.BarcodeOptions.ShowText := cbShowText.IsChecked;
    TMSFMXCloudBarcode1.BarcodeOptions.Width := StrToInt(edWidth.Text);
    TMSFMXCloudBarcode1.BarcodeOptions.Height := StrToInt(edHeight.Text);

    TMSFMXCloudImage1.Width := TMSFMXCloudBarcode1.BarcodeOptions.Width;
    TMSFMXCloudImage1.Height := TMSFMXCloudBarcode1.BarcodeOptions.Height;
    TMSFMXCloudImage1.URL := TMSFMXCloudBarcode1.GetBarcodeURL(Edit1.Text, bctype);
  end
  else
  begin
    case cbSize.ItemIndex of
      0: TMSFMXCloudBarcode1.QRcodeOptions.Size := qs42;
      1: TMSFMXCloudBarcode1.QRcodeOptions.Size := qs105;
      2: TMSFMXCloudBarcode1.QRcodeOptions.Size := qs210;
    end;

    TMSFMXCloudImage1.Width := StrToInt(StringReplace(GetEnumName(TypeInfo(TQRcodeSize),
      Ord(TMSFMXCloudBarcode1.QRcodeOptions.Size)), 'qs', '', []));
    TMSFMXCloudImage1.Height := TMSFMXCloudImage1.Width;
    TMSFMXCloudImage1.URL := TMSFMXCloudBarcode1.GetQRCodeURL(Edit1.Text);
  end;
end;

procedure TForm9.Button2Click(Sender: TObject);
var
  sv: TSaveDialog;
begin
  sv := TSaveDialog.Create(Self);
  sv.FileName := 'barcode' + Edit1.Text + '.png';
  if sv.Execute then
    TMSFMXCloudImage1.Bitmap.SaveToFile(sv.FileName);
end;

procedure TForm9.cbCodeTypeChange(Sender: TObject);
begin
  InitOptions;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  InitOptions;
end;

procedure TForm9.InitOptions;
begin
  if cbCodeType.ItemIndex = 0 then
  begin
    cbType.Enabled := true;
    edWidth.Enabled := true;
    edHeight.Enabled := true;
    cbShowText.Enabled := true;
    cbSize.Enabled := false;
  end
  else
  begin
    cbType.Enabled := false;
    edWidth.Enabled := false;
    edHeight.Enabled := false;
    cbShowText.Enabled := false;
    cbSize.Enabled := true;
  end;
end;

end.
