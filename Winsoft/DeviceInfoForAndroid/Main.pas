unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid, FMX.Layouts;

type
  TFormMain = class(TForm)
    StringGrid: TStringGrid;
    StringColumnValue: TStringColumn;
    StringColumnName: TStringColumn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Androidapi.JNI.Os, Androidapi.Helpers;

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
var
  SdkInt: Integer;
  OsVersion: string;
begin
  StringGrid.Cells[0, 0] := 'Board';
  StringGrid.Cells[1, 0] := JStringToString(TJBuild.JavaClass.BOARD);

  StringGrid.Cells[0, 1] := 'Bootloader';
  StringGrid.Cells[1, 1] := JStringToString(TJBuild.JavaClass.BOOTLOADER);

  StringGrid.Cells[0, 2] := 'Brand';
  StringGrid.Cells[1, 2] := JStringToString(TJBuild.JavaClass.BRAND);

  StringGrid.Cells[0, 3] := 'CPU ABI';
  StringGrid.Cells[1, 3] := JStringToString(TJBuild.JavaClass.CPU_ABI);

  StringGrid.Cells[0, 4] := 'CPU ABI 2';
  StringGrid.Cells[1, 4] := JStringToString(TJBuild.JavaClass.CPU_ABI2);

  StringGrid.Cells[0, 5] := 'Device';
  StringGrid.Cells[1, 5] := JStringToString(TJBuild.JavaClass.DEVICE);

  StringGrid.Cells[0, 6] := 'Display';
  StringGrid.Cells[1, 6] := JStringToString(TJBuild.JavaClass.DISPLAY);

  StringGrid.Cells[0, 7] := 'Fingerprint';
  StringGrid.Cells[1, 7] := JStringToString(TJBuild.JavaClass.FINGERPRINT);

  StringGrid.Cells[0, 8] := 'Fingerprint';
  StringGrid.Cells[1, 8] := JStringToString(TJBuild.JavaClass.FINGERPRINT);

  StringGrid.Cells[0, 9] := 'Hardware';
  StringGrid.Cells[1, 9] := JStringToString(TJBuild.JavaClass.HARDWARE);

  StringGrid.Cells[0, 10] := 'Host';
  StringGrid.Cells[1, 10] := JStringToString(TJBuild.JavaClass.HOST);

  StringGrid.Cells[0, 11] := 'ID';
  StringGrid.Cells[1, 11] := JStringToString(TJBuild.JavaClass.ID);

  StringGrid.Cells[0, 12] := 'Manufacturer';
  StringGrid.Cells[1, 12] := JStringToString(TJBuild.JavaClass.MANUFACTURER);

  StringGrid.Cells[0, 13] := 'Model';
  StringGrid.Cells[1, 13] := JStringToString(TJBuild.JavaClass.MODEL);

  StringGrid.Cells[0, 14] := 'Product';
  StringGrid.Cells[1, 14] := JStringToString(TJBuild.JavaClass.PRODUCT);

  StringGrid.Cells[0, 15] := 'Radio';
  StringGrid.Cells[1, 15] := JStringToString(TJBuild.JavaClass.RADIO);

  StringGrid.Cells[0, 16] := 'Radio version';
  StringGrid.Cells[1, 16] := JStringToString(TJBuild.JavaClass.getRadioVersion);

  StringGrid.Cells[0, 17] := 'Serial';
  StringGrid.Cells[1, 17] := JStringToString(TJBuild.JavaClass.SERIAL);

  StringGrid.Cells[0, 18] := 'Tags';
  StringGrid.Cells[1, 18] := JStringToString(TJBuild.JavaClass.TAGS);

  StringGrid.Cells[0, 19] := 'Type';
  StringGrid.Cells[1, 19] := JStringToString(TJBuild.JavaClass.&TYPE);

  StringGrid.Cells[0, 20] := 'User';
  StringGrid.Cells[1, 20] := JStringToString(TJBuild.JavaClass.USER);

  StringGrid.Cells[0, 21] := 'OS Codename';
  StringGrid.Cells[1, 21] := JStringToString(TJBuild_VERSION.JavaClass.CODENAME);

  StringGrid.Cells[0, 22] := 'OS Incremental';
  StringGrid.Cells[1, 22] := JStringToString(TJBuild_VERSION.JavaClass.INCREMENTAL);

  StringGrid.Cells[0, 23] := 'OS Release';
  StringGrid.Cells[1, 23] := JStringToString(TJBuild_VERSION.JavaClass.RELEASE);

  StringGrid.Cells[0, 24] := 'OS SDK';
  StringGrid.Cells[1, 24] := JStringToString(TJBuild_VERSION.JavaClass.SDK);

  SdkInt := TJBuild_VERSION.JavaClass.SDK_INT;
  if SdkInt = TJBuild_VERSION_CODES.JavaClass.BASE then
    OsVersion := 'Android 1.0 (Base)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.BASE_1_1 then
    OsVersion := 'Android 1.1 (Base 1.1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.CUPCAKE then
    OsVersion := 'Android 1.5 (Cupcake)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.CUR_DEVELOPMENT then
    OsVersion := 'Android current development'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.DONUT then
    OsVersion := 'Android 1.6 (Donut)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.ECLAIR then
    OsVersion := 'Android 2.0 (Eclair)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.ECLAIR_0_1 then
    OsVersion := 'Android 2.0.1 (Eclair 0.1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.ECLAIR_MR1 then
    OsVersion := 'Android 2.1 (Eclair MR1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.FROYO then
    OsVersion := 'Android 2.2 (Froyo)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.GINGERBREAD then
    OsVersion := 'Android 2.3 (Gingerbread)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.GINGERBREAD_MR1 then
    OsVersion := 'Android 2.3.3 (Gingerbread MR1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.HONEYCOMB then
    OsVersion := 'Android 3.0 (Honeycomb)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.HONEYCOMB_MR1 then
    OsVersion := 'Android 3.1 (Honeycomb MR1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.HONEYCOMB_MR2 then
    OsVersion := 'Android 3.2 (Honeycomb MR2)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.ICE_CREAM_SANDWICH then
    OsVersion := 'Android 4.0 (Ice Cream Sandwich)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.ICE_CREAM_SANDWICH_MR1 then
    OsVersion := 'Android 4.0.3 (Ice Cream Sandwich MR1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.JELLY_BEAN then
    OsVersion := 'Android 4.1 (Jelly Bean)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.JELLY_BEAN_MR1 then
    OsVersion := 'Android 4.2 (Jelly Bean MR1)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.JELLY_BEAN_MR2 then
    OsVersion := 'Android 4.3 (Jelly Bean MR2)'
  else if SdkInt = TJBuild_VERSION_CODES.JavaClass.KITKAT then
    OsVersion := 'Android 4.4 (Kitkat)'
  else if SdkInt = 20 then
    OsVersion := 'Android 4.4W (Kitkat Watch)'
  else if SdkInt = 21 then
    OsVersion := 'Android 5.0 (Lollipop)'
  else
    OsVersion := 'Unknown';

  StringGrid.Cells[0, 25] := 'OS Version';
  StringGrid.Cells[1, 25] := OsVersion;
end;

end.
