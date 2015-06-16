unit UDatePicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUILabel,
  FMX.TMSNativeUIButton, FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIDatePicker,
  FMX.TMSNativeUISwitch;

type
  TForm935 = class(TForm)
    TMSFMXNativeUIDatePicker1: TTMSFMXNativeUIDatePicker;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUISwitch1: TTMSFMXNativeUISwitch;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
    procedure TMSFMXNativeUIDatePicker1ValueChanged(ASender: TObject;
      ADateTime: TDateTime);
    procedure TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
      AValue: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateLabel;
  end;

var
  Form935: TForm935;

implementation

{$R *.fmx}

procedure TForm935.FormCreate(Sender: TObject);
begin
  UpdateLabel;
end;

procedure TForm935.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeUIDatePicker1.DateTime := Now;
  UpdateLabel;
end;

procedure TForm935.TMSFMXNativeUIDatePicker1ValueChanged(ASender: TObject;
  ADateTime: TDateTime);
begin
  UpdateLabel;
end;

procedure TForm935.TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
  AValue: Boolean);
begin
  if AValue then
    TMSFMXNativeUIDatePicker1.Mode := dpmDatePickerModeDateAndTime
  else
    TMSFMXNativeUIDatePicker1.Mode := dpmDatePickerModeDate;

  UpdateLabel;
end;

procedure TForm935.UpdateLabel;
begin
  if TMSFMXNativeUISwitch1.Value then
    TMSFMXNativeUILabel1.Text := DateTimeToStr(TMSFMXNativeUIDatePicker1.DateTime)
  else
    TMSFMXNativeUILabel1.Text := DateToStr(TMSFMXNativeUIDatePicker1.DateTime);
end;

end.
