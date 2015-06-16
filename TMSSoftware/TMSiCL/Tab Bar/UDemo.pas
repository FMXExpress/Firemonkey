unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUITabBarController, FMX.TMSNativeUIButton,
  FMX.TMSNativeUIDatePicker, FMX.TMSNativeUILabel, FMX.TMSNativeMKMapView,
  FMX.TMSNativeUIView, FMX.TMSNativeUICore;

type
  TForm1014 = class(TForm)
    TMSFMXNativeUITabBarController1: TTMSFMXNativeUITabBarController;
    TMSFMXNativeUITabBarItem1: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeUITabBarItem2: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeUITabBarItem3: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeMKMapView1: TTMSFMXNativeMKMapView;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    TMSFMXNativeUIDatePicker1: TTMSFMXNativeUIDatePicker;
    TMSFMXNativeUILabel3: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel4: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel5: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel6: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel7: TTMSFMXNativeUILabel;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUILabel8: TTMSFMXNativeUILabel;
    TMSFMXNativeUIButton2: TTMSFMXNativeUIButton;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    procedure TMSFMXNativeUIButton2Click(Sender: TObject);
    procedure TMSFMXNativeUIDatePicker1ValueChanged(ASender: TObject;
      ADateTime: TDateTime);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeMKMapView1DidUpdateUserLocation(Sender: TObject;
      AUserLocation: TTMSFMXNativeMKMapLocation);
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1014: TForm1014;

implementation

uses
  iOSApi.UIKit;

{$R *.fmx}

procedure TForm1014.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIDatePicker1.DateTime := Now;
  TMSFMXNativeUILabel8.Text := DateTimeToStr(TMSFMXNativeUIDatePicker1.DateTime);
end;

procedure TForm1014.TMSFMXNativeMKMapView1DidUpdateUserLocation(Sender: TObject;
  AUserLocation: TTMSFMXNativeMKMapLocation);
begin
  TMSFMXNativeMKMapView1.SetCenterLocation(AUserLocation, True);
end;

procedure TForm1014.TMSFMXNativeUIButton1Click(Sender: TObject);
var
  shopCount: Integer;
begin
  ShowMessage('Add to cart');
  shopCount := StrToInt(TMSFMXNativeUITabBarItem1.Badge);
  Inc(shopCount);
  TMSFMXNativeUITabBarItem1.Badge := inttostr(shopCount);
end;

procedure TForm1014.TMSFMXNativeUIButton2Click(Sender: TObject);
begin
  TMSFMXNativeMKMapView1.ShowsUserLocation := True;
end;

procedure TForm1014.TMSFMXNativeUIDatePicker1ValueChanged(ASender: TObject;
  ADateTime: TDateTime);
begin
  TMSFMXNativeUILabel8.Text := DateTimeToStr(ADateTime);
end;

end.
