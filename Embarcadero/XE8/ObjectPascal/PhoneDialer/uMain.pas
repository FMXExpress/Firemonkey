unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.PhoneDialer,
  FMX.Platform, FMX.Edit, FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation;

type
  TPhoneDialerForm = class(TForm)
    btnGetCarrierInfo: TButton;
    btnMakeCall: TButton;
    edtTelephoneNumber: TEdit;
    lblTelephoneNumber: TLabel;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    CarrierNameItem: TListBoxItem;
    CountryCodeItem: TListBoxItem;
    NetworkCodeItem: TListBoxItem;
    MobileNetworkItem: TListBoxItem;
    procedure btnGetCarrierInfoClick(Sender: TObject);
    procedure btnMakeCallClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PhoneDialerForm: TPhoneDialerForm;

implementation

{$R *.fmx}

procedure TPhoneDialerForm.btnGetCarrierInfoClick(Sender: TObject);
var
  PhoneDialerService: IFMXPhoneDialerService;

begin
  { test whether the PhoneDialer services are supported }
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(PhoneDialerService)) then
  begin
    { if yes, then update the labels with the retrieved information }
    CarrierNameItem.ItemData.Detail := PhoneDialerService.GetCarrier.GetCarrierName;
    CountryCodeItem.ItemData.Detail := PhoneDialerService.GetCarrier.GetIsoCountryCode;
    NetworkCodeItem.ItemData.Detail := PhoneDialerService.GetCarrier.GetMobileCountryCode;
    MobileNetworkItem.ItemData.Detail := PhoneDialerService.GetCarrier.GetMobileNetwork;
  end
  else
    ShowMessage('PhoneDialer service not supported');
end;

procedure TPhoneDialerForm.btnMakeCallClick(Sender: TObject);
var
  PhoneDialerService: IFMXPhoneDialerService;

begin
  { test whether the PhoneDialer services are supported }
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(PhoneDialerService)) then
  begin
    { if the Telephone Number is entered in the edit box then make the call, else
      display an error message }
    if edtTelephoneNumber.Text <> '' then
      PhoneDialerService.Call(edtTelephoneNumber.Text)
    else
    begin
      ShowMessage('Please type in a telephone number.');
      edtTelephoneNumber.SetFocus;
    end;
  end
  else
    ShowMessage('PhoneDialer service not supported');
end;

end.
