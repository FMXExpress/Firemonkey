unit SettingsProjectForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.TabControl, FMX.StdCtrls, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  System.Actions, FMX.ActnList;

type
  TSettingsForm = class(TForm)
    ToolBar1: TToolBar;
    SettingsMain: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    SettingsList1: TListBox;
    AccountType: TListBoxItem;
    PaymentType: TListBoxItem;
    RenewalType: TListBoxItem;
    SyncSettings: TListBoxGroupHeader;
    SyncUSB: TListBoxItem;
    SyncWifi: TListBoxItem;
    SyncCollections: TListBoxItem;
    Switch1: TSwitch;
    Switch2: TSwitch;
    Switch3: TSwitch;
    AccountInfo: TListBoxGroupHeader;
    ToolBar2: TToolBar;
    SettingsDetails: TLabel;
    SettingsList2: TListBox;
    SelectAcctType: TListBoxItem;
    SelectPayment: TListBoxItem;
    AcctTypes: TListBoxGroupHeader;
    AcctCombo: TComboBox;
    Business: TListBoxItem;
    Personal: TListBoxItem;
    PaymentCombo: TComboBox;
    CreditCard: TListBoxItem;
    Check: TListBoxItem;
    BindingsList1: TBindingsList;
    LinkFillControlToPropertyItemDataDetail: TLinkFillControlToProperty;
    LinkFillControlToPropertyItemDataDetail2: TLinkFillControlToProperty;
    BackButton: TSpeedButton;
    ActionList1: TActionList;
    ChangeTabAction1: TChangeTabAction;
    SelectRenewal: TListBoxItem;
    RenewalCombo: TComboBox;
    Monthly: TListBoxItem;
    Annually: TListBoxItem;
    Quarterly: TListBoxItem;
    LinkFillControlToPropertyItemDataDetail3: TLinkFillControlToProperty;
    ChangeTabAction2: TChangeTabAction;
    procedure ListBoxItemTab1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TSettingsForm.ListBoxItemTab1Click(Sender: TObject);
begin
  ChangeTabAction2.ExecuteTarget(self);
end;

end.
