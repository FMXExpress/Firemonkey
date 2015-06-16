unit UFormSample2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Data.DB, Datasnap.DBClient, FMX.Layouts, FMX.Grid, FMX.ExtCtrls, FMX.Edit,
  FMX.ListBox, FMX.Objects;

type
  TForm2 = class(TForm)
    CalendarEdit1: TCalendarEdit;
    Calendar1: TCalendar;
    Image1: TImage;
    ComboBox1: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

end.
