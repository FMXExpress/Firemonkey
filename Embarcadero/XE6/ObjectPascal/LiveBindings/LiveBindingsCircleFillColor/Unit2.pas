
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.DBLinks,
  Fmx.Bind.DBLinks, FMX.Edit, FMX.Layouts, Fmx.Bind.Navigator,
  Data.Bind.DBScope, Data.DB, Datasnap.DBClient, System.Rtti,
  System.Bindings.Outputs, FMX.Filter.Effects, FMX.Objects, FMX.Ani,
  FMX.StdCtrls, FMX.Effects;

type
  TForm2 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    BindSourceDB1: TBindSourceDB;
    BindNavigator1: TBindNavigator;
    Edit1: TEdit;
    BindingsList1: TBindingsList;
    DBLinkEdit1SpeciesName1: TBindDBEditLink;
    Edit2: TEdit;
    DBLinkEdit2Lengthcm1: TBindDBEditLink;
    Circle1: TCircle;
    CircleTransitionEffect1: TCircleTransitionEffect;
    Label1: TLabel;
    Label2: TLabel;
    ImageControl1: TImageControl;
    DBLinkImageControl1Graphic1: TBindDBImageLink;
    Label3: TLabel;
    Label4: TLabel;
    DBLinkLabel4Common_Name3: TBindDBTextLink;
    BindLinkCircle12: TBindLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses CustomMethods;

{$R *.fmx}

const
  cComputeColor = 'ComputeColor';
initialization
  TMethodsHelper.RegisterMethod<Double, TAlphaColor>(cComputeColor,
    function(AParam: Double): TAlphaColor
    var
      LColor: TAlphaColor;
    begin
      if AParam < 50 then
        LColor := TAlphaColorRec.Red
      else
        LColor := TAlphaColorRec.Green;
      Result := LColor;
    end
  );

end.
