unit unitMyClasses;

interface

uses Generics.Collections;

type

  ///	<remarks>
  ///	  base object for the demo with Interface support as this can then easily
  ///	  be used by the children.
  ///	</remarks>
  TMyBaseObject = class(TInterfacedObject)
  strict private
    FID: Integer;
  public
    constructor Create(aID : Integer); reintroduce;
    property ID : Integer read FID;
  end;

  ///	<remarks>
  ///	  Example Interface to setup a contract to return a string
  ///	</remarks>
  IMyInterface = interface
    ['{6E1078FA-DE3C-4CFD-900F-C674C630359C}']
    function GetAsString: string;
  end;

  TMyFoo = class;
  TMyFee = class;
  TMyInterfaceHelper = class;

  ///	<summary>
  ///	  TMyFee implements TMyBaseObject (So it has the ID Property) It also has
  ///	  a "Fee" property that is a string.
  ///	</summary>
  TMyFee = class(TMyBaseObject)
  strict private
    FFee: string;
    procedure SetFee(const Value: string);
    function GetAsString: string;
  public
    constructor Create(aID : Integer; aFee: string); reintroduce;
    property Fee : string read FFee write SetFee;
    property AsString : string read GetAsString;
  end;

  ///	<summary>
  ///	  TMyFoo, like TMyFee descends from TMyBaseObject. The only difference is
  ///	  that it implements the interface IMyInterface so that the GetAsString
  ///	  value can be accessed via class or interface.
  ///	</summary>
  TMyFoo = class(TMyBaseObject, IMyInterface)
  strict private
    FFoo: string;
    ///	<value>
    ///	  MyIHFooObj : IMyInterface is a reference to the helper object that we
    ///	  are using for the TMyFoo.
    ///	</value>
    ///	<remarks>
    ///	  This helps prevent the object TMyFoo going out of scope when we get a
    ///	  handle on the interface, as instead, the reference count is increased
    ///	</remarks>
    MIHFooObj : IMyInterface;
    function GetMyInterfaceObj: IMyInterface;
    procedure SetFoo(const Value: string);
    function GetAsString: string;
  public
    constructor Create(aID : Integer; aFoo: string); reintroduce;
    property Foo : string read FFoo write SetFoo;

    ///	<remarks>
    ///	  Calls MIHFooObj to get the string value
    ///	</remarks>
    property AsString : string read GetAsString;

    ///	<remarks>
    ///	  Completes the contract for the Interface IMyInterface using the
    ///	  Implements key word.
    ///	</remarks>
    property MyInterface : IMyInterface read GetMyInterfaceObj implements IMyInterface;
  end;


  ///	<remarks>
  ///	  Helper type for creating a function that returns the string value for
  ///	  GetAsString for IMyInterface
  ///	</remarks>
  TMyInterfaceHelperFunc = reference to function(): string;

  ///	<remarks>
  ///	  Helper class that implements IMyInterface using the TMyInterfaceHelperFunc to keep this generic for use.
  ///	</remarks>
  TMyInterfaceHelper = class(TInterfacedObject, IMyInterface)
  strict private
    MIF_func : TMyInterfaceHelperFunc;
  public
    constructor Create(fetchStr : TMyInterfaceHelperFunc);
    function GetAsString: string;
  end;

  TMyBaseList = class(TObjectList<TMyBaseObject>);

implementation

uses SysUtils;

{ TMyBaseObjectDesc1 }

constructor TMyFoo.Create(aID: Integer; aFoo: string);
begin
  inherited Create(aID);
  MIHFooObj := TMyInterfaceHelper.Create(function : string
                                         begin
                                           Result := Self.Foo;
                                         end);
  Foo := aFoo;
end;


function TMyFoo.GetAsString: string;
begin
  Result := MIHFooObj.GetAsString;
end;

function TMyFoo.GetMyInterfaceObj: IMyInterface;
begin
  Result := MIHFooObj;
end;

procedure TMyFoo.SetFoo(const Value: string);
begin
  FFoo := Value;
end;

{ TMyBaseObjectDesc2 }

constructor TMyFee.Create(aID: Integer; aFee: string);
begin
  inherited Create(aID);
  Fee := aFee;
end;

function TMyFee.GetAsString: string;
begin
  Result := Fee;
end;

procedure TMyFee.SetFee(const Value: string);
begin
  FFee := Value;
end;

{ TMyBaseObject }

constructor TMyBaseObject.Create(aID: Integer);
begin
  inherited Create;
  FID := aID;
end;

{ TFooToString }

constructor TMyInterfaceHelper.Create(fetchStr : TMyInterfaceHelperFunc);
begin
  inherited Create;
  MIF_func := fetchStr;
end;

function TMyInterfaceHelper.GetAsString: string;
begin
  Result := MIF_Func;
end;

end.
