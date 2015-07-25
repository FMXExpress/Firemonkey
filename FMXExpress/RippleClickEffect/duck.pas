unit duck;

// *****************************************************************************
//      DUCK DUCK DELPHI
// *****************************************************************************
//
//  Original Author:
//    Jason Southwell
//
//  Description:
//    This unit adds simple duck typing to Delphi objects with RTTI and
//    provides an RTTI helper class to simplify many common RTTI tasks.
//
//  For more information on Duck Typing, please check out this article on
//  wikipedia:
//    http://en.wikipedia.org/wiki/Duck_typing
//
//  For more information on this project, please visit:
//    http:/arcana.sivv.com/duckduckdelphi
//
// *****************************************************************************
//      DUCK TYPING
// *****************************************************************************
//
//  Instead of:
//    if obj is TControl then
//      TCoontrol(obj).Visible := True
//
//  You can simply call
//    obj.duck.setTo('Visible',True);
//
//  If the property does not exist on the object, the call fails silently.
//
//  If you must know that the property can be set:
//    if obj.duck.has('Visible') then
//      obj.duck.setTo('Visible',True);
//
//  "obj" in the above examples can be any object with RTTI.
//
// *****************************************************************************
//      ANNONYMOUS EVENTS
// *****************************************************************************
//
//  Instead of:
//    obj.OnChange := MyObjectOnChange;
//
//  You can now define event handlers inline with anonymous methods
//    obj.OnChange := Events.Notify(procedure(Sender : TObject)
//    begin
//      DoSomethingHere();
//    end);
//
//  To release the memory used by the anonymous method, instead of setting the
//  property to nil, you must release the event:
//    Event.NotifyRelease(obj.OnChange);
//
//  If the event is of a type that does not have an explicit helper, you can
//  use the From method as follows:
//    TKeyEventReference = reference to procedure(Sender: TObject; var Key: Word; Shift: TShiftState);
//    obj.OnKeyUp := Events.From<TKeyEvent, TKeyEventReference>(procedure(Sender: TObject; var Key: Word; Shift: TShiftState)
//    begin
//      DoSomethingHere();
//    end);
//
// *****************************************************************************
//      LICENSE
// *****************************************************************************
//
//  The MIT License (MIT)
//  Copyright (c) 2012 by Sivv LLC, All Rights Reseved
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
//
// *****************************************************************************
//      RELEASE NOTES
// *****************************************************************************
//   2014-07-07 : R6 Jason Southwell
//                 + XE6 & AppMethod Support
//                 + Firemonkey Desktop & Mobile Support
//                 + Overloaded Method support
//                 * Minor Performance Improvements
//                 * Several bug fixes
//   2012-06-14 : R4 Jason Southwell
//                 * implemented duck<I> overload to allow duck typing to a
//                   specified interface and assume the implementation.
//   2012-02-17 : R3 Jason Southwell
//                 + Added asA<I> to return a custom variant wrapper around the
//                   object allowing it to be called as though it were the
//                   interface supplied without using the duck interface methods.
//                   allowing you to see if the object has all of the methods
//                   and properties required of the interface even if it
//                   doesn't implement the interface.
//                 + Added impersonates<I> check to the object class helper
//                   allowing you to see if the object has all of the methods
//                   and properties required of the interface even if it
//                   doesn't implement the interface.
//                 + Added several overloads to better support indexed properties
//                 + Added eachProperty and eachMethod iterators to the TRTTI
//                   class.
//                 + Added selector comparisson functions, equalTo, notEqualTo,
//                   moreThan, and lessThan.
//                 + Added new duck functions to return selectors including:
//                   this (the same object via selector), related (all related
//                   objects, note that this is the same as "all" used to be),
//                   props (all object properties of the object), go (executes
//                   a method and returns a selector of the result.
//                 * Altered duck.all to return all related objects as well as
//                   any properties of type TObject.
//                 + Added new selector functions First, Last, Middle, Even, Odd
//                   which return new selectors filtered accordingly.
//                 + Added new selector function "use" which alters the current
//                   context of the selector.
//                 + Added new selector action function "replace" which alters
//                   the value of a property only if it matches the value
//                   supplied
//                 + Added new duck procedure "replace" which altersthe value
//                   of a property only if it matches the value supplied
//                 * Fixed isa.  It's implementation had gone missing.
//                 * Renamed parameters in TRTTI from ctrl to obj.  ctrl was a
//                   holdover from when the class was written to work only on
//                   TControl descendants and no longer made sense.
//                 * Changed all methods called "sett" to "setTo" due to Nick
//                   Hodges consistent badgering and the overwheming will
//                   demonstrated by a very scientific facebook poll.
//                 + Added new setTo functions on Selector and Duck to allow
//                   setting of multiple properties via TPropValue array.
//                 + Added new asValue class method to TRTTI to simplify
//                   convertion to TValue where needed.
//                 + Started documenting TRTTI class
//   2012-02-15 : R2 Jason Southwell
//                 + Added "each" method to ISelector allowing an anonymous
//                   method to be called for each object in the selector.
//                   Example:
//                      Form1.duck.each(procedure(obj : TObject)
//                      begin
//                        obj.duck.setTo('hint','This is the new hint');
//                      end);
//                 + Added "filter" method to ISelector allowing an anonymous
//                   method to be called for each object in the selector which
//                   is used to reduce the objects in the filter.  Return True
//                   to include the item in the resulting selector.
//                   Example:
//                      Form1.duck.filter(function(obj : TObject) : boolean
//                      begin
//                        result := (obj.tag = 1) and SomeGlobalVariable;
//                      end).SetTo('Visible',False);
//                 + Added "on" method to ISelector which returns a new slector
//                   filled with the results of the object property specfied.
//                   Example:
//                      duck.all.on('Font').setTo('Color',clRed).setTo('Size',18);
//   2012-02-14 : R1 Jason Southwell
//                 + Initial Release to Open Source under MIT license
// *****************************************************************************

interface

uses System.SysUtils, System.Classes, System.Types, System.Rtti, System.TypInfo,
  System.Generics.Collections, System.Variants;

{$SCOPEDENUMS ON}

type
  TNotifyReference = TProc<TObject>;

  PInterface = ^IInterface;

  IImpersonated = Variant;

  TRTTI = class
  public
    ///	<summary>
    ///	  Comapres two TValue variables to determine if they are the same value
    ///	  of the same type.
    ///	</summary>
    ///	<param name="Value1">
    ///	  First Value to compare
    ///	</param>
    ///	<param name="Value2">
    ///	  Second Value to compare
    ///	</param>
    ///	<returns>
    ///	  True if the two values represent the same value of the same type.
    ///	  False if they do not.
    ///	</returns>
    class function isSame(const Value1, Value2 : TValue) : boolean; overload; static;
    class function isSame(const Params1, Params2: TArray<TRttiParameter>) : boolean; overload; static;

    ///	<summary>
    ///	  Checks an object's property to determine if it is of the specified
    ///	  type.
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type to check against
    ///	</typeparam>
    ///	<param name="obj">
    ///	  The object which has the property to test
    ///	</param>
    ///	<param name="PropertyName">
    ///	  The property name which to test
    ///	</param>
    ///	<returns>
    ///	  Returns true if the specified property exists on the object and if it
    ///	  is of the specified type.  Returns false if either it does not exist
    ///	  or if it is not of the type.
    ///	</returns>
    ///	<remarks>
    ///	  <para>
    ///	    TRTTI.isA&lt;boolean&gt;(Edit1,'Visible') = True
    ///	  </para>
    ///	  <para>
    ///	    TRTTI.isA&lt;string&gt;(Edit1,'Visible') = False
    ///	  </para>
    ///	</remarks>
    class function isA<T>(obj: TObject; const PropertyName: string) : boolean; overload; static;

    ///	<summary>
    ///	  Returns a typed value from the specifed property on the supplied
    ///	  object
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of the property which you will be returning
    ///	</typeparam>
    ///	<param name="obj">
    ///	  The object from which to return the specified property
    ///	</param>
    ///	<param name="PropertyName">
    ///	  The property name you wish to return
    ///	</param>
    ///	<returns>
    ///	  The value of the property on the specified object
    ///	</returns>
    ///	<remarks>
    ///	  <para>
    ///	    TRTTI.get&lt;boolean&gt;(Edit1,'Visible') = true or false
    ///	  </para>
    ///	  <para>
    ///	    if the property does not exist on the specified object, the value
    ///	    returned would be the typecasted value of 0 or nil.
    ///	  </para>
    ///	</remarks>
    class function get<T>(obj: TObject; const PropertyName: string) : T; overload; static;

    class function field<T>(obj: TObject; const FieldName : string) : T; static;

    ///	<summary>
    ///	  Attempts to return a typed value from the specifed property on the
    ///	  supplied object
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of the property which you will be returning
    ///	</typeparam>
    ///	<param name="obj">
    ///	  The object from which to return the specified property
    ///	</param>
    ///	<param name="PropertyName">
    ///	  The property name you wish to return
    ///	</param>
    ///	<param name="AResult">
    ///	  The value of the property to be returned.
    ///	</param>
    ///	<returns>
    ///	  True if the property was able to be returned, False if it was not.
    ///	</returns>
    ///	<remarks>
    ///	  <para>
    ///	    TRTTI.tryGet&lt;boolean&gt;(Edit1,'Visible',bIsVisible) = true or
    ///	    false
    ///	  </para>
    ///	  <para>
    ///	    if the property does not exist on the specified object, the value
    ///	    returned from the function would be false.  bIsVisible would hold
    ///	    the value of the returned property.
    ///	  </para>
    ///	</remarks>
    class function tryGet<T>(obj: TObject; const PropertyName: string; var AResult : T) : boolean; overload; static;

    ///	<summary>
    ///	  Sets a typed value to the specifed property on the supplied object
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of the property which you will be setting
    ///	</typeparam>
    ///	<param name="obj">
    ///	  The object on which to set the specified property
    ///	</param>
    ///	<param name="PropertyName">
    ///	  The property name you wish to set
    ///	</param>
    ///	<param name="Value">
    ///	  The value to set to the property
    ///	</param>
    ///	<remarks>
    ///	  This procedure fails silently if the property was unable to be set.
    ///	</remarks>
    class procedure setTo<T>(obj: TObject; const PropertyName: string; Value: T); overload; static;
    ///	<summary>
    ///	  Attempts to set a typed value to the specifed property on the
    ///	  supplied object
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of the property which you will be setting
    ///	</typeparam>
    ///	<param name="obj">
    ///	  The object on which to set the specified property
    ///	</param>
    ///	<param name="PropertyName">
    ///	  The property name you wish to set
    ///	</param>
    ///	<param name="Value">
    ///	  The value to set to the property.
    ///	</param>
    ///	<returns>
    ///	  True if the property was able to be set, False if it was not.
    ///	</returns>
    ///	<remarks>
    ///	  <para>
    ///	    TRTTI.trySet&lt;boolean&gt;(Edit1,'Visible',bIsVisible) = true or
    ///	    false
    ///	  </para>
    ///	  <para>
    ///	    if the property does not exist on the specified object, or if bIsVisible was unable to be set to the Visible property, the valuereturned from the function would be false. 
    ///	  </para>
    ///	</remarks>
    class function trySet<T>(obj: TObject; const PropertyName: string; Value: T) : boolean; overload; static;

    class function isA<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>) : boolean; overload; static;
    class function get<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>) : T; overload; static;
    class function tryGet<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>; var AResult : T) : boolean; overload; static;
    class procedure setTo<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>; Value: T); overload; static;
    class function trySet<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>; Value: T) : boolean; overload; static;

    class function asValue<T>(Value : T) : TValue;

    class function getValue(obj: TObject; const PropertyName: string) : TValue; overload; static;
    class function getValue(obj: TObject; const PropertyName: string; args : TArray<TValue>) : TValue; overload; static;
    class procedure setValue(obj: TObject; const PropertyName: string; Value : TValue); overload; static;
    class procedure setValue(obj: TObject; const PropertyName: string; args : TArray<TValue>; Value : TValue); overload; static;

    class function call(obj : TObject; const MethodName : string; args : TArray<TValue>) : TValue;
    class function tryCall(obj : TObject; const MethodName : string; args : TArray<TValue>; var AResult : TValue) : boolean;

    class function isProperty(obj: TObject; const PropertyName: string) : boolean; overload; static;
    class function isProperty(pti : PTypeInfo; const PropertyName: string) : boolean; overload; static;
    class function isProperty(obj: TObject; const PropertyName: string; OfType : TClass) : boolean; overload; static;
    class function isProperty(pti : PTypeInfo; const PropertyName: string; OfType : TClass) : boolean; overload; static;
    class function isMethod(obj: TObject; const MethodName: string) : boolean;  overload; static;
    class function isMethod(pti : PTypeInfo; const MethodName: string) : boolean;  overload; static;
    class function isMethod(obj: TObject; const MethodName: string; Params : TArray<TRTTIParameter>) : boolean;  overload; static;
    class function isMethod(pti : PTypeInfo; const MethodName: string; Params : TArray<TRTTIParameter>) : boolean;  overload; static;
    class function isMethod(obj: TObject; const MethodName: string; Params : TArray<TRTTIParameter>; Returns : TRTTIType) : boolean;  overload; static;
    class function isMethod(pti : PTypeInfo; const MethodName: string; Params : TArray<TRTTIParameter>; Returns : TRTTIType) : boolean;  overload; static;
    class function exists(obj: TObject; const name: string) : boolean; overload; static;
    class function exists(pti : PTypeInfo; const name: string) : boolean; overload; static;
    class procedure eachProperty(obj : TObject; proc : TProc<TRTTIProperty>); overload;
    class procedure eachProperty(pti : PTypeInfo; proc : TProc<TRTTIProperty>); overload;
    class procedure eachMethod(obj : TObject; proc : TProc<TRTTIMethod>); overload;
    class procedure eachMethod(pti : PTypeInfo; proc : TProc<TRTTIMethod>); overload;
    class procedure eachProperty(obj : TObject; proc : TFunc<TRTTIProperty, boolean>); overload;
    class procedure eachProperty(pti : PTypeInfo; proc : TFunc<TRTTIProperty, boolean>); overload;
    class procedure eachMethod(obj : TObject; proc : TFunc<TRTTIMethod, boolean>); overload;
    class procedure eachMethod(pti : PTypeInfo; proc : TFunc<TRTTIMethod, boolean>); overload;
  end;

  TEvent = class
  private
    FEvents : TDictionary<TPair<Pointer,Pointer>,IInterface>;
  protected
    procedure MakeEvent(const ameth; var evt);  virtual;
    procedure ParseMethod(const meth; var p1, p2 : Pointer); virtual;
    function IsSameEvent(const ameth; const evt) : boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function From<TEvent, TReference>(proc: TReference): TEvent;
    procedure ReleaseAndNil<TEvent>(var event : TEvent);
    procedure Release<TEvent>(event : TEvent);
    function Notify(proc : TNotifyReference) : TNotifyEvent; virtual;
    procedure NotifyRelease(event : TNotifyEvent); virtual;
  end;

  TPropValue = record
    PropertyName : string;
    Value: TValue;
    constructor Create(const APropertyName : string; const AValue: TValue);
  end;

  ISelector = interface(IInterface)
    // chained selection refinement methods
    function has(PropertyName : string) : ISelector; overload;
    function has(PropertyNames : TArray<string>) : ISelector; overload;
    function has(NameValues : TArray<TPropValue>) : ISelector; overload;
    function has(PropertyName : string; Value : TValue) : ISelector; overload;
    function can(MethodName : string) : ISelector; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>) : ISelector; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : ISelector; overload;
    function isa(ClassType : TClass) : ISelector;
    function &on(PropertyName : string) : ISelector;
    function first(Cnt : integer) : ISelector; overload;
    function first : ISelector; overload;
    function last(Cnt : integer) : ISelector; overload;
    function last : ISelector; overload;
    function middle(Idx, Cnt : integer) : ISelector;
    function even : ISelector;
    function odd : ISelector;
    function use(Context : string) : ISelector;

    // retrieves items enumerator
    function count : integer;
    function items : TArray<TObject>;

    // Property Actions upon the entire selection
    function setTo(Value : TValue) : ISelector; overload;
    function setTo(propertyName : string; Value : TValue) : ISelector; overload;
    function setTo(NameValues : TArray<TPropValue>) : ISelector; overload;
    function replace(Find : TValue; Replace : TValue) : ISelector; overload;
    function replace(propertyName : string; Find : TValue; Replace : TValue) : ISelector; overload;
    function add(Value : TValue) : ISelector; overload;
    function add(propertyName : string; Value : TValue) : ISelector; overload;
    function subtract(Value : TValue) : ISelector; overload;
    function subtract(propertyName : string; Value : TValue) : ISelector; overload;

    function notEqualTo(Value : TValue) : ISelector; overload;
    function notEqualTo(propertyName : string; Value : TValue) : ISelector; overload;
    function equalTo(Value : TValue) : ISelector; overload;
    function equalTo(propertyName : string; Value : TValue) : ISelector; overload;
    function moreThan(Value : TValue) : ISelector; overload;
    function moreThan(propertyName : string; Value : TValue) : ISelector; overload;
    function lessThan(Value : TValue) : ISelector; overload;
    function lessThan(propertyName : string; Value : TValue) : ISelector; overload;

    // Method Actions upon the entire selection
    function go : ISelector; overload;
    function go(methodName : string) : ISelector; overload;
    function go(args : TArray<TValue>) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;

    function call : TArray<TValue>; overload;
    function call(methodName : string) : TArray<TValue>; overload;
    function call(args : TArray<TValue>) : TArray<TValue>; overload;
    function call(args : array of variant) : TArray<TValue>; overload;
    function call(methodName : string; args : TArray<TValue>) : TArray<TValue>; overload;
    function call(methodName : string; args : array of Variant; var AResult : TValue) : TArray<TValue>; overload;

    function each(method : TProc<TObject>) : ISelector; overload;
    function filter(method : TFunc<TObject,boolean>) : ISelector;
  end;

  IDuck = interface;

  TInterceptBeforeFunc = reference to procedure(Args : TArray<TValue>; var Return : TValue; var Continue : boolean);
  TInterceptAfterFunc = reference to procedure(Args : TArray<TValue>; var Return : TValue);
  TInterceptBeforeProc = reference to procedure(Args : TArray<TValue>; var Continue : boolean);
  TInterceptAfterProc = reference to procedure(Args : TArray<TValue>);
  TInterceptExceptionProc = reference to procedure(E : Exception; Args : TArray<TValue>; var Return : TValue; var RaiseException : boolean);
  IIntercept = interface
    function before(proc : TInterceptBeforeProc) : IIntercept; overload;
    function before(proc : TInterceptAfterProc) : IIntercept; overload;
    function before(proc : TInterceptBeforeFunc) : IIntercept; overload;
    function before(proc : TInterceptAfterFunc) : IIntercept; overload;
    function after(proc : TInterceptAfterProc) : IIntercept; overload;
    function after(proc : TInterceptAfterFunc) : IIntercept; overload;
    function exception(proc : TInterceptExceptionProc) : IIntercept;
    function abort : IIntercept;
    function skip : IIntercept;
  end;

  IDuck = interface
    procedure setTo(propertyName : string; Value : TValue); overload;
    procedure setTo(NameValues : TArray<TPropValue>); overload;
    procedure replace(propertyName : string; Find : TValue; Replace : TValue);
    function get(propertyName : string) : TValue; overload;
    function get(propertyName : string; OfType : TClass) : TValue; overload;
  	function has(propertyName : string) : boolean; overload;
  	function has(propertyName : string; OfType : TClass) : boolean; overload;
	  function can(methodName : string) : boolean; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>) : boolean; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : boolean; overload;
    function call(methodName : string) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>) : TValue; overload;
    function call(methodName : string; args : array of variant) : TValue; overload;
    function call(methodName : string; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : array of Variant; var Exists : boolean) : TValue; overload;
    function call(methodName : string; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : TArray<TValue>; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : array of Variant; var AResult : TValue) : boolean; overload;

    function go(methodName : string) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;

    function all : ISelector;
    function props : ISelector;
    function related : ISelector;

    function this : ISelector;
    function obj : TObject;
    function intercept(methodName : string) : IIntercept;
  end;

  TSoftInterface<I: IInterface> = class(TVirtualInterface)
  private
    FSource : TObject;
  public
    constructor Create(Source : TObject);
    procedure DoInvoke(Method : TRttiMethod; const Args : TArray<TValue>; out Result : TValue);
  end;

  TDuckHelper = class helper for TObject
  public
    function duck : IDuck; overload;
    // this should be under IDuck, but interfaces don't currently support generic methods.
    function impersonates<Intf> : boolean;
    function asA<Intf> : IImpersonated;
    function duck<I : IInterface> : TSoftInterface<I>; overload;
  end;

  TDuck = class(TObject)
  public
    function duck : IDuck; overload;
    // this should be under IDuck, but interfaces don't currently support generic methods.
    function impersonates<Intf> : boolean;
    function asA<Intf> : IImpersonated;
    function duck<I : IInterface> : TSoftInterface<I>; overload;
  end;

  TDuckVarData = packed record
    VType : TVarType;
    VDuck : IDuck;
    VIType : PTypeInfo;
    Reserved1: LongInt;
    Reserved2 : WordBool;
  end;

type
  TDuckVariantType = class(TInvokeableVariantType)
  protected
    procedure DispInvoke(Dest: PVarData; [Ref] const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  end;


var
  DuckVariant : TDuckVariantType;


var
  Event : TEvent;


function PV(const APropertyName : string; const AValue : TValue) : TPropValue;
procedure RegisterPropertyEnumerator(const IndexedProperty : string; const CountProperty : string);

function DuckVarType : TVarType;
function VarIsDuck(const AValue : Variant) : Boolean;
function VarAsDuck(const AValue : Variant) : IImpersonated;

resourcestring
  S_MISSINGCONTEXT = 'Context missing for opperation on selector. Try explicitly stating identifier in call to method or call "use" to specify context.';

implementation

uses
  System.SysConst;

type
  TSelectorImpl = class(TInterfacedObject, ISelector)
  private
    FResults : TList<TObject>;
    FObject : TObject;
    FContext : string;
  protected
    procedure RequireContext;
  public
    constructor Create(AObject : TObject; AContext : string; AResults : TList<TObject>); virtual;
    destructor Destroy; override;

    // chained selection refinement methods
    function has(PropertyName : string) : ISelector; overload;
    function has(PropertyNames : TArray<string>) : ISelector; overload;
    function has(NameValues : TArray<TPropValue>) : ISelector; overload;
    function has(PropertyName : string; Value : TValue) : ISelector; overload;
    function can(MethodName : string) : ISelector; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>) : ISelector; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : ISelector; overload;
    function isa(ClassType : TClass) : ISelector;
    function &on(PropertyName : string) : ISelector;
    function first(Cnt : integer) : ISelector; overload;
    function first : ISelector; overload;
    function last(Cnt : integer) : ISelector; overload;
    function last : ISelector; overload;
    function middle(Idx, Cnt : integer) : ISelector;
    function even : ISelector;
    function odd : ISelector;
    function use(Context : string) : ISelector;

    // retrieves items array
    function count : integer;
    function items : TArray<TObject>;

    // Property Actions upon the entire selection
    function setTo(Value : TValue) : ISelector; overload;
    function setTo(NameValues : TArray<TPropValue>) : ISelector; overload;
    function setTo(propertyName : string; Value : TValue) : ISelector; overload;
    function replace(Find : TValue; Replace : TValue) : ISelector; overload;
    function replace(propertyName : string; Find : TValue; Replace : TValue) : ISelector; overload;
    function add(Value : TValue) : ISelector; overload;
    function add(propertyName : string; Value : TValue) : ISelector; overload;
    function subtract(Value : TValue) : ISelector; overload;
    function subtract(propertyName : string; Value : TValue) : ISelector; overload;

    function notEqualTo(Value : TValue) : ISelector; overload;
    function notEqualTo(propertyName : string; Value : TValue) : ISelector; overload;
    function equalTo(Value : TValue) : ISelector; overload;
    function equalTo(propertyName : string; Value : TValue) : ISelector; overload;
    function moreThan(Value : TValue) : ISelector; overload;
    function moreThan(propertyName : string; Value : TValue) : ISelector; overload;
    function lessThan(Value : TValue) : ISelector; overload;
    function lessThan(propertyName : string; Value : TValue) : ISelector; overload;

    // Method Actions upon the entire selection
    function go : ISelector; overload;
    function go(methodName : string) : ISelector; overload;
    function go(args : TArray<TValue>) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;
    function call : TArray<TValue>; overload;
    function call(methodName : string) : TArray<TValue>; overload;
    function call(args : TArray<TValue>) : TArray<TValue>; overload;
    function call(args : array of variant) : TArray<TValue>; overload;
    function call(methodName : string; args : TArray<TValue>) : TArray<TValue>; overload;
    function call(methodName : string; args : array of Variant; var AResult : TValue) : TArray<TValue>; overload;
    function each(method : TProc<TObject>) : ISelector; overload;
    function filter(method : TFunc<TObject,boolean>) : ISelector;
  end;

  TDuckImpl = class(TInterfacedObject, IDuck)
  private
    FOwner : TObject;
    function IndexParam(idx : integer) : TArray<TValue>;
  public
    constructor Create(AOwner: TObject); virtual;
    procedure EnumerateAssociatedObjects(AOwner : TObject; List : TList<TObject>); virtual;
    procedure EnumerateObjectProperties(AOwner : TObject; List : TList<TObject>); virtual;

    // IDuck
    procedure setTo(propertyName : string; Value : TValue); overload;
    procedure setTo(NameValues : TArray<TPropValue>); overload;
    procedure replace(propertyName : string; Find : TValue; Replace : TValue);
    function get(propertyName : string) : TValue; overload;
    function get(propertyName : string; OfType : TClass) : TValue; overload;
  	function has(propertyName : string) : boolean; overload;
  	function has(propertyName : string; OfType : TClass) : boolean; overload;
	  function can(methodName : string) : boolean; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>) : boolean; overload;
	  function can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : boolean; overload;
    function go(methodName : string) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;
    function call(methodName : string) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>) : TValue; overload;
    function call(methodName : string; args : array of variant) : TValue; overload;
    function call(methodName : string; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : array of Variant; var Exists : boolean) : TValue; overload;
    function call(methodName : string; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : TArray<TValue>; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : array of Variant; var AResult : TValue) : boolean; overload;
    function intercept(methodName: string): IIntercept;
    function obj : TObject;
    function all : ISelector;
    function props : ISelector;
    function related : ISelector;

    function this : ISelector;
  end;

  TInterceptImpl = class(TInterfacedObject, IIntercept)
  type
    TInterceptInfo = class
    private
      FImpl: TMethodImplementation;
      FOriginalCode: Pointer;
      FProxyCode: Pointer;
      FMethod: TRttiMethod;
    public
      constructor Create(AOriginalCode: Pointer; AMethod: TRttiMethod;
        const ACallback: TMethodImplementationCallback);
      destructor Destroy; override;
      property OriginalCode: Pointer read FOriginalCode;
      property ProxyCode: Pointer read FProxyCode;
      property Method: TRttiMethod read FMethod;
    end;
  private
    FContext: TRttiContext;
    FDuck : IDuck;
    FDoBeforeFunc : TInterceptBeforeFunc;
    FDoBeforeSimpleFunc : TInterceptAfterFunc;
    FDoBefore : TInterceptBeforeProc;
    FDoBeforeSimple : TInterceptAfterProc;
    FDoAfter : TInterceptAfterProc;
    FDoAfterFunc : TInterceptAfterFunc;
    FDoExcept : TInterceptExceptionProc;
    FObject : TObject;
    FMethodName : string;
    FImplementationCallback: TMethodImplementationCallback;
    FOriginalClass: TClass;
    FIntercept: TInterceptInfo;
    FProxyClassData: Pointer;
    FProxyClass: TClass;
    procedure RawCallback(UserData: Pointer; const Args: TArray<TValue>;
      out Result: TValue);
  public
    constructor Create(ADuck : IDuck; AObject : TObject; AMethodName : string);
    destructor Destroy; override;

    function before(proc : TInterceptBeforeProc) : IIntercept; overload;
    function before(proc : TInterceptAfterProc) : IIntercept; overload;
    function before(proc : TInterceptBeforeFunc) : IIntercept; overload;
    function before(proc : TInterceptAfterFunc) : IIntercept; overload;
    function after(proc : TInterceptAfterProc) : IIntercept; overload;
    function after(proc : TInterceptAfterFunc) : IIntercept; overload;
    function exception(proc : TInterceptExceptionProc) : IIntercept;
    function abort : IIntercept;
    function skip : IIntercept;
  end;

var
  enumprops : TArray<TArray<string>>;

function DuckVarType : TVarType;
begin
  Result := DuckVariant.VarType;
end;

function VarIsDuck(const AValue : Variant) : Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = DuckVarType;
end;

function VarAsDuck(const AValue : Variant) : IImpersonated;
begin
  if not VarIsDuck(AValue) then
    VarCast(Result,AValue,DuckVarType)
  else
    Result := AValue;
end;

function PV(const APropertyName : string; const AValue : TValue) : TPropValue;
begin
  Result.Create(APropertyName, AValue);
end;

procedure RegisterPropertyEnumerator(const IndexedProperty : string; const CountProperty : string);
begin
  setLength(enumprops,length(enumprops)+1);
  setLength(enumprops[length(enumprops)-1],2);
  enumprops[length(enumprops)-1][0] := IndexedProperty;
  enumprops[length(enumprops)-1][1] := CountProperty;
end;

procedure InitializeEnumProps;
begin
  RegisterPropertyEnumerator('Children','ChildCount');
  RegisterPropertyEnumerator('Components','ComponentCount');
  RegisterPropertyEnumerator('Items','Count');
  RegisterPropertyEnumerator('Items','ItemCount');
  RegisterPropertyEnumerator('Objects','Count');
  RegisterPropertyEnumerator('Objects','ObjectCount');
  RegisterPropertyEnumerator('Commands','CommandCount');
  RegisterPropertyEnumerator('DataSets','Dataset');
  RegisterPropertyEnumerator('Fields','FieldCount');
  RegisterPropertyEnumerator('FieldDefs','FieldDefCount');
  RegisterPropertyEnumerator('IndexFields','IndexFieldCount');
  RegisterPropertyEnumerator('Errors','ErrorCount');
  RegisterPropertyEnumerator('Sessions','SessionCount');
  RegisterPropertyEnumerator('Databases','DatabaseCount');
  RegisterPropertyEnumerator('BindComps','BindCompCount');
  RegisterPropertyEnumerator('Nodes','NodeCount');
  RegisterPropertyEnumerator('Forms','FormCount');
  RegisterPropertyEnumerator('DataModules','DataModuleCount');
  RegisterPropertyEnumerator('Columns','ColumnCount');
  RegisterPropertyEnumerator('ListItems','Count');
  RegisterPropertyEnumerator('Printers','Count');
  RegisterPropertyEnumerator('Points','Count');
  RegisterPropertyEnumerator('Rows','RowCount');
end;

{ TSoftInterface<I> }

constructor  TSoftInterface<I>.Create(Source : TObject);
begin
  FSource := Source;
  inherited Create(TypeInfo(I), DoInvoke);
end;

procedure TSoftInterface<I>.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  ary : TArray<TValue>;
  i : integer;
begin
  SetLength(ary,Length(Args)-1);
  for i := 1 to length(args)-1 do
    ary[i-1] := Args[i];

  Result := TRTTI.call(FSource,Method.Name,ary);
end;

{ TRTTI }

class function TRTTI.call(obj: TObject; const MethodName: string; args: TArray<TValue>): TValue;
var
  vResult : TValue;
  bCalled : boolean;
begin
  Result := nil;
  bCalled := False;
  vResult := TValue.Empty;
  TRTTI.eachMethod(obj,function(m : TRTTIMethod) : boolean
  var
    params : TArray<TRTTIParameter>;
    i : integer;
    b : boolean;
  begin
    Result := True;
    if (CompareText(m.Name,MethodName)=0) then
    begin
      params :=  m.GetParameters;
      if length(params) = length(args) then
      begin
        b := True;
        for i := 0 to Length(args)-1 do
        begin
          if args[i].TypeInfo^.Kind <> params[i].ParamType.TypeKind then
          begin
            b := false;
            break;
          end;
        end;
        if b then
        begin
          vResult := m.Invoke(obj,args);
          Result := false;
          bCalled := True;
        end;
      end;
    end;
  end);
  if not bCalled then
    raise EMethodNotFound.Create('Could not find method "'+MethodName+'" on Object of class "'+obj.ClassName+'"');
  Result := vResult;
end;

class function TRTTI.get<T>(obj: TObject; const PropertyName: string): T;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := T(nil);
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj).AsType<T>;
  end;
end;

class function TRTTI.get<T>(obj: TObject; const PropertyName: string; args : TArray<TValue>): T;
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  Result := T(nil);
  prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj, args).AsType<T>;
  end;
end;

class function TRTTI.getValue(obj: TObject; const PropertyName: string): TValue;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := nil;
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj);
  end;
end;

class function TRTTI.getValue(obj: TObject; const PropertyName: string; args : TArray<TValue>) : TValue;
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  Result := nil;
  prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj, args);
  end;
end;

class function TRTTI.asValue<T>(Value : T) : TValue;
begin
  Result := TValue.From<T>(Value);
end;

class function TRTTI.exists(obj: TObject; const Name: string): boolean;
begin
  Result := isProperty(obj, Name) or isMethod(obj, Name);
end;

class function TRTTI.exists(pti : PTypeInfo; const Name: string): boolean;
begin
  Result := isProperty(pti, Name) or isMethod(pti, Name);
end;

class function TRTTI.field<T>(obj: TObject; const FieldName: string): T;
var
  cxt : TRTTIContext;
  prop : TRttiField;
begin
  Result := T(nil);
  prop := cxt.GetType(obj.ClassInfo).GetField(FieldName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj).AsType<T>;
  end;
end;

class function TRTTI.isProperty(obj: TObject; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := (cxt.GetType(obj.ClassInfo).GetProperty(PropertyName) <> nil) or
            (cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName) <> nil);
end;

class function TRTTI.isProperty(pti : PTypeInfo; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := (cxt.GetType(pti).GetProperty(PropertyName) <> nil) or
            (cxt.GetType(pti).GetIndexedProperty(PropertyName) <> nil);
end;

class function TRTTI.isProperty(obj: TObject; const PropertyName: string; OfType : TClass): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiMember;
begin
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  Result := (prop <> nil) and ((prop.ClassType = OfType) or prop.InheritsFrom(OfType));
  if not Result then
  begin
    prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
    Result := (prop <> nil) and ((prop.ClassType = OfType) or prop.InheritsFrom(OfType));
  end;
end;

class function TRTTI.isProperty(pti : PTypeInfo; const PropertyName: string; OfType : TClass): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiMember;
begin
  prop := cxt.GetType(pti).GetProperty(PropertyName);
  Result := (prop <> nil) and ((prop.ClassType = OfType) or prop.InheritsFrom(OfType));
  if not Result then
  begin
    prop := cxt.GetType(pti).GetIndexedProperty(PropertyName);
    Result := (prop <> nil) and ((prop.ClassType = OfType) or prop.InheritsFrom(OfType));
  end;
end;


class function TRTTI.isSame(const Value1, Value2: TValue): boolean;
begin
  result :=
    (Value1.TypeInfo = Value2.TypeInfo) and
    (  ((Value1.DataSize = Value2.DataSize) and (Value1.GetReferenceToRawData = Value2.GetReferenceToRawData)) or
       (Value1.IsObject and Value2.IsObject and (Value1.AsObject = Value2.AsObject)) or
       (Value1.IsClass and Value2.IsClass and (Value1.AsClass = Value2.AsClass)) or
       (Value1.IsOrdinal and Value2.IsOrdinal and (Value1.AsOrdinal = Value2.AsOrdinal)) or
       (Value1.IsClass and Value2.IsClass and (Value1.AsClass = Value2.AsClass)) or
       (Value1.IsType<string> and Value2.IsType<string> and (Value1.AsString = Value2.AsString)) or
       (Value1.IsType<boolean> and Value2.IsType<boolean> and (Value1.AsBoolean = Value2.AsBoolean)) or
       (Value1.IsType<extended> and Value2.IsType<string> and (Value1.AsExtended = Value2.AsExtended)) or
       (Value1.IsType<IInterface> and Value2.IsType<IInterface> and (Value1.AsInterface = Value2.AsInterface)) or
       (Value1.IsType<Currency> and Value2.IsType<string> and (Value1.AsCurrency = Value2.AsCurrency)) or
       (Value1.IsType<Variant> and Value2.IsType<Variant> and (Value1.AsVariant = Value2.AsVariant))
    );
end;

class function TRTTI.isSame(const Params1, Params2: TArray<TRttiParameter>) : boolean;
var
  i: Integer;
begin
  Result := Length(Params1) = Length(Params2);
  if Result then
    for i := Low(Params1) to High(Params1) do
    begin
      Result := Params1[i].ParamType = Params2[i].ParamType;
      if not Result then
        break;
    end;
end;

class function TRTTI.isA<T>(obj: TObject; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := False;
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj).IsType(TypeInfo(T));
  end;
end;

class function TRTTI.isA<T>(obj: TObject; const PropertyName: string;
  args: TArray<TValue>): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  Result := False;
  prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(obj, args).IsType(TypeInfo(T));
  end;
end;

class function TRTTI.isMethod(obj: TObject; const MethodName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(obj.ClassInfo).GetMethod(MethodName) <> nil;
end;

class function TRTTI.isMethod(pti : PTypeInfo; const MethodName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(pti).GetMethod(MethodName) <> nil;
end;

class function TRTTI.isMethod(obj: TObject; const MethodName: string; Params : TArray<TRTTIParameter>) : boolean;
var
  cxt : TRTTIContext;
  m : TRTTIMethod;
begin
  m := cxt.GetType(obj.ClassInfo).GetMethod(MethodName);
  Result := (m <> nil) and isSame(m.GetParameters,Params);
end;

class function TRTTI.isMethod(pti : PTypeInfo; const MethodName: string; Params : TArray<TRTTIParameter>) : boolean;
var
  cxt : TRTTIContext;
  m : TRTTIMethod;
begin
  m := cxt.GetType(pti).GetMethod(MethodName);
  Result := (m <> nil) and isSame(m.GetParameters,Params);
end;

class function TRTTI.isMethod(obj: TObject; const MethodName: string; Params : TArray<TRTTIParameter>; Returns : TRTTIType) : boolean;
var
  cxt : TRTTIContext;
  b : boolean;
begin
  b := False;
  TRTTI.eachMethod(obj,
    function(meth : TRTTIMethod) : boolean
    begin
      b := (meth <> nil) and (Returns = meth.ReturnType) and isSame(meth.GetParameters,Params);
      Result := not b;
    end
  );
  result := b;
end;

class function TRTTI.isMethod(pti : PTypeInfo; const MethodName: string; Params : TArray<TRTTIParameter>; Returns : TRTTIType) : boolean;
var
  cxt : TRTTIContext;
  m : TRTTIMethod;
begin
  m := cxt.GetType(pti).GetMethod(MethodName);
  Result := (m <> nil) and (Returns = m.ReturnType) and isSame(m.GetParameters,Params);
end;

class procedure TRTTI.setTo<T>(obj: TObject; const PropertyName: string; Value: T);
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(obj, TValue.From<T>(Value));
  end;
end;

class procedure TRTTI.setTo<T>(obj: TObject; const PropertyName: string;
  args : TArray<TValue>; Value: T);
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(obj, args, TValue.From<T>(Value));
  end;
end;

class procedure TRTTI.setValue(obj: TObject; const PropertyName: string; Value: TValue);
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(obj, Value);
  end;
end;

class procedure TRTTI.setValue(obj: TObject; const PropertyName: string; args : TArray<TValue>; Value : TValue);
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  prop := cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(obj, args, Value);
  end;
end;

class function TRTTI.tryCall(obj: TObject; const MethodName: string; args: TArray<TValue>; var AResult: TValue): boolean;
begin
  result := isMethod(obj, MethodName);
  if result then
    AResult := call(obj, MethodName, args);
end;

class function TRTTI.tryGet<T>(obj: TObject; const PropertyName: string;
  args : TArray<TValue>; var AResult: T): boolean;
begin
  result := isProperty(obj, PropertyName);
  if result then
    AResult := get<T>(obj, PropertyName, args);
end;

class function TRTTI.tryGet<T>(obj: TObject; const PropertyName: string; var AResult: T): boolean;
begin
  result := isProperty(obj, PropertyName);
  if result then
    AResult := get<T>(obj, PropertyName);
end;

class function TRTTI.trySet<T>(obj: TObject; const PropertyName: string; Value: T): boolean;
begin
  result := isProperty(obj,propertyName);
  if result then
    setTo<T>(obj, propertyName, Value);
end;

class function TRTTI.trySet<T>(obj: TObject; const PropertyName: string;
  args : TArray<TValue>; Value: T): boolean;
begin
  result := isProperty(obj,propertyName);
  if result then
    setTo<T>(obj, propertyName, args, Value);
end;

class procedure TRTTI.eachProperty(obj : TObject; proc : TProc<TRTTIProperty>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiProperty>;
  i: Integer;
begin
  ary := cxt.GetType(obj.ClassInfo).GetProperties;
  for i := 0 to length(ary)-1 do
    proc(ary[i]);
end;

class procedure TRTTI.eachProperty(pti : PTypeInfo; proc : TProc<TRTTIProperty>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiProperty>;
  i: Integer;
begin
  ary := cxt.GetType(pti).GetProperties;
  for i := 0 to length(ary)-1 do
    proc(ary[i]);
end;

class procedure TRTTI.eachMethod(obj : TObject; proc : TProc<TRTTIMethod>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiMethod>;
  i: Integer;
begin
  ary := cxt.GetType(obj.ClassInfo).GetMethods;
  for i := 0 to length(ary)-1 do
    proc(ary[i]);
end;

class procedure TRTTI.eachMethod(pti : PTypeInfo; proc : TProc<TRTTIMethod>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiMethod>;
  i: Integer;
begin
  ary := cxt.GetType(pti).GetMethods;
  for i := 0 to length(ary)-1 do
    proc(ary[i]);
end;

class procedure TRTTI.eachProperty(obj : TObject; proc : TFunc<TRTTIProperty, boolean>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiProperty>;
  i: Integer;
begin
  ary := cxt.GetType(obj.ClassInfo).GetProperties;
  for i := 0 to length(ary)-1 do
    if not proc(ary[i]) then
      break;
end;

class procedure TRTTI.eachProperty(pti : PTypeInfo; proc : TFunc<TRTTIProperty, boolean>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiProperty>;
  i: Integer;
begin
  ary := cxt.GetType(pti).GetProperties;
  for i := 0 to length(ary)-1 do
    if not proc(ary[i]) then
      break;
end;

class procedure TRTTI.eachMethod(obj : TObject; proc : TFunc<TRTTIMethod, boolean>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiMethod>;
  i: Integer;
begin
  ary := cxt.GetType(obj.ClassInfo).GetMethods;
  for i := 0 to length(ary)-1 do
    if not proc(ary[i]) then
      break;
end;

class procedure TRTTI.eachMethod(pti : PTypeInfo; proc : TFunc<TRTTIMethod, boolean>);
var
  cxt : TRTTIContext;
  ary : TArray<TRttiMethod>;
  i: Integer;
begin
  ary := cxt.GetType(pti).GetMethods;
  for i := 0 to length(ary)-1 do
    if not proc(ary[i]) then
      break;
end;

{ TEvent }

constructor TEvent.Create;
begin
  inherited Create;
  FEvents := TDictionary<TPair<Pointer,Pointer>,IInterface>.Create;
end;

destructor TEvent.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure TEvent.MakeEvent(const ameth; var evt);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  if Pointer(ameth) <> nil then
  begin
    TMethod(evt).Code := PPVtable(ameth)^^[3];
    TMethod(evt).Data := Pointer(ameth);
  end else
  begin
    TMethod(evt).Code := nil;
    TMethod(evt).Data := nil;
  end;
end;

procedure TEvent.ParseMethod(const meth; var p1, p2 : Pointer);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  p1 := TMethod(meth).Code;
  p2 := TMethod(meth).Data;
end;

function TEvent.From<TEvent, TReference>(proc: TReference): TEvent;
var
  intf : PInterface;
  p1, p2 : Pointer;
begin
  TMonitor.Enter(Self);
  try
    MakeEvent(proc,Result);
    intf := PInterface(@proc);

    ParseMethod(Result, p1, p2);
    FEvents.AddOrSetValue(TPair<Pointer,Pointer>.Create(p1, p2),intf^);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TEvent.ReleaseAndNil<TEvent>(var event : TEvent);
begin
  Release<TEvent>(event);
  event := TEvent(nil);
end;

procedure TEvent.Release<TEvent>(event: TEvent);
type
  PInterface = ^IInterface;
var
  intf : IInterface;
  p1, p2 : Pointer;
begin
  TMonitor.Enter(Self);
  try
    ParseMethod(event, p1, p2);
    FEvents.Remove(TPair<Pointer,Pointer>.Create(p1, p2));
  finally
    TMonitor.Exit(Self);
  end;
end;

function TEvent.Notify(proc: TNotifyReference): TNotifyEvent;
begin
  Result := From<TNotifyEvent, TNotifyReference>(proc)
end;

procedure TEvent.NotifyRelease(event: TNotifyEvent);
begin
  Release<TNotifyEvent>(event);
end;

function TEvent.IsSameEvent(const ameth, evt): boolean;
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  result := (TMethod(evt).Code = PPVtable(ameth)^^[3]) and
            (TMethod(evt).Data = Pointer(ameth));
end;

{ TDuckImpl }

constructor TDuckImpl.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDuckImpl.EnumerateAssociatedObjects(AOwner: TObject; List : TList<TObject>);
  procedure DoIt(const IndexProp : string; const CountProp : string);
  var
    i, iCnt : integer;
    obj : TObject;
  begin
    if TRTTI.tryGet<integer>(FOwner,CountProp, iCnt) then
      for i := 0 to iCnt-1 do
      begin
        if TRTTI.isA<TObject>(FOwner,IndexProp,IndexParam(i)) and
           TRTTI.tryGet<TObject>(FOwner,IndexProp,IndexParam(i), obj) then
        begin
          List.Add(obj);
        end;
      end;
  end;
var
  i : integer;
begin
  for i := 0 to Length(enumprops)-1 do
    DoIt(enumprops[i][0], enumprops[i][1]);
end;

procedure TDuckImpl.EnumerateObjectProperties(AOwner : TObject; List : TList<TObject>);
begin
  TRTTI.eachProperty(AOwner,procedure(prop : TRTTIProperty)
  begin
    case prop.PropertyType.TypeKind of
      TTypeKind.tkClass:
        List.Add(prop.GetValue(AOwner).AsObject);
      {TTypeKind.tkClassRef:
        List.Add(prop.GetValue(AOwner).AsClass);} // TODO: support class properties?
    end;
  end);
end;

function TDuckImpl.intercept(methodName: string): IIntercept;
begin
  Result := TInterceptImpl.Create(Self, FOwner, methodName);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>): TValue;
begin
  call(methodName,args,Result);
end;

function TDuckImpl.call(methodName : string; args : array of variant) : TValue;
begin
  call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>;
  var Exists: boolean): TValue;
begin
  Exists := call(methodName,args,Result);
end;

function TDuckImpl.call(methodName : string; args : array of Variant; var Exists : boolean) : TValue;
begin
  Exists := call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; var Exists: boolean): TValue;
var
  args : TArray<TValue>;
begin
  Exists := call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>;
  var AResult: TValue): boolean;
var
  cxt : TRTTIContext;
begin
  Result := TRTTI.isMethod(FOwner,methodName);
  if Result then
  begin
    AResult := cxt.GetType(FOwner.ClassInfo).GetMethod(methodName).Invoke(FOwner,Args);
  end;
end;

function TDuckImpl.call(methodName: string; args: array of variant;
  var AResult: TValue): boolean;
var
  cxt : TRTTIContext;
  vargs : TArray<TValue>;
  i: Integer;
  vResult : TValue;
begin
  Result := TRTTI.isMethod(FOwner,methodName);
  if Result then
  begin
    SetLength(vargs, Length(args));
    for i := low(vargs) to High(vargs) do
      vargs[i] := TValue.FromVariant(args[i]);

    vResult := TValue.Empty;
    TRTTI.eachMethod(FOwner,
      function(meth : TRTTIMethod) : boolean
      var
        ary : TArray<TRTTIParameter>;
        b : boolean;
        i : integer;
      begin
        result := true;
        if SameText(meth.Name, methodName) then
        begin
          ary := meth.GetParameters;
          if length(ary) = Length(vargs) then
          begin
            b := True;
            for i := 0 to length(ary)-1 do
            begin
              if ary[i].ParamType.TypeKind <> vargs[i].Kind then
              begin
                b := false;
                break;
              end;
            end;
            if b then
            begin
              vResult := meth.Invoke(FOwner,vargs);
                result := false;
            end;
          end;
        end;
      end
    );
  end;
  AResult := vResult;
  result := not vResult.IsEmpty;
end;

function TDuckImpl.call(methodName: string; var AResult: TValue): boolean;
var
  args : TArray<TValue>;
begin
  Result := call(methodName,args,AResult);
end;

function TDuckImpl.call(methodName: string): TValue;
var
  args : TArray<TValue>;
begin
  call(methodName, args, Result);
end;

function TDuckImpl.go(methodName : string) : ISelector;
var
  lst : TList<TObject>;
  v : TValue;
begin
  lst := TList<TObject>.Create;
  try
    v := call(methodName);
    if v.IsObject then
      lst.Add(v.AsObject);
    Result := TSelectorImpl.Create(FOwner,'',lst);
  finally
    lst.Free;
  end;
end;

function TDuckImpl.go(methodName : string; args : TArray<TValue>) : ISelector;
var
  lst : TList<TObject>;
  v : TValue;
begin
  lst := TList<TObject>.Create;
  try
    v := call(methodName, args);
    if v.IsObject then
      lst.Add(v.AsObject);
    Result := TSelectorImpl.Create(FOwner,'',lst);
  finally
    lst.Free;
  end;
end;

function TDuckImpl.get(propertyName: string): TValue;
begin
  Result := TRTTI.getValue(FOwner,propertyName);
end;

function TDuckImpl.get(propertyName: string; OfType : TClass): TValue;
begin
  if TRTTI.isProperty(FOwner,propertyName, OfType) then
    Result := TRTTI.getValue(FOwner,propertyName)
  else
    Result := TValue.Empty;
end;

function TDuckImpl.all: ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    EnumerateAssociatedObjects(FOwner, lst);
    EnumerateObjectProperties(FOwner, lst);
    Result := TSelectorImpl.Create(FOwner, '', lst);
  finally
    lst.Free;
  end;
end;

function TDuckImpl.props : ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    EnumerateObjectProperties(FOwner, lst);
    Result := TSelectorImpl.Create(FOwner, '', lst);
  finally
    lst.Free;
  end;
end;

function TDuckImpl.related : ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    EnumerateAssociatedObjects(FOwner, lst);
    Result := TSelectorImpl.Create(FOwner, '', lst);
  finally
    lst.Free;
  end;
end;

function TDuckImpl.this : ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    lst.Add(Self);
    Result := TSelectorIMpl.Create(FOwner,'',lst);
  finally
    lst.Free;
  end;
end;

procedure TDuckImpl.setTo(propertyName: string; Value: TValue);
begin
  TRTTI.setValue(FOwner,propertyName, Value);
end;

procedure TDuckImpl.setTo(NameValues : TArray<TPropValue>);
var
  i : integer;
begin
  for i := 0 to Length(NameValues)-1 do
    TRTTI.setValue(FOwner,NameValues[i].PropertyName, NameValues[i].Value);
end;

procedure TDuckImpl.replace(propertyName : string; Find : TValue; Replace : TValue);
begin
  if TRTTI.isSame(TRTTI.getValue(FOwner,propertyName), Find) then
    TRTTI.setValue(FOwner,propertyName,Replace);
end;

function TDuckImpl.has(propertyName : string) : boolean;
begin
  Result := TRTTI.isProperty(FOwner, propertyName);
end;

function TDuckImpl.has(propertyName : string; OfType : TClass) : boolean;
begin
  Result := TRTTI.isProperty(FOwner, propertyName, OfType);
end;

function TDuckImpl.IndexParam(idx: integer): TArray<TValue>;
begin
  SetLength(Result,1);
  Result[0] := idx;
end;

function TDuckImpl.obj: TObject;
begin
  Result := FOwner;
end;

function TDuckImpl.can(methodName : string) : boolean;
begin
  Result := TRTTI.isMethod(FOwner, methodName);
end;

function TDuckImpl.can(methodName : string; Params : TArray<TRttiParameter>) : boolean;
begin
  Result := TRTTI.isMethod(FOwner, methodName, Params);
end;

function TDuckImpl.can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : boolean;
begin
  Result := TRTTI.isMethod(FOwner, methodName, Params, Returns);
end;

{ TDuckHelper }

function TDuckHelper.duck: IDuck;
begin
  Result := TDuckImpl.Create(Self);
end;

function TDuckHelper.impersonates<Intf> : boolean;
var
  bOK : boolean;
begin
  bOK := True;
  TRTTI.eachProperty(TypeInfo(Intf),procedure(prop : TRTTIProperty)
  begin
    bOK := bOK and duck.has(prop.Name, prop.ClassType);
  end);
  TRTTI.eachMethod(TypeInfo(Intf),procedure(method : TRTTIMethod)
  begin
    bOK := bOK and duck.can(method.Name, method.GetParameters, method.ReturnType);
  end);
  Result := bOK;
end;

function TDuckHelper.asa<Intf> : IImpersonated;
var
  p : pointer;
begin
  System.VarClear(Result);
  p := @Result;
  TDuckVarData(p^).VType := DuckVariant.VarType;
  TDuckVarData(p^).VDuck := duck;
  TDuckVarData(p^).VIType := TypeInfo(Intf);
end;

function TDuckHelper.duck<I> : TSoftInterface<I>;
begin
  Result := TSoftInterface<I>.Create(Self);
end;


{ TDuck }

function TDuck.duck: IDuck;
begin
  Result := TDuckImpl.Create(Self);
end;

function TDuck.impersonates<Intf> : boolean;
var
  bOK : boolean;
begin
  bOK := True;
  TRTTI.eachProperty(TypeInfo(Intf),procedure(prop : TRTTIProperty)
  begin
    bOK := bOK and duck.has(prop.Name, prop.ClassType);
  end);
  TRTTI.eachMethod(TypeInfo(Intf),procedure(method : TRTTIMethod)
  begin
    bOK := bOK and duck.can(method.Name, method.GetParameters, method.ReturnType);
  end);
  Result := bOK;
end;

function TDuck.asA<Intf> : IImpersonated;
var
  p : pointer;
begin
  System.VarClear(Result);
  p := @Result;
  TDuckVarData(p^).VType := DuckVariant.VarType;
  TDuckVarData(p^).VDuck := duck;
  TDuckVarData(p^).VIType := TypeInfo(Intf);
end;

function TDuck.duck<I> : TSoftInterface<I>;
var
  Intf : IInterface;
begin
  Result := TSoftInterface<I>.Create(Self);
end;


{ TSelectorImpl }

constructor TSelectorImpl.Create(AObject: TObject; AContext : string; AResults : TList<TObject>);
var
  i : integer;
begin
  inherited Create;
  FObject := AObject;
  FContext := AContext;
  FResults := TList<TObject>.Create;
  for i := 0 to AResults.Count-1 do
  begin
    if AResults[i] <> nil then
    begin
      if not FResults.Contains(AResults[i]) then
        FResults.Add(AResults[i]);
    end;
  end;
end;

destructor TSelectorImpl.Destroy;
begin
  FResults.Free;
  inherited;
end;

function TSelectorImpl.each(method: TProc<TObject>): ISelector;
var
  i: Integer;
begin
  for i := 0 to FResults.Count-1 do
    method(FResults.Items[i]);
  Result := Self;
end;

function TSelectorImpl.even : ISelector;
var
  lst : TList<TObject>;
  i: Integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
      if (i+1) mod 2 = 0 then
        lst.Add(FResults.Items[i]);
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.odd : ISelector;
var
  lst : TList<TObject>;
  i: Integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
      if (i+1) mod 2 <> 0 then
        lst.Add(FResults.Items[i]);
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.filter(method: TFunc<TObject, boolean>): ISelector;
var
  i: Integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if method(FResults.Items[i]) then
        lst.Add(FResults.Items[i])
    end;
    Result := TSelectorImpl.Create(FObject,'',lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.go(args: TArray<TValue>): ISelector;
begin
  call(args);
  Result := Self;
end;

function TSelectorImpl.go(methodName: string): ISelector;
begin
  call(methodName);
  result := Self;
end;

function TSelectorImpl.go: ISelector;
begin
  call;
  result := self;
end;

function TSelectorImpl.go(methodName: string; args: TArray<TValue>): ISelector;
begin
  call(methodName, args);
  result := Self;
end;

function TSelectorImpl.call(methodName: string): TArray<TValue>;
var
  i : integer;
  args : TArray<TValue>;
begin
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],methodName, args);
  end;
end;

function TSelectorImpl.add(propertyName: string; Value: TValue): ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],propertyName).AsVariant + Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.add(Value: TValue): ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],FContext).AsVariant + Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.call(methodName: string;
  args: TArray<TValue>): TArray<TValue>;
var
  i : integer;
begin
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],methodName, args);
  end;
end;

function TSelectorImpl.call(methodName : string; args : array of Variant; var AResult : TValue) : TArray<TValue>;
var
  i,j : integer;
  vargs : TArray<TValue>;
begin
  setLength(vargs,Length(args));
  for i := low(vargs) to High(vargs) do
    vargs[i] := TValue.FromVariant(args[i]);

  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],methodName, vargs);
  end;
end;

function TSelectorImpl.call: TArray<TValue>;
var
  i : integer;
  args : TArray<TValue>;
begin
  RequireContext;
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    try
      result[i] := TRTTI.call(FResults.Items[i],FContext, args);
    except
      on e: EMethodNotFound do
        Continue
      else
        raise;
    end;
  end;
end;

function TSelectorImpl.call(args: TArray<TValue>): TArray<TValue>;
var
  i : integer;
begin
  RequireContext;
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],FContext, args);
  end;
end;

function TSelectorImpl.call(args : array of variant) : TArray<TValue>;
var
  i : integer;
  vargs : TArray<TValue>;
begin
  RequireContext;
  setLength(vargs, Length(args));
  for i := low(vargs) to high(vargs) do
    vargs[i] := TValue.FromVariant(args[i]);

  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],FContext, vargs);
  end;
end;

function TSelectorImpl.can(MethodName: string): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isMethod(FResults.Items[i],methodName) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, MethodName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.can(methodName : string; Params : TArray<TRttiParameter>) : ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isMethod(FResults.Items[i],methodName, Params) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, MethodName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.can(methodName : string; Params : TArray<TRttiParameter>; Returns : TRttiType) : ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isMethod(FResults.Items[i],methodName, Params, Returns) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, MethodName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.count: integer;
begin
  Result := FResults.Count;
end;

function TSelectorImpl.first(Cnt : integer) : ISelector;
var
  lst : TList<TObject>;
  i, iMax : integer;
begin
  lst := TList<TObject>.Create;
  try
    if Count > 0 then
    begin
      if Cnt > Count then
        iMax := Count-1
      else
        iMax := Cnt-1;
      for i := 0 to iMax do
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.first : ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    if Count > 0 then
      lst.Add(FResults.Items[0]);
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(NameValues: TArray<TPropValue>): ISelector;
var
  lst : TList<TObject>;
  i, j : integer;
  b : boolean;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      b := True;

      for j := 0 to length(NameValues)-1 do
      begin
        if (not TRTTI.isProperty(FResults.Items[i],NameValues[j].PropertyName)) or
           (not TRTTI.isSame(TRTTI.getValue(FResults.Items[i],NameValues[j].PropertyName), NameValues[j].Value)) then
        begin
          b := False;
          break;
        end;
      end;
      if b then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, '', lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(PropertyNames: TArray<string>): ISelector;
var
  lst : TList<TObject>;
  i, j : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      for j := 0 to Length(PropertyNames)-1 do
        if TRTTI.isProperty(FResults.Items[i],PropertyNames[j]) then
        begin
          lst.Add(FResults.Items[i]);
        end;
    end;
    Result := TSelectorImpl.Create(FObject,'',lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(PropertyName: string): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isProperty(FResults.Items[i],propertyName) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, PropertyName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.isa(ClassType: TClass): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
      if FResults.Items[i] is ClassType then
        lst.Add(FResults.Items[i]);
    Result := TSelectorImpl.Create(FObject, '', lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.items : TArray<TObject>;
begin
  Result := FResults.ToArray;
end;

function TSelectorImpl.last(Cnt : integer) : ISelector;
var
  lst : TList<TObject>;
  i, iMin : integer;
begin
  lst := TList<TObject>.Create;
  try
    if Count > 0 then
    begin
      iMin := FResults.Count-Cnt;
      if iMin < 0 then
        iMin := 0;
      for i := iMin to FResults.Count-1 do
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.last : ISelector;
var
  lst : TList<TObject>;
  i, iMin, iMax : integer;
begin
  lst := TList<TObject>.Create;
  try
    if Count > 0 then
    begin
      lst.Add(FResults.Items[FResults.Count-1]);
    end;
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.middle(Idx, Cnt : integer) : ISelector;
var
  lst : TList<TObject>;
  i, iMax : integer;
begin
  lst := TList<TObject>.Create;
  try
    if Count > 0 then
    begin
      iMax := idx+Cnt-1;
      if iMax > Count-1 then
        iMax := Count-1;
      for i := idx to iMax do
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject, FContext, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.on(propertyName: string): ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isProperty(FResults.Items[i],propertyName) and
         TRTTI.getValue(FResults.Items[i],propertyName).IsObject then
        lst.Add(TRTTI.getValue(FResults.Items[i],propertyName).AsObject);
    end;
    Result := TSelectorImpl.Create(FObject,'',lst); // Context cannot be propertyName as the filtered objects apply to different objects.
  finally
    lst.Free;
  end;
end;


procedure TSelectorImpl.RequireContext;
begin
  if FContext = '' then
    raise Exception.Create(S_MISSINGCONTEXT);
end;

function TSelectorImpl.setTo(Value: TValue) : ISelector;
var
  i : integer;
begin
  RequireContext;

  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, Value);
  end;
  Result := Self;
end;

function TSelectorImpl.setTo(NameValues : TArray<TPropValue>) : ISelector;
var
  i, j : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    for j := 0 to Length(NameValues)-1 do
      TRTTI.setValue(FResults.Items[i],NameValues[j].PropertyName, NameValues[j].Value);
  end;
  Result := Self;
end;

function TSelectorImpl.setTo(propertyName: string; Value: TValue) : ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, Value);
  end;
  Result := Self;
end;

function TSelectorImpl.replace(Find : TValue; Replace : TValue) : ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    if TRTTI.isProperty(FResults.Items[i], FContext) then
    begin
      if TRTTI.isSame(TRTTI.getValue(FResults.Items[i],FContext),Find) then
        TRTTI.setValue(FResults.Items[i],FContext, Replace);
    end;
  end;
  Result := Self;
end;

function TSelectorImpl.replace(propertyName : string; Find : TValue; Replace : TValue) : ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    if TRTTI.isProperty(FResults.Items[i], propertyName) then
    begin
      if TRTTI.isSame(TRTTI.getValue(FResults.Items[i],propertyName),Find) then
        TRTTI.setValue(FResults.Items[i],propertyName, Replace);
    end;
  end;
  Result := Self;
end;

function TSelectorImpl.subtract(Value: TValue): ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],FContext).AsVariant - Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.subtract(propertyName: string; Value: TValue): ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],propertyName).AsVariant - Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.notEqualTo(Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  RequireContext;
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],FContext).AsVariant <> Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.notEqualTo(propertyName : string; Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],propertyName).AsVariant <> Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.equalTo(Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  RequireContext;
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],FContext).AsVariant = Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.equalTo(propertyName : string; Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],propertyName).AsVariant = Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.moreThan(Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  RequireContext;
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],FContext).AsVariant > Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.moreThan(propertyName : string; Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],propertyName).AsVariant > Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.lessThan(Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  RequireContext;
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],FContext).AsVariant < Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.lessThan(propertyName : string; Value : TValue) : ISelector;
var
  i : integer;
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.getValue(FResults.Items[i],propertyName).AsVariant < Value.AsVariant then
        lst.Add(FResults.Items[i]);
    end;
    Result := TSelectorImpl.Create(FObject,FContext,lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(PropertyName: string; Value: TValue): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isProperty(FResults.Items[i],propertyName) then
      begin
        if TRTTI.isSame(TRTTI.getValue(FResults.Items[i],propertyName),Value) then
          lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject,PropertyName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.use(Context : string) : ISelector;
begin
  FContext := Context;
  Result := Self;
end;

{ TPropValue }

constructor TPropValue.Create(const APropertyName: string; const AValue: TValue);
begin
  Self.PropertyName := APropertyName;
  Self.Value := AValue;
end;


{ TDuckVariantType }

procedure TDuckVariantType.Clear(var V: TVarData);
begin
  TDuckVarData(V).VDuck := nil;
  TDuckVarData(V).VIType := nil;
end;

procedure TDuckVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if not (Indirect and VarDataIsByRef(Source)) then
  begin
    TDuckVarData(Dest).VType := VarType;
    TDuckVarData(Dest).VDuck := TDuckVarData(Source).VDuck;
    TDuckVarData(Dest).VIType := TDuckVarData(Source).VIType;
  end else
    VarDataCopyNoInd(Dest,Source);
end;

procedure TDuckVariantType.DispInvoke(Dest: PVarData; [Ref] const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer);
type
  PParamRec = ^TParamRec;
  TParamRec = array[0..3] of LongInt;
const
  CDoMethod    = $01;
  CPropertyGet = $02;
  CPropertySet = $04;
var
  I, LArgCount: Integer;
  LIdent: string;
  LCasedIdent : string;
  LTemp: TVarData;
  VarParams : TVarDataArray;
  Strings: TStringRefList;
  LDest: PVarData;
  v : variant;
  args : TArray<TValue>;
  name : PByteArray;
begin
  // Grab the identifier
  LArgCount := CallDesc^.ArgCount;
  name := @CallDesc^.ArgTypes[LArgCount];
  LCasedIdent := String(PChar(TEncoding.Convert(TEncoding.ANSI,TEncoding.Unicode,name^)));
  LIdent := FixupIdent(LCasedIdent);

  FillChar(Strings, SizeOf(Strings), 0);
  VarParams := GetDispatchInvokeArgs(CallDesc, Params, Strings, true);

  // What type of invoke is this?
  case CallDesc^.CallType of
    CPropertyGet: begin
      if ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 0)) then                       // only no args
      begin
        v := TRTTI.getValue(TDuckVarData(Source).VDuck.obj,LCasedIdent).AsVariant;
        if VarIsNull(v) then
          RaiseDispError;
        Variant(Dest^) := v;
      end else
      begin
        if not ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 1)) then
          RaiseDispError;
        setLength(args,1);
        args[0] := TValue.FromVariant(Variant(VarParams[0]));
        v := TRTTI.getValue(TDuckVarData(Source).VDuck.obj,LCasedIdent,args).AsVariant;
        if VarIsNull(v) then
          RaiseDispError;
        Variant(Dest^) := v;
      end;
    end;
    CPropertySet:
      if ((Dest = nil) and                         // there must be a dest
          (LArgCount = 1)) then
      begin
        setLength(args,1);
        args[0] := TValue.FromVariant(Variant(VarParams[0]));
        TRTTI.setValue(TDuckVarData(Source).VDuck.obj,LCasedIdent,args,TValue.FromVariant(Variant(VarParams[0])));
      end else
      begin
        RaiseDispError;
        (*if not ((Dest = nil) {and                         // there must be a dest
              (LArgCount = 2))} then
          RaiseDispError;
        setLength(args,LArgCount);
        for i := 0 to LArgCount-1 do
          args[i] := TValue.FromVariant((Variant(VarParams[i]));
        TDuckVarData(Source).VDuck.setTo(LCasedIdent,TValue.FromVariant(Variant(VarParams[0]));*)
      end;
  else
    if ((Dest <> nil) and                         // there must be a dest
        (LArgCount = 0)) then                       // only no args
    begin
      v := NULL;
      setLength(args,0);
      if TRTTI.isProperty(TDuckVarData(Source).VDuck.obj,LCasedIdent) then
        v := TRTTI.getValue(TDuckVarData(Source).VDuck.obj,LCasedIdent).AsVariant
      else if TRTTI.isMethod(TDuckVarData(Source).VDuck.obj,LCasedIdent) then
        v := TRTTI.call(TDuckVarData(Source).VDuck.obj,LCasedIdent, args).AsVariant;

      if VarIsNull(v) then
          RaiseDispError;
        Variant(Dest^) := v;
    end else
    begin
      setLength(args,LArgCount);
      for i := 0 to LArgCount-1 do
        args[i] := TValue.FromVariant(Variant(VarParams[i]));
      v := TRTTI.call(TDuckVarData(Source).VDuck.obj,LCasedIdent,args).AsVariant;

      Variant(Dest^) := v;
      exit;
    end;
  end;

  for I := 0 to Length(Strings) - 1 do
  begin
    if Pointer(Strings[I].Wide) = nil then
      Break;
    if Strings[I].Ansi <> nil then
      Strings[I].Ansi^ := String(Strings[I].Wide)
    else if Strings[I].Unicode <> nil then
      Strings[I].Unicode^ := UnicodeString(Strings[I].Wide)
  end;
end;

(*{ TInterceptorImpl }

class constructor TInterceptorImpl.Create;
begin
  FInterceptors := TList<TVirtualMethodInterceptor>.Create;
end;

constructor TInterceptorImpl.Create(AObject: TObject; AMethod: string);
begin
  inherited Create;
  FMethod := AMethod;
  FObject := AObject;
end;

destructor TInterceptorImpl.Destroy;
begin

  inherited;
end;

class destructor TInterceptorImpl.Destroy;
var
  i: Integer;
begin
  for i := 0 to FInterceptors.Count-1 do
    FInterceptors[i].Free;
  FInterceptors.Free;

end;

function TInterceptorImpl.doAfter(proc: TInterceptProc): IInterceptor;
var
  vmi : TVirtualMethodInterceptor;
begin
  Result := Self;
  vmi := TVirtualMethodInterceptor.Create(FClass);
  vmi.OnAfter := procedure(Instance: TObject; Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue)
  begin
    proc(Instance, Args, Result);
  end;
  FInterceptors.Add(vmi);
end;

function TInterceptorImpl.doBefore(proc: TInterceptProc): IInterceptor;
var
  vmi : TVirtualMethodInterceptor;
begin
  Result := Self;
  vmi := TVirtualMethodInterceptor.Create(FClass);
  vmi.OnBefore := procedure(Instance: TObject; Method: TRttiMethod; const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue)
  begin
    DoInvoke := proc(Instance, Args, Result);
  end;
  FInterceptors.Add(vmi);
end;

function TInterceptorImpl.doOnException(proc: TInterceptExProc): IInterceptor;
var
  vmi : TVirtualMethodInterceptor;
begin
  Result := Self;
  vmi := TVirtualMethodInterceptor.Create(FClass);
  vmi.OnException := procedure(Instance: TObject; Method: TRttiMethod; const Args: TArray<TValue>; out RaiseException: Boolean; TheException: Exception; out Result: TValue)
  begin
    RaiseException := proc(Instance, TheException, Args, Result);
  end;
  FInterceptors.Add(TPair<TClass, String>.Create(FClass, FMethod),vmi);
end;
*)

{ TInterceptImpl }

function TInterceptImpl.abort: IIntercept;
begin
  FDoBefore := procedure(Args : TArray<TValue>; var Continue : boolean)
  begin
    Abort;
  end;
  Result := Self;
end;

type

  PProxyClassData = ^TProxyClassData;
  TProxyClassData = record
    SelfPtr: TClass;
    IntfTable: Pointer;
    AutoTable: Pointer;
    InitTable: Pointer;
    TypeInfo: PTypeInfo;
    FieldTable: Pointer;
    MethodTable: Pointer;
    DynamicTable: Pointer;
    ClassName: Pointer;
    InstanceSize: Integer;
    Parent: ^TClass;
  end;

function TInterceptImpl.after(proc: TInterceptAfterProc): IIntercept;
begin
  FDoAfter := proc;
  result := self;
end;

function TInterceptImpl.before(proc: TInterceptBeforeProc): IIntercept;
begin
  FDoBefore := proc;
  result := self;
end;

function TInterceptImpl.before(proc: TInterceptAfterProc): IIntercept;
begin
  FDoBeforeSimple := proc;
  result := self;
end;

constructor TInterceptImpl.Create(ADuck: IDuck; AObject: TObject;
  AMethodName: string);
{$POINTERMATH ON}
type
  PVtable = ^Pointer;
{$POINTERMATH OFF}
var
  t: TRttiType;
  m: TRttiMethod;
  size, classOfs: Integer;
begin
  inherited Create;
  FDuck := ADuck;
  FObject := AObject;
  FMethodName := AMethodName;

  FOriginalClass := AObject.ClassType;
  FImplementationCallback := RawCallback;

  t := FContext.GetType(FOriginalClass);
  size := (t as TRttiInstanceType).VmtSize;
  classOfs := -vmtSelfPtr;
  FProxyClassData := AllocMem(size);
  FProxyClass := TClass(PByte(FProxyClassData) + classOfs);
  Move((PByte(FOriginalClass) - classOfs)^, FProxyClassData^, size);
  PProxyClassData(FProxyClassData)^.Parent := @FOriginalClass;
  PProxyClassData(FProxyClassData)^.SelfPtr := FProxyClass;

  m := t.GetMethod(FMethodName);
  if (m.DispatchKind = dkVtable) and (m.MethodKind in [mkFunction, mkProcedure]) and m.HasExtendedInfo then
    FIntercept := TInterceptInfo.Create(PVtable(FOriginalClass)[m.VirtualIndex], m, FImplementationCallback)
  else
    raise System.SysUtils.Exception.Create('Specified Method cannot be intercepted.');

  PVtable(FProxyClass)[m.VirtualIndex] := FIntercept.ProxyCode;

  if PPointer(FObject)^ <> FOriginalClass then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  PPointer(FObject)^ := FProxyClass;

end;

destructor TInterceptImpl.Destroy;
begin
  if PPointer(FObject)^ <> FProxyClass then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  PPointer(FObject)^ := FOriginalClass;
  FreeMem(FProxyClassData);
  inherited;
end;

function TInterceptImpl.exception(proc: TInterceptExceptionProc): IIntercept;
begin

end;

procedure TInterceptImpl.RawCallback(UserData: Pointer; const Args: TArray<TValue>;
  out Result: TValue);
  procedure PascalShiftSelfLast(cc: TCallConv);
{$IFDEF CPUX86}
  var
    receiver: array[1..SizeOf(TValue)] of Byte;
  begin
    if cc <> ccPascal then Exit;
    Move(Args[0], receiver, SizeOf(TValue));
    Move(Args[1], Args[0], SizeOf(TValue) * (Length(Args) - 1));
    Move(receiver, Args[Length(Args) - 1], SizeOf(TValue));
  end;
{$ELSE !CPUX86}
  begin
  end;
{$ENDIF !CPUX86}

  procedure PascalShiftSelfFirst(cc: TCallConv);
{$IFDEF CPUX86}
  var
    receiver: array[1..SizeOf(TValue)] of Byte;
  begin
    if cc <> ccPascal then Exit;
    Move(Args[Length(Args) - 1], receiver, SizeOf(TValue));
    Move(Args[0], Args[1], SizeOf(TValue) * (Length(Args) - 1));
    Move(receiver, Args[0], SizeOf(TValue));
  end;
{$ELSE !CPUX86}
  begin
  end;
{$ENDIF !CPUX86}

var
  inst: TObject;
  ii: TInterceptInfo;
  argList: TArray<TValue>;
  parList: TArray<TRttiParameter>;
  i: Integer;
  bContinue : boolean;
begin
  ii := UserData;
  inst := Args[0].AsObject;

  SetLength(argList, Length(Args) - 1);
  for i := 1 to Length(Args) - 1 do
    argList[i - 1] := Args[i];
  try
    if Assigned(FDoBefore) then
    begin
      bContinue := True;
      FDoBefore(argList, bContinue);
      if not bContinue then
        exit;
    end;
    if Assigned(FDoBeforeSimple) then
    begin
      FDoBeforeSimple(argList);
    end;
    if Assigned(FDoBeforeFunc) then
    begin
      bContinue := True;
      FDoBeforeFunc(argList, Result, bContinue);
      if not bContinue then
        exit;
    end;
    if Assigned(FDoBeforeSimpleFunc) then
    begin
      FDoBeforeSimpleFunc(argList, Result);
    end;
    try
      parList := ii.Method.GetParameters;
      for i := 1 to Length(Args) - 1 do
      begin
        if
{$IFDEF CPUX86}
          ((ii.Method.CallingConvention in [ccCdecl, ccStdCall, ccSafeCall]) and (pfConst in parList[i-1].Flags) and (parList[i-1].ParamType.TypeKind = tkVariant)) or
{$ENDIF CPUX86}
          ((pfConst in parList[i - 1].Flags) and (parList[i - 1].ParamType.TypeSize > SizeOf(Pointer)))
          or ([pfVar, pfOut] * parList[i - 1].Flags <> []) then
          Args[i] := argList[i - 1].GetReferenceToRawData
        else
          Args[i] := argList[i - 1];
      end;

      PascalShiftSelfLast(ii.Method.CallingConvention);
      try
        if ii.Method.ReturnType <> nil then
          Result := Invoke(ii.OriginalCode, Args, ii.Method.CallingConvention, ii.Method.ReturnType.Handle)
        else
          Result := Invoke(ii.OriginalCode, Args, ii.Method.CallingConvention, nil);
      finally
        PascalShiftSelfFirst(ii.Method.CallingConvention);
      end;
    except
      on e: System.SysUtils.Exception do
      begin
        if Assigned(FDoExcept) then
        begin
          bContinue := True;
          FDoExcept(e, argList, Result, bContinue);
          if bContinue then
            raise;
        end;
      end;
    end;
    if Assigned(FDoAfterFunc) then
    begin
      FDoAfterFunc(argList, Result);
    end;
    if Assigned(FDoAfter) then
    begin
      FDoAfter(argList);
    end;
  finally
    // Set modified by-ref arguments
    for i := 1 to Length(Args) - 1 do
      Args[i] := argList[i - 1];
  end;
end;

function TInterceptImpl.skip: IIntercept;
begin
  FDoBefore := procedure(Args : TArray<TValue>; var Continue : boolean)
  begin
    Continue := False;
  end;
  Result := Self;
end;

function TInterceptImpl.after(proc: TInterceptAfterFunc): IIntercept;
begin
  FDoAfterFunc := proc;
  Result := Self;
end;

function TInterceptImpl.before(proc: TInterceptBeforeFunc): IIntercept;
begin
  FDoBeforeFunc := proc;
  Result := Self;
end;

function TInterceptImpl.before(proc: TInterceptAfterFunc): IIntercept;
begin
  FDoBeforeSimpleFunc := proc;
  Result := Self;
end;

{ TInterceptImpl.TInterceptInfo }

constructor TInterceptImpl.TInterceptInfo.Create(AOriginalCode: Pointer;
  AMethod: TRttiMethod; const ACallback: TMethodImplementationCallback);
begin
  FImpl := AMethod.CreateImplementation(Self, ACallback);
  FOriginalCode := AOriginalCode;
  FProxyCode := FImpl.CodeAddress;
  FMethod := AMethod;
end;

destructor TInterceptImpl.TInterceptInfo.Destroy;
begin
  FImpl.Free;
  inherited;
end;

initialization
  Event := TEvent.Create;
  InitializeEnumProps;
  DuckVariant := TDuckVariantType.Create;

finalization
  Event.Free;
  FreeAndNil(DuckVariant);

end.
