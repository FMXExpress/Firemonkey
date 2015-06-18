unit Execute.MacOS;
{
   MacOS OpenGL Window for Delphi XE2/XE5

   (c)2014 Execute SARL  <contact@execute.re>

   http://www.execute.re

}
interface

type
  TEvent = procedure of object;

  NSApplication = class
  private
    FApp     : THandle;
    FWindow  : THandle;
    FView    : THandle;
    FWidth   : Integer;
    FHeight  : Integer;
    FQueue   : THandle;
    FNotify  : THandle;
    function PixelFormat: THandle;
    procedure DoIdle;
  protected
    procedure SetupGL; virtual;
    procedure OnResize; virtual; abstract;
    procedure OnIdle; virtual;
    procedure OnPaint; virtual; abstract;
  public
    constructor Create(AWidth, AHeight: Integer);
    procedure Run;
    procedure Invalidate;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

implementation

const
  libdl   = '/usr/lib/libdl.dylib';
  libobjc = 'libobjc.A.dylib';

  RTLD_LAZY   = 1;

 // NSWindow.styleMask
  NSTitledWindowMask         = 1;
  NSClosableWindowMask       = 2;
  NSMiniaturizableWindowMask = 4;
  NSResizableWindowMask      = 8;

 // NSWindow.NSBackingStoreType
  NSBackingStoreBuffered = 2;

 // NSNotificationQueue.NSPostingStyle
  NSPostWhenIdle = 1;
  NSPostASAP     = 2;
  NSPostNow      = 3;

  // NSOpenGLPixelFormatAttribute
  NSOpenGLPFADoubleBuffer  = 5;
  NSOpenGLPFADepthSize     = 12;
  NSOpenGLPFAStencilSize   = 13;
  NSOpenGLPFASampleBuffers = 55;
  NSOpenGLPFASamples       = 56;
  NSOpenGLPFAMultisample   = 59;
  NSOpenGLPFAAccelerated   = 73;


type
  NSPoint = record
    x: Single;
    y: Single;
  end;

  NSSize = record
    width: Single;
    height: Single;
  end;

  NSRect = record
    origin: NSPoint;
    size: NSSize;
  end;

  GetContentForFrame = function(id, sel: THandle; Rect: NSRect): NSRect; cdecl varargs;

  NSOpenGLPixelFormatAttribute = Longword;

var
  attrs: array [0..5] of NSOpenGLPixelFormatAttribute = (
  // Accelerated
    NSOpenGLPFAAccelerated,
  // Standard
  //NSOpenGLPFADoubleBuffer, -- not in NSOpenGLView ?
		NSOpenGLPFADepthSize, 32,
	  NSOpenGLPFAStencilSize, 0,
    0
  );

var
  lib_AppKit : THandle;

  msg_alloc  : THandle;
  msg_application_Idle: THandle;
  msg_contentRectForFrameRect: THandle;
  msg_enqueueNotification_postingStyle: THandle;
  msg_frame : THandle;
  msg_init   : THandle;
  msg_initWithAttributes: THandle;
  msg_makeKeyAndOrderFront: THandle;
  msg_setContentView: THandle;
  msg_setDelegate : THandle;
  msg_setFrameSize : THandle;
  msg_setNeedsDisplay: THandle;
  msg_stringWithCharacters: THandle;

  cls_AppDelegate: THandle;
  cls_Object     : THandle;
  cls_OpenGLView: THandle;
  cls_PixelFormat: THandle;
  cls_String     : THandle;
  cls_WindowDelegate: THandle;

function dlopen(Filename: PAnsiChar; Flag: Integer): NativeUInt; cdecl;
  external libdl name '_dlopen';

function objc_getClass(const name: PAnsiChar): THandle; cdecl;
  external libobjc name '_objc_getClass';

function sel_getUid(const str: PAnsiChar): THandle; cdecl;
  external libobjc name '_sel_getUid';

function objc_msgSend(theReceiver, theSelector: THandle): THandle; cdecl; varargs;
  external libobjc name '_objc_msgSend';

function objc_allocateClassPair(superclass: THandle; name: PAnsiChar; ExtraBytes: Longword): THandle; cdecl;
  external libobjc name '_objc_allocateClassPair';

function class_addMethod(Cls: THandle; theSelector: THandle; Impl: Pointer; types: PAnsiChar): Integer;
  cdecl; external libobjc name '_class_addMethod';

procedure objc_registerClassPair(Cls: THandle); cdecl;
  external libobjc name '_objc_registerClassPair';

function objc_msgSend_stretNSRect(theReceiver, theSelector: THandle): NSRect; cdecl; varargs;
  external libobjc name '_objc_msgSend_stret';

procedure objc_msgSend_stret(theReceiver, theSelector: THandle); cdecl; varargs;
  external libobjc name '_objc_msgSend_stret';

var
  Application: NSApplication;

function alloc(const className: PAnsiChar): THandle;
begin
  Result := objc_msgSend(objc_getClass(className), msg_alloc);
end;

function StrToNSString(const Str: string): THandle;
begin
  Result := objc_msgSend(cls_string, msg_stringWithCharacters, PWideChar(Str), Length(Str));
end;

function applicationShouldTerminateAfterLastWindowClosed(id, sel, App: THandle): Boolean; cdecl;
begin
  Result := True;
end;

procedure applicationDidFinishLaunching(id, sel, notification: THandle); cdecl;
begin
  objc_msgSend(Application.FWindow, msg_makeKeyAndOrderFront, Application.FWindow);
  //Application.DoIdle;
end;

procedure Application_OnIdle(id, sel, notification: THandle); cdecl;
begin
  Application.DoIdle;
end;

procedure windowDidResize(id, sel, notification: THandle); cdecl;
var
  Rect  : NSRect;
begin
  // GetBounds
  Rect := objc_msgSend_stretNSRect(Application.FWindow, msg_frame);
  // BoundsRect -> ClientRect
  Rect := GetContentForFrame(@objc_msgSend_stret)(Application.FWindow, msg_contentRectForFrameRect, Rect);
  Application.FWidth := Round(Rect.size.width);
  Application.FHeight := Round(Rect.size.height);
  Application.OnResize;
  // Adjust Frame
  objc_msgSend(Application.FView, msg_SetFrameSize, Rect.size);
end;

procedure drawRect(id, sel:THandle; rect: NSRect); cdecl;
begin
  Application.OnPaint;
end;

procedure prepareOpenGL(id, sel:THandle); cdecl;
begin
  Application.SetupGL;
end;

{ NSApplication }

constructor NSApplication.Create(AWidth, AHeight: Integer);
var
  Pool    : THandle;
  Delegate: THandle;
  Rect    : NSRect;
  NotificationCenter: THandle;
  NotificationName  : THandle;
begin
// Singleton
  Application := Self;

// Required for MacOS API
  lib_AppKit := dlopen('/System/Library/Frameworks/AppKit.framework/AppKit', RTLD_LAZY);

// classes
  cls_Object := objc_getClass('NSObject');
  cls_PixelFormat := objc_getClass('NSOpenGLPixelFormat');
  cls_String := objc_getClass('NSString');

// some messages
  msg_alloc := sel_getUid('alloc');
  msg_application_Idle := sel_getUid('Application_OnIdle:');
  msg_contentRectforFrameRect := sel_getUid('contentRectForFrameRect:');
  msg_frame := sel_getUid('frame');
  msg_init  := sel_getUid('init');
  msg_initWithAttributes := sel_getUid('initWithAttributes:');
  msg_makeKeyAndOrderFront := sel_getUid('makeKeyAndOrderFront:');
  msg_enqueueNotification_postingStyle := sel_getUid('enqueueNotification:postingStyle:');
  msg_setContentView := sel_getUid('setContentView:');
  msg_setDelegate := sel_getUid('setDelegate:');
  msg_setFrameSize := sel_getUId('setFrameSize:');
  msg_setNeedsDisplay := sel_getUid('setNeedsDisplay:');
  msg_stringWithCharacters := sel_getUid('stringWithCharacters:length:');

// Memory pool
  Pool := objc_msgSend(alloc('NSAutoreleasePool'), msg_init);

// NSAppliation
  FApp := objc_msgSend(objc_getClass('NSApplication'), sel_getUid('sharedApplication'));

// AppDelegate
  cls_AppDelegate := objc_allocateClassPair(cls_Object, 'TAppDelegate', 0);
  class_addMethod(cls_AppDelegate, sel_getUid('applicationShouldTerminateAfterLastWindowClosed:'), @applicationShouldTerminateAfterLastWindowClosed, 'B12@0:4@8');
  class_addMethod(cls_AppDelegate, sel_getUid('applicationDidFinishLaunching:'), @applicationDidFinishLaunching, nil);
  class_addMethod(cls_AppDelegate, msg_application_Idle, @Application_OnIdle, 'v12@0:4@8');
  objc_registerClassPair(cls_AppDelegate);
  Delegate := objc_msgSend(objc_msgSend(cls_AppDelegate, msg_alloc), msg_init);

// Bind NSApplication and AppDelegate
  objc_msgSend(FApp, msg_setDelegate, Delegate);

// Notification message for OnIdle
  NotificationCenter := objc_msgSend(objc_getClass('NSNotificationCenter'), sel_getUid('defaultCenter'));
  NotificationName := StrToNSString('Application.OnIdleEvent');
  objc_msgSend(
    NotificationCenter,
    sel_getUid('addObserver:selector:name:object:'),
    Delegate,
    msg_application_Idle,
    NotificationName,
    0
  );
  FQueue := objc_msgSend(objc_getClass('NSNotificationQueue'), sel_getUid('defaultQueue'));
  FNotify := objc_msgSend(objc_getClass('NSNotification'), sel_getUid('notificationWithName:object:'), NotificationName, Delegate);

  objc_msgSend(FQueue, msg_enqueueNotification_postingStyle, FNotify, NSPostWhenIdle);

// Main Window
  Rect.origin.x := 10;
  Rect.origin.y := 10;
  Rect.size.width := AWidth;
  Rect.size.height := AHeight;
  FWindow := objc_msgSend(
    alloc('NSWindow'),
    sel_getUid('initWithContentRect:styleMask:backing:defer:'),
    Rect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered,
    True
  );

// Windows Delegate
  cls_WindowDelegate := objc_allocateClassPair(cls_Object, 'TWindowDelegate', 0);
  class_addMethod(cls_WindowDelegate, sel_getUid('windowDidResize:'), @windowDidResize, 'v12@0:4@8');
  objc_registerClassPair(cls_WindowDelegate);
  Delegate := objc_msgSend(objc_msgSend(cls_WindowDelegate, msg_alloc), msg_init);
  objc_msgSend(FWindow, msg_setDelegate, Delegate);

  objc_msgSend(FWindow, sel_getUid('setTitle:'), StrToNSString('CrossGL by Execute'));

// OpenGL View
  cls_OpenGLView := objc_allocateClassPair(objc_getClass('NSOpenGLView'), 'TGLView', 0);
  class_addMethod(cls_OpenGLView, sel_getUid('drawRect:'), @drawRect, 'v@:{ffff}');
  class_addMethod(cls_OpenGLView, sel_getUid('prepareOpenGL'), @prepareOpenGL, nil);
  objc_registerClassPair(cls_OpenGLView);
  Rect.origin.x := 0;
  Rect.origin.y := 0;
  FView := objc_msgSend(objc_msgSend(cls_OpenGLView, msg_alloc), sel_getUid('initWithFrame:pixelFormat:'), REct, PixelFormat);
  objc_msgSend(FWindow, msg_setContentView, FView);

// Done with pool
  objc_msgSend(Pool, sel_getUid('release'));
end;

procedure NSApplication.DoIdle;
begin
  OnIdle;
  objc_msgSend(FQueue, msg_enqueueNotification_postingStyle, FNotify, NSPostWhenIdle);
end;

procedure NSApplication.Invalidate;
begin
  objc_msgSend(FView, msg_setNeedsDisplay, 1);
end;

procedure NSApplication.OnIdle;
begin
end;

function NSApplication.PixelFormat: THandle;
var
  cls: THandle;
begin
  cls := objc_msgSend(cls_PixelFormat, msg_alloc);
  Result := objc_msgSend(cls, msg_initWithAttributes, @attrs[0]);
  if Result = 0 then
  begin
    Result := objc_msgSend(cls, msg_initWithAttributes, @attrs[1]);
    if Result = 0 then
      Result := objc_msgSend(cls_OpenGLView, sel_getUid('defaultPixelFormat'));
  end;
end;

procedure NSApplication.Run;
begin
  objc_msgSend(FApp, sel_getUid('run'));
end;


procedure NSApplication.SetupGL;
begin
// Set focus
  objc_msgSend(FWindow, msg_makeKeyAndOrderFront, FApp);
end;

end.
