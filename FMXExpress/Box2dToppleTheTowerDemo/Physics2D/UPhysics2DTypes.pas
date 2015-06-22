unit UPhysics2DTypes;

{ box2D 2.3.0 translation

  ###  This unit is written based on Box2D maintained by Erin Catto (http://www.box2d.org)
  All type names follow the Delphi custom Txxx and xxx means the corresponding
  type in cpp source. This rule doesn't apply to the following types b2Vec2,
  b2Vec3, b2Mat22 and b2Mat33. Because I think these types are so primary that you
  may want to reuse them in other projects.

  ###  box2D 2.1.0 version is so different from 2.0.1 version that Erin nearly
  overwrote all codes. For more information I recommend reading the following page
  http://box2dflash.boristhebrave.com/docs/2.1a/updating

  ###  I translated version 2.0.1 without considering much of class encapsulation so
  you can find many m_xxx class memebers left out in public field. In this version
  I fixed all things and you should use Set/Get procedures to access these members.
  But for some members owning both Set and Get procedures I changed them to
  properties. For example I simplify SetUserData and GetUserData to such code
     Property UserData: Pointer read m_userData write m_userData;
  Another thing is that I changed some reference declaration(delphi doesn't support)
  to pointer to avoid unnecessary memory copy. You can check Tb2Body.GetTransform
  and Tb2DynamicTree.GetFatAABB for more details.

  ###  Another difference from version 2.0.1 is that I changed Tb2Contact
  declaration from class to record for effciency. Because class is created on
  heap and Tb2Contact is so frequently created and destroyed.

  ###  Because versions before Delphi 2006 don't support operator overloading, so
  I write two versions of all math operations for vector and matrix, etc. But
  later I found that the version without operator overloading runs faster.
  So if you want a better performance, DEFINE BETTER_PERFORMANCE in Physics2D.inc
  which will UNDEFINE OP_OVERLOAD even if you are using Delphi 2010.

  ###  This library supports three kinds of floats, Single(32bit), Double(64bit) and
  Extended(80bit). Double precision is always recommended because I will test all
  codes and demos using double precision. Using other float systems may cause some
  unpredictable exceptions.
       flags        EXTENDED_PRECISION        DOUBLE_PRECISION
     Extended               ON                     whatever
      Double(default)       OFF                       ON
      Single                OFF                       OFF
  There is also a flag SINGLE_PRECISION in the include file but it doesn't affect
  Float type definition.

  ###  Controllers are added as enhancement and can be flagged by CONTROLLERS.
  If you don't need them, please unflag to reduce code size.

  ###  If you want to do benchmark or something else, please flag COMPUTE_PHYSICS_TIME.
  Time consumed by each step is updated and stored in Tb2World.GetPhysicsTime.

  ###  All assertions are ignored.

  Translator: Qianyuan Wang(ÍõÇ¬Ôª)
  Contact me: http://hi.baidu.com/wqyfavor
              wqyfavor@qq.com
              QQ: 466798985
              Tweet: t.qq.com/wqyfavor
}

interface
{$I Physics2D.inc}

uses
   Math;

type
   Int8   = ShortInt;
   Int16  = SmallInt;
   Int32  = Integer;
   PInt32 = ^Int32;
   UInt8  = Byte;
   UInt16 = Word;
   UInt32 = Cardinal;
   PUInt16 = ^UInt16;

   TPointFloat = Single;
   TPointFloatArray = array of TPointFloat;

   PPointF = ^TPointF;
   TPointF = packed record
      x, y: TPointFloat;
      {$IFDEF OP_OVERLOAD}
      procedure SetZero; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(x, y: TPointFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      {$ENDIF}
   end;
   TPointsF = array of TPointF;

   // Float type
   PPhysicsFloat = ^PhysicsFloat;
   {$IFDEF EXTENDED_PRECISION}
   PhysicsFloat = Extended;
   {$ELSE}
      {$IFDEF DOUBLE_PRECISION}
      PhysicsFloat = Double;
      {$ELSE}
      PhysicsFloat = Single;
      {$ENDIF}
   {$ENDIF}
   Float32 = Single;
   Float64 = Double;
   Float80 = Extended;

type
   TPhysicsFloatArray = array of PhysicsFloat;

const
   {$IFDEF EXTENDED_PRECISION}
   FLT_EPSILON = 1.084202172485504E-19;
   FLT_MAX = MaxExtended;
   {$ELSE}
      {$IFDEF DOUBLE_PRECISION}
      FLT_EPSILON = 2.2204460492503131e-16;
      FLT_MAX = MaxDouble;
      {$ELSE}
      FLT_EPSILON = 1.192092896e-7;
      FLT_MAX = MaxSingle;
      {$ENDIF}
   {$ENDIF}

   function IsValid(f: PhysicsFloat): Boolean; {$IFNDEF OP_OVERLOAD}overload;{$ENDIF}

type
   PVector2 = ^TVector2;
   TVector2Arraied = array[0..1] of PhysicsFloat; // The same with TVector2
   TVector2 = record
      x, y: PhysicsFloat;

      {$IFDEF OP_OVERLOAD}
      function IsValid: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function Length: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function SqrLength: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function Normalize: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function Skew: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} /// Get the skew vector such that dot(skew_vec, other) == cross(vec, other)
      procedure SetZero; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetNegative; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(x, y: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetLength(value: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class function From(const x, y: PhysicsFloat): TVector2; static; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      // Operators
      class operator Negative(const AValue: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Add(const Left, Right: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Subtract(const Left, Right: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class operator Multiply(const Left: TVector2; const Right: Single): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: TVector2; const Right: Double): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: TVector2; const Right: Extended): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Single; const Right: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Double; const Right: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Extended; const Right: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class operator Divide(const Left: TVector2; const Right: Single): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Divide(const Left: TVector2; const Right: Double): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Divide(const Left: TVector2; const Right: Extended): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure AddBy(const Operand: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SubtractBy(const Operand: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      {$ENDIF}
   end;

   TVectorArray = array of TVector2;
   TVectorArray4 = array[0..3] of TVector2;

   TVector3 = record // Added from v2.1.0
      x, y, z: PhysicsFloat;

      {$IFDEF OP_OVERLOAD}
      function IsValid: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function Length: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function SqrLength: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function Normalize: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetZero; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetNegative; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(x, y, z: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetLength(value: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class function From(const x, y, z: PhysicsFloat): TVector3; static; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class operator Negative(const AValue: TVector3): TVector3; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Add(const Left, Right: TVector3): TVector3; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Subtract(const Left, Right: TVector3): TVector3; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class operator Multiply(const Left: TVector3; const Right: Single): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: TVector3; const Right: Double): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: TVector3; const Right: Extended): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Single; const Right: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Double; const Right: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Multiply(const Left: Extended; const Right: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      class operator Divide(const Left: TVector3; const Right: Single): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Divide(const Left: TVector3; const Right: Double): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      class operator Divide(const Left: TVector3; const Right: Extended): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure AddBy(const Operand: TVector3); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SubtractBy(const Operand: TVector3); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure MultiplyBy(const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure DivideBy(const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      {$ENDIF}
   end;

   TMatrix22 = record
      ex, ey: TVector2;

      {$IFDEF OP_OVERLOAD}
      procedure SetIdentity; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetZero; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(const _col1, _col2: TVector2); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function Invert: TMatrix22;
      function GetInverse: TMatrix22; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} // The same with Invert, imported from v2.1.0
      function Solve(const b: TVector2): TVector2; // Solve A * x = b, where b is a column vector.

      // Operators
      class operator Negative(const AValue: TMatrix22): TMatrix22;
      class operator Add(const Left, Right: TMatrix22): TMatrix22;
      class operator Subtract(const Left, Right: TMatrix22): TMatrix22;
      {$ENDIF}
   end;

   TMatrix33 = record // Added from v2.1.0
      ex, ey, ez: TVector3;

      {$IFDEF OP_OVERLOAD}
      procedure SetIdentity; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetZero; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(const _col1, _col2, _col3: TVector3); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      /// Solve A * x = b, where b is a column vector. This is more efficient
	    /// than computing the inverse in one-shot cases.
      function Solve33(const b: TVector3): TVector3;
      /// Solve A * x = b, where b is a column vector. This is more efficient
      /// than computing the inverse in one-shot cases. Solve only the upper
      /// 2-by-2 matrix equation.
      function Solve22(const b: TVector2): TVector2;

      /// Get the inverse of this matrix as a 2-by-2.
      /// Returns the zero matrix if singular.
      procedure GetInverse22(var dest: TMatrix33);

      /// Get the symmetric inverse of this matrix as a 3-by-3.
      /// Returns the zero matrix if singular.
      procedure GetSymInverse33(var dest: TMatrix33);

      // Operators
      class operator Negative(const AValue: TMatrix33): TMatrix33;
      class operator Add(const Left, Right: TMatrix33): TMatrix33;
      class operator Subtract(const Left, Right: TMatrix33): TMatrix33;
      {$ENDIF}
   end;

   /// Rotation
   Tb2Rot = record
      s, c: PhysicsFloat; /// Sine and cosine

      {$IFDEF OP_OVERLOAD}
      procedure SetAngle(angle: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetIdentity; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      function GetAngle: PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function GetXAxis: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function GetYAxis: TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      {$ENDIF}
   end;

   /// Renamed to Tb2Transform from v2.1.0
   /// A transform contains translation and rotation. It is used to represent
   /// the position and orientation of rigid bodies.
   Pb2Transform = ^Tb2Transform;
   Tb2Transform = record
      p: TVector2;
      q: Tb2Rot;

      {$IFDEF OP_OVERLOAD}
      procedure SetIdentity; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      procedure SetValue(const position: TVector2; angle: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      {$ENDIF}
   end;

   /// This describes the motion of a body/shape for TOI computation.
   /// Shapes are defined with respect to the body origin, which may
   /// no coincide with the center of mass. However, to support dynamics
   /// we must interpolate the center of mass position.
   Tb2Sweep = record
      localCenter: TVector2; // local center of mass position
      c0, c: TVector2; // center world positions
      a0, a: PhysicsFloat; // world angles

      /// Fraction of the current time step in the range [0,1]
      /// c0 and a0 are the positions at alpha0.
      alpha0: PhysicsFloat;

      {$IFDEF OP_OVERLOAD}
      /// Get the interpolated transform at a specific time.
      /// @param beta is a factor in [0,1], where 0 indicates alpha0.
    	procedure GetTransform(var xfb: Tb2Transform; beta: PhysicsFloat); /// Renamed to GetTransform from v2.1.0

      /// Advance the sweep forward, yielding a new initial state.
      /// @param alpha the new initial time.
      procedure Advance(alpha: PhysicsFloat);

	    /// Normalize the angles. Normalize an angle in radians to be between -pi and pi
	    procedure Normalize;
      {$ENDIF}
   end;

const
   b2Pnt2_Zero: TPointF = (X: 0.0; Y: 0.0);
   b2Vec2_Zero: TVector2 = (X: 0.0; Y: 0.0);
   b2Vec3_Zero: TVector3 = (X: 0.0; Y: 0.0; Z: 0.0);
   b2Mat22_identity: TMatrix22 = (ex: (X: 1.0; Y: 0.0); ey: (X: 0.0; Y: 1.0));
   b2Mat33_identity: TMatrix33 = (ex: (X: 1.0; Y: 0.0; Z: 0.0); ey: (X: 0.0; Y: 1.0; Z: 0.0); ez: (X: 0.0; Y: 0.0; Z: 1.0));

const
   UInt8_MAX = $FF;
   UINT16_MAX = $FFFF;

   /// The maximum number of contact points between two convex shapes. Do not change this value.
   b2_maxManifoldPoints = 2;

   /// The maximum number of vertices on a convex polygon. You cannot increase
   /// this too much because b2BlockAllocator has a maximum object size.
   b2_maxPolygonVertices = 8;

   b2_maxProxies = 512; // this must be a power of two
   b2_maxPairs = 8 * b2_maxProxies;	// this must be a power of two

   /// This is used to fatten AABBs in the dynamic tree. This allows proxies
   /// to move by a small amount without triggering a tree adjustment.
   /// This is in meters.
   b2_aabbExtension =	0.1; // Added from v2.1.0
   /// This is used to fatten AABBs in the dynamic tree. This is used to predict
   /// the future position based on the current displacement.
   /// This is a dimensionless multiplier.
   b2_aabbMultiplier = 2.0; // Added from v2.1.0

   // Dynamics
   /// A small length used as a collision and constraint tolerance. Usually it is
   /// chosen to be numerically significant, but visually insignificant.
   b2_linearSlop = 0.005;	// 0.5 cm

   /// A small angle used as a collision and constraint tolerance. Usually it is
   /// chosen to be numerically significant, but visually insignificant.
   b2_angularSlop = 2.0 / 180.0 * Pi;			// 2 degrees

   /// The radius of the polygon/edge shape skin. This should not be modified. Making
   /// this smaller means polygons will have an insufficient buffer for continuous collision.
   /// Making it larger may create artifacts for vertex collision.
   b2_polygonRadius = (2.0 * b2_linearSlop); // Added from v2.1.0

   /// Maximum number of sub-steps per contact in continuous physics simulation.
   b2_maxSubSteps = 8;

   /// Maximum number of contacts to be handled to solve a TOI impact.
   b2_maxTOIContacts = 32;

   /// A velocity threshold for elastic collisions. Any collision with a relative linear
   /// velocity below this threshold will be treated as inelastic.
   b2_velocityThreshold = 1.0;		// 1 m/s

   /// The maximum linear position correction used when solving constraints. This helps to
   /// prevent overshoot.
   b2_maxLinearCorrection = 0.2;	// 20 cm

   /// The maximum angular position correction used when solving constraints. This helps to
   /// prevent overshoot.
   b2_maxAngularCorrection = 8.0 / 180.0 * Pi;			// 8 degrees

   /// The maximum linear velocity of a body. This limit is very large and is used
   /// to prevent numerical problems. You shouldn't need to adjust this.
   b2_maxTranslation = 2.0;
   b2_maxTranslationSquared = b2_maxTranslation * b2_maxTranslation;

   /// The maximum angular velocity of a body. This limit is very large and is used
   /// to prevent numerical problems. You shouldn't need to adjust this.
   b2_maxRotation = 0.5 * Pi;
   b2_maxRotationSquared = b2_maxRotation * b2_maxRotation;

   /// This scale factor controls how fast overlap is resolved. Ideally this would be 1 so
   /// that overlap is removed in one time step. However using values close to 1 often lead
   /// to overshoot.
   b2_baumgarte = 0.2;
   b2_toiBaugarte = 0.75;

   // Sleep
   /// The time that a body must be still before it will go to sleep.
   b2_timeToSleep  = 0.5;									// half a second

   /// A body cannot sleep if its linear velocity is above this tolerance.
   b2_linearSleepTolerance = 0.01;		// 1 cm/s

   /// A body cannot sleep if its angular velocity is above this tolerance.
   b2_angularSleepTolerance = 2.0 / 180.0;		// 2 degrees/s

function MakePoint(x, y: TPointFloat): TPointF; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function MakeVector(x, y: PhysicsFloat): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function MakeVector(x, y, z: PhysicsFloat): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetZero(var p: TPointF); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetZero(var v: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var p: TPointF; ax, ay: TPointFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var v: TVector2; ax, ay: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var v: TVector3; ax, ay, az: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

{$IFNDEF OP_OVERLOAD}
// For TVector2
function IsValid(const v: TVector2): Boolean; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function LengthVec(const v: TVector2): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function SqrLength(const v: TVector2): PhysicsFloat; overload;  {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Normalize(var v: TVector2): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Skew(const v: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetNegative(var v: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetLengthVec(var v: TVector2; value: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Negative(const AValue: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Add(const Left, Right: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Add(const p1, p2, p3: TVector2): TVector2; overload;
function Subtract(const Left, Right: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Multiply(const Left: TVector2; const Right: Single): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Multiply(const Left: TVector2; const Right: Double): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Multiply(const Left: TVector2; const Right: Extended): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Divide(const Left: TVector2; const Right: Single): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Divide(const Left: TVector2; const Right: Double): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Divide(const Left: TVector2; const Right: Extended): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

procedure AddBy(var v: TVector2; const Operand: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SubtractBy(var v: TVector2; const Operand: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector2; const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector2; const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector2; const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector2; const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector2; const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector2; const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// For TVector3
function IsValid(const v: TVector3): Boolean; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function LengthVec(const v: TVector3): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function SqrLength(const v: TVector3): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Normalize(var v: TVector3): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetNegative(var v: TVector3); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetLengthVec(var v: TVector3; value: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Negative(const AValue: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Add(const Left, Right: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Add(const p1, p2, p3: TVector3): TVector3; overload;
function Subtract(const Left, Right: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Multiply(const Left: TVector3; const Right: Single): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Multiply(const Left: TVector3; const Right: Double): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Multiply(const Left: TVector3; const Right: Extended): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Divide(const Left: TVector3; const Right: Single): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Divide(const Left: TVector3; const Right: Double): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Divide(const Left: TVector3; const Right: Extended): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

procedure AddBy(var v: TVector3; const Operand: TVector3); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SubtractBy(var v: TVector3; const Operand: TVector3); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector3; const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector3; const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure MultiplyBy(var v: TVector3; const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector3; const Operand: Single); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector3; const Operand: Double); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure DivideBy(var v: TVector3; const Operand: Extended); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// For TMatrix22
procedure SetIdentity(var m: TMatrix22); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetZero(var m: TMatrix22); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var m: TMatrix22; const _ex, _ey: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function Invert(const m: TMatrix22): TMatrix22;
function GetInverse(const m: TMatrix22): TMatrix22; {$IFDEF INLINE_AVAIL}inline;{$ENDIF} // The same with Invert, imported from v2.1.0
function Solve(const m: TMatrix22; const b: TVector2): TVector2; // Solve A * x = b, where b is a column vector.

function Negative(const AValue: TMatrix22): TMatrix22; overload;
function Add(const Left, Right: TMatrix22): TMatrix22; overload;
function Add(const m1, m2, m3: TMatrix22): TMatrix22; overload;
function Subtract(const Left, Right: TMatrix22): TMatrix22; overload;

// For TMatrix33
procedure SetIdentity(var m: TMatrix33); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetZero(var m: TMatrix33); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var m: TMatrix33; const _col1, _col2, _col3: TVector3); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function Solve33(var m: TMatrix33; const b: TVector3): TVector3;
function Solve22(var m: TMatrix33; const b: TVector2): TVector2;
procedure GetInverse22(var m: TMatrix33; var dest: TMatrix33);
procedure GetSymInverse33(var m: TMatrix33; var dest: TMatrix33);

function Negative(const AValue: TMatrix33): TMatrix33; overload;
function Add(const Left, Right: TMatrix33): TMatrix33; overload;
function Add(const m1, m2, m3: TMatrix33): TMatrix33; overload;
function Subtract(const Left, Right: TMatrix33): TMatrix33; overload;

//Tb2Rot
procedure SetAngle(var r: Tb2Rot; angle: PhysicsFloat); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetIdentity(var r: Tb2Rot); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function GetAngle(const r: Tb2Rot): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetXAxis(const r: Tb2Rot): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetYAxis(const r: Tb2Rot): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// For Tb2Transform
procedure SetIdentity(var xf: Tb2Transform); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure SetValue(var xf: Tb2Transform; const position: TVector2; angle: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// For Tb2Sweep
procedure GetTransform(const Sweep: Tb2Sweep; var xfb: Tb2Transform; beta: PhysicsFloat);
procedure Advance(var Sweep: Tb2Sweep; alpha: PhysicsFloat);
procedure Normalize(var Sweep: Tb2Sweep); overload;

{$ENDIF}

function b2Max(const a, b: PhysicsFloat): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Max(const a, b: TVector2): TVector2; overload;
function b2Min(const a, b: PhysicsFloat): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Min(const a, b: TVector2): TVector2; overload;
function b2Max(const a, b: Int32): Int32; overload;
function b2Min(const a, b: Int32): Int32; overload;

procedure b2Swap(var a, b: PhysicsFloat); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure b2Swap(var a, b: Int32); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
procedure b2Swap(var a, b: TVector2); overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function b2Clamp(const a, low, high: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Clamp(const a, low, high: PhysicsFloat): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function b2MiddlePoint(const a, b: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function b2Dot(const a, b: TVector2): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Dot(const a, b: TVector3): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Cross(const a, b: TVector2): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Cross(const a: TVector2; s: PhysicsFloat): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Cross(s: PhysicsFloat; const a: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Cross(const a, b: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const A: TMatrix22; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul22(const A: TMatrix33; const v: TVector2): TVector2; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const A: TMatrix22; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Distance(const a, b: TVector2): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2DistanceSquared(const a, b: TVector2): PhysicsFloat; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const A, B: TMatrix22): TMatrix22; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const A, B: TMatrix22): TMatrix22; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const T: Tb2Transform; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const T: Tb2Transform; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const A: TMatrix33; const v: TVector3): TVector3; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const A, B: Tb2Transform): Tb2Transform; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const A, B: Tb2Transform): Tb2Transform; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const q, r: Tb2Rot): Tb2Rot; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const q, r: Tb2Rot): Tb2Rot; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Mul(const q: Tb2Rot; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2MulT(const q: Tb2Rot; const v: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Abs(const a: TVector2): TVector2; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function b2Abs(const a: TMatrix22): TMatrix22; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function RandomFloat: PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function RandomFloat(lo, hi: PhysicsFloat): PhysicsFloat; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

procedure SinCos(const Theta: PhysicsFloat; var Sin, Cos: PhysicsFloat);

implementation

procedure SinCos(const Theta: PhysicsFloat; var Sin, Cos: PhysicsFloat);
var
	S, C: Extended;
begin
	Math.SinCos(Theta, S, C);
	Sin := S;
	Cos := C;
end;

function RandomFloat: PhysicsFloat;
begin
   Result := Random;
end;

function RandomFloat(lo, hi: PhysicsFloat): PhysicsFloat;
begin
   Result := (hi - lo) * Random + lo;
end;

function MakePoint(x, y: TPointFloat): TPointF;
begin
   Result.x := x;
   Result.y := y;
end;

function MakeVector(x, y: PhysicsFloat): TVector2;
begin
   Result.x := x;
   Result.y := y;
end;

function MakeVector(x, y, z: PhysicsFloat): TVector3;
begin
   Result.x := x;
   Result.y := y;
   Result.z := z;
end;

procedure SetZero(var p: TPointF);
begin
   p := b2Pnt2_Zero;
end;

procedure SetZero(var v: TVector2);
begin
   v := b2Vec2_Zero;
end;

procedure SetValue(var p: TPointF; ax, ay: TPointFloat);
begin
   with p do
   begin
      x := ax;
      y := ay;
   end;
end;

procedure SetValue(var v: TVector2; ax, ay: PhysicsFloat);
begin
   with v do
   begin
      x := ax;
      y := ay;
   end;
end;

procedure SetValue(var v: TVector3; ax, ay, az: PhysicsFloat);
begin
   with v do
   begin
      x := ax;
      y := ay;
      z := az;
   end;
end;

{$IFNDEF OP_OVERLOAD}
// For TVector2
function IsValid(const v: TVector2): Boolean;
begin
   Result := UPhysics2DTypes.IsValid(v.x) and UPhysics2DTypes.IsValid(v.y);
end;

function LengthVec(const v: TVector2): PhysicsFloat;
begin
   with v do
      Result := Sqrt(x * x + y * y);
end;

function SqrLength(const v: TVector2): PhysicsFloat;
begin
   with v do
      Result := x * x + y * y;
end;

function Normalize(var v: TVector2): PhysicsFloat;
begin
   Result := LengthVec(v);
	 if Result < FLT_EPSILON then
   begin
      Result := 0.0;
      Exit;
   end;

   with v do
   begin
      x := x / Result;
      y := y / Result;
   end;
end;

function Skew(const v: TVector2): TVector2;
begin
   Result.x := -v.y;
   Result.y := v.x;
end;

procedure SetNegative(var v: TVector2);
begin
   with v do
   begin
      x := -x;
      y := -y;
   end;
end;

procedure SetLengthVec(var v: TVector2; value: PhysicsFloat);
var
   l: PhysicsFloat;
begin
   with v do
   begin
      l := value / LengthVec(v);
      x := x * l;
      y := y * l;
   end;
end;

function Negative(const AValue: TVector2): TVector2;
begin
   Result.x := -AValue.x;
   Result.y := -AValue.y;
end;

function Add(const Left, Right: TVector2): TVector2;
begin
   Result.x := Left.x + Right.x;
   Result.y := Left.y + Right.y;
end;

function Add(const p1, p2, p3: TVector2): TVector2;
begin
   Result.x := p1.x + p2.x + p3.x;
   Result.y := p1.y + p2.y + p3.y;
end;

function Subtract(const Left, Right: TVector2): TVector2;
begin
   Result.x := Left.x - Right.x;
   Result.y := Left.y - Right.y;
end;

function Multiply(const Left: TVector2; const Right: Single): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

function Multiply(const Left: TVector2; const Right: Double): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

function Multiply(const Left: TVector2; const Right: Extended): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

function Divide(const Left: TVector2; const Right: Single): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

function Divide(const Left: TVector2; const Right: Double): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

function Divide(const Left: TVector2; const Right: Extended): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

procedure AddBy(var v: TVector2; const Operand: TVector2);
begin
   with v do
   begin
      x := x + Operand.x;
      y := y + Operand.y;
   end;
end;

procedure SubtractBy(var v: TVector2; const Operand: TVector2);
begin
   with v do
   begin
      x := x - Operand.x;
      y := y - Operand.y;
   end;
end;

procedure MultiplyBy(var v: TVector2; const Operand: Single);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
   end;
end;

procedure MultiplyBy(var v: TVector2; const Operand: Double);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
   end;
end;

procedure MultiplyBy(var v: TVector2; const Operand: Extended);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
   end;
end;

procedure DivideBy(var v: TVector2; const Operand: Single);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
   end;
end;

procedure DivideBy(var v: TVector2; const Operand: Double);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
   end;
end;

procedure DivideBy(var v: TVector2; const Operand: Extended);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
   end;
end;

// For TVector3
function IsValid(const v: TVector3): Boolean;
begin
   Result := UPhysics2DTypes.IsValid(v.x) and UPhysics2DTypes.IsValid(v.y) and
      UPhysics2DTypes.IsValid(v.z);
end;

function LengthVec(const v: TVector3): PhysicsFloat;
begin
   with v do
      Result := Sqrt(x * x + y * y + z * z);
end;

function SqrLength(const v: TVector3): PhysicsFloat;
begin
   with v do
      Result := x * x + y * y + z * z;
end;

function Normalize(var v: TVector3): PhysicsFloat;
begin
   Result := LengthVec(v);
	 if Result < FLT_EPSILON then
   begin
      Result := 0.0;
      Exit;
   end;

   with v do
   begin
      x := x / Result;
      y := y / Result;
      z := z / Result;
   end;
end;

procedure SetNegative(var v: TVector3);
begin
   with v do
   begin
      x := -x;
      y := -y;
      z := -z;
   end;
end;

procedure SetLengthVec(var v: TVector3; value: PhysicsFloat);
var
   l: PhysicsFloat;
begin
   with v do
   begin
      l := value / LengthVec(v);
      x := x * l;
      y := y * l;
      z := z * l;
   end;
end;

function Negative(const AValue: TVector3): TVector3;
begin
   Result.x := -AValue.x;
   Result.y := -AValue.y;
   Result.z := -AValue.z;
end;

function Add(const Left, Right: TVector3): TVector3;
begin
   Result.x := Left.x + Right.x;
   Result.y := Left.y + Right.y;
   Result.z := Left.z + Right.z;
end;

function Add(const p1, p2, p3: TVector3): TVector3;
begin
   Result.x := p1.x + p2.x + p3.x;
   Result.y := p1.y + p2.y + p3.y;
   Result.z := p1.z + p2.z + p3.z;
end;

function Subtract(const Left, Right: TVector3): TVector3;
begin
   Result.x := Left.x - Right.x;
   Result.y := Left.y - Right.y;
   Result.z := Left.z - Right.z;
end;

function Multiply(const Left: TVector3; const Right: Single): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

function Multiply(const Left: TVector3; const Right: Double): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

function Multiply(const Left: TVector3; const Right: Extended): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

function Divide(const Left: TVector3; const Right: Single): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

function Divide(const Left: TVector3; const Right: Double): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

function Divide(const Left: TVector3; const Right: Extended): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

procedure AddBy(var v: TVector3; const Operand: TVector3);
begin
   with v do
   begin
      x := x + Operand.x;
      y := y + Operand.y;
      z := z + Operand.z;
   end;
end;

procedure SubtractBy(var v: TVector3; const Operand: TVector3);
begin
   with v do
   begin
      x := x - Operand.x;
      y := y - Operand.y;
      z := z - Operand.z;
   end;
end;

procedure MultiplyBy(var v: TVector3; const Operand: Single);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
      z := z * Operand;
   end;
end;

procedure MultiplyBy(var v: TVector3; const Operand: Double);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
      z := z * Operand;
   end;
end;

procedure MultiplyBy(var v: TVector3; const Operand: Extended);
begin
   with v do
   begin
      x := x * Operand;
      y := y * Operand;
      z := z * Operand;
   end;
end;

procedure DivideBy(var v: TVector3; const Operand: Single);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
      z := z / Operand;
   end;
end;

procedure DivideBy(var v: TVector3; const Operand: Double);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
      z := z / Operand;
   end;
end;

procedure DivideBy(var v: TVector3; const Operand: Extended);
begin
   with v do
   begin
      x := x / Operand;
      y := y / Operand;
      z := z / Operand;
   end;
end;

// For Matrix22
procedure SetIdentity(var m: TMatrix22);
begin
   m := b2Mat22_identity;
end;

procedure SetZero(var m: TMatrix22);
begin
   with m do
   begin
      ex.x := 0.0;
      ey.x := 0.0;
      ex.y := 0.0;
      ey.y := 0.0;
   end;
end;

procedure SetValue(var m: TMatrix22; const _ex, _ey: TVector2);
begin
   with m do
   begin
      ex := _ex;
      ey := _ey;
   end;
end;

function Invert(const m: TMatrix22): TMatrix22;
var
   a, b, c, d, det: PhysicsFloat;
begin
   with m do
   begin
      a := ex.x;
      b := ey.x;
      c := ex.y;
      d := ey.y;
   end;

   det := a * d - b * c;
   if det <> 0.0 then
      det := 1.0 / det;
   with Result do
   begin
      ex.x :=  det * d;
      ey.x := -det * b;
      ex.y := -det * c;
      ey.y :=  det * a;
   end;
end;

function GetInverse(const m: TMatrix22): TMatrix22;
begin
   Result := Invert(m);
end;

function Solve(const m: TMatrix22; const b: TVector2): TVector2;
var
   a11, a12, a21, a22, det: PhysicsFloat;
begin
   with m do
   begin
      a11 := ex.x;
      a12 := ey.x;
      a21 := ex.y;
      a22 := ey.y;
   end;
   det := a11 * a22 - a12 * a21;
   if det <> 0.0 then
      det := 1.0 / det;
   Result.x := det * (a22 * b.x - a12 * b.y);
   Result.y := det * (a11 * b.y - a21 * b.x);
end;

function Negative(const AValue: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.x := -AValue.ex.x;
      ex.y := -AValue.ex.y;
      ey.x := -AValue.ey.x;
      ey.y := -AValue.ey.y;
   end;
end;

function Add(const Left, Right: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.x := Left.ex.x + Right.ex.x;
      ex.y := Left.ex.y + Right.ex.y;
      ey.x := Left.ey.x + Right.ey.x;
      ey.y := Left.ey.y + Right.ey.y;
   end;
end;

function Add(const m1, m2, m3: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.x := m1.ex.x + m2.ex.x + m3.ex.x;
      ex.y := m1.ex.y + m2.ex.y + m3.ex.y;
      ey.x := m1.ey.x + m2.ey.x + m3.ey.x;
      ey.y := m1.ey.y + m2.ey.y + m3.ey.y;
   end;
end;

function Subtract(const Left, Right: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.x := Left.ex.x - Right.ex.x;
      ex.y := Left.ex.y - Right.ex.y;
      ey.x := Left.ey.x - Right.ey.x;
      ey.y := Left.ey.y - Right.ey.y;
   end;
end;

// For TMatrix33

procedure SetIdentity(var m: TMatrix33);
begin
   m := b2Mat33_identity;
end;

procedure SetZero(var m: TMatrix33);
begin
   with m do
   begin
      ex := b2Vec3_Zero;
      ey := b2Vec3_Zero;
      ez := b2Vec3_Zero;
   end;
end;

procedure SetValue(var m: TMatrix33; const _col1, _col2, _col3: TVector3);
begin
   with m do
   begin
      ex := _col1;
      ey := _col2;
      ez := _col3;
   end;
end;

function Solve33(var m: TMatrix33; const b: TVector3): TVector3;
var
   det: PhysicsFloat;
begin
   with m do
   begin
      det := b2Dot(ex, b2Cross(ey, ez));
      if det <> 0.0 then
         det := 1.0 / det;
      Result.x := det * b2Dot(b, b2Cross(ey, ez));
      Result.y := det * b2Dot(ex, b2Cross(b, ez));
      Result.z := det * b2Dot(ex, b2Cross(ey, b));
   end;
end;

function Solve22(var m: TMatrix33; const b: TVector2): TVector2;
var
   det: PhysicsFloat;
begin
   with m do
   begin
      det := ex.x * ey.y - ey.x * ex.y;
      if det <> 0.0 then
         det := 1.0 / det;
      Result.x := det * (ey.y * b.x - ey.x * b.y);
      Result.y := det * (ex.x * b.y - ex.y * b.x);
   end;
end;

procedure GetInverse22(var m: TMatrix33; var dest: TMatrix33);
var
   a, b, c, d, det: PhysicsFloat;
begin
   with m do
   begin
      a := ex.x;
      b := ey.x;
      c := ex.y;
      d := ey.y;
      det := a * d - b * c;
      if det <> 0.0 then
         det := 1.0 / det;

      dest.ex.x :=  det * d;
      dest.ey.x := -det * b;
      dest.ex.z := 0.0;
      dest.ex.y := -det * c;
      dest.ey.y :=  det * a;
      dest.ey.z := 0.0;
      dest.ez.x := 0.0;
      dest.ez.y := 0.0;
      dest.ez.z := 0.0;
   end;
end;

procedure GetSymInverse33(var m: TMatrix33; var dest: TMatrix33);
var
   det: PhysicsFloat;
   a11, a12, a13, a22, a23, a33: PhysicsFloat;
begin
   with m do
   begin
      det := b2Dot(ex, b2Cross(ey, ez));
      if det <> 0.0 then
         det := 1.0 / det;

      a11 := ex.x;
      a12 := ey.x;
      a13 := ez.x;
      a22 := ey.y;
      a23 := ez.y;
      a33 := ez.z;

      dest.ex.x := det * (a22 * a33 - a23 * a23);
      dest.ex.y := det * (a13 * a23 - a12 * a33);
      dest.ex.z := det * (a12 * a23 - a13 * a22);

      dest.ey.x := dest.ex.y;
      dest.ey.y := det * (a11 * a33 - a13 * a13);
      dest.ey.z := det * (a13 * a12 - a11 * a23);

      dest.ez.x := dest.ex.z;
      dest.ez.y := dest.ey.z;
      dest.ez.z := det * (a11 * a22 - a12 * a12);
   end;
end;

function Negative(const AValue: TMatrix33): TMatrix33;
begin
   Result := AValue;
   with Result do
   begin
      SetNegative(ex);
      SetNegative(ey);
      SetNegative(ez);
   end;
end;

function Add(const Left, Right: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex := Add(Left.ex, Right.ex);
      ey := Add(Left.ey, Right.ey);
      ez := Add(Left.ez, Right.ez);
   end;
end;

function Add(const m1, m2, m3: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex := Add(m1.ex, m2.ex, m3.ex);
      ey := Add(m1.ey, m2.ey, m3.ey);
      ez := Add(m1.ez, m2.ez, m3.ez);
   end;
end;

function Subtract(const Left, Right: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex := Subtract(Left.ex, Right.ex);
      ey := Subtract(Left.ey, Right.ey);
      ez := Subtract(Left.ez, Right.ez);
   end;
end;

// For Tb2Rot
procedure SetAngle(var r: Tb2Rot; angle: PhysicsFloat);
begin
   SinCos(angle, r.s, r.c);
end;

procedure SetIdentity(var r: Tb2Rot);
begin
   r.s := 0.0;
   r.c := 1.0;
end;

function GetAngle(const r: Tb2Rot): PhysicsFloat;
begin
   Result := ArcTan2(r.s, r.c);
end;

function GetXAxis(const r: Tb2Rot): TVector2;
begin
   Result := MakeVector(r.c, r.s);
end;

function GetYAxis(const r: Tb2Rot): TVector2;
begin
   Result := MakeVector(-r.s, r.c);
end;

// For Tb2Transform

procedure SetIdentity(var xf: Tb2Transform);
begin
   with xf do
   begin
      p := b2Vec2_Zero;
      SetIdentity(q);
   end;
end;

procedure SetValue(var xf: Tb2Transform; const position: TVector2; angle: PhysicsFloat);
begin
   with xf do
   begin
      p := position;
      SetAngle(q, angle);
   end;
end;

// For Tb2Sweep

procedure GetTransform(const Sweep: Tb2Sweep; var xfb: Tb2Transform; beta: PhysicsFloat);
var
   angle: PhysicsFloat;
begin
   with Sweep do
   begin
      xfb.p := Add(Multiply(c0, 1.0 - beta), Multiply(c, beta));
      angle := (1.0 - beta) * a0 + beta * a;
      SetAngle(xfb.q, angle);

      // Shift to origin
      SubtractBy(xfb.p, b2Mul(xfb.q, localCenter));
   end;
end;

procedure Advance(var Sweep: Tb2Sweep; alpha: PhysicsFloat);
var
   beta: PhysicsFloat;
begin
   with Sweep do
   begin
      //b2Assert(alpha0 < 1.0f);
      beta := (alpha - alpha0) / (1.0 - alpha0);
      AddBy(c0, Multiply(Subtract(c, c0), beta));
    	a0 := a0 + beta * (a - a0);
      alpha0 := alpha;
   end;
end;

procedure Normalize(var Sweep: Tb2Sweep);
const
   Pi2 = 2 * Pi;
var
   d: PhysicsFloat;
begin
   with Sweep do
   begin
      d :=  Pi2 * Floor(a0 / Pi2);
      a0 := a0 - d;
      a := a - d;
   end;
end;

{$ENDIF}

function b2Max(const a, b: PhysicsFloat): PhysicsFloat;
begin
   if a >= b then
      Result := a
   else
      Result := b;
end;

function b2Max(const a, b: TVector2): TVector2;
begin
   Result.x := b2Max(a.x, b.x);
   Result.y := b2Max(a.y, b.y);
end;

function b2Min(const a, b: PhysicsFloat): PhysicsFloat;
begin
   if a >= b then
      Result := b
   else
      Result := a;
end;

function b2Min(const a, b: TVector2): TVector2;
begin
   Result.x := b2Min(a.x, b.x);
   Result.y := b2Min(a.y, b.y);
end;

function b2Max(const a, b: Int32): Int32;
begin
   if a >= b then
      Result := a
   else
      Result := b;
end;

function b2Min(const a, b: Int32): Int32;
begin
   if a >= b then
      Result := b
   else
      Result := a;
end;

procedure b2Swap(var a, b: PhysicsFloat);
var
   tmp: PhysicsFloat;
begin
   tmp := a;
   a := b;
   b := tmp;
end;

procedure b2Swap(var a, b: Int32);
var
   tmp: Int32;
begin
   tmp := a;
   a := b;
   b := tmp;
end;

procedure b2Swap(var a, b: TVector2);
var
   tmp: TVector2;
begin
   tmp := a;
   a := b;
   b := tmp;
end;

function b2Clamp(const a, low, high: TVector2): TVector2;
begin
   Result := b2Max(low, b2Min(a, high));
end;

function b2Clamp(const a, low, high: PhysicsFloat): PhysicsFloat;
begin
   if a < low then
      Result := low
   else if a > high then
      Result := high
   else
      Result := a;
end;

function b2MiddlePoint(const a, b: TVector2): TVector2;
begin
   Result.x := (a.x + b.x) * 0.5;
   Result.y := (a.y + b.y) * 0.5;
end;

function IsValid(f: PhysicsFloat): Boolean;
begin
   Result := not (IsNan(f) or IsInfinite(f));
end;

/// Peform the dot product on two vectors.
function b2Dot(const a, b: TVector2): PhysicsFloat;
begin
   Result := a.x * b.x + a.y * b.y;
end;

function b2Dot(const a, b: TVector3): PhysicsFloat;
begin
   Result := a.x * b.x + a.y * b.y + a.z * b.z;
end;

/// Perform the cross product on two vectors. In 2D this produces a scalar.
function b2Cross(const a, b: TVector2): PhysicsFloat;
begin
	 Result := a.x * b.y - a.y * b.x;
end;

/// Perform the cross product on a vector and a scalar. In 2D this produces a vector.
function b2Cross(const a: TVector2; s: PhysicsFloat): TVector2;
begin
   Result.x := s * a.y;
   Result.y := -s * a.x;
end;

/// Perform the cross product on a scalar and a vector. In 2D this produces a vector.
function b2Cross(s: PhysicsFloat; const a: TVector2): TVector2;
begin
   Result.x := -s * a.y;
   Result.y := s * a.x;
end;

function b2Cross(const a, b: TVector3): TVector3;
begin
   Result.x := a.y * b.z - a.z * b.y;
   Result.y := a.z * b.x - a.x * b.z;
   Result.z := a.x * b.y - a.y * b.x;
end;

/// Multiply a matrix times a vector. If a rotation matrix is provided,
/// then this transforms the vector from one frame to another.
function b2Mul(const A: TMatrix22; const v: TVector2): TVector2;
begin
   Result.x := A.ex.x * v.x + A.ey.x * v.y;
   Result.y := A.ex.y * v.x + A.ey.y * v.y;
end;

/// Multiply a matrix times a vector.
function b2Mul22(const A: TMatrix33; const v: TVector2): TVector2;
begin
   Result.x := A.ex.x * v.x + A.ey.x * v.y;
   Result.y := A.ex.y * v.x + A.ey.y * v.y;
end;

/// Multiply a matrix transpose times a vector. If a rotation matrix is provided,
/// then this transforms the vector from one frame to another (inverse transform).
function b2MulT(const A: TMatrix22; const v: TVector2): TVector2;
begin
   Result.x := b2Dot(v, A.ex);
   Result.y := b2Dot(v, A.ey);
end;

{$IFDEF OP_OVERLOAD}
function b2Distance(const a, b: TVector2): PhysicsFloat;
begin
   Result := (a - b).Length;
end;

function b2DistanceSquared(const a, b: TVector2): PhysicsFloat;
begin
   Result := (a - b).SqrLength;
end;
{$ELSE}
function b2Distance(const a, b: TVector2): PhysicsFloat;
begin
   Result := LengthVec(Subtract(a, b));
end;

function b2DistanceSquared(const a, b: TVector2): PhysicsFloat;
begin
   Result := SqrLength(Subtract(a, b));
end;
{$ENDIF}

// A * B
function b2Mul(const A, B: TMatrix22): TMatrix22;
begin
   Result.ex := b2Mul(A, B.ex);
   Result.ey := b2Mul(A, B.ey);
end;

// A^T * B
function b2MulT(const A, B: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.x := b2Dot(A.ex, B.ex);
      ex.y := b2Dot(A.ey, B.ex);
      ey.x := b2Dot(A.ex, B.ey);
      ey.y := b2Dot(A.ey, B.ey);
   end;
end;

// Tb2Transform * TVector2
function b2Mul(const T: Tb2Transform; const v: TVector2): TVector2;
begin
   Result.x := (T.q.c * v.x - T.q.s * v.y) + T.p.x;
   Result.y := (T.q.s * v.x + T.q.c * v.y) + T.p.y;
end;

function b2MulT(const T: Tb2Transform; const v: TVector2): TVector2;
var
   px, py: PhysicsFloat;
begin
   px := v.x - T.p.x;
   py := v.y - T.p.y;
   Result.x := (T.q.c * px + T.q.s * py);
   Result.y := (-T.q.s * px + T.q.c * py);
end;

{$IFDEF OP_OVERLOAD}
function b2Mul(const A: TMatrix33; const v: TVector3): TVector3;
begin
   Result := v.x * A.ex + v.y * A.ey + v.z * A.ez;
end;
{$ELSE}
function b2Mul(const A: TMatrix33; const v: TVector3): TVector3;
begin
   Result := Add(Multiply(A.ex, v.x), Multiply(A.ey, v.y), Multiply(A.ez, v.z));
end;
{$ENDIF}

// Tb2Transform * Tb2Transform
// v2 = A.q.Rot(B.q.Rot(v1) + B.p) + A.p
//    = (A.q * B.q).Rot(v1) + A.q.Rot(B.p) + A.p
function b2Mul(const A, B: Tb2Transform): Tb2Transform;
begin
   Result.q := b2Mul(A.q, B.q);
   {$IFDEF OP_OVERLOAD}
   Result.p := b2Mul(A.q, B.p) + A.p;
   {$ELSE}
   Result.p := Add(b2Mul(A.q, B.p), A.p);
   {$ENDIF}
end;

// v2 = A.q' * (B.q * v1 + B.p - A.p)
//    = A.q' * B.q * v1 + A.q' * (B.p - A.p)
function b2MulT(const A, B: Tb2Transform): Tb2Transform;
begin
   Result.q := b2MulT(A.q, B.q);
   {$IFDEF OP_OVERLOAD}
   Result.p := b2MulT(A.q, B.p - A.p);
   {$ELSE}
   Result.p := b2MulT(A.q, Subtract(B.p, A.p));
   {$ENDIF}
end;

// Tb2Rot * Tb2Rot
function b2Mul(const q, r: Tb2Rot): Tb2Rot;
begin
   // [qc -qs] * [rc -rs] = [qc*rc-qs*rs -qc*rs-qs*rc]
   // [qs  qc]   [rs  rc]   [qs*rc+qc*rs -qs*rs+qc*rc]
   // s = qs * rc + qc * rs
   // c = qc * rc - qs * rs
   Result.s := q.s * r.c + q.c * r.s;
   Result.c := q.c * r.c - q.s * r.s;
end;

function b2MulT(const q, r: Tb2Rot): Tb2Rot;
begin
   // [ qc qs] * [rc -rs] = [qc*rc+qs*rs -qc*rs+qs*rc]
   // [-qs qc]   [rs  rc]   [-qs*rc+qc*rs qs*rs+qc*rc]
   // s = qc * rs - qs * rc
   // c = qc * rc + qs * rs
   Result.s := q.c * r.s - q.s * r.c;
   Result.c := q.c * r.c + q.s * r.s;
end;

function b2Mul(const q: Tb2Rot; const v: TVector2): TVector2;
begin
   Result := MakeVector(q.c * v.x - q.s * v.y, q.s * v.x + q.c * v.y);
end;

function b2MulT(const q: Tb2Rot; const v: TVector2): TVector2;
begin
   Result := MakeVector(q.c * v.x + q.s * v.y, -q.s * v.x + q.c * v.y);
end;

function b2Abs(const a: TVector2): TVector2;
begin
   Result.x := Abs(a.x);
   Result.y := Abs(a.y);
end;

function b2Abs(const a: TMatrix22): TMatrix22;
begin
   Result.ex := b2Abs(a.ex);
   Result.ey := b2Abs(a.ey);
end;

{ TPointF }

{$IFDEF OP_OVERLOAD}

procedure TPointF.SetZero;
begin
   x := 0.0;
   y := 0.0;
end;

procedure TPointF.SetValue(x, y: TPointFloat);
begin
   Self.x := x;
   Self.y := y;
end;

{$ENDIF}

{ TVector2 }

{$IFDEF OP_OVERLOAD}

function TVector2.IsValid: Boolean;
begin
   Result := UPhysics2DTypes.IsValid(x) and UPhysics2DTypes.IsValid(y);
end;

function TVector2.Length: PhysicsFloat;
begin
   Result := Sqrt(x * x + y * y);
end;

function TVector2.Normalize: PhysicsFloat;
begin
   Result := Length();
	 if Result < FLT_EPSILON then
   begin
      Result := 0.0;
      Exit;
   end;
   x := x / Result;
   y := y / Result;
end;

function TVector2.Skew: TVector2;
begin
   Result.x := -y;
   Result.y := x;
end;

procedure TVector2.SetZero;
begin
   x := 0.0;
   y := 0.0;
end;

procedure TVector2.SetNegative;
begin
   x := -x;
   y := -y;
end;

procedure TVector2.SetValue(x, y: PhysicsFloat);
begin
   Self.x := x;
   Self.y := y;
end;

procedure TVector2.SetLength(value: PhysicsFloat);
var
   l: PhysicsFloat;
begin
   l := value / Length;
   x := x * l;
   y := y * l;
end;

function TVector2.SqrLength: PhysicsFloat;
begin
   Result := x * x + y * y;
end;

class function TVector2.From(const x, y: PhysicsFloat): TVector2;
begin
   Result.x := x;
   Result.y := y;
end;

class operator TVector2.Negative(const AValue: TVector2): TVector2;
begin
   Result.x := -AValue.x;
   Result.y := -AValue.y;
end;

class operator TVector2.Add(const Left, Right: TVector2): TVector2;
begin
   Result.x := Left.x + Right.x;
   Result.y := Left.y + Right.y;
end;

class operator TVector2.Subtract(const Left, Right: TVector2): TVector2;
begin
   Result.x := Left.x - Right.x;
   Result.y := Left.y - Right.y;
end;

class operator TVector2.Multiply(const Left: TVector2; const Right: Single): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

class operator TVector2.Multiply(const Left: TVector2; const Right: Double): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

class operator TVector2.Multiply(const Left: TVector2; const Right: Extended): TVector2;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
end;

class operator TVector2.Multiply(const Left: Single; const Right: TVector2): TVector2;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
end;

class operator TVector2.Multiply(const Left: Double; const Right: TVector2): TVector2;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
end;

class operator TVector2.Multiply(const Left: Extended; const Right: TVector2): TVector2;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
end;

class operator TVector2.Divide(const Left: TVector2; const Right: Single): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

class operator TVector2.Divide(const Left: TVector2; const Right: Double): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

class operator TVector2.Divide(const Left: TVector2; const Right: Extended): TVector2;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
end;

procedure TVector2.AddBy(const Operand: TVector2);
begin
   x := x + Operand.x;
   y := y + Operand.y;
end;

procedure TVector2.SubtractBy(const Operand: TVector2);
begin
   x := x - Operand.x;
   y := y - Operand.y;
end;

procedure TVector2.MultiplyBy(const Operand: Single);
begin
   x := x * Operand;
   y := y * Operand;
end;

procedure TVector2.MultiplyBy(const Operand: Double);
begin
   x := x * Operand;
   y := y * Operand;
end;

procedure TVector2.MultiplyBy(const Operand: Extended);
begin
   x := x * Operand;
   y := y * Operand;
end;

procedure TVector2.DivideBy(const Operand: Single);
begin
   x := x / Operand;
   y := y / Operand;
end;

procedure TVector2.DivideBy(const Operand: Double);
begin
   x := x / Operand;
   y := y / Operand;
end;

procedure TVector2.DivideBy(const Operand: Extended);
begin
   x := x / Operand;
   y := y / Operand;
end;

{$ENDIF}

{ TMatrix22 }

{$IFDEF OP_OVERLOAD}
procedure TMatrix22.SetIdentity;
begin
   Self := b2Mat22_identity;
end;

procedure TMatrix22.SetZero;
begin
   ex := b2Vec2_Zero;
   ey := b2Vec2_Zero;
end;

procedure TMatrix22.SetValue(const _col1, _col2: TVector2);
begin
   ex := _col1;
   ey := _col2;
end;

function TMatrix22.Invert: TMatrix22;
var
   a, b, c, d, det: PhysicsFloat;
begin
   a := ex.x;
   b := ey.x;
   c := ex.y;
   d := ey.y;

   det := a * d - b * c;
   if det <> 0.0 then
      det := 1.0 / det;
   with Result do
   begin
      ex.x :=  det * d;
      ey.x := -det * b;
      ex.y := -det * c;
      ey.y :=  det * a;
   end;
end;

function TMatrix22.GetInverse: TMatrix22;
begin
   Result := Invert();
end;

function TMatrix22.Solve(const b: TVector2): TVector2;
var
   det: PhysicsFloat;
begin
   det := ex.x * ey.y - ey.x * ex.y;
   if det <> 0.0 then
      det := 1.0 / det;
   Result.x := det * (ey.y * b.x - ey.x * b.y);
   Result.y := det * (ex.x * b.y - ex.y * b.x);
end;

class operator TMatrix22.Negative(const AValue: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex.SetNegative;
      ey.SetNegative;
   end;
end;

class operator TMatrix22.Add(const Left, Right: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex := Left.ex + Right.ex;
      ey := Left.ey + Right.ey;
   end;
end;

class operator TMatrix22.Subtract(const Left, Right: TMatrix22): TMatrix22;
begin
   with Result do
   begin
      ex := Left.ex - Right.ex;
      ey := Left.ey - Right.ey;
   end;
end;

{$ENDIF}

{ TMatrix33}

{$IFDEF OP_OVERLOAD}

procedure TMatrix33.SetIdentity;
begin
   Self := b2Mat33_identity;
end;

procedure TMatrix33.SetZero;
begin
   ex := b2Vec3_Zero;
   ey := b2Vec3_Zero;
   ez := b2Vec3_Zero;
end;

procedure TMatrix33.SetValue(const _col1, _col2, _col3: TVector3);
begin
   ex := _col1;
   ey := _col2;
   ez := _col3;
end;

function TMatrix33.Solve33(const b: TVector3): TVector3;
var
   det: PhysicsFloat;
begin
   det := b2Dot(ex, b2Cross(ey, ez));
   if det <> 0.0 then
      det := 1.0 / det;
   Result.x := det * b2Dot(b, b2Cross(ey, ez));
   Result.y := det * b2Dot(ex, b2Cross(b, ez));
   Result.z := det * b2Dot(ex, b2Cross(ey, b));
end;

function TMatrix33.Solve22(const b: TVector2): TVector2;
var
   det: PhysicsFloat;
begin
   det := ex.x * ey.y - ey.x * ex.y;
	 if det <> 0.0 then
      det := 1.0 / det;
   Result.x := det * (ey.y * b.x - ey.x * b.y);
   Result.y := det * (ex.x * b.y - ex.y * b.x);
end;

procedure TMatrix33.GetInverse22(var dest: TMatrix33);
var
   a, b, c, d, det: PhysicsFloat;
begin
   a := ex.x;
   b := ey.x;
   c := ex.y;
   d := ey.y;
   det := a * d - b * c;
   if det <> 0.0 then
      det := 1.0 / det;

   dest.ex.x :=  det * d;
   dest.ey.x := -det * b;
   dest.ex.z := 0.0;
   dest.ex.y := -det * c;
   dest.ey.y :=  det * a;
   dest.ey.z := 0.0;
   dest.ez.x := 0.0;
   dest.ez.y := 0.0;
   dest.ez.z := 0.0;
end;

/// Returns the zero matrix if singular.
procedure TMatrix33.GetSymInverse33(var dest: TMatrix33);
var
   det: PhysicsFloat;
   a11, a12, a13, a22, a23, a33: PhysicsFloat;
begin
   det := b2Dot(ex, b2Cross(ey, ez));
   if det <> 0.0 then
      det := 1.0 / det;

   a11 := ex.x;
   a12 := ey.x;
   a13 := ez.x;
   a22 := ey.y;
   a23 := ez.y;
   a33 := ez.z;

   dest.ex.x := det * (a22 * a33 - a23 * a23);
   dest.ex.y := det * (a13 * a23 - a12 * a33);
   dest.ex.z := det * (a12 * a23 - a13 * a22);

   dest.ey.x := dest.ex.y;
   dest.ey.y := det * (a11 * a33 - a13 * a13);
   dest.ey.z := det * (a13 * a12 - a11 * a23);

   dest.ez.x := dest.ex.z;
   dest.ez.y := dest.ey.z;
   dest.ez.z := det * (a11 * a22 - a12 * a12);
end;

class operator TMatrix33.Negative(const AValue: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex.SetNegative;
      ey.SetNegative;
      ez.SetNegative;
   end;
end;

class operator TMatrix33.Add(const Left, Right: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex := Left.ex + Right.ex;
      ey := Left.ey + Right.ey;
      ez := Left.ez + Right.ez;
   end;
end;

class operator TMatrix33.Subtract(const Left, Right: TMatrix33): TMatrix33;
begin
   with Result do
   begin
      ex := Left.ex - Right.ex;
      ey := Left.ey - Right.ey;
      ez := Left.ez - Right.ez;
   end;
end;

{$ENDIF}

{ Rotation }
{$IFDEF OP_OVERLOAD}

procedure Tb2Rot.SetAngle(angle: PhysicsFloat);
begin
   SinCos(angle, s, c);
end;

procedure Tb2Rot.SetIdentity;
begin
   s := 0.0;
   c := 1.0;
end;

function Tb2Rot.GetAngle: PhysicsFloat;
begin
   Result := ArcTan2(s, c);
end;

function Tb2Rot.GetXAxis: TVector2;
begin
   Result := MakeVector(c, s);
end;

function Tb2Rot.GetYAxis: TVector2;
begin
   Result := MakeVector(-s, c);
end;

{$ENDIF}

{ Tb2Transform }
{$IFDEF OP_OVERLOAD}
procedure Tb2Transform.SetIdentity;
begin
   p.SetZero;
   q.SetIdentity;
end;

procedure Tb2Transform.SetValue(const position: TVector2; angle: PhysicsFloat);
begin
   p := position;
   q.SetAngle(angle);
end;

{$ENDIF}

{ Tb2Sweep }
{$IFDEF OP_OVERLOAD}
procedure Tb2Sweep.GetTransform(var xfb: Tb2Transform; beta: PhysicsFloat);
var
   angle: PhysicsFloat;
begin
   xfb.p := (1.0 - beta) * c0 + beta * c;
   angle := (1.0 - beta) * a0 + beta * a;
   xfb.q.SetAngle(angle);

   // Shift to origin
   xfb.p.SubtractBy(b2Mul(xfb.q, localCenter));
end;

procedure Tb2Sweep.Advance(alpha: PhysicsFloat);
var
   beta: PhysicsFloat;
begin
   //b2Assert(alpha0 < 1.0f);
   beta := (alpha - alpha0) / (1.0 - alpha0);
   c0.AddBy(beta * (c - c0));
	 a0 := a0 + beta * (a - a0);
   alpha0 := alpha;
end;

procedure Tb2Sweep.Normalize;
const
   Pi2 = 2 * Pi;
var
   d: PhysicsFloat;
begin
   d := Pi2 * Floor(a0 / Pi2);
   a0 := a0 - d;
   a := a - d;
end;
{$ENDIF}

{ TVector3 }

{$IFDEF OP_OVERLOAD}
function TVector3.IsValid: Boolean;
begin
   Result := UPhysics2DTypes.IsValid(x) and UPhysics2DTypes.IsValid(y) and
      UPhysics2DTypes.IsValid(z);
end;

function TVector3.Length: PhysicsFloat;
begin
   Result := Sqrt(x * x + y * y + z * z);
end;

function TVector3.Normalize: PhysicsFloat;
begin
   Result := Length();
	 if Result < FLT_EPSILON then
   begin
      Result := 0.0;
      Exit;
   end;
   x := x / Result;
   y := y / Result;
   z := z / Result;
end;

procedure TVector3.SetZero;
begin
   x := 0.0;
   y := 0.0;
   z := 0.0;
end;

procedure TVector3.SetNegative;
begin
   x := -x;
   y := -y;
   z := -z;
end;

procedure TVector3.SetValue(x, y, z: PhysicsFloat);
begin
   Self.x := x;
   Self.y := y;
   Self.z := z;
end;

procedure TVector3.SetLength(value: PhysicsFloat);
var
   l: PhysicsFloat;
begin
   l := value / Length;
   x := x * l;
   y := y * l;
   z := z * l;
end;

function TVector3.SqrLength: PhysicsFloat;
begin
   Result := x * x + y * y + z * z;
end;

class function TVector3.From(const x, y, z: PhysicsFloat): TVector3;
begin
   Result.x := x;
   Result.y := y;
   Result.z := z;
end;

class operator TVector3.Negative(const AValue: TVector3): TVector3;
begin
   Result.x := -AValue.x;
   Result.y := -AValue.y;
   Result.z := -AValue.z;
end;

class operator TVector3.Add(const Left, Right: TVector3): TVector3;
begin
   Result.x := Left.x + Right.x;
   Result.y := Left.y + Right.y;
   Result.z := Left.z + Right.z;
end;

class operator TVector3.Subtract(const Left, Right: TVector3): TVector3;
begin
   Result.x := Left.x - Right.x;
   Result.y := Left.y - Right.y;
   Result.z := Left.z - Right.z;
end;

class operator TVector3.Multiply(const Left: TVector3; const Right: Single): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

class operator TVector3.Multiply(const Left: TVector3; const Right: Double): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

class operator TVector3.Multiply(const Left: TVector3; const Right: Extended): TVector3;
begin
   Result.x := Left.x * Right;
   Result.y := Left.y * Right;
   Result.z := Left.z * Right;
end;

class operator TVector3.Multiply(const Left: Single; const Right: TVector3): TVector3;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
   Result.z := Left * Right.z;
end;

class operator TVector3.Multiply(const Left: Double; const Right: TVector3): TVector3;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
   Result.z := Left * Right.z;
end;

class operator TVector3.Multiply(const Left: Extended; const Right: TVector3): TVector3;
begin
   Result.x := Left * Right.x;
   Result.y := Left * Right.y;
   Result.z := Left * Right.z;
end;

class operator TVector3.Divide(const Left: TVector3; const Right: Single): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

class operator TVector3.Divide(const Left: TVector3; const Right: Double): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

class operator TVector3.Divide(const Left: TVector3; const Right: Extended): TVector3;
begin
   Result.x := Left.x / Right;
   Result.y := Left.y / Right;
   Result.z := Left.z / Right;
end;

procedure TVector3.AddBy(const Operand: TVector3);
begin
   x := x + Operand.x;
   y := y + Operand.y;
   z := z + Operand.z;
end;

procedure TVector3.SubtractBy(const Operand: TVector3);
begin
   x := x - Operand.x;
   y := y - Operand.y;
   z := z - Operand.z;
end;

procedure TVector3.MultiplyBy(const Operand: Single);
begin
   x := x * Operand;
   y := y * Operand;
   z := z * Operand;
end;

procedure TVector3.MultiplyBy(const Operand: Double);
begin
   x := x * Operand;
   y := y * Operand;
   z := z * Operand;
end;

procedure TVector3.MultiplyBy(const Operand: Extended);
begin
   x := x * Operand;
   y := y * Operand;
   z := z * Operand;
end;

procedure TVector3.DivideBy(const Operand: Single);
begin
   x := x / Operand;
   y := y / Operand;
   z := z / Operand;
end;

procedure TVector3.DivideBy(const Operand: Double);
begin
   x := x / Operand;
   y := y / Operand;
   z := z / Operand;
end;

procedure TVector3.DivideBy(const Operand: Extended);
begin
   x := x / Operand;
   y := y / Operand;
   z := z / Operand;
end;
{$ENDIF}

end.
