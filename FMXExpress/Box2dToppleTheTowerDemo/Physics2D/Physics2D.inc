{.$DEFINE EXTENDED_PRECISION}
{$DEFINE DOUBLE_PRECISION}

{$IFNDEF EXTENDED_PRECISION}
   {$IFNDEF DOUBLE_PRECISION}
      {$DEFINE SINGLE_PRECISION}
   {$ENDIF}
{$ELSE}
   {$UNDEF DOUBLE_PRECISION}
{$ENDIF}

// Please unflag to reduce code size if object dumping is not needed.
{$DEFINE ENABLE_DUMP}

// Please unflag to reduce code size if controllers are not needed.
{$DEFINE CONTROLLERS}

// Please unflag to calculate time used by physics in each step.
{$DEFINE COMPUTE_PHYSICS_TIME}

{ Though Delphi 2006(up) supports operator overloading but in fact
  Add(vector1, vector2) performs faster than vector1 + vector2. }
{$DEFINE BETTER_PERFORMANCE}

{.$DEFINE DEBUG_SIMULATE_D2007}
{.$DEFINE DEBUG_SIMULATE_D2005}
{.$DEFINE DEBUG_SIMULATE_D7}

// BDS XE6 (BDS 14.0)
{$IFDEF VER270}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS XE5 (BDS 12.0)
{$IFDEF VER260}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS XE4 (BDS 11.0)
{$IFDEF VER250}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS XE3 (BDS 10.0)
{$IFDEF VER240}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS XE2 (BDS 9.0)
{$IFDEF VER230}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS XE (BDS 8.0)
{$IFDEF VER220}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// Operator overloading and inline availability
// BDS 2010 (BDS 7.0)
{$IFDEF VER210}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS 2009 (BDS 6.0)
{$IFDEF VER200}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
  {$DEFINE D2009UP}
{$ENDIF}

// BDS 2007 (BDS 5.0)
{$IFDEF VER190}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

// BDS 2006
{$IFDEF VER180}
  {$DEFINE OP_OVERLOAD}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

// Delphi 2005
{$IFDEF VER170}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

{$IFNDEF OP_OVERLOAD}
   {$IFNDEF VER170}
      {$IFNDEF VER160}
         {$IFNDEF VER150}
            {$IFNDEF VER140}
               {$IFNDEF VER130}
                  {$IFNDEF VER125}
                     {$IFNDEF VER120}
                        {$IFNDEF VER110}
                           {$IFNDEF VER100}
                              {$IFNDEF VER93}
                                 {$IFNDEF VER90}
                                    {$IFNDEF VER80}
                                       // Must be using a Delphi newer than D2010
                                       {$DEFINE OP_OVERLOAD}
                                       {$DEFINE INLINE_AVAIL}

                                       {$IFNDEF VER190}
                                          {$IFNDEF VER180}
                                             {$IFNDEF VER170}
                                                {$DEFINE D2009UP}
                                             {$ENDIF}
                                          {$ENDIF}
                                       {$ENDIF}

                                    {$ENDIF}
                                 {$ENDIF}
                              {$ENDIF}
                           {$ENDIF}
                        {$ENDIF}
                     {$ENDIF}
                  {$ENDIF}
               {$ENDIF}
            {$ENDIF}
         {$ENDIF}
      {$ENDIF}
   {$ENDIF}
{$ENDIF}

{$IFDEF DEBUG_SIMULATE_D2007}
   {$UNDEF D2009UP}
{$ENDIF}

{$IFDEF DEBUG_SIMULATE_D2005}
   {$UNDEF D2009UP}
   {$UNDEF OP_OVERLOAD}
{$ENDIF}

{$IFDEF DEBUG_SIMULATE_D7}
   {$UNDEF D2009UP}
   {$UNDEF OP_OVERLOAD}
   {$UNDEF INLINE_AVAIL}
{$ENDIF}

{$IFDEF BETTER_PERFORMANCE}
   {$UNDEF OP_OVERLOAD}
{$ENDIF}
