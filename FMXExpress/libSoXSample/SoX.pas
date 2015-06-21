unit SoX;

{
  libSoX Library Public Interface.
}

interface

uses
  System.SysUtils,
  System.Math

{$IFDEF Android}
  ,Androidapi.Log
  ,Androidapi.AppGlue
  ,Androidapi.Looper
  ,Androidapi.Egl
  ,Androidapi.Gles
  ,Androidapi.NativeWindow
{$ENDIF Android}

{$IFDEF MSWINDOWS}
  ,Winapi.Windows
  ,Winapi.Messages
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}
  ,Macapi.Mach
  ,Macapi.CoreFoundation
{$ENDIF MACOS};

{ Contains the interface exposed to clients of the libSoX library. }
{ Symbols starting with "sox_" or "SOX_" are part of the public interface for }
{ libSoX clients (applications that consume libSoX). Symbols starting with }
{ "lsx_" or "LSX_" are internal use by libSoX and plugins. }
{ LSX_ and lsx_ symbols should not be used by libSoX-based applications. }

{$DEFINE SOX_H}

{$DEFINE DEBUG}

{$IFDEF ANDROID}
const
  {$DEFINE CDECL}
{$ENDIF ANDROID}

{$IFDEF MSWINDOWS}
const
{$IFDEF WIN32}
  {$IFDEF DEBUG}
  libsox = 'LibSoX32.dll';
  {$ELSE}
  libsox = 'LibSoX32.dll';
  {$ENDIF}
{$ENDIF WIN32}
{$IFDEF WIN64}
  {$IFDEF DEBUG}
  libsox = 'LibSoX64.dll';
  {$ELSE}
  libsox = 'LibSoX64.dll';
  {$ENDIF}
{$ENDIF WIN32}
  {$DEFINE STDCALL}
  _PU = '';
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}
const
  {$DEFINE CDECL}
  {$IFDEF IOS}
    libsox = 'libsox.a';
  {$ELSE}
    libsox = 'libsox.1.dylib';
  {$ENDIF}
{$ENDIF MACOS}

  SOX_UNSPEC = 0; // Members of sox_signalinfo_t are set to SOX_UNSPEC (= 0) if the actual value is not yet known.
  SOX_DEFAULT_CHANNELS = 2; // Default channel count is 2 (stereo).
  SOX_DEFAULT_RATE = 48000; // Default rate is 48000Hz.
  SOX_DEFAULT_PRECISION = 16; // Default precision is 16 bits per sample.

  SOX_MAX_NLOOPS = 8; // Maximum number of loops supported by sox_oob_t = 8.

  SOX_FILE_NOSTDIO = $0001; // Does not use stdio routines
  SOX_FILE_DEVICE = $0002; // File is an audio device
  SOX_FILE_PHONY = $0004; // Phony file/device (for example /dev/null)
  SOX_FILE_REWIND = $0008; // File should be rewound to write header
  SOX_FILE_BIT_REV = $0010; // Is file bit-reversed?
  SOX_FILE_NIB_REV = $0020; // Is file nibble-reversed?
  SOX_FILE_ENDIAN = $0040; // Is file format endian?
  SOX_FILE_ENDBIG = $0080; // For endian file format, is it big endian?
  SOX_FILE_MONO = $0100; // Do channel restrictions allow mono?
  SOX_FILE_STEREO = $0200; // Do channel restrictions allow stereo?
  SOX_FILE_QUAD = $0400; // Do channel restrictions allow quad?

  SOX_FILE_CHANS = (SOX_FILE_MONO or SOX_FILE_STEREO or SOX_FILE_QUAD); // No channel restrictions
  SOX_FILE_LIT_END = (SOX_FILE_ENDIAN or 0); // File is little-endian
  SOX_FILE_BIG_END = (SOX_FILE_ENDIAN or SOX_FILE_ENDBIG); // File is big-endian

  SOX_EFF_CHAN = 1; // Effect might alter the number of channels
  SOX_EFF_RATE = 2; // Effect might alter sample rate
  SOX_EFF_PREC = 4; // Effect does its own calculation of output sample precision (otherwise a default value is taken, depending on the presence of SOX_EFF_MODIFY)
  SOX_EFF_LENGTH = 8; // Effect might alter audio length (as measured in time units, not necessarily in samples)
  SOX_EFF_MCHAN = 16; // Effect handles multiple channels internally
  SOX_EFF_NULL = 32; // Effect does nothing (can be optimized out of chain)
  SOX_EFF_DEPRECATED = 64; // Effect will soon be removed from SoX
  SOX_EFF_GAIN = 128; // Effect does not support gain -r
  SOX_EFF_MODIFY = 256; // Effect does not modify sample values (but might remove or duplicate samples or insert zeros)
  SOX_EFF_ALPHA = 512; // Effect is experimental/incomplete
  SOX_EFF_INTERNAL = 1024; // Effect present in libSoX but not valid for use by SoX command-line tools

  // When used as the "whence" parameter of sox_seek, indicates that the specified
  // offset is relative to the beginning of the file.
  SOX_SEEK_SET = 0;

type
PMarshaledAString = ^MarshaledAString;

// Signed twos-complement 8-bit type. Typically defined as signed char.
{$IFDEF SCHAR_MAX==127 && SCHAR_MIN==(-128)}
  sox_int8_t = Char;
{$ELSE CHAR_MAX==127 && CHAR_MIN==(-128)}
  sox_int8_t = Char;
{$ELSE}
  // #error Unable to determine an appropriate definition for sox_int8_t.
{$ENDIF}

// Unsigned 8-bit type. Typically defined as unsigned char.
{$IFDEF UCHAR_MAX==0xff}
  sox_uint8_t = Byte;
{$ELSE CHAR_MAX==0xff && CHAR_MIN==0}
  sox_uint8_t = Char;
{$ELSE}
  // #error Unable to determine an appropriate definition for sox_uint8_t.
{$ENDIF}

// Signed twos-complement 16-bit type. Typically defined as short.
{$IFDEF SHRT_MAX==32767 && SHRT_MIN==(-32768)}
  sox_int16_t = SmallInt;
{$ELSE INT_MAX==32767 && INT_MIN==(-32768)}
  sox_int16_t = Integer;
{$ELSE}
// #error Unable to determine an appropriate definition for sox_int16_t.
{$ENDIF}

// Unsigned 16-bit type. Typically defined as unsigned short.
{$IFDEF USHRT_MAX==0xffff}
  sox_uint16_t = Word;
{$ELSE UINT_MAX==0xffff}
  sox_uint16_t = Word;
{$ELSE}
  // #error Unable to determine an appropriate definition for sox_uint16_t.
{$ENDIF}

// Signed twos-complement 32-bit type. Typically defined as int.
{$IFDEF INT_MAX==2147483647 && INT_MIN==(-2147483647-1)}
  sox_int32_t = Integer;
{$ELSE LONG_MAX==2147483647 && LONG_MIN==(-2147483647-1)}
  sox_int32_t = LongInt;
{$ELSE}
// #error Unable to determine an appropriate definition for sox_int32_t.
{$ENDIF}

// Unsigned 32-bit type. Typically defined as unsigned int.
{$IFDEF UINT_MAX==0xffffffff}
  sox_uint32_t = Word;
{$ELSE ULONG_MAX==0xffffffff}
  sox_uint32_t = LongInt;
{$ELSE}
  // #error Unable to determine an appropriate definition for sox_uint32_t.
{$ENDIF}

// Signed twos-complement 64-bit type. Typically defined as long or long long.
{$IFDEF LONG_MAX==9223372036854775807 && LONG_MIN==(-9223372036854775807-1)}
  sox_int64_t = LongInt;
{$ELSE _MSC_VER}
  sox_int64_t = Int64;
{$ELSE}
  sox_int64_t = LONG LONG;
{$ENDIF}

// Unsigned 64-bit type. Typically defined as unsigned long or unsigned long long.
{$IFDEF ULONG_MAX==0xffffffffffffffff}
  sox_uint64_t = LongInt;
{$ELSE _MSC_VER}
  sox_uint64_t = UInt64;
{$ELSE}
  sox_uint64_t = UNSIGNED LONG LONG;
{$ENDIF}

  psize_t = ^size_t;
  size_t = Integer;
  psox_comments_t = ^Pchar;
  sox_comments_t = PPChar;
  psox_rate_t = ^sox_rate_t;
  sox_rate_t = Double;
  sox_sample_t = sox_int32_t;
  psox_sample_t = ^sox_sample_t;
  psox_format_handler_t = ^sox_format_handler_t;
  sox_version_flags_t = (sox_version_none);

  sox_bool = (
    // Ensure a signed type
    sox_bool_dummy = -1,
    // False = 0
    sox_false = 0,
    // True = 1
    sox_true = 1);

  // Flags for sox_encodings_info_t: lossless/lossy1/lossy2.
  sox_encodings_flags_t = (
    // No flags specified (implies lossless encoding) = 0.
    sox_encodings_none   = 0,
    // Encode, decode: lossy once = 1.
    sox_encodings_lossy1 = 1,
    // Encode, decode, encode, decode: lossy twice = 2.
    sox_encodings_lossy2 = 2);

  // Is file a real file, a pipe, or a url?
  lsx_io_type = (
    // File is a real file = 0.
    lsx_io_file,
    // File is a pipe (no seeking) = 1.
    lsx_io_pipe,
    // File is a URL (no seeking) = 2.
    lsx_io_url);

  // The libSoX-specific error codes.
  // libSoX functions may return these codes or others that map from errno codes.
  sox_error_t = (
    // Function succeeded = 0.
    SOX_SUCCESS = 0,
    // End Of File or other error = -1
    SOX_EOF = -1,
    // Invalid Audio Header = 2000.
    SOX_EHDR = 2000,
    // Unsupported data format = 2001.
    SOX_EFMT,
    // Can't alloc memory = 2002.
    SOX_ENOMEM,
    // Operation not permitted = 2003.
    SOX_EPERM,
    // Operation not supported = 2004.
    SOX_ENOTSUP,
    // Invalid argument = 2005.
    SOX_EINVAL);

  sox_encoding_t = (
    SOX_ENCODING_UNKNOWN, // encoding has not yet been determined
    SOX_ENCODING_SIGN2, // signed linear 2's comp: Mac
    SOX_ENCODING_UNSIGNED, // unsigned linear: Sound Blaster
    SOX_ENCODING_FLOAT, // floating point (binary format)
    SOX_ENCODING_FLOAT_TEXT, // floating point (text format)
    SOX_ENCODING_FLAC, // FLAC compression
    SOX_ENCODING_HCOM, // Mac FSSD files with Huffman compression
    SOX_ENCODING_WAVPACK, // WavPack with integer samples
    SOX_ENCODING_WAVPACKF, // WavPack with float samples
    SOX_ENCODING_ULAW, // u-law signed logs: US telephony, SPARC
    SOX_ENCODING_ALAW, // A-law signed logs: non-US telephony, Psion
    SOX_ENCODING_G721, // G.721 4-bit ADPCM
    SOX_ENCODING_G723, // G.723 3 or 5 bit ADPCM
    SOX_ENCODING_CL_ADPCM, // Creative Labs 8 --> 2,3,4 bit Compressed PCM
    SOX_ENCODING_CL_ADPCM16, // Creative Labs 16 --> 4 bit Compressed PCM
    SOX_ENCODING_MS_ADPCM, // Microsoft Compressed PCM
    SOX_ENCODING_IMA_ADPCM, // IMA Compressed PCM
    SOX_ENCODING_OKI_ADPCM, // Dialogic/OKI Compressed PCM
    SOX_ENCODING_DPCM, // Differential PCM: Fasttracker 2 (xi)
    SOX_ENCODING_DWVW, // Delta Width Variable Word
    SOX_ENCODING_DWVWN, // Delta Width Variable Word N-bit
    SOX_ENCODING_GSM, // GSM 6.10 33byte frame lossy compression
    SOX_ENCODING_MP3, // MP3 compression
    SOX_ENCODING_VORBIS, // Vorbis compression
    SOX_ENCODING_AMR_WB, // AMR-WB compression
    SOX_ENCODING_AMR_NB, // AMR-NB compression
    SOX_ENCODING_CVSD, // Continuously Variable Slope Delta modulation
    SOX_ENCODING_LPC10, //Linear Predictive Coding
    SOX_ENCODINGS // End of list marker
  );

  // No, yes, or default (default usually implies some kind of auto-detect logic).
  sox_option_t = (
    sox_option_no, // Option specified as no = 0.
    sox_option_yes, // Option specified as yes = 1.
    sox_option_default // Option unspecified = 2.
  );

  // Loop modes: upper 4 bits mask the loop blass, lower 4 bits describe
  // the loop behaviour, for example single shot, bidirectional etc.
  sox_loop_flags_t = (
    sox_loop_none = 0, // single-shot = 0
    sox_loop_forward = 1, // forward loop = 1
    sox_loop_forward_back = 2, // forward/back loop = 2
    sox_loop_8 = 32, // 8 loops (??) = 32
    sox_loop_sustain_decay = 64 // AIFF style, one sustain & one decay loop = 64
  );

  psox_plot_t = ^sox_plot_t;
  // Type of plot.
  sox_plot_t = (
    sox_plot_off, // No plot = 0.
    sox_plot_octave, // Octave plot = 1.
    sox_plot_gnuplot, // Gnuplot plot = 2.
    sox_plot_data // Plot data = 3.
  );

  // Is option argument unsupported, required, or optional.
  lsx_option_arg_t = (
    lsx_option_arg_none, // Option does not have an argument.
    lsx_option_arg_required, // Option requires an argument.
    lsx_option_arg_optional // Option can optionally be followed by an argument.
  );

  // lsx_getopt_init options.
  lsx_getopt_flags_t = (
    lsx_getopt_flag_none = 0, // no flags (no output, not long-only)
    lsx_getopt_flag_opterr = 1, // if set, invalid options trigger lsx_warn output
    lsx_getopt_flag_longonly = 2 // if set, recognize -option as a long option
  );

  plsx_enum_item = ^lsx_enum_item;
  // String name and integer values for enumerated types (type metadata), for use
  // with LSX_ENUM_ITEM, lsx_find_enum_text, and lsx_find_enum_value.
  lsx_enum_item = record
    // String name of enumeration.
    text: MarshaledAString;
    // Integer value of enumeration.
    value: LongInt
  end;

  // Callback to write a message to an output device (console or log file),
  // used by sox_globals_t.output_message_handler.
  sox_output_message_handler_t = procedure(
    // 1 = FAIL, 2 = WARN, 3 = INFO, 4 = DEBUG, 5 = DEBUG_MORE, 6 = DEBUG_MOST.
    level: Word;
    // Source code __FILENAME__ from which message originates.
    filename: MarshaledAString;
    // Message format string.
    fmt: MarshaledAString;
    // Message format parameters.
    ap: Pointer);

  psox_format_t = ^sox_format_t;
  // Callback to initialize reader (decoder), used by
  // sox_format_handler.startread.
  sox_format_handler_startread = function(
    // Format pointer.
    ft: psox_format_t): Integer;

  // Callback to read (decode) a block of samples,
  // used by sox_format_handler.read.
  // returns number of samples read, or 0 if unsuccessful.
  sox_format_handler_read = function(
    // Format pointer.
    ft: psox_format_t;
    // Buffer from which to read samples.
    buf: psox_sample_t;
    // Number of samples available in buf.
    len: size_t): size_t;

  overwrite_permitted = function(const filename: MarshaledAString): sox_bool;

  // Callback to get information about an effect handler,
  // used by the table returned from sox_get_effect_fns(void).
  // Returns Pointer to information about an effect handler.
  sox_effect_handler_t = procedure(); //: TODO: sox_effect_fn_t ???;

  // Callback called while flow is running (called once per buffer),
  // used by sox_flow_effects.callback.
  // Returns SOX_SUCCESS to continue, other value to abort flow.
  sox_flow_effects_callback = function(all_done: sox_bool; client_data: Pointer): Integer;

  // Callback for enumerating the contents of a playlist,
  // used by the sox_parse_playlist function.
  // Returns SOX_SUCCESS if successful, any other value to abort playlist enumeration.
  sox_playlist_callback_t = function(callback_data: Pointer; const filename: MarshaledAString): Integer;

  // Callback to close reader (decoder),
  // used by sox_format_handler.stopread.
  // Returns SOX_SUCCESS if successful.
  sox_format_handler_stopread = function(
    // Format pointer.
    ft: psox_format_t): Integer;

  // Callback to retrieve information about a format handler,
  // used by sox_format_tab_t.fn.
  // Returns format handler information.
  sox_format_fn_t = function(): psox_format_handler_t;

  // Callback to initialize writer (encoder),
  // used by sox_format_handler.startwrite.
  // Returns SOX_SUCCESS if successful.
  sox_format_handler_startwrite = function(
    ft: psox_format_t // Format pointer.
  ): Integer;

  // Callback to write (encode) a block of samples,
  // used by sox_format_handler.write.
  // returns number of samples written, or 0 if unsuccessful.
  sox_format_handler_write = function(
    ft: psox_format_t; // Format pointer.
    const buf: sox_sample_t; // Buffer to which samples are written.
    len: size_t // Capacity of buf, measured in samples.
  ): size_t;

  // Callback to close writer (decoder),
  // used by sox_format_handler.stopwrite.
  // returns SOX_SUCCESS if successful.
  sox_format_handler_stopwrite = function(
    ft: psox_format_t // Format pointer.
  ): Integer;

  // Callback to reposition reader,
  // used by sox_format_handler.seek.
  // Returns SOX_SUCCESS if successful.
  sox_format_handler_seek = function(
    ft: psox_format_t; // Format pointer.
    offset: sox_uint64_t // Sample offset to which reader should be positioned.
  ): Integer;

  // Iformation about a build of libSoX, returned from the sox_version_info function.
  sox_version_info_t = record
    // Structure size = sizeof(sox_version_info_t)
    size: SIZE_T;
    // Feature flags = popen | magic | threads | memopen
    flags: sox_version_flags_t;
    // Version number = 0x140400
    version_code: sox_uint32_t;
    // Version string = sox_version(), for example, "14.4.0"
    {const} version: MarshaledAString;
    // Version extra info or null = "PACKAGE_EXTRA", for example, "beta"
    {const} version_extra: MarshaledAString;
    // Build time = "__DATE__ __TIME__", for example, "Jan 7 2010 03:31:50"
    {const} time: MarshaledAString;
    // Distro or null = "DISTRO", for example, "Debian"
    {const} distro: MarshaledAString;
    // Compiler info or null, for example, "msvc 160040219"
    {const} compiler: MarshaledAString;
    // Arch, for example, "1248 48 44 L OMP"
    // new info should be added at the end for version backwards-compatibility.*/
    {const} arch: MarshaledAString;
  end;

  psox_globals_t = ^sox_globals_t;
  // Global parameters (for effects & formats), returned from the sox_get_globals function.
  sox_globals_t = record
    // Messages are only written if globals.verbosity >= message.level
    verbosity: LongWord;
    // Client-specified message output callback
    output_message_handler: sox_output_message_handler_t;
    // True to use pre-determined timestamps and PRNG seed
    repeatable: sox_bool;
    // Default size (in bytes) used by libSoX for blocks of sample data.
    // Plugins should use similarly-sized buffers to get best performance.
    bufsiz: size_t;
    // Default size (in bytes) used by libSoX for blocks of input sample data.
    // Plugins should use similarly-sized buffers to get best performance.
    input_bufsiz: size_t;
    // Can be used to re-seed libSoX's PRNG
    ranqd1: sox_int32_t;
    // Private: tracks the name of the handler currently using stdin
    {const} stdin_in_use_by: MarshaledAString;
    // Private: tracks the name of the handler currently using stdout
    {const} stdout_in_use_by: MarshaledAString;
    // Private: tracks the name of the handler currently writing an output message
    {const} subsystem: MarshaledAString;
    // Private: client-configured path to use for temporary files
    tmp_path: MarshaledAString;
    // Private: true if client has requested use of 'magic' file-type detection
    use_magic: sox_bool;
    // Private: true if client has requested parallel effects processing
    use_threads: sox_bool;
  end;

  psox_encodinginfo_t = ^sox_encodinginfo_t;
  sox_encodinginfo_t = record
    // Format of sample numbers.
    encoding: sox_encoding_t;
    // 0 if unknown or variable; uncompressed value if lossless; compressed value if lossy
    bits_per_sample: LongWord;
    // compression factor (where applicable)
    // Should bytes be reversed? If this is default during sox_open_read or
    // sox_open_write, libSoX will set them to either no or yes according to the
    // machine or format default.
    compression: Double;
    // Should nibbles be reversed? If this is default during sox_open_read or
    // sox_open_write, libSoX will set them to either no or yes according to the
    // machine or format default.
    reverse_bytes: sox_option_t;
    // Should bits be reversed? If this is default during sox_open_read or
    // sox_open_write, libSoX will set them to either no or yes according to the
    // machine or format default.
    reverse_nibbles: sox_option_t;
    // If set to true, the format should reverse its default endianness.
    reverse_bits: sox_option_t;
    opposite_endian: sox_bool;
  end;

  psox_format_tab_t = ^sox_format_tab_t;
  // Information about a loaded format handler, including the format name and a
  // function pointer that can be invoked to get additional information about the
  // format.
  sox_format_tab_t = record
    name: MarshaledAString; // Name of format handler
    fn: sox_format_fn_t; // Function to call to get format handler's information
  end;

  psox_signalinfo_t = ^sox_signalinfo_t;
  sox_signalinfo_t = record
    // Samples per second, 0 if unknown.
    rate: sox_rate_t;
    // Number of sound channels, 0 if unknown.
    channels: LongWord;
    // Bits per sample, 0 if unknown.
    precision: LongWord;
    // Samples* chans in file, 0 if unknown, -1 if unspecified.
    length: sox_uint64_t;
    // Effects headroom multiplier; may be null.
    mult: PDouble;
  end;

  // Instrument information.
  sox_instrinfo_t = record
    // For unity pitch playback
    MIDInote: ShortInt;
    // MIDI pitch-bend low range
    MIDIlow: ShortInt;
    // MIDI pitch-bend high range
    MIDIhi: ShortInt;
    // 0=no, 1=forward, 2=forward/back (see sox_loop_* values)
    loopmode: Byte;
    // Number of active loops (max SOX_MAX_NLOOPS).
    nloops: LongWord;
  end;

  // Looping parameters (out-of-band data).
  sox_loopinfo_t = record
    // first sample
    start: SOX_UINT64_T;
    // length
    length: SOX_UINT64_T;
    // number of repeats, 0=forever
    count: LongWord;
    // 0=no, 1=forward, 2=forward/back (see sox_loop_* for valid values).
    type_: Byte;
  end;

  psox_oob_t = ^sox_oob_t;
  sox_oob_t = record
    // Comment strings in id=value format.
    comments: sox_comments_t;
    // Instrument specification.
    instr: sox_instrinfo_t;
    // Looping specification.
    loops: Array[0..SOX_MAX_NLOOPS - 1] of sox_loopinfo_t;
  end;

  sox_format_handler_t = record
    // Checked on load; must be 1st in struct.
    sox_lib_version_code: LongWord;
    // Short description of format.
    {const} description: MarshaledAString;
    // null-terminated array of filename extensions that are handled by this format.
    {const} names: array of MarshaledAString;
    // File flags (SOX_FILE_* values).
    flags: LongInt;
    // Called to initialize reader (decoder).
    startread: sox_format_handler_startread;
    // Called to read (decode) a block of samples.
    read: sox_format_handler_read;
    // Called to close reader (decoder); may be null if no closing necessary.
    stopread: sox_format_handler_stopread;
    // Called to initialize writer (encoder).
    startwrite: sox_format_handler_startwrite;
    // Called to write (encode) a block of samples.
    write: sox_format_handler_write;
    // Called to close writer (decoder); may be null if no closing necessary.
    stopwrite: sox_format_handler_stopwrite;
    // Called to reposition reader; may be null if not supported.
    seek: sox_format_handler_seek;
    // Array of values indicating the encodings and precisions supported for
    // writing (encoding). Precisions specified with default precision first.
    // Encoding, precision, precision, ..., 0, repeat. End with one more 0.
    // Example:
    // unsigned const * formats = {
      // SOX_ENCODING_SIGN2, 16, 24, 0, // Support SIGN2 at 16 and 24 bits, default to 16 bits.
      // SOX_ENCODING_UNSIGNED, 8, 0,   // Support UNSIGNED at 8 bits, default to 8 bits.
      // 0 // No more supported encodings.
    // };
    {const} write_formats: PLongWord;
    // Array of sample rates (samples per second) supported for writing (encoding).
    // NULL if all (or almost all) rates are supported. End with 0.
    {const} write_rates: psox_rate_t;
    // SoX will automatically allocate a buffer in which the handler can store data.
    // Specify the size of the buffer needed here. Usually this will be sizeof(your_struct).
    // The buffer will be allocated and zeroed before the call to startread/startwrite.
    // The buffer will be freed after the call to stopread/stopwrite.
    // The buffer will be provided via format.priv in each call to the handler.
    priv_size: size_t;
  end;

  sox_format_t = record
    // File name.
    filename: MarshaledAString;
    // Signal specifications for reader (decoder) or writer (encoder):
    // sample rate, number of channels, precision, length, headroom multiplier.
    // Any info specified by the user is here on entry to startread or
    // startwrite. Info will be SOX_UNSPEC if the user provided no info.
    // At exit from startread, should be completely filled in, using
    // either data from the file's headers (if available) or whatever
    // the format is guessing/assuming (if header data is not available).
    // At exit from startwrite, should be completely filled in, using
    // either the data that was specified, or values chosen by the format
    // based on the format's defaults or capabilities.
    signal: sox_signalinfo_t;
    // Encoding specifications for reader (decoder) or writer (encoder):
    // encoding (sample format), bits per sample, compression rate, endianness.
    // Should be filled in by startread. Values specified should be used
    // by startwrite when it is configuring the encoding parameters.
    encoding: sox_encodinginfo_t;
    // Type of file, as determined by header inspection or libmagic.
    filetype: MarshaledAString;
    // Comments, instrument info, loop info (out-of-band data).
    oob: sox_oob_t;
    // Can seek on this file.
    seekable: sox_bool;
    // Read or write mode ('r' or 'w').
    mode: Byte;
    // Samples* chans written to file.
    olength: sox_uint64_t;
    // Incremented if clipping occurs.
    clips: sox_uint64_t;
    // Failure error code.
    sox_errno: Integer;
    // Failure error text.
    sox_errstr: Array[0..255] of Byte;
    // File stream pointer.
    fp: Pointer;
    // Stores whether this is a file, pipe or URL.
    io_type: lsx_io_type;
    // Current offset within file.
    tell_off: sox_uint64_t;
    // Offset at which headers end and sound data begins (set by lsx_check_read_params).
    data_start: sox_uint64_t;
    // Format handler for this file.
    handler: sox_format_handler_t;
    // Format handler's private data area.
    priv: Pointer;
  end;

  psox_effects_globals_t = ^sox_effects_globals_t;
  sox_effects_globals_t = record
    // To help the user choose effect & options.
    plot: sox_plot_t;
    // Pointer to associated SoX globals.
    global_info: psox_globals_t;
  end;

  ppsox_effect_t = ^psox_effect_t;
  psox_effect_t = ^sox_effect_t;
  // Effect information.
  sox_effect_t = record
    global_info: psox_effects_globals_t; // global effect parameters
    in_signal: sox_signalinfo_t; // Information about the incoming data stream
    out_signal: sox_signalinfo_t; // Information about the outgoing data stream
    {const} in_encoding: psox_encodinginfo_t; // Information about the incoming data encoding
    {const} out_encoding: psox_encodinginfo_t; // Information about the outgoing data encoding
    handler: sox_effect_handler_t; // The handler for this effect
    clips: sox_uint64_t; // increment if clipping occurs
    flows: size_t; // 1 if MCHAN, number of chans otherwise
    flow: size_t; // flow number
    priv: Pointer; // Effect's private data area (each flow has a separate copy)
    // The following items are private to the libSoX effects chain functions.
    obuf: psox_sample_t; // output buffer
    obeg: size_t; // output buffer: start of valid data section
    oend: size_t; // output buffer: one past valid data section (oend-obeg is length of current content)
    imin: size_t; // minimum input buffer content required for calling this effect's flow function; set via lsx_effect_set_imin()
  end;

  psox_effects_chain_t = ^sox_effects_chain_t;
  // Chain of effects to be applied to a stream.
  sox_effects_chain_t = record
    effects: ppsox_effect_t; //Table of effects to be applied to a stream.
    length: size_t; // Number of effects to be applied
    global_info: sox_effects_globals_t; // Copy of global effects settings
    {const} in_enc: psox_encodinginfo_t; // Input encoding
    {const} out_enc: psox_encodinginfo_t; // Output encoding
    // The following items are private to the libSoX effects chain functions.
    table_size: size_t; // Size of effects table (including unused entries)
    il_buf: psox_sample_t; // Channel interleave buffer */
  end;

  psox_encodings_info_t = ^sox_encodings_info_t;
  // Basic information about an encoding.
  sox_encodings_info_t = record
    flags: sox_encodings_flags_t; // lossy once (lossy1), lossy twice (lossy2), or lossless (none).
    {const} name: MarshaledAString; // encoding name.
    {const} desc: MarshaledAString; // encoding description.
  end;

  plsx_option_t = ^lsx_option_t;
  // lsx_getopt long option descriptor.
  lsx_option_t = record
    name: MarshaledAString; // Name of the long option.
    has_arg: lsx_option_arg_t; // Whether the long option supports an argument and, if so, whether the argument is required or optional.
    flag: PInteger; // Flag to set if argument is present.
    val: Integer; // Value to put in flag if argument is present.
  end;

  plsx_getopt_t = ^lsx_getopt_t;
  // lsx_getopt session information (initialization data and state).
  lsx_getopt_t = record
    argc: Integer; // IN argc: Number of arguments in argv
    argv: MarshaledAString; // IN argv: Array of arguments
    shortopts: MarshaledAString; // IN shortopts: Short option characters
    longopts: plsx_option_t; // IN longopts: Array of long option descriptors
    flags: lsx_getopt_flags_t; // IN flags: Flags for longonly and opterr
    curpos: MarshaledAString; // INOUT curpos: Maintains state between calls to lsx_getopt
    ind: Integer; // INOUT optind: Maintains the index of next element to be processed
    opt: Integer; // OUT optopt: Receives the option character that caused error
    arg: MarshaledAString; // OUT optarg: Receives the value of the option's argument
    lngind: Integer; // OUT lngind: Receives the index of the matched long option or -1 if not a long option
  end;

const
  SOX_UNKNOWN_LEN = sox_uint64_t(-1); // sox_signalinfo_t.length is set to SOX_UNKNOWN_LEN (= -1) within the effects chain if the actual length is not known. Format handlers currently use SOX_UNSPEC instead.
  SOX_IGNORE_LENGTH = sox_uint64_t(-2); // sox_signalinfo_t.length is set to SOX_IGNORE_LENGTH (= -2) to indicate that a format handler should ignore length information in file headers.
  SOX_DEFAULT_ENCODING = sox_encoding_t.SOX_ENCODING_SIGN2; // Default encoding is SIGN2 (linear 2's complement PCM).
  SOX_LOOP_NONE_ = Byte(sox_loop_none); // single-shot = 0
  SOX_LOOP_8_ = Byte(sox_loop_8); // 8 loops = 32
  SOX_LOOP_SUSTAIN_DECAY_ = Byte(sox_loop_sustain_decay); // AIFF style, one sustain & one decay loop = 64

{$IFDEF ANDROID}
var
{$ENDIF}

// Functions:
{------------------------------------------------------------------------------}
// Returns version number string of libSoX, for example, "14.4.0".
{$IFDEF ANDROID}sox_version:{$ENDIF} function {$IFNDEF ANDROID}sox_version{$ENDIF}: MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_version';{$ENDIF}

// Initialize effects library.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_init:{$ENDIF} function {$IFNDEF ANDROID}sox_init{$ENDIF}: Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_init';{$ENDIF}

// Close effects library and unload format handler plugins.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_quit:{$ENDIF} function {$IFNDEF ANDROID}sox_quit{$ENDIF}: Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_quit';{$ENDIF}

// Opens a decoding session for a file. Returned handle must be closed with sox_close().
// Returns The handle for the new session, or null on failure.
{$IFDEF ANDROID}sox_open_read:{$ENDIF} function {$IFNDEF ANDROID}sox_open_read{$ENDIF}(
  // Path to file to be opened (required).
  const path: MarshaledAString;
  // Information already known about audio stream, or NULL if none.
  const signal: psox_signalinfo_t;
  // Information already known about sample encoding, or NULL if none.
  const encoding: psox_encodinginfo_t;
  // Previously-determined file type, or NULL to auto-detect.
  const filetype: MarshaledAString): psox_format_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_open_read';{$ENDIF}

// Opens an encoding session for a file. Returned handle must be closed with sox_close().
// Returns The new session handle, or null on failure.
{$IFDEF ANDROID}sox_open_write:{$ENDIF} function {$IFNDEF ANDROID}sox_open_write{$ENDIF}(
  // Path to file to be written (required).
  const path: MarshaledAString;
  // Information about desired audio stream (required).
  const signal: psox_signalinfo_t;
  // Information about desired sample encoding, or NULL to use defaults.
  const encoding: psox_encodinginfo_t;
  // Previously-determined file type, or NULL to auto-detect.
  const filetype: MarshaledAString;
  // Out-of-band data to add to file, or NULL if none.
  const oob: psox_oob_t;
  // Called if file exists to determine whether overwrite is ok.
  overwrite_permitted_: overwrite_permitted): psox_format_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_open_write';{$ENDIF}

// Returns an array containing the known effect handlers.
// Returns An array containing the known effect handlers.
// function sox_get_effect_fns: psox_effect_fn_t; TODO: ???
// Initializes an effects chain. Returned handle must be closed with sox_delete_effects_chain().
// Returns Handle, or null on failure.
{$IFDEF ANDROID}sox_create_effects_chain:{$ENDIF} function {$IFNDEF ANDROID}sox_create_effects_chain{$ENDIF}(
  // Input encoding.
  const in_enc: psox_encodinginfo_t;
  // Output encoding.
  const out_enc: psox_encodinginfo_t): psox_effects_chain_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_create_effects_chain';{$ENDIF}

// Finds the effect handler with the given name.
// Returns Effect pointer, or null if not found.
{$IFDEF ANDROID}sox_find_effect:{$ENDIF} function {$IFNDEF ANDROID}sox_find_effect{$ENDIF}(
  // Name of effect to find.
  const name: MarshaledAString): sox_effect_handler_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_find_effect';{$ENDIF}

// Creates an effect using the given handler.
// Returns The new effect, or null if not found.
{$IFDEF ANDROID}sox_create_effect:{$ENDIF} function {$IFNDEF ANDROID}sox_create_effect{$ENDIF}(
  // Handler to use for effect.
  const eh: sox_effect_handler_t): psox_effect_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_create_effect';{$ENDIF}

// Applies the command-line options to the effect.
// Returns the number of arguments consumed.
{$IFDEF ANDROID}sox_effect_options:{$ENDIF} function {$IFNDEF ANDROID}sox_effect_options{$ENDIF}(
  // Effect pointer on which to set options.
  effp: psox_effect_t;
  // Number of arguments in argv.
  argc: Integer;
  // Array of command-line options.
  const argv: array of MarshaledAString): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_effect_options';{$ENDIF}

// Adds an effect to the effects chain, returns SOX_SUCCESS if successful.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_add_effect:{$ENDIF} function {$IFNDEF ANDROID}sox_add_effect{$ENDIF}(
  // Effects chain to which effect should be added.
  chain: psox_effects_chain_t;
  // Effect to be added.
  effp: psox_effect_t;
  // Input format.
  in_: psox_signalinfo_t;
  // Output format.
  const out_: psox_signalinfo_t): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_add_effect';{$ENDIF}

// Runs the effects chain, returns SOX_SUCCESS if successful.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_flow_effects:{$ENDIF} function {$IFNDEF ANDROID}sox_flow_effects{$ENDIF}(
  // Effects chain to run.
  chain: psox_effects_chain_t;
  // Callback for monitoring flow progress.
  callback: sox_flow_effects_callback;
  // Data to pass into callback.
  client_data: Pointer): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_flow_effects';{$ENDIF}

// Closes an effects chain.
{$IFDEF ANDROID}sox_delete_effects_chain:{$ENDIF} procedure {$IFNDEF ANDROID}sox_delete_effects_chain{$ENDIF}(
  // // Effects chain pointer.
  ecp: psox_effects_chain_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_delete_effects_chain';{$ENDIF}

// Closes an encoding or decoding session.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_close:{$ENDIF} function {$IFNDEF ANDROID}sox_close{$ENDIF}(
  // Format pointer.
  ft: psox_format_t): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_close';{$ENDIF}

// Returns information about this build of libsox.
{$IFDEF ANDROID}sox_version_info:{$ENDIF} function {$IFNDEF ANDROID}sox_version_info{$ENDIF}: sox_version_info_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_version_info';{$ENDIF}

// Returns a pointer to the structure with libSoX's global settings.
{$IFDEF ANDROID}sox_get_globals:{$ENDIF} function {$IFNDEF ANDROID}sox_get_globals{$ENDIF}: psox_globals_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_get_globals';{$ENDIF}

// Returns a pointer to the list of available encodings.
// End of list indicated by name == NULL.
{$IFDEF ANDROID}sox_get_encodings_info:{$ENDIF} function {$IFNDEF ANDROID}sox_get_encodings_info{$ENDIF}: psox_encodings_info_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_get_encodings_info';{$ENDIF}

// Fills in an encodinginfo with default values.
{$IFDEF ANDROID}sox_init_encodinginfo:{$ENDIF} procedure {$IFNDEF ANDROID}sox_init_encodinginfo{$ENDIF}(
  // Pointer to uninitialized encoding info structure to be initialized.
  e: psox_encodinginfo_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_init_encodinginfo';{$ENDIF}

// Given an encoding (for example, SIGN2) and the encoded bits_per_sample (for
// example, 16), returns the number of useful bits per sample in the decoded data
// (for example, 16), or returns 0 to indicate that the value returned by the
// format handler should be used instead of a pre-determined precision.
// Returns the number of useful bits per sample in the decoded data (for example
// 16), or returns 0 to indicate that the value returned by the format handler
// should be used instead of a pre-determined precision.
{$IFDEF ANDROID}sox_precision:{$ENDIF} function {$IFNDEF ANDROID}sox_precision{$ENDIF}(
  // Encoding for which to lookup precision information.
  encoding: SOX_ENCODING_T;
  bits_per_sample: LongWord): LongWord; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_precision';{$ENDIF}

// Returns the number of items in the metadata block.
{$IFDEF ANDROID}sox_num_comments:{$ENDIF} function {$IFNDEF ANDROID}sox_num_comments{$ENDIF}(
  // Metadata block.
  comments: sox_comments_t): size_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_num_comments';{$ENDIF}

// Adds an "id=value" item to the metadata block.
{$IFDEF ANDROID}sox_append_comment:{$ENDIF} procedure {$IFNDEF ANDROID}sox_append_comment{$ENDIF}(
  // Metadata block.
  comments: psox_comments_t;
  // Item to be added in "id=value" format.
  const item: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_append_comment';{$ENDIF}

// Adds a newline-delimited list of "id=value" items to the metadata block.
{$IFDEF ANDROID}sox_append_comments:{$ENDIF} procedure {$IFNDEF ANDROID}sox_append_comments{$ENDIF}(
  // Metadata block.
  comments: psox_comments_t;
  // Newline-separated list of items to be added, for example "id1=value1" "nid2=value2".
  const Items: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_append_comments';{$ENDIF}

// Duplicates the metadata block.
// Returns the copied metadata block.
{$IFDEF ANDROID}sox_copy_comments:{$ENDIF} function {$IFNDEF ANDROID}sox_copy_comments{$ENDIF}(comments: sox_comments_t): sox_comments_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_copy_comments';{$ENDIF}

// Frees the metadata block.
{$IFDEF ANDROID}sox_delete_comments:{$ENDIF} procedure {$IFNDEF ANDROID}sox_delete_comments{$ENDIF}(comments: psox_comments_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_delete_comments';{$ENDIF}

// If "id=value" is found, return value, else return null.
// Returns value, or null if value not found.
{$IFDEF ANDROID}sox_find_comment:{$ENDIF} function {$IFNDEF ANDROID}sox_find_comment{$ENDIF}(
  // Metadata block in which to search.
  comments: sox_comments_t; const id: MarshaledAString): MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_find_comment';{$ENDIF}

// Find and load format handler plugins.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_format_init:{$ENDIF} function {$IFNDEF ANDROID}sox_format_init{$ENDIF}: Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_format_init';{$ENDIF}

// Unload format handler plugins.
{$IFDEF ANDROID}sox_format_quit:{$ENDIF} procedure {$IFNDEF ANDROID}sox_format_quit{$ENDIF}; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_format_quit';{$ENDIF}

// Returns the table of format handler names and functions.
{$IFDEF ANDROID}sox_get_format_fns:{$ENDIF} function {$IFNDEF ANDROID}sox_get_format_fns{$ENDIF}: psox_format_tab_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_get_format_fns';{$ENDIF}

// Opens a decoding session for a memory buffer. Returned handle must be closed with sox_close().
// Returns The handle for the new session, or null on failure.
{$IFDEF ANDROID}sox_open_mem_read:{$ENDIF} function {$IFNDEF ANDROID}sox_open_mem_read{$ENDIF}(
  // Pointer to audio data buffer (required).
  buffer: Pointer;
  // Number of bytes to read from audio data buffer.
  buffer_size: size_t;
  // Information already known about audio stream, or NULL if none.
  const signal: psox_signalinfo_t;
  // Information already known about sample encoding, or NULL if none.
  const encoding: psox_encodinginfo_t;
  // Previously-determined file type, or NULL to auto-detect.
  const filetype: MarshaledAString): psox_format_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_open_mem_read';{$ENDIF}

// Returns true if the format handler for the specified file type supports the specified encoding.
// Returns true if the format handler for the specified file type supports the specified encoding.
{$IFDEF ANDROID}sox_format_supports_encoding:{$ENDIF} function {$IFNDEF ANDROID}sox_format_supports_encoding{$ENDIF}(
  // Path to file to be examined (required if filetype is NULL).
  const path: MarshaledAString;
  // Previously-determined file type, or NULL to use extension from path.
  const filetype: MarshaledAString;
  // Encoding for which format handler should be queried.
  const encoding: psox_encodinginfo_t): sox_bool; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_format_supports_encoding';{$ENDIF}

// Gets the format handler for a specified file type.
// Returns åhe found format handler, or null if not found.
{$IFDEF ANDROID}sox_write_handler:{$ENDIF} function {$IFNDEF ANDROID}sox_write_handler{$ENDIF}(
  // Path to file (required if filetype is NULL).
  const path: MarshaledAString;
  // Filetype for which handler is needed, or NULL to use extension from path.
  const filetype: MarshaledAString;
  // Receives the filetype that was detected. Pass NULL if not needed.
  const filetype1: PMarshaledAString): psox_format_handler_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_write_handler';{$ENDIF}

// Opens an encoding session for a memory buffer. Returned handle must be closed with sox_close().
// Returns The new session handle, or null on failure.
{$IFDEF ANDROID}sox_open_mem_write:{$ENDIF} function {$IFNDEF ANDROID}sox_open_mem_write{$ENDIF}(
  // Pointer to audio data buffer that receives data (required).
  buffer: Pointer;
  // Maximum number of bytes to write to audio data buffer.
  buffer_size: size_t;
  // Information about desired audio stream (required).
  const signal: psox_signalinfo_t;
  // Information about desired sample encoding, or NULL to use defaults.
  const encoding: psox_encodinginfo_t;
  // Previously-determined file type, or NULL to auto-detect.
  const filetype: MarshaledAString;
  // Out-of-band data to add to file, or NULL if none.
  const oob: psox_oob_t): psox_format_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_open_mem_write';{$ENDIF}

// Opens an encoding session for a memstream buffer. Returned handle must be closed with sox_close().
// Returns The new session handle, or null on failure.
{$IFDEF ANDROID}sox_open_memstream_write:{$ENDIF} function {$IFNDEF ANDROID}sox_open_memstream_write{$ENDIF}(
  // Receives pointer to audio data buffer that receives data (required).
  buffer_ptr: PMarshaledAString;
  // Receives size of data written to audio data buffer (required).
  buffer_size_ptr: psize_t;
  // Information about desired audio stream (required).
  const signal: psox_signalinfo_t;
  // Information about desired sample encoding, or NULL to use defaults.
  const encoding: psox_encodinginfo_t;
  // Previously-determined file type, or NULL to auto-detect.
  const filetype: MarshaledAString;
  // Out-of-band data to add to file, or NULL if none.
  const oob: psox_oob_t): psox_format_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_open_memstream_write';{$ENDIF}

// Reads samples from a decoding session into a sample buffer.
// Returns Number of samples decoded, or 0 for EOF.
{$IFDEF ANDROID}sox_read:{$ENDIF} function {$IFNDEF ANDROID}sox_read{$ENDIF}(
  // Format pointer.
  ft: psox_format_t;
  // Buffer from which to read samples.
  buf: psox_sample_t;
  // Number of samples available in buf.
  len: size_t): size_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_read';{$ENDIF}

// Writes samples to an encoding session from a sample buffer.
// Returns Number of samples encoded.
{$IFDEF ANDROID}sox_write:{$ENDIF} function {$IFNDEF ANDROID}sox_write{$ENDIF}(
  // Format pointer.
  ft: psox_format_t;
  // Buffer from which to read samples.
  const buf: psox_sample_t;
  // Number of samples available in buf.
  len: size_t): size_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_write';{$ENDIF}

// Sets the location at which next samples will be decoded. Returns SOX_SUCCESS if successful.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_seek:{$ENDIF} function {$IFNDEF ANDROID}sox_seek{$ENDIF}(
  // Format pointer.
  ft: psox_format_t;
  // Sample offset at which to position reader.
  offset: sox_uint64_t;
  // Set to SOX_SEEK_SET.
  whence: Integer): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_seek';{$ENDIF}

// Finds a format handler by name.
// Returns Format handler data, or null if not found.
{$IFDEF ANDROID}sox_find_format:{$ENDIF} function {$IFNDEF ANDROID}sox_find_format{$ENDIF}(
  // Name of format handler to find.
  const name: MarshaledAString;
  // Set to true to ignore device names.
  ignore_devices: sox_bool): psox_format_handler_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_find_format';{$ENDIF}

// Returns global parameters for effects
// Returns global parameters for effects.
{$IFDEF ANDROID}sox_get_effects_globals:{$ENDIF} function {$IFNDEF ANDROID}sox_get_effects_globals{$ENDIF}: psox_effects_globals_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_get_effects_globals';{$ENDIF}

// Gets the number of clips that occurred while running an effects chain.
// Returns the number of clips that occurred while running an effects chain.
{$IFDEF ANDROID}sox_effects_clips:{$ENDIF} function {$IFNDEF ANDROID}sox_effects_clips{$ENDIF}(
  // Effects chain from which to read clip information.
  chain: psox_effects_chain_t): sox_uint64_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_effects_clips';{$ENDIF}

// Shuts down an effect (calls stop on each of its flows).
// Returns the number of clips from all flows.
{$IFDEF ANDROID}sox_stop_effect:{$ENDIF} function {$IFNDEF ANDROID}sox_stop_effect{$ENDIF}(
  // Effect to stop.
  effp: psox_effect_t): sox_uint64_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_stop_effect';{$ENDIF}

// Adds an already-initialized effect to the end of the chain.
{$IFDEF ANDROID}sox_push_effect_last:{$ENDIF} procedure {$IFNDEF ANDROID}sox_push_effect_last{$ENDIF}(
  // Effects chain to which effect should be added.
  chain: psox_effects_chain_t;
  // Effect to be added.
  effp: psox_effect_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_push_effect_last';{$ENDIF}

// Removes and returns an effect from the end of the chain.
// Returns the removed effect, or null if no effects.
{$IFDEF ANDROID}sox_pop_effect_last:{$ENDIF} function {$IFNDEF ANDROID}sox_pop_effect_last{$ENDIF}(
  // Effects chain from which to remove an effect.
  chain: psox_effects_chain_t): psox_effect_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_pop_effect_last';{$ENDIF}

// Shut down and delete an effect.
{$IFDEF ANDROID}sox_delete_effect:{$ENDIF} procedure {$IFNDEF ANDROID}sox_delete_effect{$ENDIF}(
  // Effect to be deleted.
  effp: psox_effect_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_delete_effect';{$ENDIF}

// Shut down and delete the last effect in the chain.
{$IFDEF ANDROID}sox_delete_effect_last:{$ENDIF} procedure {$IFNDEF ANDROID}sox_delete_effect_last{$ENDIF}(
  // Effects chain from which to remove the last effect.
  chain: psox_effects_chain_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_delete_effect_last';{$ENDIF}

// Shut down and delete all effects in the chain.
{$IFDEF ANDROID}sox_delete_effects:{$ENDIF} procedure {$IFNDEF ANDROID}sox_delete_effects{$ENDIF}(
  // Effects chain from which to delete effects.
  chain: psox_effects_chain_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_delete_effects';{$ENDIF}

// Gets the sample offset of the start of the trim, useful for efficiently
// skipping the part that will be trimmed anyway (get trim start, seek, then
// clear trim start).
// Returns the sample offset of the start of the trim.
{$IFDEF ANDROID}sox_trim_get_start:{$ENDIF} function {$IFNDEF ANDROID}sox_trim_get_start{$ENDIF}(
  // Trim effect.
  effp: psox_effect_t): sox_uint64_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_trim_get_start';{$ENDIF}

// Clears the start of the trim to 0.
{$IFDEF ANDROID}sox_trim_clear_start:{$ENDIF} procedure {$IFNDEF ANDROID}sox_trim_clear_start{$ENDIF}(
  // Trim effect.
  effp: psox_effect_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_trim_clear_start';{$ENDIF}

// Returns true if the specified file is a known playlist file type.
// Returns true if the specified file is a known playlist file type.
{$IFDEF ANDROID}sox_is_playlist:{$ENDIF} function {$IFNDEF ANDROID}sox_is_playlist{$ENDIF}(
  // Name of file to examine.
  const filename: MarshaledAString): sox_bool; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_is_playlist';{$ENDIF}

// Parses the specified playlist file.
// Returns SOX_SUCCESS if successful.
{$IFDEF ANDROID}sox_parse_playlist:{$ENDIF} function {$IFNDEF ANDROID}sox_parse_playlist{$ENDIF}(
  // Callback to call for each item in the playlist.
  callback: sox_playlist_callback_t;
  // Data to pass to callback.
  p: Pointer;
  // Filename of playlist file.
  const listname: MarshaledAString): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_parse_playlist';{$ENDIF}

// Converts a SoX error code into an error string.
// Returns error string corresponding to the specified error code,
// or a generic message if the error code is not recognized.
{$IFDEF ANDROID}sox_strerror:{$ENDIF} function {$IFNDEF ANDROID}sox_strerror{$ENDIF}(
  // Error code to look up.
  sox_errno: Integer): MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF} external libsox name _PU + 'sox_strerror';{$ENDIF}

// Gets the basename of the specified file; for example, the basename of
// "/a/b/c.d" would be "c".
// Returns the number of characters written to base_buffer, excluding the null,
// or 0 on failure.
{$IFDEF ANDROID}sox_basename:{$ENDIF} function {$IFNDEF ANDROID}sox_basename{$ENDIF}(
  // Buffer into which basename should be written.
  base_buffer: MarshaledAString;
  // Size of base_buffer, in bytes.
  base_buffer_len: size_t;
  // Filename from which to extract basename.
  const filename: MarshaledAString): size_t {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'sox_basename';{$ENDIF}

// WARNING - The items in this section are subject to instability. They only
// exist in the public header because sox (the application) currently uses them.
// These may be changed or removed in future versions of libSoX.

// Print a fatal error in libSoX.
{$IFDEF ANDROID}lsx_fail_impl:{$ENDIF} procedure {$IFNDEF ANDROID}lsx_fail_impl{$ENDIF}(
  // Printf-style format string.
  const fmt: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_fail_impl';{$ENDIF}

// Print a warning in libSoX.
{$IFDEF ANDROID}lsx_warn_impl:{$ENDIF} procedure {$IFNDEF ANDROID}lsx_warn_impl{$ENDIF}(
  // Printf-style format string.
  const fmt: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_warn_impl';{$ENDIF}

// Print an informational message in libSoX.
{$IFDEF ANDROID}lsx_report_impl:{$ENDIF} procedure {$IFNDEF ANDROID}lsx_report_impl{$ENDIF}(
  // Printf-style format string.
  const fmt: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_report_impl';{$ENDIF}

// Print a debug message in libSoX.
{$IFDEF ANDROID}lsx_debug_impl:{$ENDIF} procedure {$IFNDEF ANDROID}lsx_debug_impl{$ENDIF}(
  // Printf-style format string.
  const fmt: MarshaledAString); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_debug_impl';{$ENDIF}

// Looks up an enumeration by name in an array of lsx_enum_items.
// Returns the corresponding item, or null if not found.
{$IFDEF ANDROID}lsx_find_enum_text:{$ENDIF} function {$IFNDEF ANDROID}lsx_find_enum_text{$ENDIF}(
  // Name of enumeration to find.
  const text: MarshaledAString;
  // Array of items to search, with text == NULL for last item.
  lsx_enum_items: plsx_enum_item;
  // Search flags: 0 (case-insensitive) or lsx_find_enum_item_case_sensitive (case-sensitive).
  flags: Integer): plsx_enum_item; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_find_enum_text';{$ENDIF}

// Looks up an enumeration by value in an array of lsx_enum_items.
// Returns the corresponding item, or null if not found.
{$IFDEF ANDROID}lsx_find_enum_value:{$ENDIF} function {$IFNDEF ANDROID}lsx_find_enum_value{$ENDIF}(
  // Enumeration value to find.
  value: LongInt;
  // Array of items to search, with text == NULL for last item.
  lsx_enum_items: plsx_enum_item): plsx_enum_item; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_find_enum_value';{$ENDIF}

// Looks up a command-line argument in a set of enumeration names, showing an
// error message if the argument is not found in the set of names.
// Returns The enumeration value corresponding to the matching enumeration, or
// INT_MAX if the argument does not match any enumeration name.
{$IFDEF ANDROID}lsx_enum_option:{$ENDIF} function {$IFNDEF ANDROID}lsx_enum_option{$ENDIF}(
  // Option character to which arg is associated, for example with -a, c would be 'a'.
  c: Integer;
  // Argument to find in enumeration list.
  arg: MarshaledAString;
  // Array of items to search, with text == NULL for last item.
  items: plsx_enum_item): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_enum_option';{$ENDIF}

// Determines whether the specified string ends with the specified suffix (case-sensitive).
// Returns true if the specified string ends with the specified suffix.
{$IFDEF ANDROID}lsx_strends:{$ENDIF} function {$IFNDEF ANDROID}lsx_strends{$ENDIF}(
  // String to search.
  str: MarshaledAString;
  // Suffix to search for.
  end_: MarshaledAString): sox_bool; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_strends';{$ENDIF}

// Finds the file extension for a filename.
// Returns the file extension, not including the '.', or null if filename does
// not have an extension.
{$IFDEF ANDROID}lsx_find_file_extension:{$ENDIF} function {$IFNDEF ANDROID}lsx_find_file_extension{$ENDIF}(
  // Filename to search for extension.
  pathname: MarshaledAString): MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_find_file_extension';{$ENDIF}

// Formats the specified number with up to three significant figures and adds a
// metric suffix in place of the exponent, such as 1.23G.
// Returns A static buffer with the formatted number, valid until the next time
// this function is called (note: not thread safe).
{$IFDEF ANDROID}lsx_sigfigs3:{$ENDIF} function {$IFNDEF ANDROID}lsx_sigfigs3{$ENDIF}(
  // Number to be formatted.
  number: Double): MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_sigfigs3';{$ENDIF}

// Formats the specified number as a percentage, showing up to three significant
// figures.
// Returns A static buffer with the formatted number, valid until the next time
// this function is called (note: not thread safe).
{$IFDEF ANDROID}lsx_sigfigs3p:{$ENDIF} function {$IFNDEF ANDROID}lsx_sigfigs3p{$ENDIF}(
  // Number to be formatted.
  percentage: Double): MarshaledAString; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_sigfigs3p';{$ENDIF}

// Allocates, deallocates, or resizes; like C's realloc, except that this version
// terminates the running application if unable to allocate the requested memory.
// Returns New buffer, or null if buffer was freed.
{$IFDEF ANDROID}lsx_realloc:{$ENDIF} function {$IFNDEF ANDROID}lsx_realloc{$ENDIF}(
  // Pointer to be freed or resized, or null if allocating a new buffer.
  ptr: Pointer;
  // New size for buffer, or 0 to free the buffer.
  newsize: size_t): Pointer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_realloc';{$ENDIF}

// Like strcmp, except that the characters are compared without regard to case.
// Returns 0 (s1 == s2), negative (s1 < s2), or positive (s1 > s2).
{$IFDEF ANDROID}lsx_strcasecmp:{$ENDIF} function {$IFNDEF ANDROID}lsx_strcasecmp{$ENDIF}(
  // First string.
  s1: MarshaledAString;
  // Second string.
  s2: MarshaledAString): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_strcasecmp';{$ENDIF}

// Like strncmp, except that the characters are compared without regard to case.
// Returns 0 (s1 == s2), negative (s1 < s2), or positive (s1 > s2).
{$IFDEF ANDROID}lsx_strncasecmp:{$ENDIF} function {$IFNDEF ANDROID}lsx_strncasecmp{$ENDIF}(
  // First string.
  s1: MarshaledAString;
  // Second string.
  s2: MarshaledAString;
  // Maximum number of characters to examine.
  n: size_t): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_strncasecmp';{$ENDIF}

// Initializes an lsx_getopt_t structure for use with lsx_getopt.
{$IFDEF ANDROID}lsx_getopt_init:{$ENDIF} procedure {$IFNDEF ANDROID}lsx_getopt_init{$ENDIF}(
  // Number of arguments in argv
  argc: Integer;
  // Array of arguments
  argv: array of MarshaledAString;
  // Short options, for example ":abc:def::ghi" (+/- not supported)
  shortopts: MarshaledAString;
  // Array of long option descriptors
  longopts: plsx_option_t;
  // Flags for longonly and opterr
  flags: lsx_getopt_flags_t;
  // First argv to check (usually 1)
  first: Integer;
  // State object to be initialized
  state: plsx_getopt_t); {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_getopt_init';{$ENDIF}

// Gets the next option. Options are parameters that start with "-" or "--".
// If no more options, returns -1. If unrecognized short option, returns '?'.
// If a recognized short option is missing a required argument,
// return (shortopts[0]==':' ? ':' : '?'). If successfully recognized short
// option, return the recognized character. If successfully recognized long
// option, returns (option.flag ? 0 : option.val).
// Note: lsx_getopt does not permute the non-option arguments.
// Returns option character (short), val or 0 (long), or -1 (no more).
{$IFDEF ANDROID}lsx_getopt:{$ENDIF} function {$IFNDEF ANDROID}lsx_getopt{$ENDIF}(
  // The getopt state pointer.
  state: plsx_getopt_t): Integer; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_getopt';{$ENDIF}

// Gets the file length, or 0 if the file is not seekable/normal.
// Returns The file length, or 0 if the file is not seekable/normal.
{$IFDEF ANDROID}lsx_filelength:{$ENDIF} function {$IFNDEF ANDROID}lsx_filelength{$ENDIF}(
    ft: psox_format_t): sox_uint64_t; {$IFDEF CDECL}cdecl;{$ENDIF} {$IFNDEF ANDROID}{$IFDEF STDCALL}stdcall;{$ENDIF}external libsox name _PU + 'lsx_filelength';{$ENDIF}

implementation

end.
