{*************************************************************************************************
*  ZLibEx.pas                                                                                    *
*                                                                                                *
*  copyright (c) 2000-2013 base2 technologies                                                    *
*  copyright (c) 1995-2002 Borland Software Corporation                                          *
*                                                                                                *
*  revision history                                                                              *
*    2012.05.21  updated for win64 (delphi xe2)                                                  *
*                added NativeInt type for delphi 2007-                                           *
*                added NativeUInt type for delphi 2007-                                          *
*    2011.07.21  fixed routines to validate size before calling Move                             *
*    2010.07.01  hide overloaded Z*String* routines for delphi 5                                 *
*    2010.05.02  added ZDelfateEx and ZInflateEx                                                 *
*    2010.04.20  added TZ*Buffer classes                                                         *
*    2010.04.15  moved core zlib routines to separate unit (ZLibExApi.pas)                       *
*                added ZDeflate* and ZInflate*                                                   *
*    2010.04.14  fixed ZInternalCompress loops                                                   *
*                fixed ZInternalDecompress loops                                                 *
*                updated ZInternalCompressStream loops                                           *
*                updated ZInternalDecompressStream loops                                         *
*    2010.01.27  updated for delphi 2010                                                         *
*    2009.04.14  added overloaded string routines for AnsiString and UnicodeString               *
*    2009.01.28  updated for delphi 2009 String (UnicodeString)                                  *
*    2008.05.15  added TStreamPos type for Stream.Position variants                              *
*                added TCustomZStream.Stream* methods                                            *
*    2007.08.17  modified TZCompressionStream.Write to use Write instead of WriteBuffer          *
*    2007.03.15  moved gzip routines to separate unit (ZLibExGZ.pas)                             *
*    2006.10.07  fixed EZLibError constructor for c++ builder compatibility                      *
*    2006.03.28  moved Z_DEFLATED to interface section                                           *
*                added custom compression levels zcLevel1 thru zcLevel9                          *
*    2006.03.27  added ZCompressStreamWeb                                                        *
*    2006.03.24  added ZAdler32 and ZCrc32                                                       *
*    2005.11.29  changed FStreamPos to Int64 for delphi 6+                                       *
*    2005.03.04  modified ZInternalCompressStream loops                                          *
*                modified ZInternalDecompressStream loops                                        *
*    2005.02.07  fixed ZInternalCompressStream loop conditions                                   *
*                fixed ZInternalDecompressStream loop conditions                                 *
*    2005.01.11  added ZCompressStrWeb                                                           *
*    2003.04.14  added ZCompress2 and ZDecompress2                                               *
*                added ZCompressStr2 and ZDecompressStr2                                         *
*                added ZCompressStream2 and ZDecompressStream2                                   *
*                added overloaded T*Stream constructors to support InflateInit2                  *
*                  and DeflateInit2                                                              *
*                fixed ZDecompressStream to use ZDecompressCheck instead of ZCompressCheck       *
*    2001.11.27  enhanced TZDecompressionStream.Read to adjust source stream position upon end   *
*                  of compression data                                                           *
*                fixed endless loop in TZDecompressionStream.Read when destination count was     *
*                  greater than uncompressed data                                                *
*    2001.10.26  renamed unit to integrate "nicely" with delphi 6                                *
*    2000.11.24  added soFromEnd condition to TZDecompressionStream.Seek                         *
*                added ZCompressStream and ZDecompressStream                                     *
*    2000.06.13  optimized, fixed, rewrote, and enhanced the zlib.pas unit included on the       *
*                  delphi cd (zlib version 1.1.3)                                                *
*                                                                                                *
*  acknowledgments                                                                               *
*    erik turner                                                                                 *
*      2001.10.26  Z*Stream routines                                                             *
*                                                                                                *
*    david bennion                                                                               *
*      2001.11.27  finding the nasty little endless loop quirk with the                          *
*                    TZDecompressionStream.Read method                                           *
*                                                                                                *
*    luigi sandon                                                                                *
*      2005.02.07  pointing out the missing loop condition (Z_STREAM_END) in                     *
*                    ZInternalCompressStream and ZInternalDecompressStream                       *
*                                                                                                *
*    ferry van genderen                                                                          *
*      2005.03.04  assisting me fine tune and beta test ZInternalCompressStream and              *
*                    ZInternalDecompressStream                                                   *
*                                                                                                *
*    j. rathlev                                                                                  *
*      2005.11.28  pointing out the FStreamPos and TStream.Position type inconsistency           *
*                                                                                                *
*    anders johansen                                                                             *
*      2006.10.07  pointing out the ELibError constructor incompatibility with c++ builder       *
*                                                                                                *
*    marcin szafranski                                                                           *
*      2009.01.28  beta testing the delphi 2009 changes                                          *
*                                                                                                *
*    iztok kacin                                                                                 *
*      2009.04.14  assisting me design and further improve support for delphi 2009               *
*                                                                                                *
*    oleg matrozov                                                                               *
*      2010.04.14  pointing out the missing loop condition (avail_in > 0) in ZInternalCompress   *
*                    and ZInternalDecompress                                                     *
*      2010.04.20  prototyping and assisting with the TZ*Buffer classes                          *
*                                                                                                *
*    edward koo                                                                                  *
*      2010.07.01  pointing out the delphi 5 incompatibility with the overloaded Z*String*       *
*                    routines                                                                    *
*                                                                                                *
*    egron elbra                                                                                 *
*      2011.07.20  pointing out the range exception when moving empty strings                    *
*                                                                                                *
*    marian pascalau                                                                             *
*      2012.05.21  providing their win64 modifications                                           *
*                                                                                                *
*  donations                                                                                     *
*    2011.05.06  farshad mohajeri                                                                *
*    2012.06.07  marat safin                                                                     *
*    2012.12.14  moacir schmidt                                                                  *
*    2013.05.23  roman ganz                                                                      *
*************************************************************************************************}

unit ZLibEx;

{$MODE Delphi}

interface


uses
  SysUtils, Classes, PasZLib;

const
  Z_RLE = 3;
  Z_FIXED = 4;
  Z_BLOCK = 5;
  Z_TREES = 6;
  z_errmsg: array [0..9] of string = (
    'Need dictionary',      // Z_NEED_DICT      (2)
    'Stream end',           // Z_STREAM_END     (1)
    'OK',                   // Z_OK             (0)
    'File error',           // Z_ERRNO          (-1)
    'Stream error',         // Z_STREAM_ERROR   (-2)
    'Data error',           // Z_DATA_ERROR     (-3)
    'Insufficient memory',  // Z_MEM_ERROR      (-4)
    'Buffer error',         // Z_BUF_ERROR      (-5)
    'Incompatible version', // Z_VERSION_ERROR  (-6)
    ''
    );

type

  {$ifndef UNICODE}

  rawbytestring = ansistring;

  unicodestring = WideString;
  unicodechar = widechar;

  {$else ifdef Version2010Plus}

  UnicodeChar = WideChar;

  {$endif}

  {$ifndef Version2009Plus}

  nativeint = integer;
  nativeuint = cardinal;

  {$endif}

  TStreamPos = {$ifdef Version6Plus} Int64 {$else} longint {$endif};

  TZCompressionLevel = (
    zcNone,
    zcFastest,
    zcDefault,
    zcMax,
    zcLevel1,
    zcLevel2,
    zcLevel3,
    zcLevel4,
    zcLevel5,
    zcLevel6,
    zcLevel7,
    zcLevel8,
    zcLevel9
    );

  TZStrategy = (
    zsDefault,
    zsFiltered,
    zsHuffman,
    zsRLE,
    zsFixed
    );

  TZError = (
    zeError,
    zeStreamError,
    zeDataError,
    zeMemoryError,
    zeBufferError,
    zeVersionError
    );

  TZFlush = (
    zfNoFlush,
    zfPartialFlush,
    zfSyncFlush,
    zfFullFlush,
    zfFinish,
    zfBlock,
    zfTrees
    );

const
  ZLevels: array [TZCompressionLevel] of integer = (
    Z_NO_COMPRESSION,       // zcNone
    Z_BEST_SPEED,           // zcFastest
    Z_DEFAULT_COMPRESSION,  // zcDefault
    Z_BEST_COMPRESSION,     // zcMax
    1,                      // zcLevel1
    2,                      // zcLevel2
    3,                      // zcLevel3
    4,                      // zcLevel4
    5,                      // zcLevel5
    6,                      // zcLevel6
    7,                      // zcLevel7
    8,                      // zcLevel8
    9                       // zcLevel9
    );

  ZStrategies: array [TZStrategy] of integer = (
    Z_DEFAULT_STRATEGY,     // zsDefault
    Z_FILTERED,             // zsFiltered
    Z_HUFFMAN_ONLY,         // zsHuffman
    Z_RLE,                  // zsRLE
    Z_FIXED                 // zsFixed
    );

  ZErrors: array [TZError] of integer = (
    Z_ERRNO,                // zeError
    Z_STREAM_ERROR,         // zeStreamError
    Z_DATA_ERROR,           // zeDataError
    Z_MEM_ERROR,            // zeMemoryError
    Z_BUF_ERROR,            // zeBufferError
    Z_VERSION_ERROR         // zeVersionError
    );

  ZFlushes: array [TZFlush] of integer = (
    Z_NO_FLUSH,             // zfNoFlush
    Z_PARTIAL_FLUSH,        // zfPartialFlush
    Z_SYNC_FLUSH,           // zfSyncFlush
    Z_FULL_FLUSH,           // zfFullFlush
    Z_FINISH,               // zfFinish
    Z_BLOCK,                // zfBlock
    Z_TREES                 // zfTrees
    );

type
  {** TZ*Function *******************************************************************************}

  TZReadFunction = function(param: Pointer; var buffer; size: integer): integer;

  TZWriteFunction = function(param: Pointer; const buffer; size: integer): integer;

  {** TZInformation *****************************************************************************}

  TZInformation = packed record
    CompressedFlags: longint;
    CompressedSize: TStreamPos;
    CompressedCrc: longint;
    CompressedAdler: longint;

    UncompressedFlags: longint;
    UncompressedSize: TStreamPos;
    UncompressedCrc: longint;
    UncompressedAdler: longint;
  end;

  {** TCustomZStream ****************************************************************************}

  TCustomZStream = class (TStream)
  private
    FStream: TStream;
    FStreamPos: TStreamPos;
    FOnProgress: TNotifyEvent;

    FZStream: TZStream;
    FBuffer: array [word] of byte;

    function GetStreamPosition: TStreamPos;
    procedure SetStreamPosition(Value: TStreamPos);
  protected
    constructor Create(stream: TStream);

    function StreamRead(var buffer; Count: longint): longint;
    function StreamWrite(const buffer; Count: longint): longint;
    function StreamSeek(offset: longint; origin: word): longint;

    procedure StreamReadBuffer(var buffer; Count: longint);
    procedure StreamWriteBuffer(const buffer; Count: longint);

    procedure DoProgress; dynamic;

    property StreamPosition: TStreamPos read GetStreamPosition write SetStreamPosition;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***********************************************************************}

  TZCompressionStream = class (TCustomZStream)
  private
    function GetCompressionRate: single;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel = zcDefault); overload;

    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy); overload;

    destructor Destroy; override;

    function Read(var buffer; Count: longint): longint; override;
    function Write(const buffer; Count: longint): longint; override;
    function Seek(offset: longint; origin: word): longint; override;

    property CompressionRate: single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *********************************************************************}

  TZDecompressionStream = class (TCustomZStream)
  public
    constructor Create(Source: TStream); overload;
    constructor Create(Source: TStream; windowBits: integer); overload;

    destructor Destroy; override;

    function Read(var buffer; Count: longint): longint; override;
    function Write(const buffer; Count: longint): longint; override;
    function Seek(offset: longint; origin: word): longint; override;

    property OnProgress;
  end;

  {** TZCustomBuffer ****************************************************************************}

  TZCustomBuffer = class (TObject)
  private
    FBuffer: Pointer;
    FBufferCapacity: integer;
    FBufferSize: integer;
  protected
    FZStream: TZStream;

    procedure BufferWrite(const buffer: Pointer; size: integer);
    procedure BufferRead(var buffer: Pointer; size: integer);

    procedure BufferCapacity(capacity: integer);

    property BufferSize: integer read FBufferSize;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Flush(flush: TZFlush); virtual;

    function Write(const buffer: Pointer; size: integer): integer; overload; virtual; abstract;

    function Write(const s: ansistring): integer; overload;

    function Read(var buffer: Pointer; size: integer): integer; overload;
    function Read(var s: ansistring): integer; overload;
  end;

  {** TZCompressionBuffer ***********************************************************************}

  TZCompressionBuffer = class (TZCustomBuffer)
  public
    constructor Create(level: TZCompressionLevel = zcDefault); overload;
    constructor Create(level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy); overload;

    destructor Destroy; override;

    procedure Clear; override;

    procedure Flush(flush: TZFlush); override;

    function Write(const buffer: Pointer; size: integer): integer; override;
  end;

  {** TZDecompressionBuffer *********************************************************************}

  TZDecompressionBuffer = class (TZCustomBuffer)
  public
    constructor Create; overload;
    constructor Create(windowBits: integer); overload;

    destructor Destroy; override;

    procedure Clear; override;

    function Write(const buffer: Pointer; size: integer): integer; override;
  end;

  {** zlib deflate routines ***********************************************************************}

function ZDeflateInit(var stream: TZStream; level: TZCompressionLevel): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZDeflateInit2(var stream: TZStream; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZDeflate(var stream: TZStream; flush: TZFlush): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZDeflateEnd(var stream: TZStream): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZDeflateReset(var stream: TZStream): integer;
{$ifdef Version2005Plus} inline; {$endif}

{** zlib inflate routines ***********************************************************************}

function ZInflateInit(var stream: TZStream): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZInflateInit2(var stream: TZStream; windowBits: integer): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZInflate(var stream: TZStream; flush: TZFlush): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZInflateEnd(var stream: TZStream): integer;
{$ifdef Version2005Plus} inline; {$endif}

function ZInflateReset(var stream: TZStream): integer;
{$ifdef Version2005Plus} inline; {$endif}

{** zlib checksum routines **********************************************************************}

function ZAdler32(adler: longint; const buffer: PChar; size: integer): longint;
{$ifdef Version2005Plus} inline; {$endif}

function ZCrc32(crc: longint; const buffer: PChar; size: integer): longint;
{$ifdef Version2005Plus} inline; {$endif}

{** zlib custom routines ************************************************************************}

procedure ZDeflateEx(var stream: TZStream; param: Pointer; Read: TZReadFunction; Write: TZWriteFunction; flush: TZFlush);

procedure ZInflateEx(var stream: TZStream; param: Pointer; Read: TZReadFunction; Write: TZWriteFunction; flush: TZFlush);

{*************************************************************************************************
*  ZCompress                                                                                     *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer  = pointer to uncompressed data                                                    *
*    inSize    = size of inBuffer (bytes)                                                        *
*    outBuffer = pointer (unallocated)                                                           *
*    level     = compression level                                                               *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to compressed data (allocated)                                          *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; level: TZCompressionLevel = zcDefault);

{*************************************************************************************************
*  ZCompress2                                                                                    *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer   = pointer to uncompressed data                                                   *
*    inSize     = size of inBuffer (bytes)                                                       *
*    outBuffer  = pointer (unallocated)                                                          *
*    level      = compression level                                                              *
*    method     = compression method                                                             *
*    windowBits = window bits                                                                    *
*    memLevel   = memory level                                                                   *
*    strategy   = compression strategy                                                           *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to compressed data (allocated)                                          *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZCompress2(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);

{*************************************************************************************************
*  ZDecompress                                                                                   *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer    = pointer to compressed data                                                    *
*    inSize      = size of inBuffer (bytes)                                                      *
*    outBuffer   = pointer (unallocated)                                                         *
*    outEstimate = estimated size of uncompressed data (bytes)                                   *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to decompressed data (allocated)                                        *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZDecompress(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; outEstimate: integer = 0);

{*************************************************************************************************
*  ZDecompress2                                                                                  *
*                                                                                                *
*  pre-conditions                                                                                *
*    inBuffer    = pointer to compressed data                                                    *
*    inSize      = size of inBuffer (bytes)                                                      *
*    outBuffer   = pointer (unallocated)                                                         *
*    windowBits  = window bits                                                                   *
*    outEstimate = estimated size of uncompressed data (bytes)                                   *
*                                                                                                *
*  post-conditions                                                                               *
*    outBuffer = pointer to decompressed data (allocated)                                        *
*    outSize   = size of outBuffer (bytes)                                                       *
*************************************************************************************************}

procedure ZDecompress2(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; windowBits: integer; outEstimate: integer = 0);

{** string routines *****************************************************************************}

{*************************************************************************************************
*  ZCompressStr                                                                                  *
*                                                                                                *
*  pre-conditions                                                                                *
*    s     = uncompressed data string                                                            *
*    level = compression level                                                                   *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function ZCompressStr(const s: ansistring; level: TZCompressionLevel = zcDefault): rawbytestring;

procedure ZCompressString(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel = zcDefault); overload;

{$ifdef Version6Plus}
procedure ZCompressString(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel = zcDefault); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStrEx                                                                                *
*                                                                                                *
*  pre-conditions                                                                                *
*    s     = uncompressed data string                                                            *
*    level = compression level                                                                   *
*                                                                                                *
*  return                                                                                        *
*    compressed data string with 4 byte (integer) header indicating                              *
*    original uncompressed data length                                                           *
*************************************************************************************************}

function ZCompressStrEx(const s: ansistring; level: TZCompressionLevel = zcDefault): rawbytestring;

procedure ZCompressStringEx(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel = zcDefault); overload;

{$ifdef Version6Plus}
procedure ZCompressStringEx(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel = zcDefault); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStr2                                                                                 *
*                                                                                                *
*  pre-conditions                                                                                *
*    s          = uncompressed data string                                                       *
*    level      = compression level                                                              *
*    windowBits = window bits                                                                    *
*    memLevel   = memory level                                                                   *
*    strategy   = compression strategy                                                           *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function ZCompressStr2(const s: ansistring; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy): rawbytestring;

procedure ZCompressString2(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy); overload;

{$ifdef Version6Plus}
procedure ZCompressString2(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy); overload;
{$endif}

{*************************************************************************************************
*  ZCompressStrWeb                                                                               *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = uncompressed data string                                                                *
*                                                                                                *
*  return                                                                                        *
*    compressed data string                                                                      *
*************************************************************************************************}

function ZCompressStrWeb(const s: ansistring): rawbytestring;

procedure ZCompressStringWeb(var Result: rawbytestring; const s: ansistring); overload;

{$ifdef Version6Plus}
procedure  ZCompressStringWeb(var result: RawByteString;
  const s: UnicodeString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStr                                                                                *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = compressed data string                                                                  *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function ZDecompressStr(const s: rawbytestring): ansistring;

procedure ZDecompressString(var Result: ansistring; const s: rawbytestring); overload;

{$ifdef Version6Plus}
procedure ZDecompressString(var result: UnicodeString;
  const s: RawByteString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStrEx                                                                              *
*                                                                                                *
*  pre-conditions                                                                                *
*    s = compressed data string with 4 byte (integer) header indicating                          *
*        original uncompressed data length                                                       *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function ZDecompressStrEx(const s: rawbytestring): ansistring;

procedure ZDecompressStringEx(var Result: ansistring; const s: rawbytestring); overload;

{$ifdef Version6Plus}
procedure ZDecompressStringEx(var result: UnicodeString;
  const s: RawByteString); overload;
{$endif}

{*************************************************************************************************
*  ZDecompressStr2                                                                               *
*                                                                                                *
*  pre-conditions                                                                                *
*    s          = compressed data string                                                         *
*    windowBits = window bits                                                                    *
*                                                                                                *
*  return                                                                                        *
*    uncompressed data string                                                                    *
*************************************************************************************************}

function ZDecompressStr2(const s: rawbytestring; windowBits: integer): ansistring;

procedure ZDecompressString2(var Result: ansistring; const s: rawbytestring; windowBits: integer); overload;

{$ifdef Version6Plus}
procedure ZDecompressString2(var result: UnicodeString;
  const s: RawByteString; windowBits: Integer); overload;
{$endif}

{** stream routines *****************************************************************************}

procedure ZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel = zcDefault);

procedure ZCompressStream2(inStream, outStream: TStream; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);

procedure ZCompressStreamWeb(inStream, outStream: TStream);

procedure ZDecompressStream(inStream, outStream: TStream);

procedure ZDecompressStream2(inStream, outStream: TStream; windowBits: integer);

{************************************************************************************************}

type
  EZLibErrorClass = class of EZlibError;

  EZLibError = class (Exception)
  private
    FErrorCode: integer;
  public
    constructor Create(code: integer; const dummy: string = ''); overload;
    constructor Create(error: TZError; const dummy: string = ''); overload;

    property ErrorCode: integer read FErrorCode write FErrorCode;
  end;

  EZCompressionError = class (EZLibError);
  EZDecompressionError = class (EZLibError);

implementation

const
  SZInvalid = 'Invalid ZStream operation!';

  {************************************************************************************************}

function ZCompressCheck(code: integer): integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;

function ZDecompressCheck(code: integer; raiseBufferError: boolean = True): integer;
begin
  Result := code;

  if code < 0 then
  begin
    if (code <> Z_BUF_ERROR) or raiseBufferError then
    begin
      raise EZDecompressionError.Create(code);
    end;
  end;
end;

{** zlib deflate routines ***********************************************************************}

function ZDeflateInit(var stream: TZStream; level: TZCompressionLevel): integer;
begin
  Result := deflateInit_(stream, ZLevels[level], ZLIB_VERSION,
    SizeOf(TZStream));
end;

function ZDeflateInit2(var stream: TZStream; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy): integer;
begin
  Result := deflateInit2_(stream, ZLevels[level], Z_DEFLATED, windowBits,
    memLevel, ZStrategies[strategy], ZLIB_VERSION, SizeOf(TZStream));
end;

function ZDeflate(var stream: TZStream; flush: TZFlush): integer;
begin
  Result := deflate(stream, ZFlushes[flush]);
end;

function ZDeflateEnd(var stream: TZStream): integer;
begin
  Result := deflateEnd(stream);
end;

function ZDeflateReset(var stream: TZStream): integer;
begin
  Result := deflateReset(stream);
end;

{** zlib inflate routines ***********************************************************************}

function ZInflateInit(var stream: TZStream): integer;
begin
  Result := inflateInit_(stream, ZLIB_VERSION, SizeOf(TZStream));
end;

function ZInflateInit2(var stream: TZStream; windowBits: integer): integer;
begin
  Result := inflateInit2_(stream, windowBits, ZLIB_VERSION,
    SizeOf(TZStream));
end;

function ZInflate(var stream: TZStream; flush: TZFlush): integer;
begin
  Result := inflate(stream, ZFlushes[flush]);
end;

function ZInflateEnd(var stream: TZStream): integer;
begin
  Result := inflateEnd(stream);
end;

function ZInflateReset(var stream: TZStream): integer;
begin
  Result := inflateReset(stream);
end;

{** zlib checksum routines **********************************************************************}

function ZAdler32(adler: longint; const buffer: PChar; size: integer): longint;
begin
  Result := adler32(adler, buffer, size);
end;

function ZCrc32(crc: longint; const buffer: PChar; size: integer): longint;
begin
  Result := crc32(crc, buffer, size);
end;

{** zlib extended routines **********************************************************************}

procedure ZDeflateEx(var stream: TZStream; param: Pointer; Read: TZReadFunction; Write: TZWriteFunction; flush: TZFlush);
const
  bufferSize = 8192;
var
  zresult: integer;
  readBuffer: array [0..bufferSize - 1] of byte;
  writeBuffer: array [0..bufferSize - 1] of byte;
  writeSize: integer;
  flushEx: TZFlush;
begin
  if Assigned(Read) then
  begin
    stream.avail_in := Read(param, readBuffer, bufferSize);
  end
  else
    stream.avail_in := 0;

  repeat
    stream.next_in := @readBuffer;

    repeat
      stream.avail_out := bufferSize;
      stream.next_out := @writeBuffer;

      flushEx := flush;

      if (flushEx = zfFinish) and (stream.avail_in = bufferSize) then
      begin
        flushEx := zfNoFlush;
      end;

      zresult := ZCompressCheck(ZDeflate(stream, flushEx));

      writeSize := bufferSize - stream.avail_out;

      Write(param, writeBuffer, writeSize);
    until stream.avail_out > 0;

    //assert: stream.avail_in = 0

    if (zresult <> Z_STREAM_END) and Assigned(Read) then
    begin
      stream.avail_in := Read(param, readBuffer, bufferSize);
    end;
  until (stream.avail_in = 0) and (flush = flushEx);
end;

procedure ZInflateEx(var stream: TZStream; param: Pointer; Read: TZReadFunction; Write: TZWriteFunction; flush: TZFlush);
const
  bufferSize = 8192;
var
  zresult: integer;
  readBuffer: array [0..bufferSize - 1] of byte;
  writeBuffer: array [0..bufferSize - 1] of byte;
  writeSize: integer;
begin
  if Assigned(Read) then
  begin
    stream.avail_in := Read(param, readBuffer, bufferSize);
  end
  else
    stream.avail_in := 0;

  zresult := Z_OK;

  while (zresult <> Z_STREAM_END) and (stream.avail_in > 0) do
  begin
    stream.next_in := @readBuffer;

    repeat
      stream.avail_out := bufferSize;
      stream.next_out := @writeBuffer;

      zresult := ZDecompressCheck(ZInflate(stream, flush), False);

      writeSize := bufferSize - stream.avail_out;

      Write(param, writeBuffer, writeSize);
    until stream.avail_out > 0;

    if (zresult <> Z_STREAM_END) and Assigned(Read) then
    begin
      stream.avail_in := Read(param, readBuffer, bufferSize);
    end;
  end;
end;

{** private buffer routines *********************************************************************}

type
  PZBufferParam = ^TZBufferParam;

  TZBufferParam = packed record
    InBuffer: Pointer;
    InPosition: integer;
    InSize: integer;
    OutBuffer: Pointer;
    OutPosition: integer;
    OutSize: integer;
  end;

function ZBufferRead(p: Pointer; var buffer; size: integer): integer;
var
  param: PZBufferParam;
begin
  param := PZBufferParam(p);

  Result := param^.InSize - param^.InPosition;
  if Result > size then
    Result := size;

  Move(Pointer(integer(param^.InBuffer) + param^.InPosition)^, buffer, Result);

  Inc(param^.InPosition, Result);
end;

function ZBufferWrite(p: Pointer; const buffer; size: integer): integer;
var
  param: PZBufferParam;
begin
  param := PZBufferParam(p);

  if param^.OutPosition + size > param^.OutSize then
  begin
    param^.OutSize := param^.OutPosition + size;

    ReallocMem(Pointer(param^.OutBuffer), param^.OutSize);
  end;

  Move(buffer, Pointer(integer(param^.OutBuffer) + param^.OutPosition)^, size);

  Inc(param^.OutPosition, size);

  Result := size;
end;

procedure ZInternalCompressEx(var zstream: TZStream; const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer);
var
  param: TZBufferParam;
begin
  FillChar(param, SizeOf(TZBufferParam), 0);

  outBuffer := nil;
  outSize := 0;

  param.InBuffer := inBuffer;
  param.InSize := inSize;

  try
    ZDeflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfFinish);

    ZCompressCheck(ZDeflateEnd(zstream));

    outBuffer := param.OutBuffer;
    outSize := param.OutSize;
  except
    FreeMem(param.OutBuffer);

    raise;
  end;
end;

procedure ZInternalDecompressEx(zstream: TZStream; const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; outEstimate: integer);
var
  param: TZBufferParam;
begin
  FillChar(param, SizeOf(TZBufferParam), 0);

  outBuffer := nil;
  outSize := 0;

  param.InBuffer := inBuffer;
  param.InSize := inSize;

  if outEstimate > 0 then
  begin
    GetMem(param.OutBuffer, outEstimate);

    param.OutSize := outEstimate;
  end;

  try
    ZInflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfNoFlush);

    ZDecompressCheck(ZInflateEnd(zstream));

    outBuffer := param.OutBuffer;
    outSize := param.OutSize;
  except
    FreeMem(param.OutBuffer);

    raise;
  end;
end;

procedure ZInternalCompress(var zstream: TZStream; const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer);
const
  delta = 256;
var
  zresult: integer;
begin
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;

  outBuffer := nil;

  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;

      repeat
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(nativeuint(outBuffer) + zstream.total_out);
        zstream.avail_out := nativeuint(outSize) - zstream.total_out;

        zresult := ZCompressCheck(ZDeflate(zstream, zfNoFlush));

        Inc(outSize, delta);
      until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

      while zresult <> Z_STREAM_END do
      begin
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PByte(nativeuint(outBuffer) + zstream.total_out);
        zstream.avail_out := nativeuint(outSize) - zstream.total_out;

        zresult := ZCompressCheck(ZDeflate(zstream, zfFinish));

        Inc(outSize, delta);
      end;
    finally
      ZCompressCheck(ZDeflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZInternalDecompress(zstream: TZStream; const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; outEstimate: integer);
var
  zresult: integer;
  delta: integer;
begin
  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then
    outSize := delta
  else
    outSize := outEstimate;

  outBuffer := nil;

  try
    try
      zresult := Z_OK;

      zstream.avail_in := inSize;
      zstream.next_in := inBuffer;

      while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
      begin
        repeat
          ReallocMem(outBuffer, outSize);

          zstream.next_out := PByte(nativeuint(outBuffer) + zstream.total_out);
          zstream.avail_out := nativeuint(outSize) - zstream.total_out;

          zresult := ZDecompressCheck(ZInflate(zstream, zfNoFlush), False);

          Inc(outSize, delta);
        until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);
      end;
    finally
      ZDecompressCheck(ZInflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);

    outSize := zstream.total_out;
  except
    if Assigned(outBuffer) then
      FreeMem(outBuffer);

    raise;
  end;
end;

{** buffer routines *****************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; level: TZCompressionLevel);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZCompressCheck(ZDeflateInit(zstream, level));

  ZInternalCompress(zstream, inBuffer, inSize, outBuffer, outSize);
end;

procedure ZCompress2(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZCompressCheck(ZDeflateInit2(zstream, level, windowBits, memLevel,
    strategy));

  ZInternalCompress(zstream, inBuffer, inSize, outBuffer, outSize);
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; outEstimate: integer);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZDecompressCheck(ZInflateInit(zstream));

  ZInternalDecompress(zstream, inBuffer, inSize, outBuffer, outSize,
    outEstimate);
end;

procedure ZDecompress2(const inBuffer: Pointer; inSize: integer; out outBuffer: Pointer; out outSize: integer; windowBits: integer; outEstimate: integer);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZDecompressCheck(ZInflateInit2(zstream, windowBits));

  ZInternalDecompress(zstream, inBuffer, inSize, outBuffer, outSize,
    outEstimate);
end;

{** string routines *****************************************************************************}

function ZCompressStr(const s: ansistring; level: TZCompressionLevel): rawbytestring;
begin
  ZCompressString(Result, s, level);
end;

procedure ZCompressString(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel);
var
  buffer: Pointer;
  size: integer;
begin
  ZCompress(Pointer(s), Length(s), buffer, size, level);

  SetLength(Result, size);

  if size > 0 then
  begin
    Move(buffer^, Result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressString(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size, level);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZCompressStrEx(const s: ansistring; level: TZCompressionLevel): rawbytestring;
begin
  ZCompressStringEx(Result, s, level);
end;

procedure ZCompressStringEx(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel);
var
  buffer: Pointer;
  size: integer;
begin
  ZCompress(Pointer(s), Length(s), buffer, size, level);

  SetLength(Result, size + SizeOf(integer));

  if size > 0 then
  begin
    Move(buffer^, Result[1 + SizeOf(integer)], size);
  end;

  size := Length(s);

  Move(size, Result[1], SizeOf(integer));

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressStringEx(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size, level);

  SetLength(result, size + SizeOf(Integer));

  if size > 0 then
  begin
    Move(buffer^, result[1 + SizeOf(Integer)], size);
  end;

  size := Length(s) * SizeOf(UnicodeChar);

  Move(size, result[1], SizeOf(Integer));

  FreeMem(buffer);
end;
{$endif}

function ZCompressStr2(const s: ansistring; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy): rawbytestring;
begin
  ZCompressString2(Result, s, level, windowBits, memLevel, strategy);
end;

procedure ZCompressString2(var Result: rawbytestring; const s: ansistring; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);
var
  buffer: Pointer;
  size: integer;
begin
  ZCompress2(Pointer(s), Length(s), buffer, size, level, windowBits,
    memLevel, strategy);

  SetLength(Result, size);

  if size > 0 then
  begin
    Move(buffer^, Result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZCompressString2(var result: RawByteString; const s: UnicodeString;
  level: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress2(Pointer(s), Length(s) * SizeOf(UnicodeChar), buffer, size,
    level, windowBits, memLevel, strategy);

  SetLength(result, size);

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZCompressStrWeb(const s: ansistring): rawbytestring;
begin
  ZCompressStringWeb(Result, s);
end;

procedure ZCompressStringWeb(var Result: rawbytestring; const s: ansistring);
begin
  ZCompressString2(Result, s, zcFastest, -15, 9, zsDefault);
end;

{$ifdef Version6Plus}
procedure ZCompressStringWeb(var result: RawBytestring;
  const s: UnicodeString);
begin
  ZCompressString2(result, s, zcFastest, -15, 9, zsDefault);
end;
{$endif}

function ZDecompressStr(const s: rawbytestring): ansistring;
begin
  ZDecompressString(Result, s);
end;

procedure ZDecompressString(var Result: ansistring; const s: rawbytestring);
var
  buffer: Pointer;
  size: integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);

  SetLength(Result, size);

  if size > 0 then
  begin
    Move(buffer^, Result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZDecompressString(var result: UnicodeString;
  const s: RawByteString);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);

  SetLength(result, size div SizeOf(UnicodeChar));

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

function ZDecompressStrEx(const s: rawbytestring): ansistring;
begin
  ZDecompressStringEx(Result, s);
end;

procedure ZDecompressStringEx(var Result: ansistring; const s: rawbytestring);
var
  buffer: Pointer;
  size: integer;
  Data: ansistring;
  dataSize: integer;
begin
  Move(s[1], size, SizeOf(integer));

  dataSize := Length(s) - SizeOf(integer);

  SetLength(Data, dataSize);

  if dataSize > 0 then
  begin
    Move(s[1 + SizeOf(integer)], Data[1], dataSize);

    ZDecompress(Pointer(Data), dataSize, buffer, size, size);

    SetLength(Result, size);

    if size > 0 then
    begin
      Move(buffer^, Result[1], size);
    end;

    FreeMem(buffer);
  end
  else
  begin
    SetLength(Result, 0);
  end;
end;

{$ifdef Version6Plus}
procedure ZDecompressStringEx(var result: UnicodeString;
  const s: RawByteString);
var
  buffer  : Pointer;
  size    : Integer;
  data    : AnsiString;
  dataSize: Integer;
begin
  Move(s[1], size, SizeOf(Integer));

  dataSize := Length(s) - SizeOf(Integer);

  if dataSize > 0 then
  begin
    SetLength(data, dataSize);

    Move(s[1 + SizeOf(Integer)], data[1], dataSize);

    ZDecompress(Pointer(data), dataSize, buffer, size, size);

    SetLength(result, size div SizeOf(UnicodeChar));

    if size > 0 then
    begin
      Move(buffer^, result[1], size);
    end;

    FreeMem(buffer);
  end
  else
  begin
    SetLength(result, 0);
  end;
end;
{$endif}

function ZDecompressStr2(const s: rawbytestring; windowBits: integer): ansistring;
begin
  ZDecompressString2(Result, s, windowBits);
end;

procedure ZDecompressString2(var Result: ansistring; const s: rawbytestring; windowBits: integer);
var
  buffer: Pointer;
  size: integer;
begin
  ZDecompress2(Pointer(s), Length(s), buffer, size, windowBits);

  SetLength(Result, size);

  if size > 0 then
  begin
    Move(buffer^, Result[1], size);
  end;

  FreeMem(buffer);
end;

{$ifdef Version6Plus}
procedure ZDecompressString2(var result: UnicodeString;
  const s: RawByteString; windowBits: Integer);
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress2(Pointer(s), Length(s), buffer, size, windowBits);

  SetLength(result, size div SizeOf(UnicodeChar));

  if size > 0 then
  begin
    Move(buffer^, result[1], size);
  end;

  FreeMem(buffer);
end;
{$endif}

{** private stream routines *********************************************************************}

type
  PZStreamParam = ^TZStreamParam;

  TZStreamParam = packed record
    InStream: TStream;
    OutStream: TStream;
  end;

function ZStreamRead(p: Pointer; var buffer; size: integer): integer;
var
  param: PZStreamParam;
begin
  param := PZStreamParam(p);

  Result := param^.InStream.Read(buffer, size);
end;

function ZStreamWrite(p: Pointer; const buffer; size: integer): integer;
var
  param: PZStreamParam;
begin
  param := PZStreamParam(p);

  Result := param^.OutStream.Write(buffer, size);
end;

procedure ZInternalCompressStreamEx(zstream: TZStream; inStream, outStream: TStream);
var
  param: TZStreamParam;
begin
  FillChar(param, SizeOf(TZStreamParam), 0);

  param.InStream := inStream;
  param.OutStream := outStream;

  ZDeflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfFinish);

  ZCompressCheck(ZDeflateEnd(zstream));
end;

procedure ZInternalDecompressStreamEx(zstream: TZStream; inStream, outStream: TStream);
var
  param: TZStreamParam;
begin
  FillChar(param, SizeOf(TZStreamParam), 0);

  param.InStream := inStream;
  param.OutStream := outStream;

  ZInflateEx(zstream, @param, @ZBufferRead, @ZBufferWrite, zfNoFlush);

  ZDecompressCheck(ZInflateEnd(zstream));
end;

procedure ZInternalCompressStream(zstream: TZStream; inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zresult: integer;
  inBuffer: array [0..bufferSize - 1] of byte;
  outBuffer: array [0..bufferSize - 1] of byte;
  outSize: integer;
begin
  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read(inBuffer, bufferSize);

  while zstream.avail_in > 0 do
  begin
    zstream.next_in := @inBuffer;

    repeat
      zstream.next_out := @outBuffer;
      zstream.avail_out := bufferSize;

      zresult := ZCompressCheck(ZDeflate(zstream, zfNoFlush));

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);
  end;

  while zresult <> Z_STREAM_END do
  begin
    zstream.next_out := @outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(ZDeflate(zstream, zfFinish));

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  end;

  ZCompressCheck(ZDeflateEnd(zstream));
end;

procedure ZInternalDecompressStream(zstream: TZStream; inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zresult: integer;
  inBuffer: array [0..bufferSize - 1] of byte;
  outBuffer: array [0..bufferSize - 1] of byte;
  outSize: integer;
begin
  try
    zresult := Z_OK;

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);

    while (zresult <> Z_STREAM_END) and (zstream.avail_in > 0) do
    begin
      zstream.next_in := @inBuffer;

      repeat
        zstream.next_out := @outBuffer;
        zstream.avail_out := bufferSize;

        zresult := ZDecompressCheck(ZInflate(zstream, zfNoFlush), False);

        outSize := bufferSize - zstream.avail_out;

        outStream.Write(outBuffer, outSize);
      until (zresult = Z_STREAM_END) or (zstream.avail_out > 0);

      if zstream.avail_in > 0 then
      begin
        inStream.Position := inStream.Position - zstream.avail_in;
      end;

      if zresult <> Z_STREAM_END then
      begin
        zstream.avail_in := inStream.Read(inBuffer, bufferSize);
      end;
    end;
  finally
    ZDecompressCheck(ZInflateEnd(zstream));
  end;
end;

{** stream routines *****************************************************************************}

procedure ZCompressStream(inStream, outStream: TStream; level: TZCompressionLevel);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZCompressCheck(ZDeflateInit(zstream, level));

  ZInternalCompressStream(zstream, inStream, outStream);
end;

procedure ZCompressStream2(inStream, outStream: TStream; level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZCompressCheck(ZDeflateInit2(zstream, level, windowBits, memLevel,
    strategy));

  ZInternalCompressStream(zstream, inStream, outStream);
end;

procedure ZCompressStreamWeb(inStream, outStream: TStream);
begin
  ZCompressStream2(inStream, outStream, zcFastest, -15, 9, zsDefault);
end;

procedure ZDecompressStream(inStream, outStream: TStream);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZDecompressCheck(ZInflateInit(zstream));

  ZInternalDecompressStream(zstream, inStream, outStream);
end;

procedure ZDecompressStream2(inStream, outStream: TStream; windowBits: integer);
var
  zstream: TZStream;
begin
  FillChar(zstream, SizeOf(TZStream), 0);

  ZDecompressCheck(ZInflateInit2(zstream, windowBits));

  ZInternalDecompressStream(zstream, inStream, outStream);
end;

{** TCustomZStream ******************************************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

function TCustomZStream.StreamRead(var buffer; Count: longint): longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  Result := FStream.Read(buffer, Count);

  FStreamPos := FStreamPos + Result;
end;

function TCustomZStream.StreamWrite(const buffer; Count: longint): longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  Result := FStream.Write(buffer, Count);

  FStreamPos := FStreamPos + Result;
end;

function TCustomZStream.StreamSeek(offset: longint; origin: word): longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  Result := FStream.Seek(offset, origin);

  FStreamPos := FStream.Position;
end;

procedure TCustomZStream.StreamReadBuffer(var buffer; Count: longint);
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  FStream.ReadBuffer(buffer, Count);

  FStreamPos := FStreamPos + Count;
end;

procedure TCustomZStream.StreamWriteBuffer(const buffer; Count: longint);
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  FStream.WriteBuffer(buffer, Count);

  FStreamPos := FStreamPos + Count;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

function TCustomZStream.GetStreamPosition: TStreamPos;
begin
  Result := FStream.Position;
end;

procedure TCustomZStream.SetStreamPosition(Value: TStreamPos);
begin
  FStream.Position := Value;
  FStreamPos := FStream.Position;
end;

{** TZCompressionStream *************************************************************************}

constructor TZCompressionStream.Create(dest: TStream; compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit(FZStream, compressionLevel));
end;

constructor TZCompressionStream.Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit2(FZStream, compressionLevel, windowBits,
    memLevel, strategy));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    while ZCompressCheck(ZDeflate(FZStream, zfFinish)) <> Z_STREAM_END do
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := @FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    ZDeflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; Count: longint): longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; Count: longint): longint;
var
  writeCount: longint;
begin
  Result := Count;

  FZStream.next_in := @buffer;
  FZStream.avail_in := Count;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

    if FZStream.avail_out = 0 then
    begin
      writeCount := StreamWrite(FBuffer, SizeOf(FBuffer));

      if writeCount = SizeOf(FBuffer) then
      begin
        FZStream.next_out := @FBuffer;
        FZStream.avail_out := SizeOf(FBuffer);

        DoProgress;
      end
      else
      begin
        StreamPosition := StreamPosition - writeCount;

        Result := cardinal(Count) - FZStream.avail_in;

        FZStream.avail_in := 0;
      end;
    end;
  end;
end;

function TZCompressionStream.Seek(offset: longint; origin: word): longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    Result := FZStream.total_in;
  end
  else
    raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: single;
begin
  if FZStream.total_in = 0 then
    Result := 0
  else
    Result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***********************************************************************}

constructor TZDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionStream.Create(Source: TStream; windowBits: integer);
begin
  inherited Create(Source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
end;

destructor TZDecompressionStream.Destroy;
begin
  ZInflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; Count: longint): longint;
var
  zresult: integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := Count;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := StreamRead(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        Result := cardinal(Count) - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := @FBuffer;

      DoProgress;
    end;

    zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    StreamPosition := StreamPosition - FZStream.avail_in;

    FZStream.avail_in := 0;
  end;

  Result := cardinal(Count) - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: longint): longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: longint; Origin: word): longint;
var
  buf: array [0..8191] of byte;
  i: integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(ZInflateReset(FZStream));

    FZStream.next_in := @FBuffer;
    FZStream.avail_in := 0;

    StreamPosition := 0;
  end
  else
    if ((offset >= 0) and (origin = soFromCurrent)) or
      (((cardinal(offset) - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
    begin
      if origin = soFromBeginning then
        Dec(offset, FZStream.total_out);

      if offset > 0 then
      begin
        for i := 1 to offset div SizeOf(buf) do
          ReadBuffer(buf, SizeOf(buf));
        ReadBuffer(buf, offset mod SizeOf(buf));
      end;
    end
    else
      if (offset = 0) and (origin = soFromEnd) then
      begin
        while Read(buf, SizeOf(buf)) > 0 do ;
      end
      else
        raise EZDecompressionError.Create(SZInvalid);

  Result := FZStream.total_out;
end;

{** TZCustomBuffer ******************************************************************************}

constructor TZCustomBuffer.Create;
begin
  inherited Create;

  FillChar(FZStream, SizeOf(TZStream), 0);

  FBuffer := nil;
  FBufferCapacity := 0;

  FBufferSize := 0;
end;

destructor TZCustomBuffer.Destroy;
begin
  BufferCapacity(0);

  inherited Destroy;
end;

procedure TZCustomBuffer.Clear;
begin
  BufferCapacity(0);

  FBufferSize := 0;
end;

procedure TZCustomBuffer.Flush(flush: TZFlush);
begin
  // to be implemented by descendents as needed
end;

function TZCustomBuffer.Write(const s: ansistring): integer;
begin
  Result := Write(Pointer(s), Length(s));
end;

function TZCustomBuffer.Read(var buffer: Pointer; size: integer): integer;
begin
  Result := BufferSize;
  if size < Result then
    Result := size;

  BufferRead(buffer, Result);
end;

function TZCustomBuffer.Read(var s: ansistring): integer;
begin
  SetLength(s, BufferSize);

  Result := Read(Pointer(s), Length(s));
end;

procedure TZCustomBuffer.BufferWrite(const buffer: Pointer; size: integer);
begin
  if size > 0 then
  begin
    BufferCapacity(FBufferSize + size);

    Move(buffer^, Pointer(integer(FBuffer) + FBufferSize)^, size);

    Inc(FBufferSize, size);
  end;
end;

procedure TZCustomBuffer.BufferRead(var buffer: Pointer; size: integer);
begin
  if size > 0 then
  begin
    Move(FBuffer^, buffer^, size);

    Move(Pointer(integer(FBuffer) + size)^, FBuffer^, FBufferSize - size);

    Dec(FBufferSize, size);
  end;
end;

procedure TZCustomBuffer.BufferCapacity(capacity: integer);
const
  delta = 8192; // must be a power of 2
begin
  if capacity > 0 then
  begin
    capacity := (capacity + (delta - 1)) and not (delta - 1);
  end;

  if FBufferCapacity <> capacity then
  begin
    if capacity = 0 then
      FreeMem(FBuffer)
    else
      if FBufferCapacity = 0 then
        GetMem(FBuffer, capacity)
      else
        ReallocMem(FBuffer, capacity);

    FBufferCapacity := capacity;
  end;
end;

{** TZCompressionBuffer *************************************************************************}

constructor TZCompressionBuffer.Create(level: TZCompressionLevel);
begin
  inherited Create;

  ZCompressCheck(ZDeflateInit(FZStream, level));
end;

constructor TZCompressionBuffer.Create(level: TZCompressionLevel; windowBits, memLevel: integer; strategy: TZStrategy);
begin
  inherited Create;

  ZCompressCheck(ZDeflateInit2(FZStream, level, windowBits, memLevel,
    strategy));
end;

destructor TZCompressionBuffer.Destroy;
begin
  ZCompressCheck(ZDeflateEnd(FZStream));

  inherited Destroy;
end;

procedure TZCompressionBuffer.Clear;
begin
  inherited Clear;

  ZCompressCheck(ZDeflateReset(FZStream));
end;

procedure TZCompressionBuffer.Flush(flush: TZFlush);
const
  outSize = 32768;
var
  zresult: integer;
  outBuffer: array [0..outSize - 1] of byte;
  outCount: integer;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  repeat
    FZStream.next_out := @outBuffer;
    FZStream.avail_out := outSize;

    zresult := ZCompressCheck(ZDeflate(FZStream, flush));

    outCount := outSize - FZStream.avail_out;

    BufferWrite(@outBuffer, outCount);
  until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
end;

function TZCompressionBuffer.Write(const buffer: Pointer; size: integer): integer;
const
  outSize = 32768;
var
  zresult: integer;
  outBuffer: array [0..outSize - 1] of byte;
  outCount: integer;
begin
  zresult := Z_OK;

  FZStream.next_in := buffer;
  FZStream.avail_in := size;

  while (zresult <> Z_STREAM_END) and (FZStream.avail_in > 0) do
  begin
    repeat
      FZStream.next_out := @outBuffer;
      FZStream.avail_out := outSize;

      zresult := ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

      outCount := outSize - FZStream.avail_out;

      BufferWrite(@outBuffer, outCount);
    until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
  end;

  Result := cardinal(size) - FZStream.avail_in;
end;

{** TZDecompressionBuffer ***********************************************************************}

constructor TZDecompressionBuffer.Create;
begin
  inherited Create;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionBuffer.Create(windowBits: integer);
begin
  inherited Create;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
end;

destructor TZDecompressionBuffer.Destroy;
begin
  ZDecompressCheck(ZInflateEnd(FZStream));

  inherited Destroy;
end;

procedure TZDecompressionBuffer.Clear;
begin
  inherited Clear;

  ZDecompressCheck(ZInflateReset(FZStream));
end;

function TZDecompressionBuffer.Write(const buffer: Pointer; size: integer): integer;
const
  outSize = 32768;
var
  zresult: integer;
  outBuffer: array [0..outSize - 1] of byte;
  outCount: integer;
begin
  zresult := Z_OK;

  FZStream.next_in := buffer;
  FZStream.avail_in := size;

  while (zresult <> Z_STREAM_END) and (FZStream.avail_in > 0) do
  begin
    repeat
      FZStream.next_out := @outBuffer;
      FZStream.avail_out := outSize;

      zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush), False);

      outCount := outSize - FZStream.avail_out;

      BufferWrite(@outBuffer, outCount);
    until (zresult = Z_STREAM_END) or (FZStream.avail_out > 0);
  end;

  Result := cardinal(size) - FZStream.avail_in;
end;

{** EZLibError **********************************************************************************}

constructor EZLibError.Create(code: integer; const dummy: string);
begin
  inherited Create(z_errmsg[2 - code]);

  FErrorCode := code;
end;

constructor EZLibError.Create(error: TZError; const dummy: string);
begin
  Create(ZErrors[error], dummy);
end;

end.
