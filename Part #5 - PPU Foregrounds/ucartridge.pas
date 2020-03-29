unit uCartridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMappers;

type
  TMirror = (miHorizontal, miVertical, miOnescreen_lo, miOnescreen_hi);

  { TCartridge }

  TCartridge = class
  private
    FPRGMemory: TBytes;
    FCHRMemory: TBytes;
    FImageValid: Boolean;
    FMapperID: Byte;
    FPRGBanks: Byte;
    FCHRBanks: Byte;
    FMapper: TMapper;
  public
    mirror: TMirror;
    constructor Create(aFileName: String);
    destructor Destroy; override;
    // Communication with Main Bus
    function cpuRead(addr: Word; var data: Byte): Boolean;
    function cpuWrite(addr: Word; data: Byte): boolean;

    // Communication with PPU Bus
    function ppuRead(addr: Word; var data: Byte): Boolean;
    function ppuWrite(addr: Word; data: Byte): boolean;

    procedure Reset;

    property ImageValid: Boolean read FImageValid;
  end;

implementation

{ TCartridge }

constructor TCartridge.Create(aFileName: String);
type
  THeader = packed record
    name: array[0..3] of Char;
    prg_rom_chunks: Byte;
    chr_rom_chunks: Byte;
    mapper1: Byte;
    mapper2: Byte;
    prg_ram_size: Byte;
    tv_system1: Byte;
    tv_system2: Byte;
    unused: array [0..4] of Byte;
  end;
var
  header: THeader;
  ifs: TFileStream;
  nFileType: Byte;
begin
  FMapperID := 0;
  FPRGBanks := 0;
  FCHRBanks := 0;
  FImageValid := False;

  try
    ifs := TFileStream.Create(aFileName, fmOpenRead);
    // Read file header
    FillByte(header, SizeOf(THeader), 0);
    ifs.ReadBuffer(header, SizeOf(THeader));

    if (header.mapper1 and $04) <> 0 then
      ifs.Seek(512, soCurrent);

    // Determine the Mapper ID
    FMapperID := ((header.mapper2 shr 4) shl 4) or (header.mapper1 shr 4);
    if (header.mapper1 and $01) <> 0 then mirror := miVertical else mirror := miHorizontal;

    // "Discover" File Format
    nFileType := 1;

    if nFileType = 0 then
    begin

    end;

    if nFileType = 1 then
    begin
      FPRGBanks := header.prg_rom_chunks;
      SetLength(FPRGMemory, FPRGBanks * 16384);
      ifs.ReadBuffer(FPRGMemory[0], Length(FPRGMemory));

      FCHRBanks := header.chr_rom_chunks;
      SetLength(FCHRMemory, FCHRBanks * 8192);
      ifs.ReadBuffer(FCHRMemory[0], Length(FCHRMemory));
    end;

    if nFileType = 2 then
    begin

    end;

    // Load appropriate mapper
    case FMapperID of
    0: FMapper := TMapper_000.Create(FPRGBanks, FCHRBanks);
    end;

    FImageValid := True;
  finally
    FreeAndNil(ifs);
  end;

end;

destructor TCartridge.Destroy;
begin
  FreeAndNil(FMapper);
  inherited Destroy;
end;

function TCartridge.cpuRead(addr: Word; var data: Byte): Boolean;
var
  mapped_addr: Cardinal;
begin
  mapped_addr := 0;
  if FMapper.cpuMapRead(addr, mapped_addr) then
  begin
    data := FPRGMemory[mapped_addr];
    Result := True;
  end else
    Result := False;
end;

function TCartridge.cpuWrite(addr: Word; data: Byte): boolean;
var
  mapped_addr: Cardinal;
begin
  mapped_addr := 0;
  if FMapper.cpuMapWrite(addr, mapped_addr) then
  begin
    FPRGMemory[mapped_addr] := data;
    Result := True;
  end else
    Result := False;
end;

function TCartridge.ppuRead(addr: Word; var data: Byte): Boolean;
var
  mapped_addr: Cardinal;
begin
  mapped_addr := 0;
  if FMapper.ppuMapRead(addr, mapped_addr) then
  begin
    data := FCHRMemory[mapped_addr];
    Result := True;
  end else
    Result := False;
end;

function TCartridge.ppuWrite(addr: Word; data: Byte): boolean;
var
  mapped_addr: Cardinal;
begin
  mapped_addr := 0;
  if FMapper.ppuMapWrite(addr, mapped_addr) then
  begin
    FCHRMemory[mapped_addr] := data;
    Result := True;
  end else
    Result := False;
end;

procedure TCartridge.Reset;
begin
  if Assigned(FMapper) then
    FMapper.Reset;
end;

end.

