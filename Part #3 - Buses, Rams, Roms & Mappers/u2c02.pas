unit u2C02;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCartridge, SfmlGraphics;

type

  { Tolc2C02 }

  Tolc2C02 = class
  private
    tblName: array [0..1, 0..1023] of Byte;
    tblPalette: array [0..31] of Byte;
    tblPattern: array [0..1, 0..4095] of Byte;
    // The Cartridge or "GamePak"
    cart: TCartridge;

    palScreen: array [0..$3F] of TSfmlColor;
    sprScreen: TSfmlImage;
    sprNameTable: array [0..1] of TSfmlImage;
    sprPatternTable: array [0..1] of TSfmlImage;

    scanline: Int16;
    cycle: Int16;
  public
    // Debug utilities
    frame_complete: Boolean;

    constructor Create;
    destructor Destroy; override;
    // Communication with Main Bus
    function cpuRead(addr: Word; rdonly: Boolean = False): Byte;
    procedure cpuWrite(addr: Word; data: Byte);

    // Communication with PPU Bus
    function ppuRead(addr: Word; rdonly: Boolean = False): Byte;
    procedure ppuWrite(addr: Word; data: Byte);

    // Interface
    procedure ConnectCartridge(cartridge: TCartridge);
    procedure Clock;

    // Debug utilities
    function GetScreen: TSfmlImage;
    function GetNameTable(index: Byte): TSfmlImage;
    function GetPatternTable(index: Byte): TSfmlImage;
  end;

implementation

{ Tolc2C02 }

constructor Tolc2C02.Create;
begin
  sprScreen := TSfmlImage.Create(256, 240);
  sprNameTable[0] := TSfmlImage.Create(256, 240);
  sprNameTable[1] := TSfmlImage.Create(256, 240);
  sprPatternTable[0] := TSfmlImage.Create(128, 128);
  sprPatternTable[1] := TSfmlImage.Create(128, 128);
  frame_complete := False;
  cart := Nil;

  palScreen[$00] := SfmlColorFromRGB(84, 84, 84);
	palScreen[$01] := SfmlColorFromRGB(0, 30, 116);
	palScreen[$02] := SfmlColorFromRGB(8, 16, 144);
	palScreen[$03] := SfmlColorFromRGB(48, 0, 136);
	palScreen[$04] := SfmlColorFromRGB(68, 0, 100);
	palScreen[$05] := SfmlColorFromRGB(92, 0, 48);
	palScreen[$06] := SfmlColorFromRGB(84, 4, 0);
	palScreen[$07] := SfmlColorFromRGB(60, 24, 0);
	palScreen[$08] := SfmlColorFromRGB(32, 42, 0);
	palScreen[$09] := SfmlColorFromRGB(8, 58, 0);
	palScreen[$0A] := SfmlColorFromRGB(0, 64, 0);
	palScreen[$0B] := SfmlColorFromRGB(0, 60, 0);
	palScreen[$0C] := SfmlColorFromRGB(0, 50, 60);
	palScreen[$0D] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$0E] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$0F] := SfmlColorFromRGB(0, 0, 0);

	palScreen[$10] := SfmlColorFromRGB(152, 150, 152);
	palScreen[$11] := SfmlColorFromRGB(8, 76, 196);
	palScreen[$12] := SfmlColorFromRGB(48, 50, 236);
	palScreen[$13] := SfmlColorFromRGB(92, 30, 228);
	palScreen[$14] := SfmlColorFromRGB(136, 20, 176);
	palScreen[$15] := SfmlColorFromRGB(160, 20, 100);
	palScreen[$16] := SfmlColorFromRGB(152, 34, 32);
	palScreen[$17] := SfmlColorFromRGB(120, 60, 0);
	palScreen[$18] := SfmlColorFromRGB(84, 90, 0);
	palScreen[$19] := SfmlColorFromRGB(40, 114, 0);
	palScreen[$1A] := SfmlColorFromRGB(8, 124, 0);
	palScreen[$1B] := SfmlColorFromRGB(0, 118, 40);
	palScreen[$1C] := SfmlColorFromRGB(0, 102, 120);
	palScreen[$1D] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$1E] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$1F] := SfmlColorFromRGB(0, 0, 0);

	palScreen[$20] := SfmlColorFromRGB(236, 238, 236);
	palScreen[$21] := SfmlColorFromRGB(76, 154, 236);
	palScreen[$22] := SfmlColorFromRGB(120, 124, 236);
	palScreen[$23] := SfmlColorFromRGB(176, 98, 236);
	palScreen[$24] := SfmlColorFromRGB(228, 84, 236);
	palScreen[$25] := SfmlColorFromRGB(236, 88, 180);
	palScreen[$26] := SfmlColorFromRGB(236, 106, 100);
	palScreen[$27] := SfmlColorFromRGB(212, 136, 32);
	palScreen[$28] := SfmlColorFromRGB(160, 170, 0);
	palScreen[$29] := SfmlColorFromRGB(116, 196, 0);
	palScreen[$2A] := SfmlColorFromRGB(76, 208, 32);
	palScreen[$2B] := SfmlColorFromRGB(56, 204, 108);
	palScreen[$2C] := SfmlColorFromRGB(56, 180, 204);
	palScreen[$2D] := SfmlColorFromRGB(60, 60, 60);
	palScreen[$2E] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$2F] := SfmlColorFromRGB(0, 0, 0);

	palScreen[$30] := SfmlColorFromRGB(236, 238, 236);
	palScreen[$31] := SfmlColorFromRGB(168, 204, 236);
	palScreen[$32] := SfmlColorFromRGB(188, 188, 236);
	palScreen[$33] := SfmlColorFromRGB(212, 178, 236);
	palScreen[$34] := SfmlColorFromRGB(236, 174, 236);
	palScreen[$35] := SfmlColorFromRGB(236, 174, 212);
	palScreen[$36] := SfmlColorFromRGB(236, 180, 176);
	palScreen[$37] := SfmlColorFromRGB(228, 196, 144);
	palScreen[$38] := SfmlColorFromRGB(204, 210, 120);
	palScreen[$39] := SfmlColorFromRGB(180, 222, 120);
	palScreen[$3A] := SfmlColorFromRGB(168, 226, 144);
	palScreen[$3B] := SfmlColorFromRGB(152, 226, 180);
	palScreen[$3C] := SfmlColorFromRGB(160, 214, 228);
	palScreen[$3D] := SfmlColorFromRGB(160, 162, 160);
	palScreen[$3E] := SfmlColorFromRGB(0, 0, 0);
	palScreen[$3F] := SfmlColorFromRGB(0, 0, 0);
end;

destructor Tolc2C02.Destroy;
begin
  FreeAndNil(sprScreen);
  FreeAndNil(sprNameTable[0]);
  FreeAndNil(sprNameTable[1]);
  FreeAndNil(sprPatternTable[0]);
  FreeAndNil(sprPatternTable[1]);
  inherited Destroy;
end;

function Tolc2C02.cpuRead(addr: Word; rdonly: Boolean): Byte;
begin
  Result := $00;
  case addr of
  $0000: // Control
    begin end;
  $0001: // Mask
    begin end;
  $0002: // Status
    begin end;
  $0003: // OAM Address
    begin end;
  $0004: // OAM Data
    begin end;
  $0005: // Scroll
    begin end;
  $0006: // PPU Address
    begin end;
  $0007: // PPU Data
    begin end;
  end;
end;

procedure Tolc2C02.cpuWrite(addr: Word; data: Byte);
begin
  case addr of
  $0000: // Control
    begin end;
  $0001: // Mask
    begin end;
  $0002: // Status
    begin end;
  $0003: // OAM Address
    begin end;
  $0004: // OAM Data
    begin end;
  $0005: // Scroll
    begin end;
  $0006: // PPU Address
    begin end;
  $0007: // PPU Data
    begin end;
  end;
end;

function Tolc2C02.ppuRead(addr: Word; rdonly: Boolean): Byte;
begin
  Result := $00;
  addr := addr and $3FFF;

  if cart.ppuRead(addr, Result) then
  begin

  end;
end;

procedure Tolc2C02.ppuWrite(addr: Word; data: Byte);
begin
  addr := addr and $3FFF;

  if cart.ppuWrite(addr, data) then
  begin

  end;
end;

procedure Tolc2C02.ConnectCartridge(cartridge: TCartridge);
begin
  cart := cartridge;
end;

procedure Tolc2C02.Clock;
begin
  // Fake some noise for now
  if Random(2) <> 0 then
  begin
    if (cycle > 0) and (cycle <= sprScreen.Size.X) and (scanline >= 0) and (scanline < sprScreen.Size.Y) then
      sprScreen.Pixel[cycle - 1, scanline] := palScreen[$3F];
  end
  else
    if (cycle > 0) and (cycle < sprScreen.Size.X) and (scanline >= 0) and (scanline < sprScreen.Size.Y) then
      sprScreen.Pixel[cycle - 1, scanline] := palScreen[$30];

  // Advance renderer - it never stops, it's relentless
  Inc(cycle);
  if cycle >= 341 then
  begin
    cycle := 0;
    Inc(scanline);
    if scanline >= 261 then
    begin
      scanline := -1;
      frame_complete := True;
    end;
  end;
end;

function Tolc2C02.GetScreen: TSfmlImage;
begin
  Result := sprScreen;
end;

function Tolc2C02.GetNameTable(index: Byte): TSfmlImage;
begin
  Result := sprNameTable[index];
end;

function Tolc2C02.GetPatternTable(index: Byte): TSfmlImage;
begin
  result := sprPatternTable[index];
end;

end.

