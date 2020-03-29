unit u2C02;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, uCartridge, SfmlGraphics;

type

  TStatusReg = bitpacked record
    case Integer of
    0: (u1, u2, u3, u4, u5,
        sprite_overflow,
        sprite_zero_hit,
        vertical_blank: Boolean;);
    1: (reg: Byte);
  end;

  TMaskReg = bitpacked record
    case Integer of
    0: (grayscale,
        render_brackground_left,
        render_sprites_left,
        render_background,
        render_sprites,
        enhance_red,
        enhance_green,
        enhance_blue: Boolean);
    1: (reg: Byte);
  end;

  TPPUCTRLReg = bitpacked record
    case Integer of
    0: (nametable_x,
        nametable_y,
        increment_mode,
        pattern_sprite,
        pattern_background,
        sprite_size,
        slave_mode,
        enable_nmi: Boolean);
    1: (reg: Byte);
  end;

  { TLoopyRegister }

  TLoopyRegister = record
  private
    function GetBits(const AIndex: Integer): Byte; inline;
    procedure SetBits(const AIndex: Integer; AValue: Byte); inline;
  public
    reg: Word;
    property coarse_x: Byte index $0005 read GetBits write SetBits;
    property coarse_y: Byte index $0505 read GetBits write SetBits;
    property nametable_x: Byte index $0A01 read GetBits write SetBits;
    property nametable_y: Byte index $0B01 read GetBits write SetBits;
    property fine_y: Byte index $0C03 read GetBits write SetBits;
    property unused: Byte index $0F01 read GetBits write SetBits;
  end;

  TObjectAttributeEntry = record
    y: Byte;         // Y position of sprite
    id: Byte;        // ID of tile from pattern memory
    attribute: Byte; // Flags define how sprite should be rendered
    x: Byte;         // X position of sprite
  end;
  PObjectAttributeEntry = ^TObjectAttributeEntry;

  { Tolc2C02 }

  Tolc2C02 = class
  private
    // The Cartridge or "GamePak"
    cart: TCartridge;

    palScreen: array [0..$3F] of TSfmlColor;
    sprScreen: TSfmlImage;
    sprNameTable: array [0..1] of TSfmlImage;
    sprPatternTable: array [0..1] of TSfmlImage;

    scanline: Int16;
    cycle: Int16;
    status: TStatusReg;
    mask: TMaskReg;
    control: TPPUCTRLReg;
    address_latch: Byte;
    ppu_data_buffer: Byte;
    vram_addr, tram_addr: TLoopyRegister;
    fine_x: Byte;
    bg_next_tile_id: Byte;
    bg_next_tile_attrib: Byte;
    bg_next_tile_lsb: Byte;
    bg_next_tile_msb: Byte;

    bg_shifter_pattern_lo: Word;
    bg_shifter_pattern_hi: Word;
    bg_shifter_attrib_lo: Word;
    bg_shifter_attrib_hi: Word;

    OAM: array [0..63] of TObjectAttributeEntry;

    spriteScanline: array [0..7] of TObjectAttributeEntry;
    sprite_count: Byte;
    sprite_shifter_pattern_lo: array [0..7] of Byte;
    sprite_shifter_pattern_hi: array [0..7] of Byte;

    bSpriteZeroHitPossible: Boolean;
    bSpriteZeroBeingRenderer: Boolean;

  public
    tblName: array [0..1, 0..1023] of Byte;
    tblPattern: array [0..1, 0..4095] of Byte;
    tblPalette: array [0..31] of Byte;

    pOAM: PByte;
    oam_addr: Byte;

    // Debug utilities
    frame_complete: Boolean;
    nmi: Boolean;

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
    procedure Reset;
    procedure Clock;

    // Debug utilities
    function GetScreen: TSfmlImage;
    function GetNameTable(index: Byte): TSfmlImage;
    function GetPatternTable(index: Byte; palette: Byte): TSfmlImage;
    function GetColourFromPaletteRam(palette, pixel: Byte): TSfmlColor;
  end;

implementation

{ TLoopyRegister }

function TLoopyRegister.GetBits(const AIndex: Integer): Byte;
var
  BitCount, Offset, Mask: Integer;
begin
  BitCount := AIndex and $FF;
  Offset := AIndex shr 8;
  Mask := ((1 shl BitCount) - 1);
  Result := Byte((reg shr Offset) and Mask);
end;

procedure TLoopyRegister.SetBits(const AIndex: Integer; AValue: Byte);
var
  BitCount, Offset, Mask: Integer;
begin
  BitCount := AIndex and $FF;
  Offset := AIndex shr 8;
  Mask := ((1 shl BitCount) - 1);
  AValue := AValue and Mask;
  reg := (reg and (not (Mask shl Offset))) or (AValue shl Offset);
end;

{ Tolc2C02 }

constructor Tolc2C02.Create;
begin
  status.reg := $00;
  mask.reg := $00;
  control.reg := $00;
  address_latch := $00;
  scanline := 0;
  cycle := 0;
  nmi := False;
  ppu_data_buffer := $00;
  vram_addr.reg := $0000;
  tram_addr.reg := $0000;
  fine_x := $00;
  sprScreen := TSfmlImage.Create(256, 240);
  sprNameTable[0] := TSfmlImage.Create(256, 240);
  sprNameTable[1] := TSfmlImage.Create(256, 240);
  sprPatternTable[0] := TSfmlImage.Create(128, 128);
  sprPatternTable[1] := TSfmlImage.Create(128, 128);
  frame_complete := False;
  cart := Nil;
  pOAM := @OAM[0];
  oam_addr := $00;

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

  if rdonly then
  begin
    case addr of
    $0000: // Control
      Result := control.reg;
    $0001: // Mask
      Result := mask.reg;
    $0002: // Status
      Result := status.reg;
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
  end else
  begin
    case addr of
    $0000: // Control
      begin end;
    $0001: // Mask
      begin end;
    $0002: // Status
      begin
        Result := (status.reg and $E0) or (ppu_data_buffer and $1F);
        status.vertical_blank := False;
        address_latch := 0;
      end;
    $0003: // OAM Address
      begin end;
    $0004: // OAM Data
      begin
        Result := pOAM[oam_addr];
      end;
    $0005: // Scroll
      begin end;
    $0006: // PPU Address
      begin end;
    $0007: // PPU Data
      begin
        Result := ppu_data_buffer;
        ppu_data_buffer := ppuRead(vram_addr.reg);

        if vram_addr.reg >= $3F00 then Result := ppu_data_buffer;
        if control.increment_mode then
          Inc(vram_addr.reg, 32)
        else
          Inc(vram_addr.reg);
      end;
    end;
  end;
end;

procedure Tolc2C02.cpuWrite(addr: Word; data: Byte);
begin
  case addr of
  $0000: // Control
    begin
      control.reg := data;
      tram_addr.nametable_x := Ord(control.nametable_x);
      tram_addr.nametable_y := Ord(control.nametable_y);
    end;
  $0001: // Mask
    begin
      mask.reg := data;
    end;
  $0002: // Status
    begin  end;
  $0003: // OAM Address
    begin
      oam_addr := data;
    end;
  $0004: // OAM Data
    begin
      pOAM[oam_addr] := data;
    end;
  $0005: // Scroll
    begin
      if address_latch = 0 then
      begin
        fine_x := data and $07;
        tram_addr.coarse_x := data shr 3;
        address_latch := 1;
      end else
      begin
        tram_addr.fine_y := data and $07;
        tram_addr.coarse_y := data shr 3;
        address_latch := 0;
      end;
    end;
  $0006: // PPU Address
    begin
      if address_latch = 0 then
      begin
        tram_addr.reg := (Word(data and $3F) shl 8) or (tram_addr.reg and $00FF);
        address_latch := 1;
      end else
      begin
        tram_addr.reg := (tram_addr.reg and $FF00) or Word(data);
        vram_addr.reg := tram_addr.reg;
        address_latch := 0;
      end;
    end;
  $0007: // PPU Data
    begin
      ppuWrite(vram_addr.reg, data);
      if control.increment_mode then
        Inc(vram_addr.reg, 32)
      else
        Inc(vram_addr.reg);
    end;
  end;
end;

function Tolc2C02.ppuRead(addr: Word; rdonly: Boolean): Byte;
begin
  Result := $00;
  addr := addr and $3FFF;

  if cart.ppuRead(addr, Result) then
  begin

  end else if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
    Result := tblPattern[(addr and $1000) shr 12, addr and $0FFF];
  end else if (addr >= $2000) and (addr <= $3EFF) then
  begin
    addr := addr and $0FFF;
    if cart.mirror = miVertical then
    begin
      // Vertical
      if {(addr >= $0000) and} (addr <= $03FF) then
        Result := tblName[0, addr and $03FF];
      if (addr >= $0400) and (addr <= $07FF) then
        Result := tblName[1, addr and $03FF];
      if (addr >= $0800) and (addr <= $0BFF) then
        Result := tblName[0, addr and $03FF];
      if (addr >= $0C00) and (addr <= $0FFF) then
        Result := tblName[1, addr and $03FF];
    end else if cart.mirror = miHorizontal then
    begin
      // Horizontal
      if {(addr >= $0000) and} (addr <= $03FF) then
        Result := tblName[0, addr and $03FF];
      if (addr >= $0400) and (addr <= $07FF) then
        Result := tblName[0, addr and $03FF];
      if (addr >= $0800) and (addr <= $0BFF) then
        Result := tblName[1, addr and $03FF];
      if (addr >= $0C00) and (addr <= $0FFF) then
        Result := tblName[1, addr and $03FF];

    end;
  end else if (addr >= $3F00) and (addr <= $3FFF) then
  begin
    addr := addr and $001F;
    if addr = $0010 then addr := $0000;
    if addr = $0014 then addr := $0004;
    if addr = $0018 then addr := $0008;
    if addr = $001C then addr := $000C;
    if mask.grayscale then
      Result := tblPalette[addr] and $30
    else
      Result := tblPalette[addr] and $3F;
  end;

end;

procedure Tolc2C02.ppuWrite(addr: Word; data: Byte);
begin
  addr := addr and $3FFF;

  if cart.ppuWrite(addr, data) then
  begin

  end else if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
    tblPattern[(addr and $1000) shr 12, addr and $0FFF] := data;
  end else if (addr >= $2000) and (addr <= $3EFF) then
  begin
    addr := addr and $0FFF;
    if cart.mirror = miVertical then
    begin
      // Vertical
      if {(addr >= $0000) and} (addr <= $03FF) then
        tblName[0, addr and $03FF] := data;
      if (addr >= $0400) and (addr <= $07FF) then
        tblName[1, addr and $03FF] := data;
      if (addr >= $0800) and (addr <= $0BFF) then
        tblName[0, addr and $03FF] := data;
      if (addr >= $0C00) and (addr <= $0FFF) then
        tblName[1, addr and $03FF] := data;
    end else if cart.mirror = miHorizontal then
    begin
      // Horizontal
      if {(addr >= $0000) and} (addr <= $03FF) then
        tblName[0, addr and $03FF] := data;
      if (addr >= $0400) and (addr <= $07FF) then
        tblName[0, addr and $03FF] := data;
      if (addr >= $0800) and (addr <= $0BFF) then
        tblName[1, addr and $03FF] := data;
      if (addr >= $0C00) and (addr <= $0FFF) then
        tblName[1, addr and $03FF] := data;
    end;
  end else if (addr >= $3F00) and (addr <= $3FFF) then
  begin
    addr := addr and $001F;
    if addr = $0010 then addr := $0000;
    if addr = $0014 then addr := $0004;
    if addr = $0018 then addr := $0008;
    if addr = $001C then addr := $000C;
    tblPalette[addr] := data;
  end;
end;

procedure Tolc2C02.ConnectCartridge(cartridge: TCartridge);
begin
  cart := cartridge;
end;

procedure Tolc2C02.Reset;
begin
  fine_x := $00;
  address_latch := $00;
	ppu_data_buffer := $00;
	scanline := 0;
	cycle := 0;
	bg_next_tile_id := $00;
	bg_next_tile_attrib := $00;
	bg_next_tile_lsb := $00;
	bg_next_tile_msb := $00;
	bg_shifter_pattern_lo := $0000;
	bg_shifter_pattern_hi := $0000;
	bg_shifter_attrib_lo := $0000;
	bg_shifter_attrib_hi := $0000;
	status.reg := $00;
	mask.reg := $00;
	control.reg := $00;
	vram_addr.reg := $0000;
	tram_addr.reg := $0000;
end;

procedure Tolc2C02.Clock;

  procedure IncrementScrollX;
  begin
    if mask.render_background or mask.render_sprites then
    begin
      // A single name table is 32x30 tiles. As we increment horizontally
			// we may cross into a neighbouring nametable, or wrap around to
			// a neighbouring nametable
      if vram_addr.coarse_x = 31 then
      begin
        // Leaving nametable so wrap address round
				vram_addr.coarse_x := 0;
				// Flip target nametable bit
				vram_addr.nametable_x := not vram_addr.nametable_x;
      end else
      begin
        // Staying in current nametable, so just increment
        vram_addr.coarse_x := vram_addr.coarse_x + 1;
      end;
    end;
  end;

  procedure IncrementScrollY;
  begin
    if mask.render_background or mask.render_sprites then
    begin
      // If possible, just increment the fine y offset
      if vram_addr.fine_y < 7 then
        vram_addr.fine_y := vram_addr.fine_y + 1
      else
      begin
        // If we have gone beyond the height of a row, we need to
				// increment the row, potentially wrapping into neighbouring
				// vertical nametables. Dont forget however, the bottom two rows
				// do not contain tile information. The coarse y offset is used
				// to identify which row of the nametable we want, and the fine
				// y offset is the specific "scanline"

        // Reset fine y offset
        vram_addr.fine_y := 0;

        // Check if we need to swap vertical nametable targets
        if vram_addr.coarse_y = 29 then
        begin
          // We do, so reset coarse y offset
          vram_addr.coarse_y := 0;
          // And flip the target nametable bit
          vram_addr.nametable_y := not vram_addr.nametable_y;
        end else if vram_addr.coarse_y = 31 then
        begin
          // In case the pointer is in the attribute memory, we
					// just wrap around the current nametable
          vram_addr.coarse_y := 0;
        end else
        begin
          // None of the above boundary/wrapping conditions apply
					// so just increment the coarse y offset
          vram_addr.coarse_y := vram_addr.coarse_y + 1;
        end;
      end;
    end;
  end;

  procedure TransferAddressX;
  begin
    // Only if rendering is enabled
    if mask.render_background or mask.render_sprites then
    begin
      vram_addr.nametable_x := tram_addr.nametable_x;
      vram_addr.coarse_x := tram_addr.coarse_x;
    end;
  end;

  procedure TransferAddressY;
  begin
    if mask.render_background or mask.render_sprites then
    begin
      vram_addr.fine_y := tram_addr.fine_y;
      vram_addr.nametable_y := tram_addr.nametable_y;
      vram_addr.coarse_y := tram_addr.coarse_y;
    end;
  end;

  procedure LoadBackgroundShifters;
  begin
    bg_shifter_pattern_lo := (bg_shifter_pattern_lo and $FF00) or bg_next_tile_lsb;
    bg_shifter_pattern_hi := (bg_shifter_pattern_hi and $FF00) or bg_next_tile_msb;
    if (bg_next_tile_attrib and %01) <> 0 then
      bg_shifter_attrib_lo := (bg_shifter_attrib_lo and $FF00) or $FF
    else
      bg_shifter_attrib_lo := (bg_shifter_attrib_lo and $FF00) or $00;
    if (bg_next_tile_attrib and %10) <> 0 then
      bg_shifter_attrib_hi := (bg_shifter_attrib_hi and $FF00) or $FF
    else
      bg_shifter_attrib_hi := (bg_shifter_attrib_hi and $FF00) or $00;
  end;

  procedure UpdateShifters;
  var
    i: Integer;
  begin
    if mask.render_background then
    begin
      bg_shifter_pattern_lo := bg_shifter_pattern_lo shl 1;
      bg_shifter_pattern_hi := bg_shifter_pattern_hi shl 1;
      bg_shifter_attrib_lo := bg_shifter_attrib_lo shl 1;
      bg_shifter_attrib_hi := bg_shifter_attrib_hi shl 1;
    end;

    if mask.render_sprites and (cycle >=1) and (cycle < 258) then
    begin
      for i:=0 to sprite_count - 1 do
      begin
        if spriteScanline[i].x > 0 then
          Dec(spriteScanline[i].x)
        else
        begin
          sprite_shifter_pattern_lo[i] := sprite_shifter_pattern_lo[i] shl 1;
          sprite_shifter_pattern_hi[i] := sprite_shifter_pattern_hi[i] shl 1;
        end;
      end;
    end;
  end;

  function flipbyte(b: Byte): Byte;
  begin
    b := ((b and $F0) shr 4) or ((b and $0F) shl 4);
    b := ((b and $CC) shr 2) or ((b and $33) shl 2);
    b := ((b and $AA) shr 1) or ((b and $55) shl 1);
    Result := b;
  end;

var
  bg_pixel, bg_palette, p0_pixel, p1_pixel, bg_pal0, bg_pal1,
    nOAMEntry, sprite_pattern_bits_lo, sprite_pattern_bits_hi,
    fg_pixel, fg_palette, fg_pixel_lo, fg_pixel_hi, pixel, palette: Byte;
  bit_mux, sprite_pattern_addr_lo, sprite_pattern_addr_hi: Word;
  diff: Int16;
  ss, i: Integer;
  fg_priority, sb: Boolean;
begin
  if (scanline >= -1) and (scanline < 240) then
  begin
    if (scanline = 0) and (cycle = 0) then
      cycle := 1;

    if (scanline = -1) and (cycle = 1) then
    begin
      status.vertical_blank := False;
      status.sprite_zero_hit := False;
      status.sprite_overflow := False;

      for i := 0 to 7 do
      begin
        sprite_shifter_pattern_lo[i] := 0;
        sprite_shifter_pattern_hi[i] := 0;
      end;
    end;

    if ((cycle >= 2) and (cycle < 258)) or ((cycle >= 321) and (cycle < 338)) then
    begin
      UpdateShifters;

      case (cycle - 1) mod 8 of
      0:
        begin
          LoadBackgroundShifters;
          bg_next_tile_id := ppuRead($2000 or (vram_addr.reg and $0FFF));
        end;
      2:
        begin
          bg_next_tile_attrib := ppuRead($23C0 or (Word(vram_addr.nametable_y) shl 11) or
                               (Word(vram_addr.nametable_x) shl 10) or
                               ((Word(vram_addr.coarse_y) shr 2) shl 3) or
                               (Word(vram_addr.coarse_x) shr 2));
          if (vram_addr.coarse_y and $02) <> 0 then bg_next_tile_attrib := bg_next_tile_attrib shr 4;
          if (vram_addr.coarse_x and $02) <> 0 then bg_next_tile_attrib := bg_next_tile_attrib shr 2;
          bg_next_tile_attrib := bg_next_tile_attrib and $03;
        end;
      4: bg_next_tile_lsb := ppuRead((Word(Ord(control.pattern_background)) shl 12) +
              (Word(bg_next_tile_id) shl 4) +
              (Word(vram_addr.fine_y) + 0));
      6: bg_next_tile_msb := ppuRead((Word(Ord(control.pattern_background)) shl 12) +
              (Word(bg_next_tile_id) shl 4) +
              (Word(vram_addr.fine_y) + 8));
      7: IncrementScrollX;
      end;
    end;

    if cycle = 256 then
    begin
      IncrementScrollY;
    end;

    if cycle = 257 then
    begin
      LoadBackgroundShifters;
      TransferAddressX;
    end;

    if (scanline = -1) and (cycle >= 280) and (cycle < 305) then
    begin
      TransferAddressY;
    end;

    if (cycle = 338) or (cycle = 340) then
      bg_next_tile_id := ppuRead($2000 or (vram_addr.reg and $0FFF));

    // Foreground Rendering ==========================
    if (cycle = 257) and (scanline >= 0) then
    begin
      FillByte(spriteScanline, 8 * SizeOf(TObjectAttributeEntry), $FF);
      sprite_count := 0;

      for i := 0 to 7 do
      begin
        sprite_shifter_pattern_lo[i] := 0;
        sprite_shifter_pattern_hi[i] := 0;
      end;
      nOAMEntry := 0;
      bSpriteZeroHitPossible := False;
      while (nOAMEntry < 64) and (sprite_count < 9) do
      begin
        diff := scanline - Int16(OAM[nOAMEntry].y);
        if control.sprite_size then ss := 16 else ss := 8;
        if (diff >= 0) and (diff < ss) then
        begin
          if sprite_count < 8 then
          begin
            // Is this sprite sprite zero?
            if nOAMEntry = 0 then
            begin
              // It is, so its possible it may trigger a
              // sprite zero hit when drawn
              bSpriteZeroHitPossible := True;
            end;

            Move(OAM[nOAMEntry], spriteScanline[sprite_count], SizeOf(TObjectAttributeEntry));
            Inc(sprite_count);
          end;
        end;
        Inc(nOAMEntry);
      end;
      status.sprite_overflow := sprite_count > 8;
    end;

    if cycle = 340 then
    begin
      for i:=0 to sprite_count - 1 do
      begin
        sb := control.sprite_size;
        //if not control.sprite_size then // This doesn't work because a bug if fpc 3.0
        if not sb then
        begin
          // 8x8 Sprite Mode - The control register determines the pattern table
          if not Boolean(spriteScanline[i].attribute and $80) then
          begin
            // Sprite is NOT flipped vertically, i.e. normal
            sprite_pattern_addr_lo := (Ord(control.pattern_sprite) shl 12) or
                                   (spriteScanline[i].id shl 4) or
                                   (scanline - spriteScanline[i].y);
          end else
          begin
            // Sprite is flipped vertically, i.e. upside down
            sprite_pattern_addr_lo := (Ord(control.pattern_sprite) shl 12) or
                                   (spriteScanline[i].id shl 4) or
                                   (7 - (scanline - spriteScanline[i].y));
          end;
        end else
        begin
          // 8x16 Sprite Mode - The sprite attribute determines the pattern table
          if (spriteScanline[i].attribute and $80) = 0 then
          begin
            // Sprite is NOT flipped vertically, i.e. normal
            if scanline - spriteScanline[i].y < 8 then
            begin
              // Reading top half tile
              sprite_pattern_addr_lo := ((spriteScanline[i].id and $01) shl 12) or
                                     ((spriteScanline[i].id and $FE) shl 4) or
                                     ((scanline - spriteScanline[i].y) and $07);
            end else
            begin
              // Reading bottom half tile
              sprite_pattern_addr_lo := ((spriteScanline[i].id and $01) shl 12) or
                                     (((spriteScanline[i].id and $FE) + 1) shl 4) or
                                     ((scanline - spriteScanline[i].y) and $07);
            end;
          end else
          begin
            // Sprite is flipped vertically, i.e. upside down
            if scanline - spriteScanline[i].y < 8 then
            begin
              // Reading top half tile
              sprite_pattern_addr_lo := ((spriteScanline[i].id and $01) shl 12) or
                                     (((spriteScanline[i].id and $FE) + 1) shl 4) or
                                     (7 - (scanline - spriteScanline[i].y) and $07);
            end else
            begin
              // Reading bottom half tile
              sprite_pattern_addr_lo := ((spriteScanline[i].id and $01) shl 12) or
                                     ((spriteScanline[i].id and $FE) shl 4) or
                                     (7 - (scanline - spriteScanline[i].y) and $07);
            end;
          end;
        end;

        sprite_pattern_addr_hi := sprite_pattern_addr_lo + 8;
        sprite_pattern_bits_lo := ppuRead(sprite_pattern_addr_lo);
        sprite_pattern_bits_hi := ppuRead(sprite_pattern_addr_hi);

        if Boolean(spriteScanline[i].attribute and $40) then
        begin
          // Flip patterns horizontaly
          sprite_pattern_bits_lo := flipbyte(sprite_pattern_bits_lo);
          sprite_pattern_bits_hi := flipbyte(sprite_pattern_bits_hi);
        end;

        sprite_shifter_pattern_lo[i] := sprite_pattern_bits_lo;
        sprite_shifter_pattern_hi[i] := sprite_pattern_bits_hi;
      end;
    end;

  end;

  if scanline = 240 then
  begin
    // Post Render Scanline - Do Nothing!
  end;
  if (scanline >= 241) and (scanline < 261) then
    if (scanline = 241) and (cycle = 1) then
    begin
      status.vertical_blank := True;
      if control.enable_nmi then nmi := True;
    end;

  bg_pixel := $00;
  bg_palette := $00;

  if mask.render_background then
  begin
    bit_mux := $8000 shr fine_x;

    p0_pixel := Ord((bg_shifter_pattern_lo and bit_mux) > 0);
    p1_pixel := Ord((bg_shifter_pattern_hi and bit_mux) > 0);
    bg_pixel := (p1_pixel shl 1) or p0_pixel;

    bg_pal0 := Ord((bg_shifter_attrib_lo and bit_mux) > 0);
    bg_pal1 := Ord((bg_shifter_attrib_hi and bit_mux) > 0);
    bg_palette := (bg_pal1 shl 1) or bg_pal0;
  end;

  fg_pixel := $00;
  fg_palette := $00;
  fg_priority := False;

  if mask.render_sprites then
  begin
    bSpriteZeroBeingRenderer := False;
    for i := 0 to sprite_count - 1 do
    begin
      if spriteScanline[i].x = 0 then
      begin
        fg_pixel_lo := Ord((sprite_shifter_pattern_lo[i] and $80) > 0);
        fg_pixel_hi := Ord((sprite_shifter_pattern_hi[i] and $80) > 0);
        fg_pixel := (fg_pixel_hi shl 1) or fg_pixel_lo;

        fg_palette := (spriteScanline[i].attribute and $03) + $04;
        fg_priority := (spriteScanline[i].attribute and $20) = 0;

        if fg_pixel <> 0 then
        begin
          if i = 0 then // Is this sprite zero?
            bSpriteZeroBeingRenderer := True;
          Break;
        end;
      end;
    end;
  end;

  pixel := $00;   // The FINAL Pixel...
  palette := $00; // The FINAL Palette...

  if (bg_pixel = 0) and (fg_pixel = 0) then
  begin
    // The background pixel is transparent
    // The foreground pixel is transparent
    // No winner, draw "background" color
    pixel := $00;
    palette := $00;
  end else if (bg_pixel = 0) and (fg_pixel > 0) then
  begin
    // The background pixel is transparent
    // The foreground pixel is visible
    // Foreground wins!
    pixel := fg_pixel;
    palette := fg_palette;
  end else if (bg_pixel > 0) and (fg_pixel = 0) then
  begin
    // The background pixel is visible
    // The foreground pixel is transparent
    // Background wins!
    pixel := bg_pixel;
    palette := bg_palette;
  end else if (bg_pixel > 0) and (fg_pixel > 0) then
  begin
    // The background pixel is visible
    // The foreground pixel is visible
    // Hmmm...
    if fg_priority then
    begin
      // Foreground cheats its way to victory!
      pixel := fg_pixel;
      palette := fg_palette;
    end else
    begin
      // Background is considered more important!
      pixel := bg_pixel;
      palette := bg_palette;
    end;

    // Sprite Zero Hit Detection
    if bSpriteZeroHitPossible and bSpriteZeroBeingRenderer then
    begin
      if mask.render_background and mask.render_sprites then
      begin
        if not (mask.render_brackground_left or mask.render_sprites_left) then
        begin
          if (cycle >= 9) and (cycle < 258) then
          begin
            status.sprite_zero_hit := True;
          end;
        end else
        begin
          if (cycle >= 1) and (cycle < 258) then
          begin
            status.sprite_zero_hit := True;
          end;
        end;
      end;
    end;
  end;


  if (cycle > 0) and (cycle <= sprScreen.Size.x) and (scanline >= 0) and (scanline < sprScreen.Size.y) then
    sprScreen.Pixel[cycle - 1, scanline] := GetColourFromPaletteRam(palette, pixel);

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

function Tolc2C02.GetPatternTable(index: Byte; palette: Byte): TSfmlImage;
var
  nTileY, nTileX, nOffset, row, col: Word;
  tile_lsb, tile_msb, pixel: Byte;
begin
  for nTileY:=0 to 15 do
    for nTileX:=0 to 15 do
    begin
      nOffset := nTileY * 256 + nTileX * 16;
      for row:=0 to 7 do
      begin
        tile_lsb := ppuRead(index * $1000 + nOffset + row + 0);
        tile_msb := ppuRead(index * $1000 + nOffset + row + 8);
        for col:=0 to 7 do
        begin
          pixel := ((tile_lsb and $01) shl 1) or (tile_msb and $01);
          tile_lsb := tile_lsb shr 1;
          tile_msb := tile_msb shr 1;

          sprPatternTable[index].Pixel[
            nTileX * 8 + (7 - col),
            nTileY * 8 + row
          ] := GetColourFromPaletteRam(palette, pixel);
        end;
      end;
    end;

  result := sprPatternTable[index];
end;

function Tolc2C02.GetColourFromPaletteRam(palette, pixel: Byte): TSfmlColor;
begin
  // This is a convenience function that takes a specified palette and pixel
	// index and returns the appropriate screen colour.
	// "0x3F00"       - Offset into PPU addressable range where palettes are stored
	// "palette << 2" - Each palette is 4 bytes in size
	// "pixel"        - Each pixel index is either 0, 1, 2 or 3
	// "& 0x3F"       - Stops us reading beyond the bounds of the palScreen array
  Result := palScreen[ppuRead($3F00 + (palette shl 2) + pixel)];

  // Note: We dont access tblPalette directly here, instead we know that ppuRead()
	// will map the address onto the seperate small RAM attached to the PPU bus.
end;

end.

