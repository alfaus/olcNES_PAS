program olcNES;
{$Mode objfpc}{$H+}

// https://github.com/OneLoneCoder/olcNES/tree/master/Part%20%234%20-%20PPU%20Backgrounds
// https://youtu.be/-THeUXqR3zY

uses u6502, uBUS, SfmlGraphics, SfmlWindow, SfmlSystem, sysutils, Classes,
  u2C02, uCartridge, uMappers;

type

  { TDemo_olc6502 }

  TDemo_olc6502 = class
  private
    FWindow: TSfmlRenderWindow;
    font: TSfmlFont;
    txt: TSfmlText;
    FScreenSprite, FPattern0Sprite, FPattern1Sprite: TSfmlSprite;
    FScreenTexture, FPattern0Texture, FPattern1Texture: TSfmlTexture;
    procedure DrawString(x, y: Integer; msg: String; color: TSfmlColor);
    procedure DrawRam(x, y: Integer; nAddr: Word; nRows, nColumns: Integer);
    procedure DrawCpu(x, y: Integer);
    procedure DrawCode(x, y, nLines: Integer);
  public
    cart: TCartridge;
    nes: TBus;
    mapAsm: TMapStrings;
    bEmulationRun: Boolean;
    fResidualTime: Single;
    nSelectedPalette: Byte;
    constructor Create(width, height: Integer);
    destructor Destroy; override;
    procedure Run;
  end;

{ TDemo_olc6502 }

procedure TDemo_olc6502.DrawString(x, y: Integer; msg: String; color: TSfmlColor
  );
begin
  txt.FillColor := color;
  txt.Position := SfmlVector2f(x, y);
  txt.&String := msg;
  FWindow.Draw(txt);
end;

procedure TDemo_olc6502.DrawRam(x, y: Integer; nAddr: Word; nRows,
  nColumns: Integer);
var
  nRamX, nRamY, row, col: Integer;
  sOffset: String;
begin
  nRamX := x; nRamY := y;
  for row:=0 to nRows - 1 do
  begin
    sOffset := '$' + hexStr(nAddr, 4) + ':';
    for col:=0 to nColumns - 1 do
    begin
      sOffset += ' ' + hexStr(nes.cpuRead(nAddr, True), 2);
      nAddr += 1;
    end;
    DrawString(nRamX, nRamY, sOffset, SfmlWhite);
    nRamY += 10;
  end;
end;

procedure TDemo_olc6502.DrawCpu(x, y: Integer);
var
  status: String;
begin
  status := 'STATUS: ';
  DrawString(x, y, status, SfmlWhite);
  if (nes.cpu.status and FLAGS6502_N) <> 0 then
    DrawString(x + 64, y, 'N', SfmlGreen)
  else
    DrawString(x + 64, y, 'N', SfmlRed);
  if (nes.cpu.status and FLAGS6502_V) <> 0 then
    DrawString(x + 80, y, 'V', SfmlGreen)
  else
    DrawString(x + 80, y, 'V', SfmlRed);
  if (nes.cpu.status and FLAGS6502_U) <> 0 then
    DrawString(x + 96, y, '-', SfmlGreen)
  else
    DrawString(x + 96, y, '-', SfmlRed);
  if (nes.cpu.status and FLAGS6502_B) <> 0 then
    DrawString(x + 112, y, 'B', SfmlGreen)
  else
    DrawString(x + 112, y, 'B', SfmlRed);
  if (nes.cpu.status and FLAGS6502_D) <> 0 then
    DrawString(x + 128, y, 'D', SfmlGreen)
  else
    DrawString(x + 128, y, 'D', SfmlRed);
  if (nes.cpu.status and FLAGS6502_I) <> 0 then
    DrawString(x + 144, y, 'I', SfmlGreen)
  else
    DrawString(x + 144, y, 'I', SfmlRed);
  if (nes.cpu.status and FLAGS6502_Z) <> 0 then
    DrawString(x + 160, y, 'Z', SfmlGreen)
  else
    DrawString(x + 160, y, 'Z', SfmlRed);
  if (nes.cpu.status and FLAGS6502_C) <> 0 then
    DrawString(x + 178, y, 'C', SfmlGreen)
  else
    DrawString(x + 178, y, 'C', SfmlRed);
  DrawString(x, y + 10, 'PC: $' + hexStr(nes.cpu.pc, 4), SfmlWhite);
  DrawString(x, y + 20, 'A: $' + hexStr(nes.cpu.a, 2) + ' [' + IntToStr(nes.cpu.a) + ']', SfmlWhite);
  DrawString(x, y + 30, 'X: $' + hexStr(nes.cpu.x, 2) + ' [' + IntToStr(nes.cpu.x) + ']', SfmlWhite);
  DrawString(x, y + 40, 'Y: $' + hexStr(nes.cpu.y, 2) + ' [' + IntToStr(nes.cpu.y) + ']', SfmlWhite);
  DrawString(x, y + 50, 'Stack P: $' + hexStr(nes.cpu.stkp, 4), SfmlWhite);
end;

procedure TDemo_olc6502.DrawCode(x, y, nLines: Integer);
var
  index, nLineY: Integer;
begin
  mapAsm.Find(nes.cpu.pc, index);
  nLineY := (nLines shr 1) * 10 + y;
  if index < mapAsm.Count then
  begin
    DrawString(x, nLineY, mapAsm.Data[index], SfmlCyan);
    while nLineY < (nLines * 10) + y do
    begin
      Inc(nLineY, 10);
      Inc(index);
      if (index < mapAsm.Count) and (index >= 0) then DrawString(x, nLineY, mapAsm.Data[index], SfmlWhite);
    end;
  end;

  mapAsm.Find(nes.cpu.pc, index);
  nLineY := (nLines shr 1) * 10 + y;
  if index < mapAsm.Count then
  begin
    while nLineY > y do
    begin
      Dec(nLineY, 10);
      Dec(index);
      if (index < mapAsm.Count) and (index >= 0) then DrawString(x, nLineY, mapAsm.Data[index], SfmlWhite);
    end;
  end;
end;

constructor TDemo_olc6502.Create(width, height: Integer);
begin
  nSelectedPalette := $00;
  FWindow := TSfmlRenderWindow.Create(SfmlVideoMode(width, height), 'olc6502 Demonstration');

  FScreenTexture := TSfmlTexture.Create(256, 240);
  FScreenSprite := TSfmlSprite.Create(FScreenTexture);
  FScreenSprite.Position := SfmlVector2f(0, 0);
  FScreenSprite.ScaleFactor := SfmlVector2f(2, 2);

  FPattern0Texture := TSfmlTexture.Create(128, 128);
  FPattern0Sprite := TSfmlSprite.Create(FPattern0Texture);
  FPattern0Sprite.Position := SfmlVector2f(516, 348);

  FPattern1Texture := TSfmlTexture.Create(128, 128);
  FPattern1Sprite := TSfmlSprite.Create(FPattern1Texture);
  FPattern1Sprite.Position := SfmlVector2f(648, 348);

  font := TSfmlFont.Create('Courier_New.ttf');
  txt := TSfmlText.Create;
  txt.Font := font.Handle;
  txt.CharacterSize := 14;

  // Load the cartridge
  cart := TCartridge.Create('smb.nes');
  if not cart.ImageValid then Exit;

  // Insert into NES
  nes := TBus.Create;
  nes.InsertCartridge(cart);

  // Extract dissassembly
  mapAsm := nes.cpu.Disassemble($0000, $FFFF);

  // Reset NES
  nes.Reset;
end;

destructor TDemo_olc6502.Destroy;
begin
  FreeAndNil(FScreenSprite);
  FreeAndNil(FScreenTexture);
  FreeAndNil(FPattern0Sprite);
  FreeAndNil(FPattern0Texture);
  FreeAndNil(FPattern1Sprite);
  FreeAndNil(FPattern1Texture);
  FreeAndNil(nes);
  FreeAndNil(cart);
  FreeAndNil(txt);
  FreeAndNil(font);
  FreeAndNil(FWindow);
  inherited Destroy;
end;

procedure TDemo_olc6502.Run;
var
  event: TSfmlEvent;
  clock: TSfmlClock;
  fElapsedTime: Single;
  step, frame: Boolean;
  nSwatchSize, p, s, y, x: Integer;
  shape: TSfmlRectangleShape;
  //sprite: TSfmlSprite;
  //spriteTex: TSfmlTexture;
  //id: Byte;
begin
  //spriteTex := TSfmlTexture.Create;
  //sprite := TSfmlSprite.Create;
  shape := TSfmlRectangleShape.Create;
  bEmulationRun := False;
  step := False;
  frame := False;
  clock := TSfmlClock.Create;
  while FWindow.IsOpen do
  begin
    fElapsedTime := clock.Restart.AsSeconds;
    FWindow.Clear(SfmlColorFromRGB(0, 0, 128));
    nes.controller[0] := $00;
    while FWindow.PollEvent(event) do
    begin
      if event.EventType = sfEvtClosed then FWindow.Close;
      if event.EventType = sfEvtKeyPressed then
        case event.Key.Code of
        sfKeyX: nes.controller[0] := nes.controller[0] or $80;
        sfKeyZ: nes.controller[0] := nes.controller[0] or $40;
        sfKeyA: nes.controller[0] := nes.controller[0] or $20;
        sfKeyS: nes.controller[0] := nes.controller[0] or $10;
        sfKeyUp: nes.controller[0] := nes.controller[0] or $08;
        sfKeyDown: nes.controller[0] := nes.controller[0] or $04;
        sfKeyLeft: nes.controller[0] := nes.controller[0] or $02;
        sfKeyRight: nes.controller[0] := nes.controller[0] or $01;

        sfKeyC: step := True;
        sfkeyF: frame := True;
        sfKeyR: nes.Reset;
        sfKeySpace: bEmulationRun := not bEmulationRun;
        sfKeyP:
          begin
            Inc(nSelectedPalette);
            nSelectedPalette := nSelectedPalette and $07;
          end;
        end;
    end;

    if bEmulationRun then
    begin
      if fResidualTime > 0.0 then
        fResidualTime -= fElapsedTime
      else
      begin
        fResidualTime += (1 / 60) - fElapsedTime;
        repeat
          nes.Clock;
        until nes.ppu.frame_complete;
        nes.ppu.frame_complete := False;
      end;
    end
    else
    begin
      if step then // Emulate code step-by-step
      begin
        step := False;
        // Clock enough times to execute a whole CPU instruction
        repeat
          nes.Clock;
        until nes.cpu.Complete;
        // CPU clock runs slower than system clock, so it may be
        // complete for additional system clock cycles. Drain
        // those out
        repeat
          nes.Clock;
        until not nes.cpu.Complete;
      end;
      if frame then // Emulate one whole frame
      begin
        frame := False;
        // Clock enough times to draw a single frame
        repeat
          nes.Clock;
        until nes.ppu.frame_complete;
        // Use residual clock cycles to complete current instruction
        repeat
          nes.Clock;
        until nes.cpu.Complete;
        nes.ppu.frame_complete := False;
      end;
    end;

    DrawCpu(516, 2);
    DrawCode(516, 72, 24);

    nSwatchSize := 6;
    for p:=0 to 7 do // for each palette
      for s:=0 to 3 do // for wach index
      begin
        shape.Position := SfmlVector2f(516 + p * (nSwatchSize * 5) + s * nSwatchSize, 340);
        shape.Size := SfmlVector2f(nSwatchSize, nSwatchSize);
        shape.FillColor := nes.ppu.GetColourFromPaletteRam(p, s);
        shape.OutlineThickness := 0;
        shape.OutlineColor := SfmlColorFromInteger(0);
        FWindow.Draw(shape);
      end;

    shape.Position := SfmlVector2f(516 + nSelectedPalette * (nSwatchSize * 5) - 1, 339);
    shape.Size := SfmlVector2f(nSwatchSize * 4, nSwatchSize);
    shape.FillColor := SfmlColorFromInteger(0);
    shape.OutlineThickness := 1;
    shape.OutlineColor := SfmlWhite;
    FWindow.Draw(shape);

    FPattern0Texture.UpdateFromImage(nes.ppu.GetPatternTable(0, nSelectedPalette), 0, 0);
    FWindow.Draw(FPattern0Sprite);
    FPattern1Texture.UpdateFromImage(nes.ppu.GetPatternTable(1, nSelectedPalette), 0, 0);
    FWindow.Draw(FPattern1Sprite);

    FScreenTexture.UpdateFromImage(nes.ppu.GetScreen, 0, 0);
    FWindow.Draw(FScreenSprite);

    //spriteTex.UpdateFromImage(nes.ppu.GetPatternTable(0, nSelectedPalette), 0, 0);
    ////sprite.SetTexture(spriteTex, True);
    //for y:=0 to 29 do
    //  for x:=0 to 31 do
    //  begin
    //    //DrawString(x * 16, y * 16, hexStr(nes.ppu.tblName[0, y * 32 + x], 2), SfmlWhite);
    //    id := nes.ppu.tblName[0, y * 32 + x];
    //    sprite.Position := SfmlVector2f(x * 16, y * 16);
    //    sprite.SetTextureRect(SfmlIntRect((id and $0F) shl 3, ((id shr 4) and $0F) shl 3, 8, 8));
    //    FWindow.Draw(sprite);
    //  end;

    FWindow.Display;
  end;
  //FreeAndNil(sprite);
  FreeAndNil(shape);
end;

var
  demo: TDemo_olc6502;

begin
  demo := TDemo_olc6502.Create(780, 480);
  demo.Run;

  FreeAndNil(demo);
end.

