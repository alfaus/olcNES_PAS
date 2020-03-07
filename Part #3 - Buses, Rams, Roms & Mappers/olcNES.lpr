program olcNES;
{$Mode objfpc}{$H+}

// https://github.com/OneLoneCoder/olcNES/tree/master/Part%20%233%20-%20Buses%2C%20Rams%2C%20Roms%20%26%20Mappers
// https://youtu.be/xdzOvpYPmGE?t=890

uses u6502, uBUS, SfmlGraphics, SfmlWindow, SfmlSystem, sysutils, Classes,
  u2C02, uCartridge, uMappers;

type

  { TDemo_olc6502 }

  TDemo_olc6502 = class
  private
    FWindow: TSfmlRenderWindow;
    font: TSfmlFont;
    txt: TSfmlText;
    FSprite: TSfmlSprite;
    FTexture: TSfmlTexture;
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
      if index < mapAsm.Count then DrawString(x, nLineY, mapAsm.Data[index], SfmlWhite);
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
      if index < mapAsm.Count then DrawString(x, nLineY, mapAsm.Data[index], SfmlWhite);
    end;
  end;
end;

constructor TDemo_olc6502.Create(width, height: Integer);
var
  ss: TStringList;
  nOffset: Word;
  b: String;
begin
  FWindow := TSfmlRenderWindow.Create(SfmlVideoMode(width, height), 'olc6502 Demonstration');

  FTexture := TSfmlTexture.Create(256, 240);
  FSprite := TSfmlSprite.Create(FTexture);
  FSprite.Position := SfmlVector2f(0, 0);
  FSprite.ScaleFactor := SfmlVector2f(2, 2);

  font := TSfmlFont.Create('Courier_New.ttf');
  txt := TSfmlText.Create;
  txt.Font := font.Handle;
  txt.CharacterSize := 14;

  // Load the cartridge
  cart := TCartridge.Create('nestest.nes');

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
  FreeAndNil(FSprite);
  FreeAndNil(FTexture);
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
begin
  bEmulationRun := False;
  clock := TSfmlClock.Create;
  while FWindow.IsOpen do
  begin
    fElapsedTime := clock.Restart.AsSeconds;
    FWindow.Clear(SfmlColorFromRGB(0, 0, 128));
    while FWindow.PollEvent(event) do
    begin
      if event.EventType = sfEvtClosed then FWindow.Close;
      if event.EventType = sfEvtKeyPressed then
        case event.Key.Code of
        sfKeyC: step := True;
        sfkeyF: frame := True;
        sfKeyR: nes.Reset;
        sfKeySpace: bEmulationRun := not bEmulationRun;
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
    DrawCode(516, 72, 26);

    FTexture.UpdateFromImage(nes.ppu.GetScreen, 0, 0);

    FWindow.Draw(FSprite);

    FWindow.Display;
  end;
end;

var
  demo: TDemo_olc6502;

begin
  demo := TDemo_olc6502.Create(780, 480);
  demo.Run;

  FreeAndNil(demo);
end.

