program olcNES;
{$Mode objfpc}{$H+}

// https://github.com/OneLoneCoder/olcNES/tree/master/Part%232%20-%20CPU
// https://www.youtube.com/watch?v=8XmxKPJDGU0&t=1081s

uses u6502, uBUS, SfmlGraphics, SfmlWindow, SfmlSystem, sysutils, Classes;

type

  { TDemo_olc6502 }

  TDemo_olc6502 = class
  private
    FWindow: TSfmlRenderWindow;
    FNES: TBus;
    FMapAsm: TMapStrings;
    font: TSfmlFont;
    txt: TSfmlText;
    procedure DrawString(x, y: Integer; msg: String; color: TSfmlColor);
    procedure DrawRam(x, y: Integer; nAddr: Word; nRows, nColumns: Integer);
    procedure DrawCpu(x, y: Integer);
    procedure DrawCode(x, y, nLines: Integer);
  public
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
      sOffset += ' ' + hexStr(FNES.ReadByte(nAddr, True), 2);
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
  if (FNES.cpu.status and FLAGS6502_N) <> 0 then
    DrawString(x + 64, y, 'N', SfmlGreen)
  else
    DrawString(x + 64, y, 'N', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_V) <> 0 then
    DrawString(x + 80, y, 'V', SfmlGreen)
  else
    DrawString(x + 80, y, 'V', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_U) <> 0 then
    DrawString(x + 96, y, '-', SfmlGreen)
  else
    DrawString(x + 96, y, '-', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_B) <> 0 then
    DrawString(x + 112, y, 'B', SfmlGreen)
  else
    DrawString(x + 112, y, 'B', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_D) <> 0 then
    DrawString(x + 128, y, 'D', SfmlGreen)
  else
    DrawString(x + 128, y, 'D', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_I) <> 0 then
    DrawString(x + 144, y, 'I', SfmlGreen)
  else
    DrawString(x + 144, y, 'I', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_Z) <> 0 then
    DrawString(x + 160, y, 'Z', SfmlGreen)
  else
    DrawString(x + 160, y, 'Z', SfmlRed);
  if (FNES.cpu.status and FLAGS6502_C) <> 0 then
    DrawString(x + 178, y, 'C', SfmlGreen)
  else
    DrawString(x + 178, y, 'C', SfmlRed);
  DrawString(x, y + 10, 'PC: $' + hexStr(FNES.cpu.pc, 4), SfmlWhite);
  DrawString(x, y + 20, 'A: $' + hexStr(FNES.cpu.a, 2) + ' [' + IntToStr(FNES.cpu.a) + ']', SfmlWhite);
  DrawString(x, y + 30, 'X: $' + hexStr(FNES.cpu.x, 2) + ' [' + IntToStr(FNES.cpu.x) + ']', SfmlWhite);
  DrawString(x, y + 40, 'Y: $' + hexStr(FNES.cpu.y, 2) + ' [' + IntToStr(FNES.cpu.y) + ']', SfmlWhite);
  DrawString(x, y + 50, 'Stack P: $' + hexStr(FNES.cpu.stkp, 4), SfmlWhite);
end;

procedure TDemo_olc6502.DrawCode(x, y, nLines: Integer);
var
  index, nLineY: Integer;
begin
  FMapAsm.Find(FNES.cpu.pc, index);
  nLineY := (nLines shr 1) * 10 + y;
  if index < FMapAsm.Count then
  begin
    DrawString(x, nLineY, FMapAsm.Data[index], SfmlCyan);
    while nLineY < (nLines * 10) + y do
    begin
      Inc(nLineY, 10);
      Inc(index);
      if index < FMapAsm.Count then DrawString(x, nLineY, FMapAsm.Data[index], SfmlWhite);
    end;
  end;

  FMapAsm.Find(FNES.cpu.pc, index);
  nLineY := (nLines shr 1) * 10 + y;
  if index < FMapAsm.Count then
  begin
    while nLineY > y do
    begin
      Dec(nLineY, 10);
      Dec(index);
      if index < FMapAsm.Count then DrawString(x, nLineY, FMapAsm.Data[index], SfmlWhite);
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

  font := TSfmlFont.Create('Courier_New.ttf');
  txt := TSfmlText.Create;
  txt.Font := font.Handle;
  txt.CharacterSize := 14;

  FNES := TBus.Create;

  // Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
		{
			*=$8000
			LDX #10
			STX $0000
			LDX #3
			STX $0001
			LDY $0000
			LDA #0
			CLC
			loop
			ADC $0001
			DEY
			BNE loop
			STA $0002
			NOP
			NOP
			NOP
		}
  ss := TStringList.Create;
  ss.Delimiter := ' ';
  ss.StrictDelimiter := True;
  ss.DelimitedText := 'A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA';
  nOffset := $8000;
  for b in ss do
  begin
    FNES.ram[nOffset] := StrToInt('$' + b);
    Inc(nOffset);
  end;
  FreeAndNil(ss);
  FNES.ram[$FFFC] := $00;
  FNES.ram[$FFFD] := $80;

  FMapAsm := FNES.cpu.Disassemble($0000, $FFFF);

  FNES.cpu.Reset;
end;

destructor TDemo_olc6502.Destroy;
begin
  FreeAndNil(txt);
  FreeAndNil(font);
  FreeAndNil(FWindow);
  inherited Destroy;
end;

procedure TDemo_olc6502.Run;
var
  event: TSfmlEvent;
begin
  while FWindow.IsOpen do
  begin
    FWindow.Clear(SfmlColorFromRGB(0, 0, 128));
    while FWindow.PollEvent(event) do
    begin
      if event.EventType = sfEvtClosed then FWindow.Close;
      if event.EventType = sfEvtKeyPressed then
      begin
        case event.Key.Code of
        sfKeySpace:
          repeat
            FNES.cpu.Clock;
          until FNES.cpu.Complete;
        sfKeyR: FNES.cpu.Reset;
        sfKeyI: FNES.cpu.Irq;
        sfKeyN: FNES.cpu.Nmi;
        end;
      end;
    end;

    // Draw Ram Page 0x00
    DrawRam(2, 2, $0000, 16, 16);
    DrawRam(2, 182, $8000, 16, 16);
    DrawCpu(448, 2);
    DrawCode(448, 72, 26);

    DrawString(10, 370, 'SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI', SfmlWhite);

    FWindow.Display;
  end;
end;

var
  demo: TDemo_olc6502;

begin
  demo := TDemo_olc6502.Create(680, 480);
  demo.Run;

  FreeAndNil(demo);
end.

