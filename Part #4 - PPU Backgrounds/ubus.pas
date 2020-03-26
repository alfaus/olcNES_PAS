unit uBUS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, u6502, u2C02, uCartridge;

type

  { TBus }

  TBus = class
  private
    // A count of how many clocks have passed
    nSystemClockCounter: Cardinal;
    controller_state: array [0..1] of Byte;
  public
    cpu: Tolc6502;

    // The 2C02 Picture Processing Unit
    ppu: Tolc2C02;

    // The Cartridge or "GamePak"
    cart: TCartridge;

    cpuRam: TBytes;
    controller: array [0..1] of Byte;
    constructor Create;
    destructor Destroy; override;
    // Bus Read & Write
    procedure cpuWrite(addr: Word; data: Byte);
    function cpuRead(addr: Word; bReadOnly: Boolean = False): Byte;
    // System Interface
    procedure InsertCartridge(cartridge: TCartridge);
    procedure Reset;
    procedure Clock;
  end;

implementation

{ TBus }

constructor TBus.Create;
begin
  cart := Nil;
  SetLength(cpuRam, 2048);
  cpu := Tolc6502.Create;
  cpu.ConnectBus(Self);
  ppu := Tolc2C02.Create;
  FillByte(cpuRam[0], Length(cpuRam), 255);
end;

destructor TBus.Destroy;
begin
  inherited Destroy;
end;

procedure TBus.cpuWrite(addr: Word; data: Byte);
begin
  if cart.cpuWrite(addr, data) then
  begin

  end else if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
    cpuRam[addr and $07FF] := data;
  end else if (addr >= $2000) and (addr <= $3FFF) then
  begin
    ppu.cpuWrite(addr and $0007, data);
  end else if (addr >= $4016) and (addr <= $4017) then
  begin
    controller_state[addr and $0001] := controller[addr and $0001];
  end;
end;

function TBus.cpuRead(addr: Word; bReadOnly: Boolean): Byte;
begin
  Result := $00;
  if cart.cpuRead(addr, Result) then
  begin
    // Cartridge Address Range
  end else if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
     Result := cpuRam[addr and $07FF];
  end else if (addr >= $2000) and (addr <= $3FFF) then
  begin
     Result := ppu.cpuRead(addr and $0007, bReadOnly);
  end else if (addr >= $4016) and (addr <= $4017) then
  begin
     Result := Ord((controller_state[addr and $0001] and $80) > 0);
     controller_state[addr and $0001] := controller_state[addr and $0001] shl 1;
  end;
end;

procedure TBus.InsertCartridge(cartridge: TCartridge);
begin
  cart := cartridge;
  ppu.ConnectCartridge(cartridge);
end;

procedure TBus.Reset;
begin
  cart.Reset;
  cpu.Reset;
  ppu.Reset;
  nSystemClockCounter := 0;
end;

procedure TBus.Clock;
begin
  ppu.Clock;
  if nSystemClockCounter mod 3 = 0 then cpu.Clock;

  if ppu.nmi then
  begin
     ppu.nmi := False;
     cpu.Nmi;
  end;
  Inc(nSystemClockCounter);
end;

end.

