unit uBUS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, u6502;

type

  { TBus }

  TBus = class
  public
    ram: TBytes;
    cpu: T6502;
    constructor Create;
    destructor Destroy; override;
    // Bus Read & Write
    procedure WriteByte(addr: Word; data: Byte);
    function ReadByte(addr: Word; bReadOnly: Boolean = False): Byte;
  end;

implementation

{ TBus }

constructor TBus.Create;
begin
  SetLength(ram, 64 * 1024);
  cpu := T6502.Create;
  cpu.ConnectBus(Self);
  FillByte(ram[0], Length(ram), 0);
end;

destructor TBus.Destroy;
begin
  inherited Destroy;
end;

procedure TBus.WriteByte(addr: Word; data: Byte);
begin
  // if (addr >= $0000) and (addr <= $FFFF) then -> No es necesario, un Word no puede tener valores fuera de ese rango
  ram[addr] := data;
end;

function TBus.ReadByte(addr: Word; bReadOnly: Boolean): Byte;
begin
  Result := ram[addr]
end;

end.

