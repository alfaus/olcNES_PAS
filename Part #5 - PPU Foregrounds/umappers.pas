unit uMappers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMapper }

  TMapper = class
  protected
    FPRGBanks: Byte;
    FCHRBanks: Byte;
  public
    constructor Create(prgBanks, chrBanks: Byte);
    function cpuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean; virtual; abstract;
    function cpuMapWrite(addr: Word; var mapped_addr: Cardinal): Boolean; virtual; abstract;
    function ppuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean; virtual; abstract;
    function ppuMapWrite(addr: Word; var mapped_addr: Cardinal): Boolean; virtual; abstract;

    procedure Reset; virtual; abstract;
  end;

  { TMapper_000 }

  TMapper_000 = class(TMapper)
  public
    constructor Create(prgBanks, chrBanks: Byte);
    function cpuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean;
      override;
    function cpuMapWrite(addr: Word; var mapped_addr: Cardinal): Boolean;
      override;
    function ppuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean;
      override;
    function ppuMapWrite(addr: Word; var mapped_addr: Cardinal): Boolean;
      override;
    procedure Reset; override;
  end;

implementation

{ TMapper_000 }

constructor TMapper_000.Create(prgBanks, chrBanks: Byte);
begin
  inherited Create(prgBanks, chrBanks);
end;

function TMapper_000.cpuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean;
begin
  Result := False;

  if (addr >= $8000) and (addr <= $FFFF) then
  begin
    if FPRGBanks > 1 then
      mapped_addr := addr and $7FFF
    else
      mapped_addr := addr and $3FFF;

    Result := True;
  end;
end;

function TMapper_000.cpuMapWrite(addr: Word; var mapped_addr: Cardinal
  ): Boolean;
begin
  Result := False;

  if (addr >= $8000) and (addr <= $FFFF) then
  begin
    if FPRGBanks > 1 then
      mapped_addr := addr and $7FFF
    else
      mapped_addr := addr and $3FFF;
    Result := True;
  end;
end;

function TMapper_000.ppuMapRead(addr: Word; var mapped_addr: Cardinal): Boolean;
begin
  Result := False;

  if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
    mapped_addr := addr;
    Result := True;
  end;
end;

function TMapper_000.ppuMapWrite(addr: Word; var mapped_addr: Cardinal
  ): Boolean;
begin
  Result := False;

  if {(addr >= $0000) and} (addr <= $1FFF) then
  begin
    if FCHRBanks = 0 then
    begin
      mapped_addr := addr;
      Result := True;
    end;
  end;
end;

procedure TMapper_000.Reset;
begin

end;

{ TMapper }

constructor TMapper.Create(prgBanks, chrBanks: Byte);
begin
  FPRGBanks := prgBanks;
  FCHRBanks := chrBanks;
end;

end.

