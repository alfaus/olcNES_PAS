unit u6502;

{$mode objfpc}{$H+}
//{$Define LOGMODE}

interface

uses
  Classes, SysUtils, fgl, strutils;

const
  FLAGS6502_C = 1 shl 0;  // Carry Bit
  FLAGS6502_Z = 1 shl 1;  // Zero
  FLAGS6502_I = 1 shl 2;  // Disable Interrupts
  FLAGS6502_D = 1 shl 3;  // Decimal Mode (unused)
  FLAGS6502_B = 1 shl 4;  // Break
  FLAGS6502_U = 1 shl 5;  // Unused
  FLAGS6502_V = 1 shl 6;  // Overflow
  FLAGS6502_N = 1 shl 7;  // Negative

type
  TMapStrings = specialize TFPGMap<Word, String>;
  TFunction = function(): Byte of object;
  TInstruction = record
    name: String;
    operate: TFunction;
    addrmode: TFunction;
    cycles: Byte;
  end;
  TInstructionList = array of TInstruction;

  { Tolc6502 }

  Tolc6502 = class
  private
    // Assisstive variables to facilitate emulation
    fetched: Byte;  // Represents the working input value to the ALU
    temp: Word;     // A convenience variable used everywhere
    addr_abs: Word; // All used memory addresses end up in here
    addr_rel: Word; // Represents absolute address following a branch
    opcode: Byte;   // Is the instruction byte
    cycles: Byte;   // Counts how many cycles the instruction has remaining
    clock_count: Cardinal; // A global accumulation of the number of clocks

    // Linkage to the communications bus
    bus: TObject;

    // This structure and the following vector are used to compile and store
	  // the opcode translation table. The 6502 can effectively have 256
	  // different instructions. Each of these are stored in a table in numerical
	  // order so they can be looked up easily, with no decoding required.
	  // Each table entry holds:
	  //	Pneumonic : A textual representation of the instruction (used for disassembly)
	  //	Opcode Function: A function pointer to the implementation of the opcode
	  //	Opcode Address Mode : A function pointer to the implementation of the
    //						  addressing mechanism used by the instruction
	  //	Cycle Count : An integer that represents the base number of clock cycles the
	  //				  CPU requires to perform the instruction
    lookup: TInstructionList;

    // Convenience functions to access status register
    function GetFlag(f: Byte): Boolean;
    procedure SetFlag(f: Byte; v: Boolean);

    // Linkage to the communications bus
    function ReadByte(adr: Word): Byte;
    procedure WriteByte(adr: Word; d: Byte);

    // The read location of data can come from two sources, a memory address, or
	  // its immediately available as part of the instruction. This function decides
	  // depending on address mode of instruction byte
    function Fetch: Byte;

    // Addressing Modes =============================================
	  // The 6502 has a variety of addressing modes to access data in
	  // memory, some of which are direct and some are indirect (like
	  // pointers in C++). Each opcode contains information about which
	  // addressing mode should be employed to facilitate the
	  // instruction, in regards to where it reads/writes the data it
	  // uses. The address mode changes the number of bytes that
	  // makes up the full instruction, so we implement addressing
	  // before executing the instruction, to make sure the program
	  // counter is at the correct location, the instruction is
	  // primed with the addresses it needs, and the number of clock
	  // cycles the instruction requires is calculated. These functions
	  // may adjust the number of cycles required depending upon where
	  // and how the memory is accessed, so they return the required
	  // adjustment.
    function _IMP: Byte; function _IMM: Byte;
    function _ZP0: Byte; function _ZPX: Byte;
    function _ZPY: Byte; function _REL: Byte;
    function _ABS: Byte; function _ABX: Byte;
    function _ABY: Byte; function _IND: Byte;
    function _IZX: Byte; function _IZY: Byte;

    // Opcodes ======================================================
	  // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
	  // have not modelled "unofficial" opcodes. As each opcode is
	  // defined by 1 byte, there are potentially 256 possible codes.
	  // Codes are not used in a "switch case" style on a processor,
	  // instead they are repsonisble for switching individual parts of
	  // CPU circuits on and off. The opcodes listed here are official,
	  // meaning that the functionality of the chip when provided with
	  // these codes is as the developers intended it to be. Unofficial
	  // codes will of course also influence the CPU circuitry in
	  // interesting ways, and can be exploited to gain additional
	  // functionality!
	  //
	  // These functions return 0 normally, but some are capable of
	  // requiring more clock cycles when executed under certain
	  // conditions combined with certain addressing modes. If that is
	  // the case, they return 1.
	  //
	  // I have included detailed explanations of each function in
	  // the class implementation file. Note they are listed in
	  // alphabetical order here for ease of finding.

	  function _ADC: Byte;	function _AND: Byte;	function _ASL: Byte;	function _BCC: Byte;
	  function _BCS: Byte;	function _BEQ: Byte;	function _BIT: Byte;	function _BMI: Byte;
	  function _BNE: Byte;	function _BPL: Byte;	function _BRK: Byte;	function _BVC: Byte;
	  function _BVS: Byte;	function _CLC: Byte;	function _CLD: Byte;	function _CLI: Byte;
	  function _CLV: Byte;	function _CMP: Byte;	function _CPX: Byte;	function _CPY: Byte;
	  function _DEC: Byte;	function _DEX: Byte;	function _DEY: Byte;	function _EOR: Byte;
	  function _INC: Byte;	function _INX: Byte;	function _INY: Byte;	function _JMP: Byte;
	  function _JSR: Byte;	function _LDA: Byte;	function _LDX: Byte;	function _LDY: Byte;
	  function _LSR: Byte;	function _NOP: Byte;	function _ORA: Byte;	function _PHA: Byte;
	  function _PHP: Byte;	function _PLA: Byte;	function _PLP: Byte;	function _ROL: Byte;
	  function _ROR: Byte;	function _RTI: Byte;	function _RTS: Byte;	function _SBC: Byte;
	  function _SEC: Byte;	function _SED: Byte;	function _SEI: Byte;	function _STA: Byte;
	  function _STX: Byte;	function _STY: Byte;	function _TAX: Byte;	function _TAY: Byte;
	  function _TSX: Byte;	function _TXA: Byte;	function _TXS: Byte;	function _TYA: Byte;

	  // I capture all "unofficial" opcodes with this function. It is
	  // functionally identical to a NOP
	  function _XXX: Byte;
  public
    // CPU Core registers, exposed as public here for ease of access from external
	  // examinors. This is all the 6502 has.
    a: Byte;      // Accumulator Register
    x: Byte;      // X Register
    y: Byte;      // Y Register
    stkp: Byte;   // Stack Pointer (points to location on bus)
    pc: Word;     // Program Counter
    status: Byte; // Status register

    constructor Create;
    destructor Destroy; override;

    // External event functions. In hardware these represent pins that are asserted
	  // to produce a change in state.
	  procedure Reset;	// Reset Interrupt - Forces CPU into known state
	  procedure Irq;		// Interrupt Request - Executes an instruction at a specific location
	  procedure Nmi;		// Non-Maskable Interrupt Request - As above, but cannot be disabled
	  procedure Clock;	// Perform one clock cycle's worth of update

    // Indicates the current instruction has completed by returning true. This is
	  // a utility function to enable "step-by-step" execution, without manually
	  // clocking every cycle
    function Complete: Boolean;

    // Link this CPU to a communications bus
    procedure ConnectBus(n: TObject);

    // Produces a map of strings, with keys equivalent to instruction start locations
	  // in memory, for the specified address range
    function Disassemble(nStart, nStop: Word): TMapStrings;
  end;

implementation

uses uBUS;

{$IfDef LOGMODE}
var
  logfile: Text;
{$EndIf}

function NewInstruction(name: String; op, adrm: TFunction; cycles: Byte): TInstruction;
begin
  Result.name := name;
  Result.operate := op;
  Result.addrmode := adrm;
  Result.cycles := cycles;
end;

{ Tolc6502 }

function Tolc6502.GetFlag(f: Byte): Boolean;
begin
  Result := (status and f) > 0;
end;

procedure Tolc6502.SetFlag(f: Byte; v: Boolean);
begin
  if v then
    status := status or f
  else
    status := status and (not f);
end;

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
function Tolc6502.ReadByte(adr: Word): Byte;
begin
  // In normal operation "read only" is set to false. This may seem odd. Some
	// devices on the bus may change state when they are read from, and this
	// is intentional under normal circumstances. However the disassembler will
	// want to read the data at an address without changing the state of the
	// devices on the bus
  Result := TBus(bus).cpuRead(adr, False);
end;

// Writes a byte to the bus at the specified address
procedure Tolc6502.WriteByte(adr: Word; d: Byte);
begin
  TBus(bus).cpuWrite(adr, d);
end;

function Tolc6502.Fetch: Byte;
begin
  if lookup[opcode].addrmode <> @_IMP then
    fetched := ReadByte(addr_abs);
  Result := fetched;
end;

// Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
function Tolc6502._IMP: Byte;
begin
  fetched := a;
  Result := 0;
end;

// Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
function Tolc6502._IMM: Byte;
begin
  addr_abs := pc;
  Inc(pc);
  Result := 0;
end;

// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
function Tolc6502._ZP0: Byte;
begin
  addr_abs := ReadByte(pc);
  Inc(pc);
  addr_abs := addr_abs and $00FF;
  Result := 0;
end;

// Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
function Tolc6502._ZPX: Byte;
begin
  addr_abs := ReadByte(pc) + x;
  Inc(pc);
  addr_abs := addr_abs and $00FF;
  Result := 0;
end;

// Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
function Tolc6502._ZPY: Byte;
begin
  addr_abs := ReadByte(pc) + y;
  Inc(pc);
  addr_abs := addr_abs and $00FF;
  Result := 0;
end;

// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
function Tolc6502._REL: Byte;
begin
  addr_rel := ReadByte(pc);
  Inc(pc);
  if (addr_rel and $80) <> 0 then
    addr_rel := addr_rel or $FF00;
  Result := 0;
end;

// Address Mode: Absolute
// A full 16-bit address is loaded and used
function Tolc6502._ABS: Byte;
var
  lo_byte, hi_byte: Word;
begin
  lo_byte := ReadByte(pc);
  Inc(pc);
  hi_byte := ReadByte(pc);
  Inc(pc);

  addr_abs := (hi_byte shl 8) or lo_byte;
  Result := 0;
end;

// Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
function Tolc6502._ABX: Byte;
var
  lo_byte, hi_byte: Word;
begin
  lo_byte := ReadByte(pc);
  Inc(pc);
  hi_byte := ReadByte(pc);
  Inc(pc);

  addr_abs := (hi_byte shl 8) or lo_byte;
  addr_abs += x;

  if (addr_abs and $FF00) <> (hi_byte shl 8) then
    Result := 1
  else
    Result := 0;
end;

// Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
function Tolc6502._ABY: Byte;
var
  lo_byte, hi_byte: Word;
begin
  lo_byte := ReadByte(pc);
  Inc(pc);
  hi_byte := ReadByte(pc);
  Inc(pc);

  addr_abs := (hi_byte shl 8) or lo_byte;
  addr_abs += y;

  if (addr_abs and $FF00) <> (hi_byte shl 8) then
    Result := 1
  else
    Result := 0;
end;

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
function Tolc6502._IND: Byte;
var
  ptr_lo, ptr_hi, ptr: Word;
begin
  ptr_lo := ReadByte(pc);
  Inc(pc);
  ptr_hi := ReadByte(pc);
  Inc(pc);

  ptr := (ptr_hi shl 8) or ptr_lo;

  if ptr_lo = $00FF then // simulate page boundary hardware bug
    addr_abs := (ReadByte(ptr and $FF00) shl 8) or ReadByte(ptr + 0)
  else // behave normally
    addr_abs := (ReadByte(ptr + 1) shl 8) or ReadByte(ptr + 0);

  Result := 0;
end;

// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
function Tolc6502._IZX: Byte;
var
  t, lo_byte, hi_byte: Word;
begin
  t := ReadByte(pc);
  Inc(pc);

  lo_byte := ReadByte((t + x) and $00FF);
  hi_byte := ReadByte((t + x + 1) and $00FF);

  addr_abs := (hi_byte shl 8) or lo_byte;

  Result := 0;
end;

// Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
function Tolc6502._IZY: Byte;
var
  t, lo_byte, hi_byte: Word;
begin
  t := ReadByte(pc);
  Inc(pc);

  lo_byte := ReadByte(t and $00FF);
  hi_byte := ReadByte((t + 1) and $00FF);

  addr_abs := (hi_byte shl 8) or lo_byte;
  addr_abs += y;

  if (addr_abs and $FF00) <> (hi_byte shl 8) then
    Result := 1
  else
    Result := 0;
end;

function Tolc6502._ADC: Byte;
begin
  // Grab the data that we are adding to the accumulator
	Fetch;

  // Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
  temp := Word(a) + Word(fetched) + Word(Ord(GetFlag(FLAGS6502_C)));

  // The carry flag out exists in the high byte bit 0
  SetFlag(FLAGS6502_C, temp > 255);

  // The zero flag is set if the result is 0
  SetFlag(FLAGS6502_Z, (temp and $00FF) = 0);

  // The signed Overflow flag is set based on all that up there! :D
  SetFlag(FLAGS6502_V, ((((not Word(a)) xor (Word(fetched))) and (Word(a) xor (Word(temp)))) and $80) <> 0);

  // The negative flag is set to the most significant bit of the result
  SetFlag(FLAGS6502_N, (temp and $80) <> 0);

  // Load the result into the accumulator (it's 8-bit dont forget!)
  a := temp and $00FF;

  // This instruction has the potential to require an additional clock cycle
  Result := 1;
end;

// Instruction: Bitwise Logic AND
// Function:    A = A & M
// Flags Out:   N, Z
function Tolc6502._AND: Byte;
begin
  Fetch;
  a := a and fetched;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 1;
end;

// Instruction: Arithmetic Shift Left
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
function Tolc6502._ASL: Byte;
begin
  Fetch;
  temp := Word(fetched) shl 1;
  SetFlag(FLAGS6502_C, (temp and $FF00) > 0);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = 0);
  SetFlag(FLAGS6502_N, (temp and $80) <> 0);
  if lookup[opcode].addrmode = @_IMP then
    a := temp and $00FF
  else
    WriteByte(addr_abs, temp and $00FF);
  Result := 0;
end;

// Instruction: Branch if Carry Clear
// Function:    if(C == 0) pc = address
function Tolc6502._BCC: Byte;
begin
  if not GetFlag(FLAGS6502_C) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Branch if Carry Set
// Function:    if(C == 1) pc = address
function Tolc6502._BCS: Byte;
begin
  if GetFlag(FLAGS6502_C) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Branch if Equal
// Function:    if(Z == 1) pc = address
function Tolc6502._BEQ: Byte;
begin
  if GetFlag(FLAGS6502_Z) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

function Tolc6502._BIT: Byte;
begin
  Fetch;
  temp := a and fetched;
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $00);
  SetFlag(FLAGS6502_N, (fetched and (1 shl 7)) <> 0);
  SetFlag(FLAGS6502_V, (fetched and (1 shl 6)) <> 0);
  Result := 0;
end;

// Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
function Tolc6502._BMI: Byte;
begin
  if GetFlag(FLAGS6502_N) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Branch if Not Equal
// Function:    if(Z == 0) pc = address
function Tolc6502._BNE: Byte;
begin
  if not GetFlag(FLAGS6502_Z) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Branch if Positive
// Function:    if(N == 0) pc = address
function Tolc6502._BPL: Byte;
begin
  if not GetFlag(FLAGS6502_N) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Break
// Function:    Program Sourced Interrupt
function Tolc6502._BRK: Byte;
begin
  Inc(pc);

  SetFlag(FLAGS6502_I, True);
  WriteByte($0100 + stkp, (pc shr 8) and $00FF);
  Dec(stkp);
  WriteByte($0100 + stkp, pc and $00FF);
  Dec(stkp);

  SetFlag(FLAGS6502_B, True);
  WriteByte($0100 + stkp, status);
  Dec(stkp);
  SetFlag(FLAGS6502_B, False);

  pc := Word(ReadByte($FFFE)) or (Word(ReadByte($FFFF)) shl 8);
  Result := 0;
end;

// Instruction: Branch if Overflow Clear
// Function:    if(V == 0) pc = address
function Tolc6502._BVC: Byte;
begin
  if not GetFlag(FLAGS6502_V) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Branch if Overflow Set
// Function:    if(V == 1) pc = address
function Tolc6502._BVS: Byte;
begin
  if GetFlag(FLAGS6502_V) then
  begin
    Inc(cycles);
    addr_abs := pc + addr_rel;

    if (addr_abs and $FF00) <> (pc and $FF00) then Inc(cycles);
    pc := addr_abs;
  end;
  Result := 0;
end;

// Instruction: Clear Carry Flag
// Function:    C = 0
function Tolc6502._CLC: Byte;
begin
  SetFlag(FLAGS6502_C, False);
  Result := 0;
end;

// Instruction: Clear Decimal Flag
// Function:    D = 0
function Tolc6502._CLD: Byte;
begin
  SetFlag(FLAGS6502_D, False);
  Result := 0;
end;

// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
function Tolc6502._CLI: Byte;
begin
  SetFlag(FLAGS6502_I, False);
  Result := 0;
end;

// Instruction: Clear Overflow Flag
// Function:    V = 0
function Tolc6502._CLV: Byte;
begin
  SetFlag(FLAGS6502_V, False);
  Result := 0;
end;

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
function Tolc6502._CMP: Byte;
begin
  Fetch;
  temp := Word(a) - Word(fetched);
  SetFlag(FLAGS6502_C, a >= fetched);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  Result := 1;
end;

// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
function Tolc6502._CPX: Byte;
begin
  Fetch;
  temp := Word(x) - Word(fetched);
  SetFlag(FLAGS6502_C, x >= fetched);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  Result := 0;
end;

// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
function Tolc6502._CPY: Byte;
begin
  Fetch;
  temp := Word(y) - Word(fetched);
  SetFlag(FLAGS6502_C, y >= fetched);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  Result := 0;
end;

// Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
function Tolc6502._DEC: Byte;
begin
  Fetch;
  temp := fetched - 1;
  WriteByte(addr_abs, temp and $00FF);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  Result := 0;
end;

// Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
function Tolc6502._DEX: Byte;
begin
  Dec(x);
  SetFlag(FLAGS6502_Z, x = $00);
  SetFlag(FLAGS6502_N, (x and $80) <> 0);
  Result := 0;
end;

// Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
function Tolc6502._DEY: Byte;
begin
  Dec(y);
  SetFlag(FLAGS6502_Z, y = $00);
  SetFlag(FLAGS6502_N, (y and $80) <> 0);
  Result := 0;
end;

// Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
function Tolc6502._EOR: Byte;
begin
  Fetch;
  a := a xor fetched;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 1;
end;

// Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
function Tolc6502._INC: Byte;
begin
  Fetch;
  temp := fetched + 1;
  WriteByte(addr_abs, temp and $00FF);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  Result := 0;
end;

// Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
function Tolc6502._INX: Byte;
begin
  Inc(x);
  SetFlag(FLAGS6502_Z, x = $00);
  SetFlag(FLAGS6502_N, (x and $80) <> 0);
  Result := 0;
end;

// Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
function Tolc6502._INY: Byte;
begin
  Inc(y);
  SetFlag(FLAGS6502_Z, y = $00);
  SetFlag(FLAGS6502_N, (y and $80) <> 0);
  Result := 0;
end;

// Instruction: Jump To Location
// Function:    pc = address
function Tolc6502._JMP: Byte;
begin
  pc := addr_abs;
  Result := 0;
end;

// Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc = address
function Tolc6502._JSR: Byte;
begin
  Dec(pc);

  WriteByte($0100 + stkp, (pc shr 8) and $00FF);
  Dec(stkp);
  WriteByte($0100 + stkp, pc and $00FF);
  Dec(stkp);

  pc := addr_abs;
  Result := 0;
end;

// Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
function Tolc6502._LDA: Byte;
begin
  Fetch;
  a := fetched;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <>0);
  Result := 1;
end;

// Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
function Tolc6502._LDX: Byte;
begin
  Fetch;
  x := fetched;
  SetFlag(FLAGS6502_Z, x = $00);
  SetFlag(FLAGS6502_N, (x and $80) <>0);
  Result := 1;
end;

// Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
function Tolc6502._LDY: Byte;
begin
  Fetch;
  y := fetched;
  SetFlag(FLAGS6502_Z, y = $00);
  SetFlag(FLAGS6502_N, (y and $80) <>0);
  Result := 1;
end;

function Tolc6502._LSR: Byte;
begin
  Fetch;
  SetFlag(FLAGS6502_C, (fetched and $0001) <> 0);
  temp := fetched shr 1;
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  if lookup[opcode].addrmode = @_IMP then
    a := temp and $00FF
  else
    WriteByte(addr_abs, temp and $00FF);
  Result := 0;
end;

function Tolc6502._NOP: Byte;
begin
  case opcode of
  $1C, $3C, $5C, $7C, $DC, $FC: Result := 1;
  else
    Result := 0;
  end;
end;

// Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
function Tolc6502._ORA: Byte;
begin
  Fetch;
  a := a or fetched;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 1;
end;

// Instruction: Push Accumulator to Stack
// Function:    A -> stack
function Tolc6502._PHA: Byte;
begin
  WriteByte($0100 + stkp, a);
  Dec(stkp);
  Result := 0;
end;

// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
function Tolc6502._PHP: Byte;
begin
  WriteByte($0100 + stkp, status or FLAGS6502_B or FLAGS6502_U);
  SetFlag(FLAGS6502_B, False);
  SetFlag(FLAGS6502_U, False);
  Dec(stkp);
  Result := 0;
end;

// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
function Tolc6502._PLA: Byte;
begin
  Inc(stkp);
  a := ReadByte($0100 + stkp);
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 0;
end;

// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
function Tolc6502._PLP: Byte;
begin
  Inc(stkp);
  status := ReadByte($0100 + stkp);
  SetFlag(FLAGS6502_U, True);
  Result := 0;
end;

function Tolc6502._ROL: Byte;
begin
  Fetch;
  temp := (Word(fetched) shl 1) or Ord(GetFlag(FLAGS6502_C));
  SetFlag(FLAGS6502_C, (temp and $FF00) <> 0);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $0000);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  if lookup[opcode].addrmode = @_IMP then
    a := temp and $00FF
  else
    WriteByte(addr_abs, temp and $00FF);
  Result := 0;
end;

function Tolc6502._ROR: Byte;
begin
  Fetch;
  temp := Word(Ord(GetFlag(FLAGS6502_C)) shl 7) or (fetched shr 1);
  SetFlag(FLAGS6502_C, (fetched and $01) <> 0);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = $00);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  if lookup[opcode].addrmode = @_IMP then
    a := temp and $00FF
  else
    WriteByte(addr_abs, temp and $00FF);
  Result := 0;
end;

function Tolc6502._RTI: Byte;
begin
  Inc(stkp);
  status := ReadByte($0100 + stkp);
  status := status and (not FLAGS6502_B);
  status := status and (not FLAGS6502_U);

  Inc(stkp);
  pc := ReadByte($0100 + stkp);
  Inc(stkp);
  pc := pc or (Word(ReadByte($0100 + stkp)) shl 8);
  Result := 0;
end;

function Tolc6502._RTS: Byte;
begin
  Inc(stkp);
  pc := ReadByte($0100 + stkp);
  Inc(stkp);
  pc := pc or (ReadByte($0100 + stkp) shl 8);

  Inc(pc);
  Result := 0;
end;

function Tolc6502._SBC: Byte;
var
  value: Word;
begin
  Fetch;

  // Operating in 16-bit domain to capture carry out

	// We can invert the bottom 8 bits with bitwise xor
  value := Word(fetched) xor $00FF;

  // Notice this is exactly the same as addition from here!
  temp := Word(a) + value + Word(Ord(GetFlag(FLAGS6502_C)));
  SetFlag(FLAGS6502_C, (temp and $FF00) <> 0);
  SetFlag(FLAGS6502_Z, (temp and $00FF) = 0);
  SetFlag(FLAGS6502_V, (((temp xor Word(a)) and (temp xor value)) and $0080) <> 0);
  SetFlag(FLAGS6502_N, (temp and $0080) <> 0);
  a := temp and $00FF;
  Result := 1;
end;

// Instruction: Set Carry Flag
// Function:    C = 1
function Tolc6502._SEC: Byte;
begin
  SetFlag(FLAGS6502_C, True);
  Result := 0;
end;

// Instruction: Set Decimal Flag
// Function:    D = 1
function Tolc6502._SED: Byte;
begin
  SetFlag(FLAGS6502_D, True);
  Result := 0;
end;

// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
function Tolc6502._SEI: Byte;
begin
  SetFlag(FLAGS6502_I, True);
  Result := 0;
end;

// Instruction: Store Accumulator at Address
// Function:    M = A
function Tolc6502._STA: Byte;
begin
  WriteByte(addr_abs, a);
  Result := 0;
end;

// Instruction: Store X Register at Address
// Function:    M = X
function Tolc6502._STX: Byte;
begin
  WriteByte(addr_abs, x);
  Result := 0;
end;

// Instruction: Store Y Register at Address
// Function:    M = Y
function Tolc6502._STY: Byte;
begin
  WriteByte(addr_abs, y);
  Result := 0;
end;

// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
function Tolc6502._TAX: Byte;
begin
  x := a;
  SetFlag(FLAGS6502_Z, x = $00);
  SetFlag(FLAGS6502_N, (x and $80) <> 0);
  Result := 0;
end;

// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
function Tolc6502._TAY: Byte;
begin
  y := a;
  SetFlag(FLAGS6502_Z, y = $00);
  SetFlag(FLAGS6502_N, (y and $80) <> 0);
  Result := 0;
end;

// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
function Tolc6502._TSX: Byte;
begin
  x := stkp;
  SetFlag(FLAGS6502_Z, x = $00);
  SetFlag(FLAGS6502_N, (x and $80) <> 0);
  Result := 0;
end;

// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
function Tolc6502._TXA: Byte;
begin
  a := x;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 0;
end;

// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
function Tolc6502._TXS: Byte;
begin
  stkp := x;
  Result := 0;
end;

// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
function Tolc6502._TYA: Byte;
begin
  a := y;
  SetFlag(FLAGS6502_Z, a = $00);
  SetFlag(FLAGS6502_N, (a and $80) <> 0);
  Result := 0;
end;

// This function captures illegal opcodes
function Tolc6502._XXX: Byte;
begin
  Result := 0;
end;

constructor Tolc6502.Create;
begin
  a := $00;
  x := $00;
  y := $00;
  stkp := $00;
  pc := $0000;
  status := $00;
  fetched := $00;
  temp := $0000;
  addr_abs := $0000;
  addr_rel := $00;
  opcode := $00;
  cycles := $00;
  clock_count := 0;

  lookup := TInstructionList.Create(
    NewInstruction( 'BRK', @_BRK, @_IMM, 7 ),NewInstruction( 'ORA', @_ORA, @_IZX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 3 ),NewInstruction( 'ORA', @_ORA, @_ZP0, 3 ),NewInstruction( 'ASL', @_ASL, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'PHP', @_PHP, @_IMP, 3 ),NewInstruction( 'ORA', @_ORA, @_IMM, 2 ),NewInstruction( 'ASL', @_ASL, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'ORA', @_ORA, @_ABS, 4 ),NewInstruction( 'ASL', @_ASL, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BPL', @_BPL, @_REL, 2 ),NewInstruction( 'ORA', @_ORA, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'ORA', @_ORA, @_ZPX, 4 ),NewInstruction( 'ASL', @_ASL, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'CLC', @_CLC, @_IMP, 2 ),NewInstruction( 'ORA', @_ORA, @_ABY, 4 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'ORA', @_ORA, @_ABX, 4 ),NewInstruction( 'ASL', @_ASL, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),
		NewInstruction( 'JSR', @_JSR, @_ABS, 6 ),NewInstruction( 'AND', @_AND, @_IZX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( 'BIT', @_BIT, @_ZP0, 3 ),NewInstruction( 'AND', @_AND, @_ZP0, 3 ),NewInstruction( 'ROL', @_ROL, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'PLP', @_PLP, @_IMP, 4 ),NewInstruction( 'AND', @_AND, @_IMM, 2 ),NewInstruction( 'ROL', @_ROL, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'BIT', @_BIT, @_ABS, 4 ),NewInstruction( 'AND', @_AND, @_ABS, 4 ),NewInstruction( 'ROL', @_ROL, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BMI', @_BMI, @_REL, 2 ),NewInstruction( 'AND', @_AND, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'AND', @_AND, @_ZPX, 4 ),NewInstruction( 'ROL', @_ROL, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'SEC', @_SEC, @_IMP, 2 ),NewInstruction( 'AND', @_AND, @_ABY, 4 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'AND', @_AND, @_ABX, 4 ),NewInstruction( 'ROL', @_ROL, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),
		NewInstruction( 'RTI', @_RTI, @_IMP, 6 ),NewInstruction( 'EOR', @_EOR, @_IZX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 3 ),NewInstruction( 'EOR', @_EOR, @_ZP0, 3 ),NewInstruction( 'LSR', @_LSR, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'PHA', @_PHA, @_IMP, 3 ),NewInstruction( 'EOR', @_EOR, @_IMM, 2 ),NewInstruction( 'LSR', @_LSR, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'JMP', @_JMP, @_ABS, 3 ),NewInstruction( 'EOR', @_EOR, @_ABS, 4 ),NewInstruction( 'LSR', @_LSR, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BVC', @_BVC, @_REL, 2 ),NewInstruction( 'EOR', @_EOR, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'EOR', @_EOR, @_ZPX, 4 ),NewInstruction( 'LSR', @_LSR, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'CLI', @_CLI, @_IMP, 2 ),NewInstruction( 'EOR', @_EOR, @_ABY, 4 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'EOR', @_EOR, @_ABX, 4 ),NewInstruction( 'LSR', @_LSR, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),
		NewInstruction( 'RTS', @_RTS, @_IMP, 6 ),NewInstruction( 'ADC', @_ADC, @_IZX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 3 ),NewInstruction( 'ADC', @_ADC, @_ZP0, 3 ),NewInstruction( 'ROR', @_ROR, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'PLA', @_PLA, @_IMP, 4 ),NewInstruction( 'ADC', @_ADC, @_IMM, 2 ),NewInstruction( 'ROR', @_ROR, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'JMP', @_JMP, @_IND, 5 ),NewInstruction( 'ADC', @_ADC, @_ABS, 4 ),NewInstruction( 'ROR', @_ROR, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BVS', @_BVS, @_REL, 2 ),NewInstruction( 'ADC', @_ADC, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'ADC', @_ADC, @_ZPX, 4 ),NewInstruction( 'ROR', @_ROR, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'SEI', @_SEI, @_IMP, 2 ),NewInstruction( 'ADC', @_ADC, @_ABY, 4 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'ADC', @_ADC, @_ABX, 4 ),NewInstruction( 'ROR', @_ROR, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),
		NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( 'STA', @_STA, @_IZX, 6 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'STY', @_STY, @_ZP0, 3 ),NewInstruction( 'STA', @_STA, @_ZP0, 3 ),NewInstruction( 'STX', @_STX, @_ZP0, 3 ),NewInstruction( '???', @_XXX, @_IMP, 3 ),NewInstruction( 'DEY', @_DEY, @_IMP, 2 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( 'TXA', @_TXA, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'STY', @_STY, @_ABS, 4 ),NewInstruction( 'STA', @_STA, @_ABS, 4 ),NewInstruction( 'STX', @_STX, @_ABS, 4 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),
		NewInstruction( 'BCC', @_BCC, @_REL, 2 ),NewInstruction( 'STA', @_STA, @_IZY, 6 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'STY', @_STY, @_ZPX, 4 ),NewInstruction( 'STA', @_STA, @_ZPX, 4 ),NewInstruction( 'STX', @_STX, @_ZPY, 4 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),NewInstruction( 'TYA', @_TYA, @_IMP, 2 ),NewInstruction( 'STA', @_STA, @_ABY, 5 ),NewInstruction( 'TXS', @_TXS, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( '???', @_NOP, @_IMP, 5 ),NewInstruction( 'STA', @_STA, @_ABX, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),
		NewInstruction( 'LDY', @_LDY, @_IMM, 2 ),NewInstruction( 'LDA', @_LDA, @_IZX, 6 ),NewInstruction( 'LDX', @_LDX, @_IMM, 2 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'LDY', @_LDY, @_ZP0, 3 ),NewInstruction( 'LDA', @_LDA, @_ZP0, 3 ),NewInstruction( 'LDX', @_LDX, @_ZP0, 3 ),NewInstruction( '???', @_XXX, @_IMP, 3 ),NewInstruction( 'TAY', @_TAY, @_IMP, 2 ),NewInstruction( 'LDA', @_LDA, @_IMM, 2 ),NewInstruction( 'TAX', @_TAX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'LDY', @_LDY, @_ABS, 4 ),NewInstruction( 'LDA', @_LDA, @_ABS, 4 ),NewInstruction( 'LDX', @_LDX, @_ABS, 4 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),
		NewInstruction( 'BCS', @_BCS, @_REL, 2 ),NewInstruction( 'LDA', @_LDA, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'LDY', @_LDY, @_ZPX, 4 ),NewInstruction( 'LDA', @_LDA, @_ZPX, 4 ),NewInstruction( 'LDX', @_LDX, @_ZPY, 4 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),NewInstruction( 'CLV', @_CLV, @_IMP, 2 ),NewInstruction( 'LDA', @_LDA, @_ABY, 4 ),NewInstruction( 'TSX', @_TSX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),NewInstruction( 'LDY', @_LDY, @_ABX, 4 ),NewInstruction( 'LDA', @_LDA, @_ABX, 4 ),NewInstruction( 'LDX', @_LDX, @_ABY, 4 ),NewInstruction( '???', @_XXX, @_IMP, 4 ),
		NewInstruction( 'CPY', @_CPY, @_IMM, 2 ),NewInstruction( 'CMP', @_CMP, @_IZX, 6 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( 'CPY', @_CPY, @_ZP0, 3 ),NewInstruction( 'CMP', @_CMP, @_ZP0, 3 ),NewInstruction( 'DEC', @_DEC, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'INY', @_INY, @_IMP, 2 ),NewInstruction( 'CMP', @_CMP, @_IMM, 2 ),NewInstruction( 'DEX', @_DEX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( 'CPY', @_CPY, @_ABS, 4 ),NewInstruction( 'CMP', @_CMP, @_ABS, 4 ),NewInstruction( 'DEC', @_DEC, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BNE', @_BNE, @_REL, 2 ),NewInstruction( 'CMP', @_CMP, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'CMP', @_CMP, @_ZPX, 4 ),NewInstruction( 'DEC', @_DEC, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'CLD', @_CLD, @_IMP, 2 ),NewInstruction( 'CMP', @_CMP, @_ABY, 4 ),NewInstruction( 'NOP', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'CMP', @_CMP, @_ABX, 4 ),NewInstruction( 'DEC', @_DEC, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),
		NewInstruction( 'CPX', @_CPX, @_IMM, 2 ),NewInstruction( 'SBC', @_SBC, @_IZX, 6 ),NewInstruction( '???', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( 'CPX', @_CPX, @_ZP0, 3 ),NewInstruction( 'SBC', @_SBC, @_ZP0, 3 ),NewInstruction( 'INC', @_INC, @_ZP0, 5 ),NewInstruction( '???', @_XXX, @_IMP, 5 ),NewInstruction( 'INX', @_INX, @_IMP, 2 ),NewInstruction( 'SBC', @_SBC, @_IMM, 2 ),NewInstruction( 'NOP', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_SBC, @_IMP, 2 ),NewInstruction( 'CPX', @_CPX, @_ABS, 4 ),NewInstruction( 'SBC', @_SBC, @_ABS, 4 ),NewInstruction( 'INC', @_INC, @_ABS, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),
		NewInstruction( 'BEQ', @_BEQ, @_REL, 2 ),NewInstruction( 'SBC', @_SBC, @_IZY, 5 ),NewInstruction( '???', @_XXX, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 8 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'SBC', @_SBC, @_ZPX, 4 ),NewInstruction( 'INC', @_INC, @_ZPX, 6 ),NewInstruction( '???', @_XXX, @_IMP, 6 ),NewInstruction( 'SED', @_SED, @_IMP, 2 ),NewInstruction( 'SBC', @_SBC, @_ABY, 4 ),NewInstruction( 'NOP', @_NOP, @_IMP, 2 ),NewInstruction( '???', @_XXX, @_IMP, 7 ),NewInstruction( '???', @_NOP, @_IMP, 4 ),NewInstruction( 'SBC', @_SBC, @_ABX, 4 ),NewInstruction( 'INC', @_INC, @_ABX, 7 ),NewInstruction( '???', @_XXX, @_IMP, 7 )
  );
end;

destructor Tolc6502.Destroy;
begin
  inherited Destroy;
end;

// Forces the 6502 into a known state. This is hard-wired inside the CPU. The
// registers are set to 0x00, the status register is cleared except for unused
// bit which remains at 1. An absolute address is read from location 0xFFFC
// which contains a second address that the program counter is set to. This
// allows the programmer to jump to a known and programmable location in the
// memory to start executing from. Typically the programmer would set the value
// at location 0xFFFC at compile time.
procedure Tolc6502.Reset;
var
  lo_byte, hi_byte: Byte;
begin
  // Get address to set the program counter to
  addr_abs := $FFFC;
  lo_byte := ReadByte(addr_abs + 0);
  hi_byte := ReadByte(addr_abs + 1);

  // Set it
  pc := (Word(hi_byte) shl 8) or lo_byte;

  // Reset internal registers
  a := 0;
  x := 0;
  y := 0;
  stkp := $FD;
  status := $00 or FLAGS6502_U;

  // Clear internal helper variables
  addr_rel := $0000;
  addr_abs := $0000;
  fetched := $00;

  // Reset takes time
  cycles := 8;
end;

// Interrupt requests are a complex operation and only happen if the
// "disable interrupt" flag is 0. IRQs can happen at any time, but
// you dont want them to be destructive to the operation of the running
// program. Therefore the current instruction is allowed to finish
// (which I facilitate by doing the whole thing when cycles == 0) and
// then the current program counter is stored on the stack. Then the
// current status register is stored on the stack. When the routine
// that services the interrupt has finished, the status register
// and program counter can be restored to how they where before it
// occurred. This is impemented by the "RTI" instruction. Once the IRQ
// has happened, in a similar way to a reset, a programmable address
// is read form hard coded location 0xFFFE, which is subsequently
// set to the program counter.
procedure Tolc6502.Irq;
var
  lo_byte, hi_byte: Byte;
begin
  // If interrupts are allowed
  if GetFlag(FLAGS6502_I) then
  begin
    // Push the program counter to the stack. It's 16-bits dont
		// forget so that takes two pushes
    WriteByte($0100 + stkp, (pc shr 8) and $00FF);
    Dec(stkp);
    WriteByte($0100 + stkp, pc and $00FF);
    Dec(stkp);

    // Then push the status register to the stack
    SetFlag(FLAGS6502_B, False);
    SetFlag(FLAGS6502_U, True);
    SetFlag(FLAGS6502_I, True);
    WriteByte($0100 + stkp, status);
    Dec(stkp);

    // Read new program counter location from fixed address
    addr_abs := $FFFE;
    lo_byte := ReadByte(addr_abs + 0);
    hi_byte := ReadByte(addr_abs + 1);
    pc := (Word(hi_byte) shl 8) or lo_byte;

    // IRQs take time
    cycles := 7;
  end;

end;

// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
// same way as a regular IRQ, but reads the new program counter address
// form location 0xFFFA.
procedure Tolc6502.Nmi;
var
  lo_byte, hi_byte: Byte;
begin
  WriteByte($0100 + stkp, (pc shr 8) and $00FF);
  Dec(stkp);
  WriteByte($0100 + stkp, pc and $00FF);
  Dec(stkp);

  SetFlag(FLAGS6502_B, False);
  SetFlag(FLAGS6502_U, True);
  SetFlag(FLAGS6502_I, True);
  WriteByte($0100 + stkp, status);
  Dec(stkp);

  addr_abs := $FFFA;
  lo_byte := ReadByte(addr_abs + 0);
  hi_byte := ReadByte(addr_abs + 1);
  pc := (Word(hi_byte) shl 8) or lo_byte;

  cycles := 8;
end;

// Perform one clock cycles worth of emulation
procedure Tolc6502.Clock;
var
{$IfDef LOGMODE}
  log_pc: Word;
{$EndIf}
  additional_cycle1, additional_cycle2: Byte;
begin
  // Each instruction requires a variable number of clock cycles to execute.
	// In my emulation, I only care about the final result and so I perform
	// the entire computation in one hit. In hardware, each clock cycle would
	// perform "microcode" style transformations of the CPUs state.
	//
	// To remain compliant with connected devices, it's important that the
	// emulation also takes "time" in order to execute instructions, so I
	// implement that delay by simply counting down the cycles required by
	// the instruction. When it reaches 0, the instruction is complete, and
	// the next one is ready to be executed.
  if cycles = 0 then
  begin
    opcode := ReadByte(pc);
    {$IfDef LOGMODE}
    log_pc := pc;
    {$EndIf}

    // Always set the unused status flag bit to 1
    SetFlag(FLAGS6502_U, True);

    // Increment program counter, we read the opcode byte
    Inc(pc);

    // Get starting number of cycles
    cycles := lookup[opcode].cycles;

    // Perform fetch of intermmediate data using the
		// required addressing mode
    additional_cycle1 := lookup[opcode].addrmode();

    // Perform operation
    additional_cycle2 := lookup[opcode].operate();

    // The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cycles += (additional_cycle1 and additional_cycle2);

    // Always set the unused status flag bit to 1
    SetFlag(FLAGS6502_U, True);

    {$IfDef LOGMODE}
    // This logger dumps every cycle the entire processor state for analysis.
		// This can be used for debugging the emulation, but has little utility
		// during emulation. Its also very slow, so only use if you have to.
    WriteLn(logfile, Format(
      '%10d:%.2d PC:%.4X %s A:%.2X X:%.2X Y:%.2X %s%s%s%s%s%s%s%s STKP:%.2X',
      [
        clock_count, 0, log_pc, 'XXX', a, x, y,
        IfThen(GetFlag(FLAGS6502_N), 'N', '.'), IfThen(GetFlag(FLAGS6502_V), 'V', '.'),
        IfThen(GetFlag(FLAGS6502_U), 'U', '.'), IfThen(GetFlag(FLAGS6502_B), 'B', '.'),
        IfThen(GetFlag(FLAGS6502_D), 'D', '.'), IfThen(GetFlag(FLAGS6502_I), 'I', '.'),
        IfThen(GetFlag(FLAGS6502_Z), 'Z', '.'), IfThen(GetFlag(FLAGS6502_C), 'C', '.'),
        stkp
      ]));
    {$EndIf}
  end;
  // Increment global clock count - This is actually unused unless logging is enabled
	// but I've kept it in because its a handy watch variable for debugging
  Inc(clock_count);

  // Decrement the number of cycles remaining for this instruction
  Dec(cycles);
end;

function Tolc6502.Complete: Boolean;
begin
  Result := cycles = 0;
end;

procedure Tolc6502.ConnectBus(n: TObject);
begin
  bus := n;
end;

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
function Tolc6502.Disassemble(nStart, nStop: Word): TMapStrings;
var
  addr: Cardinal;
  line_addr: Word;
  value, lo_byte, hi_byte, op: Byte;
  sInst: String;
begin
  addr := nStart;
  value := $00; lo_byte := $00; hi_byte := $00;
  Result := TMapStrings.Create;
  Result.Sorted := True;
  line_addr := 0;

  // Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
  while addr <= nStop do
  begin
    line_addr := addr;

    // Prefix line with instruction address
    sInst := '$' + hexStr(addr, 4) + ': ';

    // Read instructin, and get its readable name
    op := TBus(bus).cpuRead(addr, True); Inc(addr);
    sInst += lookup[op].name + ' ';

    // Get operands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
    if lookup[op].addrmode = @_IMP then
    begin
      sInst += ' {IMP}';
    end
    else if lookup[op].addrmode = @_IMM then
    begin
      value := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '#$' + hexStr(value, 2) + ' {IMM}';
    end
    else if lookup[op].addrmode = @_ZP0 then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := $00;
      sInst += '$' + hexStr(lo_byte, 2) + ' {ZP0}';
    end
    else if lookup[op].addrmode = @_ZPX then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := $00;
      sInst += '$' + hexStr(lo_byte, 2) + ', X {ZPX}';
    end
    else if lookup[op].addrmode = @_ZP0 then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := $00;
      sInst += '$' + hexStr(lo_byte, 2) + ', Y {ZPY}';
    end
    else if lookup[op].addrmode = @_IZX then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := $00;
      sInst += '($' + hexStr(lo_byte, 2) + ', X) {IZX}';
    end
    else if lookup[op].addrmode = @_IZY then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := $00;
      sInst += '($' + hexStr(lo_byte, 2) + ', Y) {IZY}';
    end
    else if lookup[op].addrmode = @_ABS then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '$' + hexStr((Word(hi_byte) shl 8) or lo_byte, 4) + ' {ABS}';
    end
    else if lookup[op].addrmode = @_ABX then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '$' + hexStr((Word(hi_byte) shl 8) or lo_byte, 4) + ', X {ABX}';
    end
    else if lookup[op].addrmode = @_ABY then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '$' + hexStr((Word(hi_byte) shl 8) or lo_byte, 4) + ', Y {ABY}';
    end
    else if lookup[op].addrmode = @_IND then
    begin
      lo_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      hi_byte := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '($' + hexStr((Word(hi_byte) shl 8) or lo_byte, 4) + ') {IND}';
    end
    else if lookup[op].addrmode = @_REL then
    begin
      value := TBus(bus).cpuRead(addr, True); Inc(addr);
      sInst += '$' + hexStr(value, 2) + ' [$' + hexStr(addr + Int8(value), 4) + '] {REL}';
    end;
    // Add the formed string to a map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
    Result.Add(line_addr, sInst);
  end;
end;

{$IfDef LOGMODE}
initialization
  AssignFile(logfile, 'olcnes.txt');
  Rewrite(logfile);

finalization
  CloseFile(logfile);
{$EndIf}

end.

