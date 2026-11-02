

def fontSet : Array UInt8 := #[
  0xF0, 0x90, 0x90, 0x90, 0xF0, -- 0
  0x20, 0x60, 0x20, 0x20, 0x70, -- 1
  0xF0, 0x10, 0xF0, 0x80, 0xF0, -- 2
  0xF0, 0x10, 0xF0, 0x10, 0xF0, -- 3
  0x90, 0x90, 0xF0, 0x10, 0x10, -- 4
  0xF0, 0x80, 0xF0, 0x10, 0xF0, -- 5
  0xF0, 0x80, 0xF0, 0x90, 0xF0, -- 6
  0xF0, 0x10, 0x20, 0x40, 0x40, -- 7
  0xF0, 0x90, 0xF0, 0x90, 0xF0, -- 8
  0xF0, 0x90, 0xF0, 0x10, 0xF0, -- 9
  0xF0, 0x90, 0xF0, 0x90, 0x90, -- A
  0xE0, 0x90, 0xE0, 0x90, 0xE0, -- B
  0xF0, 0x80, 0x80, 0x80, 0xF0, -- C
  0xE0, 0x90, 0x90, 0x90, 0xE0, -- D
  0xF0, 0x80, 0xF0, 0x80, 0xF0, -- E
  0xF0, 0x80, 0xF0, 0x80, 0x80  -- F
]

structure State where
    memory: Fin 4096 -> UInt8
    vs : Fin 16 -> UInt8
    I : Fin 4096
    pc : Fin 4096
    stack : Fin 16 -> Fin 4096
    sp : Fin 16
    delayTimer: UInt8
    soundTimer: UInt8
    screen: Fin 64 -> Fin 32 -> Bool
    rng: StdGen
    keys: Fin 16 -> Bool


def State.init (seed: Nat): State := {
    memory := λaddr =>
        if addr.val < 80 then fontSet.getD addr.val 0 else 0
    vs := λ_ => 0
    I := 0
    pc := 512
    stack := λ_ => 0
    sp := 0
    delayTimer := 0
    soundTimer := 0
    screen := λ_ => λ_ => False
    rng := mkStdGen seed
    keys := λ_ => False
}

def State.tick (cpu: State) : State :=
  let new_dt := if cpu.delayTimer > 0 then cpu.delayTimer - 1 else 0
  let new_st := if cpu.soundTimer > 0 then cpu.soundTimer - 1 else 0
  {cpu with delayTimer:=new_dt, soundTimer:=new_st}

def State.loadROM (cpu: State) (rom: ByteArray): State :=
  let new_memory :=
    λ addr:Fin 4096 =>
      if 512 ≤ addr && addr ≤ (512 + rom.size) then rom.get! (addr-512)
      else cpu.memory (Fin.ofNat 4096 addr)
  {cpu with memory := new_memory}
